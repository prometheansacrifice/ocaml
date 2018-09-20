open Cmm

type symbol_kind = 
  | Sfunction
  | Sdata

type typed_expression =
  | Tconst_int of int
  | Tconst_natint of nativeint
  | Tconst_float of float
  | Tconst_symbol of string * symbol_kind * machtype
  | Tblockheader of nativeint * Debuginfo.t
  | Tvar of Ident.t * machtype
  | Tlet of Ident.t * typed_expression * typed_expression
  | Tassign of Ident.t * typed_expression
  | Ttuple of typed_expression list
  | Top of operation * typed_expression list * Debuginfo.t * machtype
  | Tsequence of typed_expression * typed_expression
  | Tifthenelse of typed_expression * typed_expression * typed_expression * machtype
  | Tswitch of typed_expression * int array * typed_expression array * Debuginfo.t * machtype
  | Tloop of typed_expression
  | Tcatch of rec_flag * (int * Ident.t list * typed_expression) list * typed_expression * machtype
  | Texit of int * typed_expression list
  | Ttrywith of typed_expression * Ident.t * typed_expression

type stack = machtype Stack.t

let print_stack_type e = 
  print_endline ("Stack type:" ^ 
    match e with 
    | [|Val|] -> "Val"
    | [|Int|] -> "Int"
    | [|Addr|] -> "Addr"
    | [|Float|] -> "Float"
    | [||] -> "Void"
    | _ -> "unknown")

let compare a b = 
  match a, b with 
  | [|Val|], [|Int|] 
  | [|Val|], [|Addr|]
  | [|Int|], [|Val|] 
  | [|Int|], [|Addr|]
  | [|Addr|], [|Int|]
  | [|Addr|], [|Val|] -> true     
  | _ -> a = b

type block = 
  | Bloop
  | Bifthenelse
  | Bswitch
  | Bswitch_case
  | Bwith of int * machtype
  | Bcatch
  | Bcheckbound1
  | Bcheckbound2
  | Bfunction

type block_stack = block Stack.t

type local = string * machtype

type func_result = string * machtype

let functions:func_result list ref = ref []

let oper_result_type = function
  | Capply ty -> ty
  | Cextcall(_s, ty, _alloc, _) -> ty
  | Cload (c, _) ->
      begin match c with
      | Word_val -> typ_val
      | Single | Double | Double_u -> typ_float
      | _ -> typ_int
      end
  | Calloc -> typ_val
  | Cstore (_c, _) -> typ_void
  | Caddi | Csubi | Cmuli | Cmulhi | Cdivi | Cmodi |
    Cand | Cor | Cxor | Clsl | Clsr | Casr |
    Ccmpi _ | Ccmpa _ | Ccmpf _ -> typ_int
  | Caddv -> typ_val
  | Cadda -> typ_addr
  | Cnegf | Cabsf | Caddf | Csubf | Cmulf | Cdivf -> typ_float
  | Cfloatofint -> typ_float
  | Cintoffloat -> typ_int
  | Craise _ -> typ_int
  | Ccheckbound -> typ_void

type env = {
    stack: stack;
    block_stack: block_stack;
    needs_return: bool;
    locals: local list ref;
}

let initial_env () = 
  let stack : stack = Stack.create () in
  let block_stack : block_stack = Stack.create() in
  let locals : local list ref = ref [] in
  Stack.push Bfunction block_stack;
  { 
    stack; 
    block_stack; 
    needs_return = true; 
    locals 
  }

let ident i = 
  i.Ident.name ^ "/" ^ string_of_int i.Ident.stamp

let get_local locals name =
  List.find_opt (fun (n, _) -> n = name) locals  

let get_func name = 
  List.find_opt (fun (n, _) -> n = name) !functions

let blockheader_details header =
  let word_size = Nativeint.shift_right header 10 in
  let tag = (Nativeint.logand header 255n) in
  (word_size, tag)

let rec process env e = 
  let stack = env.stack in
  let block_stack = env.block_stack in
  let needs_return = env.needs_return in
  let locals = env.locals in
  let stack_size_before = Stack.length stack in
  let check_stack_plus_one () =
    assert (Stack.length stack = stack_size_before + 1)
  in
  let check_stack_equal () =
    assert (Stack.length stack = stack_size_before)
  in  
  match e with 
    | Cconst_int i -> 
        Stack.push typ_int stack;
        Tconst_int i
    | Cconst_natint i -> 
        Stack.push typ_int stack;
        Tconst_natint i
    | Cconst_float f ->
        Stack.push typ_float stack;
        Tconst_float f
    | Cconst_symbol "dropme" ->
        Stack.push typ_void stack;
        Tconst_symbol ("dropme", Sdata, typ_int)
    | Cconst_symbol s -> 
        let func = get_func s in
        let (k, t) = match func with 
        | Some (_, t) -> (Sfunction, t)
        | None -> (Sdata, typ_int)
        in
        Stack.push t stack;
        Tconst_symbol (s, k, t)
    | Cblockheader (n, d) -> 
        Stack.push typ_int stack;
        Tblockheader (n, d)
    | Cvar i ->
        let local = get_local !locals (ident i) in
        let t = match local with 
          | Some (_, t) -> t
          | None -> assert false
        in
        Stack.push t stack;
        Tvar (i, t)
    | Clet (i, r, b) -> 
        let r = process {env with needs_return = true} r in
        env.locals := !locals @ [(ident i, Stack.top stack)];
        ignore(Stack.pop stack);
        let result = Tlet (i, r, process env b) in
        result
    | Cassign (i, e) -> 
        let result = Tassign (i, process {env with needs_return = false} e) in
        ignore(Stack.pop stack);
        Stack.push typ_void stack;
        env.locals := !locals @ [(ident i, Stack.top stack)];
        result
    | Ctuple [] -> 
        Stack.push typ_void stack;
        Ttuple []
    | Ctuple el ->      
        let result = Ttuple (List.mapi (fun i e -> 
            (if i > 0 then 
                ignore(Stack.pop stack);
            process env e)) el) in
        check_stack_plus_one ();
        result
    | Cop (o, el, d) ->
        (match o, el with
        | Capply mt, (Cconst_symbol s) :: _ -> 
            functions := !functions @ [(s, mt)];
        | Calloc, Cblockheader (header, _) :: rest ->                        
            let (_, tag) = blockheader_details header in
            let is_closure = ((Nativeint.to_int tag) land 255) == 247 in
            if is_closure then (
              List.iter (fun i ->   
                match i with 
                | Cconst_symbol s -> 
                    functions := !functions @ [(s, typ_val)]
                | _ -> ()
              ) rest                
            )
        | _ -> ());

        let r = oper_result_type o in    
        let expected_length = Stack.length stack + List.length el in
        let processed = List.map (process {env with needs_return = true}) el in        
        let result = Top (o, processed, d, r) in         
        (if Stack.length stack <> expected_length then
            failwith ("Not correct:" ^ string_of_int (Stack.length stack) ^ " vs " ^ string_of_int expected_length)
        );
        List.iter (fun _ -> ignore(Stack.pop stack)) el;                
        Stack.push r stack;
        result
    | Csequence (f, Ctuple []) ->
        let processed_f = process {env with needs_return = true} f in
        ignore(Stack.pop stack);
        check_stack_equal ();
        Stack.push typ_void stack;
        Tsequence (processed_f, Ttuple [])
    | Csequence (Clet _ as f, s) ->
        let processed_f = process {env with needs_return = false} f in    
        ignore(Stack.pop stack);
        check_stack_equal ();
        Tsequence (processed_f, process env s)
    | Csequence (Ctuple [], s) ->
        Tsequence (Ttuple [], process env s)    
    | Csequence (f, s) -> 
        let processed_f = process env f in        
        ignore(Stack.pop stack);
        check_stack_equal ();
        Tsequence (processed_f, process env s)
    | Cifthenelse (i, t, e) ->            
        Stack.push Bifthenelse block_stack;
        let processed_i = process env i in  
        ignore(Stack.pop stack);              
        let processed_t = process env t in        
        let then_type = Stack.top stack in
        ignore(Stack.pop stack);        
        let processed_e = process env e in        
        let else_type = Stack.top stack in
        ignore(Stack.pop stack);
        let (processed_t, processed_e, rt) = 
          (* sanderspies: hack hack hack :-/ *)
          if not (compare then_type else_type) then (
            if then_type = typ_void then                            
              (processed_t, Tsequence(processed_e, Ttuple []), typ_void)
            else if else_type = typ_void then
              (Tsequence(processed_t, Ttuple []), processed_e, typ_void)
            else 
              failwith "Then and else need to return the same type"
          ) else (
            (processed_t, processed_e, then_type)
        )
        in
        let result = Tifthenelse (processed_i, processed_t, processed_e, (if needs_return then rt else typ_void)) in
        Stack.push rt stack;
        ignore(Stack.pop block_stack);
        check_stack_plus_one ();
        result
    | Cswitch (e, ia, ea, d) -> 
        let match_ = process {env with needs_return = true} e in
        ignore(Stack.pop stack);
        let cases = Array.map (process env) ea in
        let switch_result = ref typ_void in
        Array.iter (fun _ -> 
            let item = Stack.pop stack in 
            switch_result := item
        ) ea;         
        Stack.push !switch_result stack;
        check_stack_plus_one ();
        Tswitch (match_, ia, cases, d, Stack.top stack)
    | Cloop e -> 
        Stack.push Bloop block_stack;
        let result = Tloop (process env e) in
        ignore(Stack.pop block_stack);
        result
    | Ccatch (r, with_, body_) ->              
        let rt = ref typ_void in
        let with_exprs = List.map(fun (i, il, expr) -> (
          List.iter (fun i -> (            
            env.locals := !locals @ [(ident i, [|Int|])]
          )) il; 
          let result = (i, il, process env expr) in
          Stack.push (Bwith (i, Stack.top stack)) block_stack;          
          rt := Stack.top stack;
          ignore(Stack.pop stack);
          result
        )) with_
        in
        let result = Tcatch (
          r, 
          with_exprs, 
          (let result = process env body_ in             
           ignore(Stack.pop stack);
           result),
          (if needs_return then !rt else typ_void)
        )           
        in        
        Stack.push !rt stack;
        check_stack_plus_one();
        result
    | Cexit (i, el) -> 
        let found = ref false in
        Stack.iter (fun f -> 
            match f with 
            | Bwith (ix, t) when ix = i -> 
                (found := true;
                 Stack.push t stack)
            | _ -> ()
        ) block_stack;
        assert !found;
        let result = Texit (i, List.map (process env) el) in
        List.iter (fun _ -> ignore(Stack.pop stack)) el;
        result
    | Ctrywith (body, exn, handler) -> 
        let body = process env body in
        let rt = Stack.top stack in
        ignore(Stack.pop stack);
        env.locals := !locals @ [(ident exn, rt)];
        let handler = process env handler in
        ignore(Stack.pop stack);
        let result = Ttrywith (body, exn, handler) in
        Stack.push rt stack;
        check_stack_plus_one();
        result

let add_types func_name fun_args e =
    let env = initial_env() in
    let stack = env.stack in
    let locals = env.locals in
    List.iter (fun (i, mt) ->
      env.locals := !locals @ [(ident i, mt)]
    ) fun_args;
    let block_stack = env.stack in
    let result = process env e in
    (if (Stack.length stack <> 1) then (
        Stack.iter print_stack_type stack;
        failwith ("Stack was expected to have a length of 1, but got " ^ string_of_int (Stack.length stack)))
    );
    (if (Stack.length block_stack <> 1) then 
        failwith ("Block stack was expected to have a length of 1, but got " ^ string_of_int (Stack.length block_stack));
    );    
    functions := !functions @ [(func_name, Stack.top stack)];
    (result, Stack.top stack)