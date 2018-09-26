open Cmm

type symbol_kind = 
  | Sfunction
  | Sdata

type typed_expression =
  | Tconst_int of int
  | Tconst_natint of nativeint
  | Tconst_float of float
  | Tconst_symbol of string * symbol_kind
  | Tblockheader of nativeint * Debuginfo.t
  | Tvar of Ident.t * machtype * int32
  | Tlet of Ident.t * typed_expression * machtype * typed_expression * int32
  | Tassign of Ident.t * typed_expression * int32
  | Ttuple of typed_expression list
  | Top of operation * (typed_expression * machtype) list * Debuginfo.t * machtype
  | Tsequence of typed_expression * typed_expression
  | Tifthenelse of typed_expression * typed_expression * typed_expression * machtype
  | Tswitch of typed_expression * int array * typed_expression array * Debuginfo.t * machtype
  | Tloop of typed_expression
  | Tcatch of rec_flag * (int * Ident.t list * typed_expression) list * typed_expression * machtype
  | Texit of int * typed_expression list * int
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

type func_result = string * machtype * (machtype list)

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

let get_func name = 
  List.find_opt (fun (n, _, _) -> n = name) !functions

let blockheader_details header =
  let word_size = Nativeint.shift_right header 10 in
  let tag = (Nativeint.logand header 255n) in
  (word_size, tag)

let add_local env local =
  let locals = env.locals in
  if not (List.exists (fun f -> f = local) !locals) then (      
    env.locals := !locals @ [local]
  )

(* let isprefix s1 s2 =
  String.length s1 <= String.length s2
  && String.sub s2 0 (String.length s1) = s1

let is_generic_function name =
  List.exists
    (fun p -> isprefix p name)
    ["caml_apply"; "caml_curry"; "caml_send"; "caml_tuplify"] *)

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
  let add_local = 
    add_local env
  in
  let get_local name = (
    let name = ident name in
    let rec find counter = (function
      | l :: _ when fst l = name -> (counter, l)
      | _ :: rest -> find (counter + 1) rest
      | [] -> assert false) 
    in
    find 0 !locals) 
  in    
  let push p = 
    Stack.push p stack
  in
  let pop () = 
    ignore(Stack.pop stack)
  in     
  match e with 
    | Cconst_int i -> 
        push typ_int;
        Tconst_int i
    | Cconst_natint i -> 
        push typ_int;
        Tconst_natint i
    | Cconst_float f ->
        push typ_float;
        Tconst_float f
    | Cconst_symbol "dropme" ->
        push typ_void;
        Tconst_symbol ("dropme", Sdata)
    | Cconst_symbol s -> 
        let func = get_func s in
        let (t, k) = match func with 
        | Some _ -> (typ_int, Sfunction)
        | None -> (typ_int, Sdata)
        in
        push t;
        Tconst_symbol (s, k)
    | Cblockheader (n, d) -> 
        push typ_int;
        Tblockheader (n, d)
    | Cvar i ->
        let (pos, (_, t)) = get_local i in        
        push t;
        Tvar (i, t, Int32.of_int pos)
    | Clet (i, r, b) ->         
        let r = process {env with needs_return = true} r in
        add_local (ident i, Stack.top stack);
        let (pos, _) = get_local i in 
        let rt = Stack.top stack in
        pop ();
        Tlet (i, r, rt, process env b, Int32.of_int pos)        
    | Cassign (i, e) -> 
        let (pos, _) = get_local i in  
        let result = Tassign (i, process {env with needs_return = false} e, Int32.of_int pos) in
        pop ();
        push typ_void;
        add_local (ident i, Stack.top stack);
        result
    | Ctuple [] -> 
        push typ_void;
        Ttuple []
    | Ctuple el ->      
        let result = Ttuple (List.mapi (fun i e -> 
            (if i > 0 then 
                pop ();
            process env e)) el) in
        check_stack_plus_one ();
        result
    | Cop (o, el, d) ->
       let copied_stack = Stack.copy stack in
       let copied_block_stack = Stack.copy block_stack in
       let args = List.map (fun e -> 
          (ignore(process {env with stack = copied_stack; block_stack = copied_block_stack; needs_return = true} e);
          Stack.top copied_stack)
        ) el in       
        (match o, el with        
        | Capply mt, (Cconst_symbol s) :: _ -> 
            functions := !functions @ [(s, mt, List.tl args)];
        | Calloc, Cblockheader (header, _) :: rest ->                        
            let (_, tag) = blockheader_details header in
            let is_closure = ((Nativeint.to_int tag) land 255) == 247 in
            if is_closure then (
              List.iter (fun i ->   
                match i with 
                | Cconst_symbol s ->
                    (* if (is_generic_function s) then (
                        let str = String.to_seq s in
                        let s3 = String.of_seq (
                            Seq.filter (fun s -> match s with 
                            | 'a' .. 'z'
                            | 'A' .. 'Z'
                            | '_' -> false
                            | _ -> true) str 
                        ) in
                        print_endline ("check:" ^ s3)
                    ); *)
                    functions := !functions @ [(s, typ_int, [])]
                | _ -> ()
              ) rest                
            )
        | _ -> ());

        let operation_type = oper_result_type o in    
        let expected_length = Stack.length stack + List.length el in
        let processed = List.map (process {env with needs_return = true}) el in        
        let result = Top (o, List.combine processed args, d, operation_type) in         
        (if Stack.length stack <> expected_length then
            failwith ("Not correct:" ^ string_of_int (Stack.length stack) ^ " vs " ^ string_of_int expected_length)
        );
        List.iter (fun _ -> pop ()) el;                
        push operation_type;
        result
    | Csequence (f, Ctuple []) ->
        let processed_f = process {env with needs_return = true} f in
        pop ();
        check_stack_equal ();
        push typ_void;
        Tsequence (processed_f, Ttuple [])
    | Csequence (Clet _ as f, s) ->
        let processed_f = process {env with needs_return = false} f in    
        pop ();
        check_stack_equal ();
        Tsequence (processed_f, process env s)
    | Csequence (Ctuple [], s) ->
        Tsequence (Ttuple [], process env s)    
    | Csequence (f, s) -> 
        let processed_f = process env f in        
        pop ();
        check_stack_equal ();
        Tsequence (processed_f, process env s)
    | Cifthenelse (i, t, e) ->            
        Stack.push Bifthenelse block_stack;
        let processed_i = process env i in  
        pop ();            
        let processed_t = process env t in        
        let then_type = Stack.top stack in
        pop ();       
        let processed_e = process env e in        
        let else_type = Stack.top stack in
        pop ();
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
        push rt;
        ignore(Stack.pop block_stack);
        check_stack_plus_one ();
        result
    | Cswitch (e, ia, ea, d) -> 
        let match_ = process {env with needs_return = true} e in
        pop ();
        let cases = Array.map (process env) ea in
        let switch_result = ref typ_void in
        Array.iter (fun _ ->
            Stack.push Bswitch_case block_stack;
            let item = Stack.pop stack in 
            switch_result := item;
            ignore(Stack.pop block_stack);
        ) ea;         
        push !switch_result;
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
            add_local (ident i, typ_int)
          )) il; 
          let result = (i, il, process env expr) in
          print_endline "bwith 1";
          Stack.push (Bwith (i, Stack.top stack)) block_stack;          
          rt := Stack.top stack;
          pop ();
          result
        )) with_
        in
        let result = Tcatch (
          r, 
          with_exprs, 
          (
           print_endline "bcatch 1";
           Stack.push Bcatch block_stack;
           let result = process env body_ in            
           ignore(Stack.pop block_stack);
           print_endline "bcatch 2";
           ignore(Stack.pop block_stack);
           print_endline "bwith 2";
           pop ();
           result),
          (if needs_return then !rt else typ_void)
        )           
        in        
        push !rt;
        check_stack_plus_one();
        result
    | Cexit (i, el) -> 
        let found = ref false in
        let index = ref (-1) in
        Stack.iter (fun f -> 
            match f with 
            | Bwith (ix, t) when ix = i -> 
                (found := true;
                 push t)
            | _ -> 
                (if !found = false then
                    index := !index + 1
                else 
                    ())
        ) block_stack;
        assert !found;
        let result = Texit (i, List.map (process env) el, !index) in
        List.iter (fun _ -> pop()) el;
        result
    | Ctrywith (body, exn, handler) -> 
        let body = process env body in
        let rt = Stack.top stack in
        pop ();
        add_local (ident exn, rt);
        let handler = process env handler in
        pop ();
        let result = Ttrywith (body, exn, handler) in
        push rt;
        check_stack_plus_one();
        result

let add_types func_name fun_args e =
    let env = initial_env() in
    let stack = env.stack in
    let locals = env.locals in
    let add_local = add_local env in
    List.iter (fun (i, mt) ->
      add_local (ident i, mt)
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
    functions := !functions @ [(func_name, Stack.top stack, snd (List.split fun_args))];
    (result, Stack.top stack, !locals)