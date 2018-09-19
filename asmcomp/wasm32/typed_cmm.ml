open Cmm

type typed_expression =
  | Tconst_int of int
  | Tconst_natint of nativeint
  | Tconst_float of float
  | Tconst_symbol of string
  | Tblockheader of nativeint * Debuginfo.t
  | Tvar of Ident.t
  | Tlet of Ident.t * typed_expression * typed_expression
  | Tassign of Ident.t * typed_expression
  | Ttuple of typed_expression list
  | Top of operation * typed_expression list * Debuginfo.t * machtype
  | Tsequence of typed_expression * typed_expression
  | Tifthenelse of typed_expression * typed_expression * typed_expression * machtype
  | Tswitch of typed_expression * int array * typed_expression array * Debuginfo.t
  | Tloop of typed_expression
  | Tcatch of rec_flag * (int * Ident.t list * typed_expression) list * typed_expression
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

type local = string * machtype option

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
    locals: local list;
}

let initial_env () = 
  let stack : stack = Stack.create () in
  let block_stack : block_stack = Stack.create() in
  let locals : local list = [] in
  Stack.push Bfunction block_stack;
  { 
    stack; 
    block_stack; 
    needs_return = true; 
    locals 
  }

let rec process env e = 
  let stack = env.stack in
  let block_stack = env.block_stack in
  let needs_return = env.needs_return in
  let locals = env.locals in
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
    | Cconst_symbol s -> 
        print_endline s;
        Stack.push typ_int stack; (* potentially incorrect *)
        Tconst_symbol s
    | Cblockheader (n, d) -> 
        Stack.push typ_int stack;
        Tblockheader (n, d)
    | Cvar i -> (* potentially incorrect *)
        print_endline (i.Ident.name ^ "/" ^ string_of_int i.Ident.stamp);
        Stack.push typ_int stack;
        Tvar i    
    | Clet (i, r, b) -> 
        let r = process {env with needs_return = true} r in      
        ignore(Stack.pop stack);
        Tlet (i, r, process env b)        
    | Cassign (i, e) -> 
        let result = Tassign (i, process {env with needs_return = false} e) in
        ignore(Stack.pop stack);
        Stack.push typ_void stack;
        result
    | Ctuple [] -> 
        Stack.push typ_void stack;
        Ttuple []
    | Ctuple el ->         
        let result = Ttuple (List.mapi (fun i e -> 
            (if i > 0 then 
                ignore(Stack.pop stack);
            process env e)) el) in
        result 
    | Cop (o, el, d) ->
        let r = oper_result_type o in    
        let expected_length = Stack.length stack + List.length el in
        let processed = List.map (process {env with needs_return = true}) el in        
        let result = Top (o, processed, d, r) in         
        (if Stack.length stack <> expected_length then
            failwith ("Not correct:" ^ string_of_int (Stack.length stack) ^ " vs " ^ string_of_int expected_length)
        );
        List.iter (fun _ -> ignore(Stack.pop stack)) el;
        if needs_return then print_endline "needs a return..."
        else print_endline "no return needed";
        Stack.push r stack;
        result
    | Csequence (f, Cconst_int 1) -> 
        print_endline "mkay!";
        let before = Stack.length stack in
        let processed_f = process {env with needs_return = false} f in
        ignore(Stack.pop stack);
        assert (Stack.length stack = before);
        Stack.push typ_int stack;
        Tsequence (processed_f, Tconst_int 1)
    | Csequence (f, Ctuple []) ->
        let before = Stack.length stack in
        let processed_f = process {env with needs_return = true} f in
        ignore(Stack.pop stack);
        assert (Stack.length stack = before);
        Stack.push typ_void stack;
        Tsequence (processed_f, Ttuple [])
    | Csequence (Clet _ as f, s) ->
        let before = Stack.length stack in
        let processed_f = process {env with needs_return = false} f in    
        ignore(Stack.pop stack);
        assert (Stack.length stack = before);
        Tsequence (processed_f, process env s)
    | Csequence (Ctuple [], s) ->
        Tsequence (Ttuple [], process env s)        
    | Csequence (f, s) -> 
        let before = Stack.length stack in
        let processed_f = process env f in        
        ignore(Stack.pop stack);
        assert (Stack.length stack = before);
        Tsequence (processed_f, process env s)
    | Cifthenelse (i, t, e) ->            
        Stack.push Bifthenelse block_stack;
        let before_stack_length = Stack.length stack in 
        let processed_i = process env i in  
        ignore(Stack.pop stack);      
        let t_stack = Stack.copy stack in
        let t_env = {
          stack = t_stack; 
          block_stack = (Stack.copy block_stack); 
          needs_return; 
          locals
        } in
        let processed_t = process t_env t in
        let e_stack = Stack.copy stack in
        let e_env = {
          stack = e_stack; 
          block_stack = (Stack.copy block_stack); 
          needs_return; 
          locals
        } in
        let processed_e = process e_env e in        
        let then_type = Stack.top t_stack in
        let else_type = Stack.top e_stack in
        if not (compare then_type else_type) then (
            print_stack_type then_type;
            print_stack_type else_type;
            failwith "Then and else need to return the same type";
        );
        let rt = if Stack.length t_stack > Stack.length stack then Stack.top t_stack else Stack.top e_stack in
        let result = Tifthenelse (processed_i, processed_t, processed_e, rt) in    
            
        Stack.push rt stack;
        ignore(Stack.pop block_stack);
        assert (Stack.length stack = before_stack_length + 1);
        result
    | Cswitch (e, ia, ea, d) -> 
        let check = Stack.length stack in
        let copied_stack = Stack.copy stack in
        let copied_block_stack = Stack.copy block_stack in
        let copied_env = {stack = copied_stack; block_stack = copied_block_stack; needs_return; locals} in
        let result = Tswitch (process {copied_env with needs_return = true} (* not sure *) e, ia, Array.map (process copied_env) ea, d) in
        let switch_result = ref typ_void in
        Array.iter (fun _ -> 
            let item = Stack.pop copied_stack in 
            switch_result := item
        ) ea;         
        Stack.push !switch_result stack;
        assert (check = Stack.length stack - 1);
        result
    | Cloop e -> 
        Stack.push Bloop block_stack;
        let result = Tloop (process env e) in
        ignore(Stack.pop block_stack);
        result
    | Ccatch (r, with_, body_) -> 
        let ccatch_stack = Stack.copy stack in
        let ccatch_block_stack = Stack.copy block_stack in
        let ccatch_env = { stack = ccatch_stack; block_stack = ccatch_block_stack; needs_return; locals } in
        let with_exprs = List.map(fun (i, il, expr) -> (
          let result = (i, il, process ccatch_env expr) in
          Stack.push (Bwith (i, Stack.top ccatch_stack)) ccatch_block_stack;
          result
        )) with_
        in
        let result = Tcatch (
          r, 
          with_exprs, 
          (let result = process ccatch_env body_ in 
           result)) 
        in
        if Stack.length ccatch_stack > Stack.length stack then
            Stack.push (Stack.top ccatch_stack) stack;
        result
    | Cexit (i, el) -> 
        print_endline ("-> exit " ^ string_of_int i);
        let found = ref false in
        Stack.iter (fun f -> 
            match f with 
            | Bwith (ix, t) when ix = i -> 
                (found := true;
                 Stack.push t stack)
            | _ -> ()
        ) block_stack;
        assert !found;
        print_endline "cexit end";
        let result = Texit (i, List.map (process env) el) in
        List.iter (fun _ -> ignore(Stack.pop stack)) el;
        result
    | Ctrywith (e, i, c) -> 
        let stack_size_before = Stack.length stack in
        let copied_env = {
          stack = (Stack.copy stack); 
          block_stack = (Stack.copy block_stack); 
          needs_return; 
          locals
        } in
        let result = Ttrywith (process env e, i, process copied_env c) in
        assert (stack_size_before = Stack.length stack - 1);
        result

let add_types func_name e =
    print_endline ("Adding types to function:" ^ func_name);    
    let env = initial_env() in
    let stack = env.stack in
    let block_stack = env.stack in
    let result = process env e in
    print_endline "finished";
    (if (Stack.length stack <> 1) then (
        Stack.iter print_stack_type stack;
        failwith ("Stack was expected to have a length of 1, but got " ^ string_of_int (Stack.length stack)))
    );
    (if (Stack.length block_stack <> 1) then 
        failwith ("Block stack was expected to have a length of 1, but got " ^ string_of_int (Stack.length block_stack));
    );
    result