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

let return_required = true
let no_return_required = false

(* let current_no_of_types = ref 0
let check_types msg add_types =
    current_no_of_types := !current_no_of_types + add_types;
    if current_no_of_types <> Stack.length stack then
        failwith msg *)

let rec process stack block_stack needs_return = function
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
        Stack.push typ_int stack; (* potentially incorrect *)
        Tconst_symbol s
    | Cblockheader (n, d) -> 
        Stack.push typ_int stack;
        Tblockheader (n, d)
    | Cvar i -> (* potentially incorrect *)
        Stack.push typ_int stack;
        Tvar i
    | Clet (i, r, b) -> 
        let r = process stack block_stack return_required r in      
        ignore(Stack.pop stack);
        let result = Tlet (i, r, process stack block_stack return_required b) in
        result
    | Cassign (i, e) -> 
        let result = Tassign (i, process stack block_stack no_return_required e) in
        ignore(Stack.pop stack);
        result
    | Ctuple el ->         
        let result = Ttuple (List.mapi (fun i e -> 
            (if i > 0 then 
                ignore(Stack.pop stack);
            process stack block_stack needs_return e)) el) in
        result
    | Cop (o, el, d) ->
        let r = oper_result_type o in    
        let expected_length = Stack.length stack + List.length el in
        let processed = List.map (process stack block_stack return_required) el in        
        let result = Top (o, processed, d, r) in         
        (if Stack.length stack <> expected_length then
            failwith ("Not correct:" ^ string_of_int (Stack.length stack) ^ " vs " ^ string_of_int expected_length)
        );
        List.iter (fun _ -> ignore(Stack.pop stack)) el;
        Stack.push r stack; (* typ_void causes challenges *)
        result
    | Csequence (f, Ctuple []) ->        
        let processed_f = process stack block_stack return_required f in
        ignore(Stack.pop stack);
        Tsequence (processed_f, Ttuple [])
    | Csequence (Clet _ as f, s) ->
        let a = Stack.length stack in
        let processed_f = process stack block_stack no_return_required f in    
        if a <> Stack.length stack then
            ignore(Stack.pop stack);
        Tsequence (processed_f, process stack block_stack needs_return s)
    | Csequence (Ctuple [], s) ->
        Tsequence (Ttuple [], process stack block_stack needs_return s)        
    | Csequence (f, s) -> 
        (* print_endline "at"  ; *)
        let processed_f = process stack block_stack return_required f in
        ignore(Stack.pop stack);
        Tsequence (processed_f, process stack block_stack needs_return s)
    | Cifthenelse (i, t, e) ->        
        Stack.push Bifthenelse block_stack;
        let before_stack_length = Stack.length stack in 
        let processed_i = process stack block_stack return_required i in  
        ignore(Stack.pop stack);      
        let t_stack = Stack.copy stack in
        let processed_t = process t_stack (Stack.copy block_stack) needs_return t in
        let e_stack = Stack.copy stack in
        let processed_e = process e_stack (Stack.copy block_stack) needs_return e in
        (if Stack.length t_stack > Stack.length e_stack then 
            match e with 
            | Cop (Calloc, _, _) -> Stack.push (Stack.top t_stack) e_stack
            | _ -> ()
        );
        (if Stack.length t_stack < Stack.length e_stack then 
            match t with 
            | Cop (Calloc, _, _) -> Stack.push (Stack.top e_stack) t_stack
            | _ -> ()
        );
        let then_type = Stack.top t_stack in
        let else_type = Stack.top e_stack in
        if not (compare then_type else_type) then (
            print_stack_type then_type;
            print_stack_type else_type;
            failwith "Then and else need to return the same type";
        );

        let rt = if (before_stack_length = Stack.length t_stack) then  
            Stack.top stack
         else 
            (print_endline "void xxx";
            typ_void)
        in
        let result = Tifthenelse (processed_i, processed_t, processed_e, rt) in    
            
        Stack.push rt stack;
        ignore(Stack.pop block_stack);
        result
    | Cswitch (e, ia, ea, d) -> 
        (* let check = Stack.length stack in *)
        let result = Tswitch (process stack block_stack true (* not sure *) e, ia, Array.map (process stack block_stack needs_return) ea, d) in
        let switch_result = ref typ_void in
        Array.iter (fun _ -> 
            let item = Stack.pop stack in 
            switch_result := item
        ) ea;         
        Stack.push !switch_result stack;
        (* assert (check = Stack.length stack - 1); *)
        result
    | Cloop e -> 
        Stack.push Bloop block_stack;
        let result = Tloop (process stack block_stack needs_return e) in
        ignore(Stack.pop block_stack);
        result
    | Ccatch (r, with_, body_) -> 
        let ccatch_stack = Stack.copy stack in
        let ccatch_block_stack = Stack.copy block_stack in
        let with_exprs = List.map(fun (i, il, expr) -> (
          print_endline (string_of_int (List.length il));
          let result = (i, il, process ccatch_stack ccatch_block_stack needs_return expr) in
          Stack.push (Bwith (i, Stack.top ccatch_stack)) ccatch_block_stack;
          result
        )) with_
        in
        let result = Tcatch (
          r, 
          with_exprs, 
          (let result = process ccatch_stack ccatch_block_stack needs_return body_ in 
           result)) 
        in
        if Stack.length ccatch_stack > Stack.length stack then
            Stack.push (Stack.top ccatch_stack) stack;
        result
    | Cexit (i, el) -> 
        Stack.iter (fun f -> 
            match f with 
            | Bwith (i, t) when i = i -> 
                (print_string "okay:";
                 print_stack_type t;
                Stack.push t stack)
            | _ -> ()
        ) block_stack;
        let result = Texit (i, List.map (process stack block_stack needs_return) el) in
        List.iter (fun _ -> ignore(Stack.pop stack)) el;
        result
    | Ctrywith (e, i, c) -> Ttrywith (process stack block_stack needs_return e, i, process (Stack.copy stack) (Stack.copy block_stack) needs_return c)

let add_types func_name e =
    print_endline ("Adding types to function:" ^ func_name);
    let stack : stack = Stack.create () in
    let block_stack : block_stack = Stack.create() in
    Stack.push Bfunction block_stack;
    let result = process stack block_stack true e in
    print_endline "finished";
    (if (Stack.length stack <> 1) then (
        Stack.iter print_stack_type stack;
        failwith ("Stack was expected to have a length of 1, but got " ^ string_of_int (Stack.length stack)))
    );
    (if (Stack.length block_stack <> 1) then 
        failwith ("Block stack was expected to have a length of 1, but got " ^ string_of_int (Stack.length block_stack));
    );
    result