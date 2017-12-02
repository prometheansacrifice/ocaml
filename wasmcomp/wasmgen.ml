(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                             Sander Spies                               *)
(*                                                                        *)
(*   Copyright 2017 -                                                     *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* From C-- to webassembly bytecode *)

[@@@ocaml.warning "-20-27"]

open Cmm
open Ast
open Values
open Wasm_types

module VarMap = Map.Make(String)

type space = {mutable map : int32 VarMap.t; mutable count : int32}


type local_space = {mutable l_map : (int32 * value_type) VarMap.t; mutable l_count : int32}

let empty () = {map = VarMap.empty; count = 0l}

type types = {space : space; mutable list : type_ list}
let empty_types () = {space = empty (); list = []}
let empty_locals () = {l_map = VarMap.empty; l_count = 0l};
type context =
  { data: space; types : types; tables : space; memories : space;
    funcs : space; locals : local_space; globals : space; labels : int32 VarMap.t }

let empty_context () =
  { data = empty (); types = empty_types (); tables = empty (); memories = empty ();
    funcs = empty (); locals = empty_locals(); globals = empty ();
    labels = VarMap.empty }

let lookup category space x =
  VarMap.find x space.map

let lookup_local category space x =
  VarMap.find x space.l_map

let type_ (c : context) x = lookup "type" c.types.space x
let data (c : context) x = lookup "data" c.data x
let local (c : context) x = lookup_local "local" c.locals x
let global (c : context) x = lookup "global" c.globals x
let table (c : context) x = lookup "table" c.tables x
let memory (c : context) x = lookup "memory" c.memories x
let label (c : context) x =
  try VarMap.find x c.labels
  with Not_found ->
    failwith ("Not_found: " ^ x)

let func_type (c : context) x =
  try (Lib.List32.nth c.types.list x)
  with Failure _ ->
    failwith "Failure"

let context = empty_context ()

let bind category space x =
  if VarMap.mem x space.map then
    failwith ("Duplicate:" ^ x);
  let i = space.count in
  space.map <- VarMap.add x space.count space.map;
  space.count <- Int32.add space.count 1l;
  if space.count = 0l then
    failwith "Too many bindings";
  i

let bind_local category (space:local_space) x value_type =
  print_endline ("bind_local:" ^ x);
  if VarMap.mem x space.l_map then
    failwith ("Duplicate:" ^ x);
  let i = space.l_count in
  (* VarMap. *)
  space.l_map <- VarMap.add x (space.l_count, value_type) space.l_map;
  space.l_count <- Int32.add space.l_count 1l;
  if space.l_count = 0l then
    failwith "Too many bindings";
  i

let bind_type (c : context) x ty =
  c.types.list <- c.types.list @ [ty];
  bind "type" c.types.space x
let bind_func (c : context) x = bind "function" c.funcs x
let bind_local (c : context) name value_type = bind_local "local" c.locals name value_type
let bind_data (c : context) x i = (
  let space = c.data in
  if VarMap.mem x space.map then
    failwith ("Duplicate:" ^ x);
    (* error x.at ("duplicate " ^ category ^ " " ^ x.it); *)
  space.map <- VarMap.add x i space.map;
  space.count <- Int32.add space.count 1l;
  if space.count = 0l then
    failwith "Too many bindings";
    (* error x.at ("too many " ^ category ^ " bindings"); *)
  i
)
let bind_global (c : context) x = bind "global" c.globals x
let bind_table (c : context) x = bind "table" c.tables x
let bind_memory (c : context) x = bind "memory" c.memories x

let bind_label (c : context) x =
  {c with labels = VarMap.add x 0l (VarMap.map (Int32.add 1l) c.labels)}
let func (c : context) x =
  try
    lookup "function" c.funcs x
  with
    | Not_found -> failwith ("Not found function:" ^ x)

let current_locals = ref []
let current_return_type = ref []

let enter_func (c : context) = (
  current_locals := [];
  current_return_type := [];
  {c with labels = VarMap.empty; locals = empty_locals()}
)

let unique_name_counter = ref 0

let global_id = bind_global context "global_offset"

let wasm_module = ref {
  types = [];
  globals = [];
  tables = Types.[{
    ttype = TableType ({min = 0l; max = Some 0l}, AnyFuncType)
  }];
  memories = Types.[{
    mtype = MemoryType {min = 100l; max = Some 100l}
  }];
  funcs = [];
  start = None;
  elems = [];
  data = [];
  imports = [];
  exports = [];
}

let oper_result_type = function
  | Capply _ -> [I32Type]
  | Cextcall _ -> [I32Type]
  | Cload (c, _) ->
    begin match c with
    | Word_val -> [I32Type]
    | Single | Double | Double_u -> [F32Type]
    | _ -> [I32Type]
    end
  | Calloc -> [I32Type]
  | Cstore _ -> []
  | Caddi | Csubi | Cmuli | Cmulhi | Cdivi | Cmodi |
  Cand | Cor | Cxor | Clsl | Clsr | Casr |
  Ccmpi _ | Ccmpa _ | Ccmpf _ -> [I32Type]
  | Caddv -> [I32Type]
  | Cadda -> [I32Type]
  | Cnegf | Cabsf | Caddf | Csubf | Cmulf | Cdivf -> [F32Type]
  | Cfloatofint -> [F32Type]
  | Cintoffloat -> [I32Type]
  | Craise _ -> [I32Type]
  | Ccheckbound -> []

type exception_fn = {
  name: string;
  expression: expression;
  mem_pointer: int32;
  locals: (string * value_type) list;
  is_handler: bool;
  exn_name: string option
}

let exception_fns:((exception_fn list) ref) = ref []
let create_exception_function ?is_handler:(is_handler = false) ?exn_name (context:context) mem_pointer (name: string) (instr:expression) = (
    (* properly store the locals info to restore later *)
    let fn_id = bind_func context name in
    let (st:((string * value_type) list) ref) = ref [] in
    VarMap.iter (fun key (value, t) -> st := !st @ [(key, t)]) context.locals.l_map;
    let e_fn = {
      name = name;
      expression = instr;
      mem_pointer = mem_pointer;
      locals = !st;
      is_handler = is_handler;
      exn_name
    }
    in
    exception_fns := !exception_fns @ [e_fn];
    fn_id
)


let rec to_operations context (expression_list:expression list) operation =
  current_return_type := oper_result_type operation;
  let result = (match operation, expression_list with
  | Capply _, _ -> (
    print_endline "!!! This apply is not handled correctly !!!";
    []
    )
    (* | Cextcall of string * machtype * bool * label option *)
  | Cextcall (s, mt, b, l), _ -> (
      let expression_list = List.fold_left (fun lst f -> lst @ (emit_expr context f)) [] expression_list in
      expression_list @
      [Call (func context s)]
    )

  (*  *)
  | Cload (memory_chunk, mutable_flag), _ -> (
      print_endline "(cload";
      let align = 0 in
      let offset = 0l in
      let instr = Ast.Types.(match memory_chunk with
      | Byte_unsigned -> [Load {ty = I32Type; align; offset; sz = Some (Mem8, ZX)}]
      | Byte_signed -> [Load {ty = I32Type; align; offset; sz = Some (Mem8, SX)}]
      | Sixteen_unsigned -> [Load {ty = I32Type; align; offset; sz = Some (Mem16, ZX)}]
      | Sixteen_signed -> [Load {ty = I32Type; align; offset; sz = Some (Mem16, SX)}]
      | Thirtytwo_unsigned -> [Load {ty = I32Type; align; offset; sz = None}]
      | Thirtytwo_signed -> [Load {ty = I32Type; align; offset; sz = None}]
      | Word_int -> [Load {ty = I32Type; align; offset; sz = None}]
      | Word_val -> [Load {ty = I32Type; align; offset; sz = None}]
      | Single -> [Load {ty = I32Type; align; offset; sz = None}]
      | Double -> [Load {ty = F32Type; align; offset; sz = None}] (* this is totally wrong but okay... *)
      | Double_u -> [Load {ty = F32Type; align; offset; sz = None}])
      (* | Double -> Load {ty = I32Type; align; offset; sz = Some (Mem8, SX)}
      | Double_u -> Load {ty = I32Type; align; offset; sz = Some (Mem8, ZX)} *)
      in
      let expression_list = List.fold_left (fun lst f -> lst @ (emit_expr context f)) [] expression_list in
      (* let var = bind_local context "load_instr" in *)
      print_endline ")";
      expression_list @ instr
      (* @ [TeeLocal var] *)
    )
  | Calloc, _ -> (
      print_endline "calloc";
      let counter = !unique_name_counter in
      unique_name_counter := !unique_name_counter + 1;
      let local_ = bind_local context ("allocate_memory_pointer_" ^ string_of_int counter) I32Type in
      let (size, calls) = List.fold_left (fun lst f ->
        let (size, calls) = lst in
        match f with
        | Cconst_int _
        | Cconst_natint _
        | Cconst_symbol _
        | Cblockheader _
        | Cvar _
        | Cconst_pointer _
        | Cconst_float _
        | Cop (Cload _, _, _)
        | Cop (Cextcall _, _, _) ->
          (size + 4,
           calls @
           [GetLocal local_;
            Const (I32 (I32.of_int_s size));
            Binary (I32 I32Op.Add)] @
           emit_expr context f @
           [Store {ty = Types.I32Type; align = 0; offset = 0l; sz = None}]
          )
        | Clet _ -> failwith "CLET!"
        | Csequence _ -> failwith "Csequence!"
        | Ccatch _ -> failwith "Ccatch!"
        | Ctrywith _ -> failwith "Ctrywith!"
        | Cassign _ -> failwith "Cassign!"
        | Ctuple _ -> failwith "Ctuple!"
        | Cop (Capply _, _, _) -> failwith "Cop 1!"
        | Cop (Calloc, _, _) -> failwith "Cop 2!"
        | Cop (Cstore _, _, _) -> failwith "Cop 3!"
        | Cop (Cabsf, el, _)
        | Cop (Cnegf, el, _)
        | Cop (Caddf, el, _)
        | Cop (Csubf, el, _)
        | Cop (Cmulf, el, _)
        | Cop (Cdivf, el, _) ->
          (size + 4,
           calls @
           [GetLocal local_;
            Const (I32 (I32.of_int_s size));
            Binary (I32 I32Op.Add)] @
           emit_expr context f @
           [Store {ty = Types.F32Type; align = 0; offset = 0l; sz = None}]
          )
        | Cop _ -> failwith "Cop 4!"
        | Cifthenelse _ -> failwith "Cifthenelse!"
        | Cswitch _ -> failwith "Cswitch!"
        | Cloop _ -> failwith "Cloop!"
        | Cexit _ -> failwith "Cexit!"
        | Cconst_natpointer _ -> failwith "Cconst_natpointer!"
        ) (0, []) expression_list
      in
      current_locals := !current_locals @ [Types.I32Type];
      [Const (I32 (I32.of_int_s size));
       Call (func context "allocate_memory");
       SetLocal local_
      ]
      @
      calls
      @
      [GetLocal local_;
       Const (I32 4l);
       Binary (I32 I32Op.Add)
      ]
      @
      (if !current_return_type = [F32Type] then
          [Convert (F32 F32Op.ReinterpretInt)]
        else
          []
      )
    )
  | Cstore (memory_chunk, initialization_or_assignment), _ ->
    print_endline "Cstore";
    let align = 0 in
    let offset = 0l in
    let instr = Ast.Types.(match memory_chunk with
    | Byte_unsigned -> [Store {ty = I32Type; align; offset; sz = Some Mem8}]
    | Byte_signed -> [Store {ty = I32Type; align; offset; sz = Some Mem8}]
    | Sixteen_unsigned -> [Store {ty = I32Type; align; offset; sz = Some Mem16}]
    | Sixteen_signed -> [Store {ty = I32Type; align; offset; sz = Some Mem16}]
    | Thirtytwo_unsigned -> [Store {ty = I32Type; align; offset; sz = None}]
    | Thirtytwo_signed -> [Store {ty = I32Type; align; offset; sz = None}]
    | Word_int -> [Store {ty = I32Type; align; offset; sz = None}]
    | Word_val -> [Store {ty = I32Type; align; offset; sz = None}]
    | Single -> [Store {ty = F32Type; align; offset; sz = None}]
    | Double -> [Store {ty = F32Type; align; offset; sz = None}] (* this all seems rather wrong -> double is 64bit not 32... *)
    | Double_u -> [Store {ty = F32Type; align; offset; sz = None}])
    in
    let expression_list = List.fold_left (fun lst f -> lst @ (emit_expr context f)) [] expression_list in
    expression_list @ instr

  | Cadda, [fst; snd]
  | Caddi, [fst; snd] ->
    (emit_expr context fst) @
     (emit_expr context snd) @
     [Binary (I32 I32Op.Add)]
  | Csubi, [fst; snd] ->
    (emit_expr context fst) @
     (emit_expr context snd) @
     [Binary (I32 I32Op.Sub)]
  | Cmuli, [fst; snd]
  | Cmulhi, [fst; snd] ->
    (emit_expr context fst) @
     (emit_expr context snd) @
     [Binary (I32 I32Op.Mul)]
  | Cdivi, [fst; snd] ->
  (emit_expr context fst) @
   (emit_expr context snd) @
   [Binary (I32 I32Op.DivS)]
  | Cmodi, [fst; snd] ->
    let counter = !unique_name_counter in
    unique_name_counter := !unique_name_counter + 1;
    let cmod_local = bind_local context ("cmod_" ^ string_of_int counter) I32Type in
    let cmod_local2 = bind_local context ("cmod2_" ^ string_of_int counter) I32Type in
    (emit_expr context fst) @
     [SetLocal cmod_local;
      GetLocal cmod_local;
      GetLocal cmod_local] @
     (emit_expr context snd) @
     [TeeLocal cmod_local2;
      Binary (I32 I32Op.DivS);
      GetLocal cmod_local2;
      Binary (I32 I32Op.Mul);
      Binary (I32 I32Op.Sub)]
  | Cand, [fst; snd] ->
    (emit_expr context fst) @
     (emit_expr context snd) @
     [Binary (I32 I32Op.And)]
  | Cor, [fst; snd] ->
    (emit_expr context fst) @
     (emit_expr context snd) @
     [Binary (I32 I32Op.Or)]
  | Cxor, [fst; snd] ->
    (emit_expr context fst) @
     (emit_expr context snd) @
     [Binary (I32 I32Op.Xor)]
  | Clsl, [fst; snd] ->
  (emit_expr context fst) @
   (emit_expr context snd) @
     [Binary (I32 I32Op.Shl)]
  | Clsr, [fst; snd] ->
  (emit_expr context fst) @
   (emit_expr context snd) @
     [Binary (I32 I32Op.ShrU)]
  | Casr, [fst; snd] ->
  (emit_expr context fst) @
   (emit_expr context snd) @
     [Binary (I32 I32Op.ShrS)]
  | Ccmpi Ceq, [fst; snd] ->
    (emit_expr context fst) @
     (emit_expr context snd) @
     [Compare (I32 I32Op.Eq)]
  | Ccmpi Cne, [fst; snd] ->
    (emit_expr context fst) @
     (emit_expr context snd) @
     [Compare (I32 I32Op.Ne)]
  | Ccmpi Clt, [fst; snd] ->
    (emit_expr context fst) @
     (emit_expr context snd) @
     [Compare (I32 I32Op.LtS)]
  | Ccmpi Cle, [fst; snd] ->
    (emit_expr context fst) @
     (emit_expr context snd) @
     [Compare (I32 I32Op.LeS)]
  | Ccmpi Cgt, [fst; snd] ->
    (emit_expr context fst) @
     (emit_expr context snd) @
     [Compare (I32 I32Op.GtS)]
  | Ccmpi Cge, [fst; snd] ->
    (emit_expr context fst) @
     (emit_expr context snd) @
     [Compare (I32 I32Op.GeS)]
  | Caddv, _ -> failwith "caddv" (* pointer addition that produces a [Val] (well-formed Caml value) *)
  (* pointer addition that produces a [Addr] (derived heap pointer) *)
  | Ccmpa _, _ -> failwith "Ccmpa"
  | Cnegf, [fst] ->
    (emit_expr context fst) @
     [Unary (F32 F32Op.Neg)]
  | Cabsf, [fst] ->
    (emit_expr context fst) @
     [Unary (F32 F32Op.Abs)]
  | Caddf, [fst; snd] ->
    (emit_expr context fst) @
     (emit_expr context snd) @
     [Binary (F32 F32Op.Add)]
  | Csubf, [fst; snd] ->
    (emit_expr context fst) @
     (emit_expr context snd) @
     [Binary (F32 F32Op.Sub)]
  | Cmulf, [fst; snd] ->
    (emit_expr context fst) @
     (emit_expr context snd) @
     [Binary (F32 F32Op.Mul)]
  | Cdivf, [fst; snd] ->
    (emit_expr context fst) @
     (emit_expr context snd) @
     [Binary (F32 F32Op.Div)]
  | Cfloatofint, [fst] ->
    (emit_expr context fst) @
     [Convert (F32 F32Op.ReinterpretInt)]
  | Cintoffloat, [fst] ->
    (emit_expr context fst) @
     [Convert (I32 I32Op.ReinterpretFloat)]
  | Ccmpf Ceq, [fst; snd] -> failwith "Ccmpf"
  | Ccmpf Cne, [fst; snd] -> failwith "Ccmpf"
  | Ccmpf Clt, [fst; snd] -> failwith "Ccmpf"
  | Ccmpf Cle, [fst; snd] -> failwith "Ccmpf"
  | Ccmpf Cgt, [fst; snd] -> failwith "Ccmpf"
  | Ccmpf Cge, [fst; snd] -> failwith "Ccmpf"
  | Craise _, _->
    print_endline ("no: " ^ string_of_int (List.length expression_list));
    print_endline "not handled CRAISE"; []
  | Ccheckbound, _ -> failwith "Ccheckbound"
  | _ ->
  failwith ("Something is not handled here   ... :" ^ string_of_int (List.length expression_list)))
  in
  (match operation with
  | Cstore _ -> print_endline "cstore yes"
  | _ -> ());
  result
and emit_expr (context:context) (expression:expression) =
  match expression with
  | Cconst_int i -> [Const (I32 (I32.of_int_s i))]
  | Cconst_natint i -> [Const (I32 (I32.of_int_s (Nativeint.to_int i)))]
  | Cconst_float s -> [Const (F32 (F32.of_float s))]
  | Cconst_symbol symbol ->
    (
    try
      let (res, _) = local context symbol in
      [Call res]
    with
    | _ -> try
      [Call (global context symbol)]
    with
    | _ -> try (
      [Call (func context symbol)]
    )
    with
    | _ -> try
      let res = data context symbol in
      [Const (I32 res)]
    with
    | _ -> (
      (* TODO: resolve this at a later point... *)
      print_endline ("TODO: " ^ symbol);
      [DelayedConst symbol]
    )
    )
  | Cconst_pointer i -> (current_return_type := [I32Type]; [Const (I32 (I32.of_int_s i))])
  | Cconst_natpointer  _ -> failwith "Cconst_natpointer"
  | Cblockheader (i, _) -> [Const (I32 (I32.of_int_s (Nativeint.to_int i)))]
  | Cvar ident ->
    (try (
      let (var, _) = local context (ident.Ident.name ^ "_" ^ string_of_int ident.Ident.stamp) in
      [GetLocal var]
    )
    with
    | _ -> (
      print_endline ("Coult not find Cvar:" ^ ident.Ident.name ^ "_" ^ string_of_int ident.Ident.stamp);
      []
    ))
  | Clet (ident, arg, fn_body) -> (
    print_endline "(clet ";
    current_locals := !current_locals @ [Types.I32Type];
    let let_id = bind_local context (ident.Ident.name ^ "_" ^ string_of_int ident.Ident.stamp) I32Type in

    (* print_endline ("let id:" ^ (ident.Ident.name ^ "_" ^ string_of_int ident.Ident.stamp) ^ " -> " ^ Int32.to_string let_id); *)

    let result = emit_expr context arg in
    let result = if result = [] then (
      print_endline "Clet - not handled action !!!";
      []
    )
    else result
    in
    print_endline ")";
    let result = result
    @
    [SetLocal let_id]
    @
    emit_expr context fn_body
    in
    result
    )
  | Cassign _ -> failwith "Cassign" (* setlocal??? *)
  | Ctuple  _ -> failwith "Ctuple" (* memory *)
  | Cop (Capply mt, (Cop (Cload _ as op, el, _))::tl, _) -> (
    print_endline "capply 1";
    let fn_args = ref [] in
    let expression_list = List.fold_left (fun lst f ->
      let result = emit_expr context f in
      fn_args := !fn_args @ !current_return_type;
      if List.length result > 0 then (
        lst @ result
      )
      else
        lst
    ) [] tl in
    let load_action = to_operations context el op in
    let type_ = FuncType (!fn_args, [I32Type]) in
    let counter = !unique_name_counter in
    unique_name_counter := !unique_name_counter + 1;
    let ftype = bind_type context ("wasm_unique_name_" ^ string_of_int counter) type_ in
    let w = !wasm_module in
    wasm_module := Ast.{w with types = w.types @ [type_]};
    expression_list @
    load_action @
    [CallIndirect ftype]
    )
  | Cop (Capply _, (Cconst_symbol hd)::tl, _) -> (
    try (
      print_endline ("looking for: " ^ hd);
      print_endline ("CALL cop capply: " ^ hd ^ "=" ^ (Int32.to_string (func context hd)));
      let expression_list = List.fold_left (fun lst f -> lst @ (emit_expr context f)) [] tl in
      expression_list @
      [Call (func context hd)] )
      with | _ ->
      print_endline "Apply: Something did go wrong here...";
      []
    )
  | Cop (Craise _, [arg], _) -> (

      (emit_expr context arg)
      @
      [Call (func context "jsRaise")]
    )
  | Cop (operation, expression_list, _) -> to_operations context expression_list operation
  | Csequence (expression1, expression2) -> (print_endline "sequence"; (emit_expr context expression1) @ (emit_expr context expression2))
  | Cifthenelse (if_, then_, else_) ->
    let i = emit_expr context if_ in
    let t = emit_expr context then_ in
    let return_ = !current_return_type in
    let e = emit_expr context else_ in
    current_return_type := return_;
    i @ [If (return_, t, e)]
  | Cswitch  _ -> failwith "Cswitch"
  | Cloop  _ -> failwith "Cloop"
  | Ccatch  _ -> print_endline "Not handled yet: Ccatch"; []
  | Cexit  _ -> failwith "Cexit"
  | Ctrywith  (body, exn, handler) ->
    (
      print_endline (exn.Ident.name ^ "_" ^ string_of_int exn.Ident.stamp);

      let memory_alloc_size = ref 0 in
      VarMap.iter (fun key (value, t) ->
        match t with
        | I32Type
        | F32Type -> memory_alloc_size := !memory_alloc_size + 4
        | I64Type
        | F64Type -> memory_alloc_size := !memory_alloc_size + 8
      ) context.locals.l_map;

      let counter = !unique_name_counter in
      unique_name_counter := !unique_name_counter + 1;
      let alloc_memory_pointer = bind_local context ("allocate_memory_pointer_" ^ string_of_int counter) I32Type in
      let store_instructions = ref
      [Const (I32 (I32.of_int_s !memory_alloc_size));
       Call (func context "allocate_memory");
       SetLocal alloc_memory_pointer
      ]
      in
      let set_blocks = ref [] in
      let get_blocks = ref [] in
      let memory_block_position = ref 4 in (* first block is reserved for the return value *)
      VarMap.iter (fun key (value, t) ->
        set_blocks := !set_blocks @ [
          GetLocal alloc_memory_pointer;
          Const (I32 (I32.of_int_s !memory_block_position));
          Binary (I32 I32Op.Add)] @
          ( match t with
            | I32Type -> (
                [GetLocal value;
                 Store {ty = I32Type; align = 0; offset = 0l; sz = None}]
              )
            | F32Type -> (
                [GetLocal value;
                 Store {ty = F32Type; align = 0; offset = 0l; sz = None}]
              )
            | _ -> failwith "not supported"
          );
        get_blocks := !get_blocks @ [
          GetLocal alloc_memory_pointer;
          Const (I32 (I32.of_int_s !memory_block_position));
          Binary (I32 I32Op.Add)] @
          ( match t with
            | I32Type -> [Load {ty = I32Type; align = 0; offset = 0l; sz = None}; SetLocal value]
            | F32Type -> [Load {ty = F32Type; align = 0; offset = 0l; sz = None}; SetLocal value]
            | _ -> failwith "not supported"
          );
        match t with
          | I32Type
          | F32Type -> (
              memory_block_position := !memory_block_position + 4;
            )
          | _ -> assert false
      ) context.locals.l_map;

      let body_fn_id = create_exception_function context alloc_memory_pointer (exn.Ident.name ^ "_" ^ (string_of_int exn.Ident.stamp) ^ "_body") body in
      let exn_name = (exn.Ident.name ^ "_" ^ string_of_int exn.Ident.stamp) in
      let handler_fn_id = create_exception_function ~is_handler:true ~exn_name context alloc_memory_pointer (exn.Ident.name ^ "_" ^ (string_of_int exn.Ident.stamp) ^ "_handler") handler in
      current_return_type := [I32Type];
      !store_instructions @
      !set_blocks @


      [
      GetLocal alloc_memory_pointer;
      Const (I32 body_fn_id);
      Const (I32 handler_fn_id);
      Call 0l] @
      !get_blocks @
      [ GetLocal alloc_memory_pointer;
        Load {ty = I32Type; align = 0; offset = 0l; sz = None}; (* TODO: also support float return value... *)
      ]
    )

let global_offset = ref 0

let name s =
  try Utf8.decode s with Utf8.Utf8 ->
    failwith "invalid UTF-8 encoding"

let setup_helper_functions () = (
  let globals = [{
    gtype = Types.GlobalType (Types.I32Type, Types.Mutable);
    value = [Const (I32 (I32.of_int_s 0))]
  }
  ]
  in
  ignore(bind_global context "global_memory_offset");

  let data = [{
    index =  0l;
    offset = [Const (I32 0l)];
    init = [Int8 0]
  }; {
    index =  0l;
    offset = [Const (I32 1l)];
    init = [Int32 0l]
  }
  ]
  in
  ignore(bind_data context "caml_globals_inited" 0l);
  ignore(bind_data context "caml_backtrace_pos" 1l);
  (* ignore(bind_data context "caml_backtrace_pos" 0l); *)
  global_offset := !global_offset + 5;

  let jsTryWithType = Types.FuncType ([Types.I32Type;Types.I32Type;Types.I32Type], []) in
  let type_ = Types.FuncType ([Types.I32Type], [Types.I32Type]) in
  let empty_type = Types.FuncType ([], []) in

(* jsTryWithType; *)
  let types = [ jsTryWithType; type_; empty_type] in

  let jsTryWithType_ = bind_type context "jsTryWithType" jsTryWithType in
  let type__ftype = bind_type context "type_" type_ in
  let empty_ftype = bind_type context "empty_type" empty_type in

  let imports = [
    {
      module_name = name "js";
      item_name = name "tryWith";
      idesc = FuncImport jsTryWithType_
    };
    {
      module_name = name "js";
      item_name = name "raise";
      idesc = FuncImport type__ftype
    };
    {
      module_name = name "js";
      item_name = name "caml_fresh_oo_id";
      idesc = FuncImport type__ftype
    }
  ]
  in
  let funcs = [
    {
      name = "allocate_memory";
      ftype = type__ftype;
      locals = [Types.I32Type];
      body = [
        GetGlobal 0l;
        TeeLocal 1l;
        GetLocal 0l;
        Binary (I32 I32Op.Add);
        SetGlobal 0l;
        GetLocal 1l;
      ]
    };
    {
      name = "camlCamlinternalFormatBasics__entry";
      ftype = empty_ftype;
      locals = [];
      body = []
    };
    {
      name = "camlPervasives__entry";
      ftype = empty_ftype;
      locals = [];
      body = []
    };
    {
      name = "camlStd_exit__entry";
      ftype = empty_ftype;
      locals = [];
      body = []
    }
  ]
  in
  ignore(bind_func context "jsTryWith");
  ignore(bind_func context "jsRaise");
  ignore(bind_func context "caml_fresh_oo_id");
  ignore(bind_func context "allocate_memory");
  ignore(bind_func context "camlCamlinternalFormatBasics__entry");
  ignore(bind_func context "camlPervasives__entry");
  ignore(bind_func context "camlStd_exit__entry");
  let exports = [
  (* {
    name = name "call_1";
    edesc = FuncExport call_1_id;
  }; *)
  {
    name = name "table";
    edesc = TableExport 0l;
  };
  {
    name = name "memory";
    edesc = MemoryExport 0l;
  }
  ]
  in
  let tables = [{ttype = TableType ({min = 7l; max = Some 7l}, AnyFuncType)}]
  in
  let elems = [
    {
      index = 0l;
      offset=[Const (I32 (I32.of_int_s 0))];
      init=[0l]
    };
    {
      index = 0l;
      offset=[Const (I32 (I32.of_int_s 1))];
      init=[1l]
    };
    {
      index = 0l;
      offset=[Const (I32 (I32.of_int_s 2))];
      init=[2l]
    };
    {
      index = 0l;
      offset=[Const (I32 (I32.of_int_s 3))];
      init=[3l]
    };
    {
      index = 0l;
      offset=[Const (I32 (I32.of_int_s 4))];
      init=[4l]
    };
    {
      index = 0l;
      offset=[Const (I32 (I32.of_int_s 5))];
      init=[5l]
    };
    {
      index = 0l;
      offset=[Const (I32 (I32.of_int_s 6))];
      init=[6l]
    }
  ]
  in
  let w = !wasm_module in
  wasm_module := Ast.{w with funcs = funcs;
              globals = globals;
              types = types;
              data = data;
              imports = imports;
              exports = exports;
              tables = tables;
              elems = elems
            };
)

let rec add_exception_functions ppf = (
  let _exception_fns = !exception_fns in
  exception_fns := [];
  List.iter (fun {name; expression; mem_pointer; locals; is_handler; exn_name} -> (
    let exn_name = match exn_name with
    | Some s -> s
    | _ -> ""
    in
    compile_wasm_phrase  ~locals ~is_handler ~exn_name ppf (Cfunction ({
      fun_name = name;
      fun_args = [];
      fun_body = expression;
      fun_fast = false;
      fun_dbg = []
      }))
    )) _exception_fns;

)
and compile_wasm_phrase ?locals ?is_handler ?exn_name ppf p =
  ignore(ppf);
  match p with
  | Cfunction ({fun_name; fun_args; fun_body; fun_fast; fun_dbg}) -> (
    let context = enter_func context in
    let args = ref [] in
    print_string ("\n\n." ^ fun_name ^ "(");

    List.iter (fun (ident, mt) ->
      print_string (ident.Ident.name ^ "_" ^ string_of_int ident.Ident.stamp ^ ", ");
      let value_type = match mt with
      | [|Val|]
      | [|Addr|]
      | [|Int|] -> I32Type
      | [|Float|] -> F32Type
      | _ -> failwith "wait what..."
      in
      ignore(bind_local context (ident.Ident.name ^ "_" ^ string_of_int ident.Ident.stamp) value_type);
      args := [Types.I32Type] @ !args;
    ) fun_args;

    (* fugly way to make a function a closure which is helpful for certain tests *)

    (* if (List.length fun_dbg > 0) then (
      args := [Types.I32Type] @ !args
    ); *)

    let (func_id, place) = try (
      (bind_func context fun_name, false)
    ) with
    | _ -> (
      (func context fun_name, true)
    )
    in
    print_endline (") " ^ Int32.to_string func_id ^ "\n================");
    let w = !wasm_module in
    let newFuncs = List.map (fun func -> (
      let new_func_body = List.map (fun f ->
        match f with
        | DelayedConst s when s = fun_name -> Const (I32 func_id)
        | _ -> f
      ) func.body
      in
      {func with body = new_func_body}
    )) w.funcs
    in
    wasm_module := Ast.{w with funcs = newFuncs};

    (* let last_item = List.hd (List.rev fun_body) in *)
    (* remove unit return value *)
    let rec remove_last_unit fun_body =
      match fun_body with
      | Csequence (expr1, Cconst_pointer 1) -> (
        expr1
        )
      | Csequence (expr1, Csequence (a, b)) -> Csequence (expr1, remove_last_unit (Csequence (a, b)))
      | _ -> fun_body
    in
    let fun_body = remove_last_unit fun_body in
    let body = (match locals with
    | Some locals ->
      let memory_block_position = ref 4 (* first block is the return value when present *) in
      let mp = bind_local context "mem_pointer" I32Type in
      args := [I32Type];

      (match is_handler, exn_name with
      | Some s, Some n when s = true -> (
        ignore(bind_local context n I32Type);
        args := !args @ [I32Type]
        )
      | _ -> ());

      List.fold_left (fun l (name, t) ->
        let local_id = bind_local context name t in

        let memory_pos = [
          GetLocal mp;
          Const (I32 (I32.of_int_s !memory_block_position));
          Binary (I32 I32Op.Add)
        ] @
          (match t with
            | I32Type ->
              [Load {ty = I32Type; align = 0; offset = 0l; sz = None};
               SetLocal local_id]
            | F32Type ->
              [Load {ty = F32Type; align = 0; offset = 0l; sz = None};
               SetLocal local_id]
            | _ -> assert false
            )
        in
        memory_block_position := !memory_block_position + 4;
        memory_pos
      ) [] locals;
    | _ -> [])
    in
    let fun_body = emit_expr context fun_body in
    let fun_body = (if List.length !current_return_type > 0 then
    (
      match !current_return_type, locals with
      | [I32Type], Some _ ->
        current_return_type := [];
        let (mp, _) = local context "mem_pointer" in
        [GetLocal mp] @
        fun_body @
        [Store {ty = I32Type; align = 0; offset = 0l; sz = None}]
      | [F32Type], Some _ ->
        current_return_type := [];
        let (mp, _) = local context "mem_pointer" in
        [GetLocal mp] @
        fun_body @
        [Store {ty = F32Type; align = 0; offset = 0l; sz = None}]
      | _ -> fun_body
      )
    else
      fun_body
    )
    in
    let body = body @ fun_body
    in
    let type_ = Types.FuncType (!args, !current_return_type) in
    let ftype = bind_type context fun_name type_ in
    let locals = ref []
    in
    VarMap.iter (fun _ (_, t) -> locals := !locals @ [t]) context.locals.l_map;
    let result = {
      name = fun_name;
      ftype;
      locals = !locals;
      body;
    } in
    let export = {
      name = name fun_name;
      edesc = FuncExport func_id;
    } in

    let w = !wasm_module in
    let funcs_ = ref [] in
    let inserted = ref false in
    List.iteri (fun i f -> (
      (* TODO: add a check for the 2 - which should match the imports *)
      if (i + 3 == Int32.to_int func_id) then (
        funcs_ := !funcs_ @ [result; f];
        inserted := true;
      ) else (
        funcs_ := !funcs_ @ [f]
      )
    )) w.funcs;
    (* correctly update the funcs index also... *)

    if (not !inserted) then (
      funcs_ := !funcs_ @ [result]
    );
    wasm_module := Ast.{w with funcs = !funcs_};

    let w = !wasm_module in
    wasm_module := Ast.{w with types = w.types @ [type_]};
    let w = !wasm_module in
    wasm_module := Ast.{w with exports = w.exports @ [export]};

    let w = !wasm_module in
    let table = List.hd w.tables in
    let tt = table.ttype in
    Types.(match tt with
    | TableType (limits, _) -> (
      let max = match limits.max with
      | Some s -> s
      | None -> 0l
      in
      wasm_module := Ast.{w with
        tables = [{ttype = TableType ({min = (Int32.add max 1l); max = Some (Int32.add max 1l)}, AnyFuncType)}];
        elems = w.elems @ [{
          index = 0l;
          offset=[Const (I32 (I32.of_int_s (List.length w.elems)))];
          init=[I32.of_int_s (List.length w.elems)]
        }]
      }
    ));

    add_exception_functions ppf;
    ())
  | Cdata dl -> (
    let init = ref [] in
    let start_offset = !global_offset in
    (* let data_id = ref 0l in *)
    let offset = ref 0 in
    List.iter (function
      | Cglobal_symbol s -> print_endline ("not handled global: " ^ s); ()
      | Csymbol_address symbol -> (
        let symbol_id = try
          (func context symbol)
        with
        | _ -> try
          data context symbol
        with
        | _ ->(
          bind_func context symbol
        )

          in
          print_endline ("SYMBOL:" ^ symbol ^ " = " ^ (Int32.to_string symbol_id));
          init := !init @ [Int32 symbol_id];
          offset := !offset + 4;
          ()
        )
      | Cdefine_symbol symbol ->  (
          print_endline ("\n-> SYMBOL:" ^ symbol);
          try (
            ignore(bind_data context symbol (I32.of_int_u (start_offset + !offset)));
            let w = !wasm_module in
            let newFuncs = List.map (fun (func:Ast.func) -> (
              let rec process_instr a = List.map (fun f ->
                match f with
                | DelayedConst s when s = symbol -> (
                  Const (I32 (I32.of_int_u (start_offset + !offset)))
                )
                | Block (s, i) -> Block (s, process_instr i)
                | Loop (s, i) -> Loop (s, process_instr i)
                | If (t, il1, il2) -> If (t, process_instr il1, process_instr il2)
                | _ -> f
              ) a
              in
              let new_func_body = process_instr func.body
              in
              {func with body = new_func_body}
            )) w.funcs
            in
            wasm_module := Ast.{w with funcs = newFuncs};
          ) with
          | _ -> ignore(data context symbol);

          ()
        )
      | Cint8 i -> (
          init := !init @ [Int8 i];
          offset := !offset + 1;
          ()
        )
      | Cint16 i -> (
          init := !init @ [Int16 i];
          offset := !offset + 2;
          ()
        )
      | Cint32 ni -> (
          init := !init @ [Nativeint ni];
          offset := !offset + 4;
          ()
        )
      | Cint ni -> (
          init := !init @ [Nativeint ni];
          offset := !offset + 4;
          ()
        )
      | Csingle d
      | Cdouble d -> (
        init := !init @ [Float32 (F32.of_float d)];
        offset := !offset + 4;
        ()
        )
      | Cstring s -> (
          init := !init @ [Ast.String s];
          (* TODO: get proper string length... *)
          offset := !offset + (String.length s);
          ()
        )
      | Cskip i -> print_string (" Cskip " ^ string_of_int i); ()
      | Calign i ->  print_string (" Calign " ^ string_of_int i); ()
    ) dl;
    let w = !wasm_module in

    global_offset := !global_offset + !offset;
    wasm_module := Ast.{w with data = w.data @ [{
      index =  0l;
      offset = [Const (I32 (I32.of_int_s start_offset))];
      init = !init
    }];
    globals = [{
      gtype = Types.GlobalType (Types.I32Type, Types.Mutable);
      value = [Const (I32 (I32.of_int_s !global_offset))]
    }]
  };
)
