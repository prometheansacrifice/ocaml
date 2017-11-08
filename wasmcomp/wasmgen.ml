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
let empty () = {map = VarMap.empty; count = 0l}

type types = {space : space; mutable list : type_ list}
let empty_types () = {space = empty (); list = []}

type context =
  { data: space; types : types; tables : space; memories : space;
    funcs : space; locals : space; globals : space; labels : int32 VarMap.t }

let empty_context () =
  { data = empty(); types = empty_types (); tables = empty (); memories = empty ();
    funcs = empty (); locals = empty (); globals = empty ();
    labels = VarMap.empty }

let lookup category space x =
  VarMap.find x space.map

let type_ (c : context) x = lookup "type" c.types.space x
let data (c : context) x = lookup "data" c.data x
let local (c : context) x = lookup "local" c.locals x
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

let bind_type (c : context) x ty =
  c.types.list <- c.types.list @ [ty];
  bind "type" c.types.space x
let bind_func (c : context) x = bind "function" c.funcs x
let bind_local (c : context) x = bind "local" c.locals x
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
  {c with labels = VarMap.empty; locals = empty ()}
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
  | Capply ty -> [I32Type]
  | Cextcall(_s, ty, _alloc, _) -> [I32Type]
  | Cload (c, _) ->
    begin match c with
    | Word_val -> [I32Type]
    | Single | Double | Double_u -> [F32Type]
    | _ -> [I32Type]
    end
  | Calloc -> [I32Type]
  | Cstore (_c, _) -> []
  | Caddi | Csubi | Cmuli | Cmulhi | Cdivi | Cmodi |
  Cand | Cor | Cxor | Clsl | Clsr | Casr |
  Ccmpi _ | Ccmpa _ | Ccmpf _ -> [I32Type]
  | Caddv -> [I32Type]
  | Cadda -> [I32Type]
  | Cnegf | Cabsf | Caddf | Csubf | Cmulf | Cdivf -> [F32Type]
  | Cfloatofint -> [F32Type]
  | Cintoffloat -> [I32Type]
  | Craise _ -> []
  | Ccheckbound -> []

let rec to_operations context (expression_list:expression list) operation =
  current_return_type := oper_result_type operation;
  match operation, expression_list with
  | Capply _, _ -> (
    print_endline "!!! This apply is not handled correctly !!!";
    []
    )
  | Cextcall _, _ -> print_endline "Not handled: Cextcall"; []
  | Cload (memory_chunk, mutable_flag), _ -> (
      let align = 0 in
      let offset = 0l in
      let instr = Ast.Types.(match memory_chunk with
      | Byte_unsigned -> Load {ty = I32Type; align; offset; sz = Some (Mem8, ZX)}
      | Byte_signed -> Load {ty = I32Type; align; offset; sz = Some (Mem8, SX)}
      | Sixteen_unsigned -> Load {ty = I32Type; align; offset; sz = Some (Mem16, ZX)}
      | Sixteen_signed -> Load {ty = I32Type; align; offset; sz = Some (Mem16, SX)}
      | Thirtytwo_unsigned -> Load {ty = I32Type; align; offset; sz = None}
      | Thirtytwo_signed -> Load {ty = I32Type; align; offset; sz = None}
      | Word_int -> Load {ty = I32Type; align; offset; sz = None}
      | Word_val -> Load {ty = I32Type; align; offset; sz = None}
      | Single -> Load {ty = I32Type; align; offset; sz = None}
      | _ -> failwith "Not supported yet I guess")
      (* | Double -> Load {ty = I32Type; align; offset; sz = Some (Mem8, SX)}
      | Double_u -> Load {ty = I32Type; align; offset; sz = Some (Mem8, ZX)} *)
      in
      let expression_list = List.fold_left (fun lst f -> lst @ (emit_expr context f)) [] expression_list in
      (* let var = bind_local context "load_instr" in *)
      expression_list @ [instr]
      (* @ [TeeLocal var] *)
    )
  | Calloc, _ -> (
      let counter = !unique_name_counter in
      unique_name_counter := !unique_name_counter + 1;
      let local_ = bind_local context ("allocate_memory_pointer_" ^ string_of_int counter) in
      let (size, calls) = List.fold_left (fun lst f -> match f with
        | Cconst_int _
        | Cconst_natint _
        | Cconst_symbol _
        | Cblockheader _
        | Cvar _
        | Cconst_float _ ->
          let (a, b) = lst in
          (a + 4, b @ [GetLocal local_; Const (I32 (I32.of_int_s a)); Binary (I32 I32Op.Add)] @ emit_expr context f @ [Store {ty = Types.I32Type; align = 0; offset = 0l; sz = None}])
        | _ -> print_endline "SIZE: not supported yet..."; lst
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
      [GetLocal local_]
    )
  | Cstore (memory_chunk, initialization_or_assignment), _ ->
    let align = 0 in
    let offset = 0l in
    let instr = Ast.Types.(match memory_chunk with
    | Byte_unsigned -> Store {ty = I32Type; align; offset; sz = Some Mem8}
    | Byte_signed -> Store {ty = I32Type; align; offset; sz = Some Mem8}
    | Sixteen_unsigned -> Store {ty = I32Type; align; offset; sz = Some Mem16}
    | Sixteen_signed -> Store {ty = I32Type; align; offset; sz = Some Mem16}
    | Thirtytwo_unsigned -> Store {ty = I32Type; align; offset; sz = None}
    | Thirtytwo_signed -> Store {ty = I32Type; align; offset; sz = None}
    | Word_int -> Store {ty = I32Type; align; offset; sz = None}
    | Word_val -> Store {ty = I32Type; align; offset; sz = None}
    | Single -> Store {ty = I32Type; align; offset; sz = None}
    | _ -> failwith "Not supported yet I guess")
    in
    let expression_list = List.fold_left (fun lst f -> lst @ (emit_expr context f)) [] expression_list in
    expression_list @ [instr]

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
  | Cdivi, [fst; snd]
  | Cmodi, [fst; snd] ->
    (emit_expr context fst) @
     (emit_expr context snd) @
     [Binary (I32 I32Op.DivS)]
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
  | Clsl, [fst] ->
    (emit_expr context fst) @
     [Binary (I32 I32Op.Shl)]
  | Clsr, [fst] ->
    (emit_expr context fst) @
     [Binary (I32 I32Op.ShrU)]
  | Casr, [fst] ->
    (emit_expr context fst) @
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
  | Craise _, _-> print_endline "not handled CRAISE"; []
  | Ccheckbound, _ -> failwith "Ccheckbound"
  | _ -> failwith ("Something is not handled here:" ^ string_of_int (List.length expression_list))
and emit_expr context (expression:expression) =
  match expression with
  | Cconst_int i ->  [Const (I32 (I32.of_int_s i))]
  | Cconst_natint i ->  [Const (I32 (I32.of_int_s (Nativeint.to_int i)))]
  | Cconst_float s ->  [Const (F32 (F32.of_float s))]
  | Cconst_symbol symbol ->
    (
    print_endline ("CALL CCONST_SYMBOL" ^ symbol);
    try
      let res = local context symbol in
      [Call res]
    with
    | _ -> try
      [Call (global context symbol)]
    with
    | _ -> try
      [Call (func context symbol)]
    with
    | _ -> try
      let res = data context symbol in
      [Const (I32 res)]
    with
    | _ ->(
      print_endline ("Not found symbol:" ^ symbol);
      [Const (I32 (bind_func context symbol))])
    )
  | Cconst_pointer i -> print_endline ("Not handled yet POINTER: " ^ string_of_int i); []
  | Cconst_natpointer  _ -> failwith "Cconst_natpointer"
  | Cblockheader (i, _) -> [Const (I32 (I32.of_int_s (Nativeint.to_int i)))]
  | Cvar ident ->

    (try (
      let var = local context (ident.Ident.name ^ "_" ^ string_of_int ident.Ident.stamp) in
      print_endline ("\t- Cvar:" ^ (ident.Ident.name ^ "_" ^ string_of_int ident.Ident.stamp));
      [GetLocal var]
    )
    with
    | _ -> (
      print_endline ("Coult not find Cvar:" ^ ident.Ident.name ^ "_" ^ string_of_int ident.Ident.stamp);
      []
    ))
  | Clet (ident, arg, fn_body) -> (
    current_locals := !current_locals @ [Types.I32Type];
    let let_id = bind_local context (ident.Ident.name ^ "_" ^ string_of_int ident.Ident.stamp) in

    (* print_endline ("let id:" ^ (ident.Ident.name ^ "_" ^ string_of_int ident.Ident.stamp) ^ " -> " ^ Int32.to_string let_id); *)

    let result = emit_expr context arg in
    let result = if result = [] then (
      print_endline "Clet - not handled action !!!";
      []
    )
    else result
    in
    result
    @
    [SetLocal let_id]
    @
    emit_expr context fn_body
    )
  | Cassign _ -> failwith "Cassign"
  | Ctuple  _ -> failwith "Ctuple"
  | Cop (Capply mt, (Cop (Cload _ as op, el, _))::tl, _) -> (
    let fn_args = ref [] in
    let expression_list = List.fold_left (fun lst f ->
      let result = emit_expr context f in
      fn_args := !fn_args @ !current_return_type;
      lst @ result
    ) [] tl in
    let load_action = to_operations context el op in
    let type_ = Types.FuncType (!fn_args, [Types.I32Type]) in
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
      print_endline ("CALL cop capply: " ^ hd ^ "=" ^ (Int32.to_string (func context hd)));
      let expression_list = List.fold_left (fun lst f -> lst @ (emit_expr context f)) [] tl in
      expression_list @
      [Call (func context hd)] )
      with | _ ->
      print_endline "Apply: Something did go wrong here...";
      []
    )
  | Cop (operation, expression_list, _) -> to_operations context expression_list operation
  | Csequence (expression1, expression2) -> (emit_expr context expression1) @ (emit_expr context expression2)
  | Cifthenelse (if_, then_, else_) ->
    let i = emit_expr context if_ in
    let t = emit_expr context then_ in
    let return_ = !current_return_type in
    let e = emit_expr context else_ in
    i @ [If (return_, t, e)]
  | Cswitch  _ -> failwith "Cswitch"
  | Cloop  _ -> failwith "Cloop"
  | Ccatch  _ -> print_endline "Not handled yet: Ccatch"; []
  | Cexit  _ -> failwith "Cexit"
  | Ctrywith  _ -> failwith "Ctrywith"

let global_offset = ref 0

let name s =
  try Utf8.decode s with Utf8.Utf8 ->
    failwith "invalid UTF-8 encoding"

let setup_helper_functions () = (
  let globals = [{
    gtype = Types.GlobalType (Types.I32Type, Types.Mutable);
    value = [Const (I32 (I32.of_int_s 0))]
  }]
  in
  ignore(bind_global context "global_memory_offset");

  let data = [{
    index =  0l;
    offset = [Const (I32 (I32.of_int_s 0))];
    init = [Int8 0]
  }]
  in
  ignore(bind_data context "caml_globals_inited" 0l);
  global_offset := !global_offset + 1;
  (* let log_type = Types.FuncType([Types.I32Type], []) in *)
  let type_ = Types.FuncType ([Types.I32Type], [Types.I32Type]) in
  (* let type2_ = Types.FuncType ([Types.I32Type; Types.I32Type], [Types.I32Type]) in *)
  let empty_type = Types.FuncType ([], []) in
  (* ignore(type2_); *)
  let types = [type_; empty_type] in

  (* let log_ftype = bind_type context "log" log_type in *)
  let type__ftype = bind_type context "type_" type_ in
  (* let type__ftype2 = bind_type context "type2_" type2_ in *)
  let empty_ftype = bind_type context "empty_type" empty_type in
(* ignore(type__ftype2); *)
  let imports = [
    (* {
      module_name = name "console";
      item_name = name "log";
      idesc = FuncImport log_ftype
    } *)
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
    (* {
      name = "call_1";
      ftype = type__ftype2;
      locals = [];
      body = [
        (* Const (I32 (I32.of_int_s 0)); *)
        GetLocal 1l;
        GetLocal 0l;
        Load {ty = I32Type; align = 0; offset = 0l; sz = None};
        Load {ty = I32Type; align = 0; offset = 0l; sz = None};
        CallIndirect 3l;
      ]
    }; *)
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
  (* ignore(bind_func context "log"); *)
  ignore(bind_func context "allocate_memory");
  (* let call_1_id = bind_func context "call_1" in *)
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
  let tables = [{ttype = TableType ({min = 4l; max = Some 4l}, AnyFuncType)}]
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

let escape_int i size =
  let str =
    match size with
    | 8 -> Printf.sprintf "%02X" i
    | 16 -> Printf.sprintf "%04X" i
    | 32 -> Printf.sprintf "%08X" i
    | _ -> failwith "Not supported escape int size"
  in
  let result = ref "" in
  for i = 0 to (String.length str / 2 - 1) do
  	let startPos = (String.length str - i * 2 - 2) in
	result := !result ^ "\\" ^ (String.sub str startPos 2);
    ()
  done;
  !result
;;

let compile_wasm_phrase ppf p =
  ignore(ppf);
  match p with
  | Cfunction ({fun_name; fun_args; fun_body; fun_fast; fun_dbg}) -> (
    let context = enter_func context in
    let args = ref [] in
    print_endline ("\n\nFunction:" ^ fun_name);
    List.iter (fun (ident, _) ->
      print_endline ("\t." ^ (ident.Ident.name ^ "_" ^ string_of_int ident.Ident.stamp));
      ignore(bind_local context (ident.Ident.name ^ "_" ^ string_of_int ident.Ident.stamp));
      args := [Types.I32Type] @ !args;
    ) fun_args;
    let (func_id, place) = try (
      (bind_func context fun_name, false)
    ) with
    | _ -> (
      (func context fun_name, true)
    )
    in
    let body = emit_expr context fun_body in
    let type_ = Types.FuncType (!args, !current_return_type) in
    let ftype = bind_type context fun_name type_ in

    let result = {
      name = fun_name;
      ftype;
      locals = !current_locals;
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
      if (i == Int32.to_int func_id) then (
        funcs_ := !funcs_ @ [result; f];
        inserted := true;
      ) else (
        funcs_ := !funcs_ @ [f]
      )
    )) w.funcs;
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
          print_endline "-> SYMBOL";
          try (
            ignore(bind_data context symbol (I32.of_int_u (start_offset + !offset)))
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
      | Csingle s -> print_string (" Csingle " ^ string_of_float s); ()
      | Cdouble d -> print_string (" Cdouble " ^ string_of_float d); ()
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
