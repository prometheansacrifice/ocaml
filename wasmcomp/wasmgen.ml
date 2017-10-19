(* open Format *)
(* open Clflags *)
[@@@ocaml.warning "-20-27"]


(* let calling_conventions = what location is the val *)


open Cmm

module Types = Wasm_types
open Ast
module Script = Script
open Values

(* add the VarMap stuff from wasm's parser.mly *)
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

let enter_func (c : context) =
  {c with labels = VarMap.empty; locals = empty ()}

let lookup category space x =
  VarMap.find x space.map
    (* error x.at ("unknown " ^ category ^ " " ^ x.it) *)


(* TODO refactor to `let symbol ...`:
   - remove all other options (not directly but later?)
   - if symbol doesnt exist create it
 *)
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
    (* error x.at ("unknown label " ^ x.it) *)


let func_type (c : context) x =
  try (Lib.List32.nth c.types.list x)
  with Failure _ ->
    failwith "Failure"
    (* error x.at ("unknown type " ^ Int32.to_string x) *)

let context = empty_context ()

let bind category space x =
  if VarMap.mem x space.map then
    failwith ("Duplicate:" ^ x);
    (* error x.at ("duplicate " ^ category ^ " " ^ x.it); *)
  let i = space.count in
  space.map <- VarMap.add x space.count space.map;
  space.count <- Int32.add space.count 1l;
  if space.count = 0l then
    failwith "Too many bindings";
    (* error x.at ("too many " ^ category ^ " bindings"); *)
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

let to_func_type fun_args = (
  print_endline "todo"
)

let func (c : context) x =
  try
    lookup "function" c.funcs x
  with
    | Not_found -> failwith ("Not found function:" ^ x)

let memory_chunk_to_string = function
  | Byte_unsigned -> "Byte_unsigned"
  | Byte_signed -> "Byte_signed"
  | Sixteen_unsigned -> "Sixteen_unsigned"
  | Sixteen_signed -> "Sixteen_signed"
  | Thirtytwo_unsigned -> "Thirtytwo_unsigned"
  | Thirtytwo_signed -> "Thirtytwo_signed"
  | Word_int -> "Word_int"
  | Word_val -> "Word_val"
  | Single -> "Single"
  | Double -> "Double"
  | Double_u -> "Double_u"

let locals = ref []

let return_type = ref []

let rec to_operations context (expression_list:expression list) operation =
  return_type := [];
  match operation, expression_list with
  | Capply _, _ -> print_endline "This capply not handled apparently *SHAME*"; []
  | Cextcall _, _ -> failwith "Cextcall"
  | Cload (memory_chunk, mutable_flag), _ -> (
      let align = 0 in
      let offset = 0l in
      (* let expression_list = List.map (fun f -> emit_expr context f) expression_list in *)
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
      print_endline ("Nr of expressions:" ^ string_of_int (List.length expression_list));
      return_type := [Types.I32Type];
      expression_list @ [instr]
    )
  | Calloc, _ -> (
      return_type := [Types.I32Type];
      let local_ = bind_local context "allocate_memory_pointer" in
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
      locals := !locals @ [Types.I32Type];
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
    return_type := [];
    expression_list @ [instr]
  | Cadda, [fst; snd]
  | Caddi, [fst; snd] ->
    return_type := [Types.I32Type];
    (emit_expr context fst) @
     (emit_expr context snd) @
     [Binary (I32 I32Op.Add)]
  | Csubi, [fst; snd] ->
    return_type := [Types.I32Type];
    (emit_expr context fst) @
     (emit_expr context snd) @
     [Binary (I32 I32Op.Sub)]
  | Cmuli, [fst; snd]
  | Cmulhi, [fst; snd] ->
    return_type := [Types.I32Type];
    (emit_expr context fst) @
     (emit_expr context snd) @
     [Binary (I32 I32Op.Mul)]
  | Cdivi, [fst; snd]
  | Cmodi, [fst; snd] ->
    return_type := [Types.I32Type];
    (emit_expr context fst) @
     (emit_expr context snd) @
     [Binary (I32 I32Op.DivS)]
  | Cand, [fst; snd] ->
    return_type := [Types.I32Type];
    (emit_expr context fst) @
     (emit_expr context snd) @
     [Binary (I32 I32Op.And)]
  | Cor, [fst; snd] ->
    return_type := [Types.I32Type];
    (emit_expr context fst) @
     (emit_expr context snd) @
     [Binary (I32 I32Op.Or)]
  | Cxor, [fst; snd] ->
    return_type := [Types.I32Type];
    (emit_expr context fst) @
     (emit_expr context snd) @
     [Binary (I32 I32Op.Xor)]
  | Clsl, [fst] ->
    return_type := [Types.I32Type];
    (emit_expr context fst) @
     [Binary (I32 I32Op.Shl)]
  | Clsr, [fst] ->
    return_type := [Types.I32Type];
    (emit_expr context fst) @
     [Binary (I32 I32Op.ShrU)]
  | Casr, [fst] ->
    return_type := [Types.I32Type];
    (emit_expr context fst) @
     [Binary (I32 I32Op.ShrS)]
  | Ccmpi Ceq, [fst; snd] ->
    return_type := [Types.I32Type];
    (emit_expr context fst) @
     (emit_expr context snd) @
     [Compare (I32 I32Op.Eq)]
  | Ccmpi Cne, [fst; snd] ->
    return_type := [Types.I32Type];
    (emit_expr context fst) @
     (emit_expr context snd) @
     [Compare (I32 I32Op.Ne)]
  | Ccmpi Clt, [fst; snd] ->
    return_type := [Types.I32Type];
    (emit_expr context fst) @
     (emit_expr context snd) @
     [Compare (I32 I32Op.LtS)]
  | Ccmpi Cle, [fst; snd] ->
    return_type := [Types.I32Type];
    (emit_expr context fst) @
     (emit_expr context snd) @
     [Compare (I32 I32Op.LeS)]
  | Ccmpi Cgt, [fst; snd] ->
    return_type := [Types.I32Type];
    (emit_expr context fst) @
     (emit_expr context snd) @
     [Compare (I32 I32Op.GtS)]
  | Ccmpi Cge, [fst; snd] ->
    return_type := [Types.I32Type];
    (emit_expr context fst) @
     (emit_expr context snd) @
     [Compare (I32 I32Op.GeS)]
  | Caddv, _ -> failwith "caddv" (* pointer addition that produces a [Val] (well-formed Caml value) *)
  (* pointer addition that produces a [Addr] (derived heap pointer) *)
  | Ccmpa _, _ -> failwith "Ccmpa"
  | Cnegf, [fst] ->
    return_type := [Types.F32Type];
    (emit_expr context fst) @
     [Unary (F32 F32Op.Neg)]
  | Cabsf, [fst] ->
    return_type := [Types.F32Type];
    (emit_expr context fst) @
     [Unary (F32 F32Op.Abs)]
  | Caddf, [fst; snd] ->
    return_type := [Types.F32Type];
    (emit_expr context fst) @
     (emit_expr context snd) @
     [Binary (F32 F32Op.Add)]
  | Csubf, [fst; snd] ->
    return_type := [Types.F32Type];
    (emit_expr context fst) @
     (emit_expr context snd) @
     [Binary (F32 F32Op.Sub)]
  | Cmulf, [fst; snd] ->
    return_type := [Types.F32Type];
    (emit_expr context fst) @
     (emit_expr context snd) @
     [Binary (F32 F32Op.Mul)]
  | Cdivf, [fst; snd] ->
    return_type := [Types.F32Type];
    (emit_expr context fst) @
     (emit_expr context snd) @
     [Binary (F32 F32Op.Div)]
  | Cfloatofint, [fst] ->
    return_type := [Types.F32Type];
    (emit_expr context fst) @
     [Convert (F32 F32Op.ReinterpretInt)]
  | Cintoffloat, [fst] ->
    return_type := [Types.I32Type];
    (emit_expr context fst) @
     [Convert (I32 I32Op.ReinterpretFloat)]
  | Ccmpf Ceq, [fst; snd] -> failwith "Ccmpf"
  | Ccmpf Cne, [fst; snd] -> failwith "Ccmpf"
  | Ccmpf Clt, [fst; snd] -> failwith "Ccmpf"
  | Ccmpf Cle, [fst; snd] -> failwith "Ccmpf"
  | Ccmpf Cgt, [fst; snd] -> failwith "Ccmpf"
  | Ccmpf Cge, [fst; snd] -> failwith "Ccmpf"
  | Craise _, _-> failwith "Craise" (*
    current idea for this to work:
    - return the "error type" - a default address that is an error, what
      it actually is should be checked further on
    - every stack action needs to be registered
    *)
  | Ccheckbound, _ -> failwith "Ccheckbound"
  | _ -> failwith ("Something is not handled here:" ^ string_of_int (List.length expression_list))
and emit_expr context (expression:expression) =
  match expression with
  | Cconst_int i -> return_type := [Types.I32Type]; [Const (I32 (I32.of_int_s i))]
  | Cconst_natint i -> return_type := [Types.I32Type]; [Const (I32 (I32.of_int_s (Nativeint.to_int i)))]
  | Cconst_float s -> return_type := [Types.F32Type]; [Const (F32 (F32.of_float s))]
  | Cconst_symbol symbol ->
    (try
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
    | _ ->
      [Const (I32 (bind_func context symbol))])
    (* print_endline ("Not found symbol:" ^ symbol); []) *)
  | Cconst_pointer i -> print_endline ("Not handled yet POINTER: " ^ string_of_int i); []
  | Cconst_natpointer  _ -> failwith "Cconst_natpointer"
  | Cblockheader (i, _) -> [Const (I32 (I32.of_int_s (Nativeint.to_int i)))]
  | Cvar ident ->
    return_type := [Types.I32Type];
    print_endline ("Getting:" ^ ident.Ident.name ^ "_" ^ string_of_int ident.Ident.stamp);
    (try (
      let var = local context (ident.Ident.name ^ "_" ^ string_of_int ident.Ident.stamp) in
      print_endline "A var local";
      [GetLocal var]
    )
    with
    | _ -> (
      print_endline "yikes...";
      []
    ))
  | Clet (ident, arg, fn_body) -> (
    locals := !locals @ [Types.I32Type];
    return_type := [Types.I32Type];
    let let_id = bind_local context (ident.Ident.name ^ "_" ^ string_of_int ident.Ident.stamp) in
    print_endline ("let id:" ^ Int32.to_string let_id);
    let result = emit_expr context arg in
    let result = if result = [] then (
      print_endline "temporary solution...";
    [Const (I32 (I32.of_int_s 0))]
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
  | Cop (Capply _, (Cconst_symbol hd)::tl, _) -> (
    try (
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
    print_endline "Cifthenelse is NOT fixed yet";
    let i = emit_expr context if_ in
    let t = emit_expr context then_ in
    let e = emit_expr context else_ in
    ignore(i);
    ignore(t);
    ignore(e);
    []
    (* [If (i, [], [])] *)
  | Cswitch  _ -> failwith "Cswitch"
  | Cloop  _ -> failwith "Cloop"
  | Ccatch  _ -> failwith "Ccatch"
  | Cexit  _ -> failwith "Cexit"
  | Ctrywith  _ -> failwith "Ctrywith"

let global_id = bind_global context "global_offset"

let wasm_module = ref {
  types = [];
  globals = [];
  tables = [];
  memories = Types.[{
    mtype = MemoryType {min = 0l; max = Some 100l}
  }];
  funcs = [];
  start = None;
  elems = [];
  data = [];
  imports = [];
  exports = [];
}

let setup_helper_functions () = (
  let globals = [{
    gtype = Types.GlobalType (Types.I32Type, Types.Mutable);
    value = [Const (I32 (I32.of_int_s 0))] (* TODO: update this location with all the other thingies properly... *)
  }]
  in
  ignore(bind_global context "global_memory_offset");
  let type_ = Types.FuncType ([Types.I32Type], [Types.I32Type]) in
  let types = [type_] in
  ignore(bind_type context "allocate_memory" type_);
  let funcs = [{
    name = "allocate_memory";
    ftype = 0l;
    locals = [Types.I32Type];
    body = [
      GetGlobal 0l;
      TeeLocal 1l;
      GetLocal 0l;
      Binary (I32 I32Op.Add);
      SetGlobal 0l;
      GetLocal 1l;
    ]
  }]
  in
  ignore(bind_func context "allocate_memory");
  let w = !wasm_module in
  wasm_module := Ast.{w with funcs = funcs;
              globals = globals;
              types = types};
)



let name s =
  try Utf8.decode s with Utf8.Utf8 ->
    failwith "invalid UTF-8 encoding"

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

let global_offset = ref 0

let compile_wasm_phrase ppf p =
  ignore(ppf);
  match p with
  | Cfunction ({fun_name; fun_args; fun_body; fun_fast; fun_dbg}) -> (
    let context = enter_func context in
    locals := [];
    return_type := [];
    let args = ref [] in
    List.iter (fun (ident, _) ->
      print_endline ("FUNC:" ^ fun_name  ^ "." ^ (ident.Ident.name ^ "_" ^ string_of_int ident.Ident.stamp));
      ignore(bind_local context (ident.Ident.name ^ "_" ^ string_of_int ident.Ident.stamp));
      args := [Types.I32Type] @ !args;
    ) fun_args;
    let func_id = try (
      bind_func context fun_name
    ) with
    | _ ->
      func context fun_name
    in
    let body = emit_expr context fun_body in
    let type_ = Types.FuncType (!args, !return_type) in
    let ftype = bind_type context fun_name type_ in
    print_endline ("Function id:" ^ (Int32.to_string ftype));

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
    wasm_module := Ast.{w with funcs = w.funcs @ [result]};
    let w = !wasm_module in
    wasm_module := Ast.{w with types = w.types @ [type_]};
    let w = !wasm_module in
    wasm_module := Ast.{w with exports = w.exports @ [export]};
    (* print_endline (Print_wat.print_func result); *)
    ())
  | Cdata dl -> (
    let datastring = ref "" in
    let start_offset = !global_offset in
    let data_id = ref 0l in
    let offset = ref 0 in
    List.iter (function
      | Cglobal_symbol s -> print_string ("global: " ^ s); ()
      | Csymbol_address s -> (
        (* either a func *)
        (* either a global *)
        (* either some memory address *)

        )
      | Cdefine_symbol s ->  (
          data_id := bind_data context s (I32.of_int_u start_offset)
        )
      | Cint8 i -> (
          datastring := !datastring ^ escape_int i 8;
          offset := !offset + 1;
          ()
        )
      | Cint16 i -> (
          datastring := !datastring ^ escape_int i 16;
          offset := !offset + 2;
          ()
        )
      | Cint32 ni -> (
          datastring := !datastring ^ escape_int (Nativeint.to_int ni) 32;
          offset := !offset + 4;
          ()
        )
      | Cint ni -> (
          datastring := !datastring ^ escape_int (Nativeint.to_int ni) 32;
          offset := !offset + 4;
          ()
        )
      | Csingle s -> print_string (" Csingle " ^ string_of_float s); ()
      | Cdouble d -> print_string (" Cdouble " ^ string_of_float d); ()
      | Cstring s -> (
          datastring := s;
          offset := !offset + 4;
          ()
        )
      | Cskip i -> print_string (" Cskip " ^ string_of_int i); ()
      | Calign i ->  print_string (" Calign " ^ string_of_int i); ()
    ) dl;
    let w = !wasm_module in
    print_endline ("datastring:" ^ !datastring);
    wasm_module := Ast.{w with data = w.data @ [{
      index =  0l;
      offset = [Const (I32 (I32.of_int_s start_offset))];
      init = !datastring
    }]};
    global_offset := !global_offset + !offset;
    )
