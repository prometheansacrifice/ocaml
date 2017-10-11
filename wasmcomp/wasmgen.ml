(* open Format *)
(* open Clflags *)
[@@@ocaml.warning "-20-27"]

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
  { types : types; tables : space; memories : space;
    funcs : space; locals : space; globals : space; labels : int32 VarMap.t }

let empty_context () =
  { types = empty_types (); tables = empty (); memories = empty ();
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

let rec to_operations context (expression_list:expression list) operation =
  match operation, expression_list with
  | Capply _, _ -> print_endline "This capply not handled apparently *SHAME*"; []
  | Cextcall _, _ -> failwith "Cextcall"
  | Cload (memory_chunk, mutable_flag), _ -> (
      let align = 0 in
      let offset = 0l in
      (* let expression_list = List.map (fun f -> emit_expr context f) expression_list in *)
      let load_instr = Ast.Types.(match memory_chunk with
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
      [load_instr]
    )
  | Calloc, _ -> print_endline "Ask our GC friend for memory"; []
  | Cstore (memory_chunk, initialization_or_assignment), _ ->
    print_endline "Cstore something... OR NOT";
    []
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
    print_endline "Ceq test...";
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
  | Cadda, _ ->  failwith "cadda" (* pointer addition that produces a [Addr] (derived heap pointer) *)
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
  | Cconst_int i -> [Const (I32 (I32.of_int_s i))]
  | Cconst_natint i -> [Const (I32 (I32.of_int_s (Nativeint.to_int i)))]
  | Cconst_float s -> [Const (F64 (F64.of_float s))]
  | Cconst_symbol symbol ->
    [Call (func context symbol)]
  | Cconst_pointer i -> print_endline ("Not handled yet POINTER: " ^ string_of_int i); []
  | Cconst_natpointer  _ -> failwith "Cconst_natpointer"
  | Cblockheader  _ -> failwith "Cblockheader"
  | Cvar ident ->
    (* print_endline ("Getting:" ^ ident.Ident.name ^ "_" ^ string_of_int ident.Ident.stamp); *)
    let var = local context (ident.Ident.name ^ "_" ^ string_of_int ident.Ident.stamp) in
    [GetLocal var]
  | Clet (ident, arg, fn_body) -> (
    let let_id = func context (ident.Ident.name ^ "_" ^ string_of_int ident.Ident.stamp) in
    emit_expr context arg
    @
    [SetLocal let_id]
    @
    emit_expr context fn_body
    )
  | Cassign _ -> failwith "Cassign"
  | Ctuple  _ -> failwith "Ctuple"
  | Cop (Capply _, (Cconst_symbol hd)::tl, _) -> (
      let expression_list = List.fold_left (fun lst f -> lst @ (emit_expr context f)) [] tl in
      expression_list @
      [Call (func context hd)]
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

let wasm_module = ref {
  types = [];
  globals = [];
  tables = [];
  memories = [];
  funcs = [];
  start = None;
  elems = [];
  data = [];
  imports = [];
  exports = [];
}

let name s =
  try Utf8.decode s with Utf8.Utf8 ->
    failwith "invalid UTF-8 encoding"

let compile_wasm_phrase ppf p =
  ignore(ppf);
  match p with
  | Cfunction ({fun_name; fun_args; fun_body; fun_fast; fun_dbg}) -> (
    let context = enter_func context in
    List.iter (fun (ident, _) ->
      ignore(bind_local context (ident.Ident.name ^ "_" ^ string_of_int ident.Ident.stamp))
    ) fun_args;
    let var = bind_func context fun_name in
    print_endline ("Function id:" ^ (Int32.to_string var));
    let result = {
      name = fun_name;
      ftype = var;
      locals = [];
      body = emit_expr context fun_body;
    } in
    let export = {
      name = name fun_name;
      edesc = FuncExport var;
    } in
    let type_ = Types.FuncType ([], []) in
    let w = !wasm_module in
    wasm_module := Ast.{w with funcs = w.funcs @ [result]};
    let w = !wasm_module in
    wasm_module := Ast.{w with types = w.types @ [type_]};
    let w = !wasm_module in
    wasm_module := Ast.{w with exports = w.exports @ [export]};
    (* print_endline (Print_wat.print_func result); *)
    ())
  | Cdata dl -> ignore(List.map (
    (* first is module exports ?! *)
    (* second is gc roots ?! *)
    function
      | Cglobal_symbol s -> print_string ("global: " ^ s); []
      | Csymbol_address s -> print_string ("csymbol: " ^ s); []
      | Cdefine_symbol s ->  print_string (" define_symbol: " ^ s); [] (* create a value in the current scope set-local foo address *)
      | Cint8 i -> print_string (" Cint " ^ string_of_int i); []
      | Cint16 i -> print_string (" Cint16 " ^ string_of_int i); []
      | Cint32 ni ->
        [Const (I32 (Nativeint.to_int32 ni));
        Store {ty = Ast.Types.I32Type; align = 0; offset = (I32.of_int_s 0); sz = Some Mem32}]
      | Cint i -> print_string (" Cint " ^ Nativeint.to_string i); []
      | Csingle s -> print_string (" Csingle " ^ string_of_float s); []
      | Cdouble d -> print_string (" Cdouble " ^ string_of_float d); []
      | Cstring s -> print_string (" Cstring " ^ s); []
      | Cskip i -> print_string (" Cskip " ^ string_of_int i); []
      | Calign i ->  print_string (" Calign " ^ string_of_int i); []
    ) dl)
