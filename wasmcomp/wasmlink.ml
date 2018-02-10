(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Link a set of .wasm files *)

open Misc
open Config
open Cmx_format
open Compilenv

let ext_obj = ".wasmo"
let ext_lib = ".wasml"

type error =
    File_not_found of string
  | Not_an_object_file of string
  | Missing_implementations of (string * string list) list
  | Inconsistent_interface of string * string * string
  | Inconsistent_implementation of string * string * string
  | Assembler_error of string
  | Linking_error
  | Multiple_definition of string * string * string
  | Missing_cmx of string * string

exception Error of error

(* Consistency check between interfaces and implementations *)

let crc_interfaces = Consistbl.create ()
let interfaces = ref ([] : string list)
let crc_implementations = Consistbl.create ()
let implementations = ref ([] : string list)
let implementations_defined = ref ([] : (string * string) list)
let cmx_required = ref ([] : string list)

let check_consistency file_name unit crc =
  begin try
    List.iter
      (fun (name, crco) ->
        interfaces := name :: !interfaces;
        match crco with
          None -> ()
        | Some crc ->
            if name = unit.ui_name
            then Consistbl.set crc_interfaces name crc file_name
            else Consistbl.check crc_interfaces name crc file_name)
      unit.ui_imports_cmi
  with Consistbl.Inconsistency(name, user, auth) ->
    raise(Error(Inconsistent_interface(name, user, auth)))
  end;
  begin try
    List.iter
      (fun (name, crco) ->
        implementations := name :: !implementations;
        match crco with
            None ->
              if List.mem name !cmx_required then
                raise(Error(Missing_cmx(file_name, name)))
          | Some crc ->
              Consistbl.check crc_implementations name crc file_name)
      unit.ui_imports_cmx
  with Consistbl.Inconsistency(name, user, auth) ->
    raise(Error(Inconsistent_implementation(name, user, auth)))
  end;
  begin try
    let source = List.assoc unit.ui_name !implementations_defined in
    raise (Error(Multiple_definition(unit.ui_name, file_name, source)))
  with Not_found -> ()
  end;
  implementations := unit.ui_name :: !implementations;
  Consistbl.set crc_implementations unit.ui_name crc file_name;
  implementations_defined :=
    (unit.ui_name, file_name) :: !implementations_defined;
  if unit.ui_symbol <> unit.ui_name then
    cmx_required := unit.ui_name :: !cmx_required

let extract_crc_interfaces () =
  Consistbl.extract !interfaces crc_interfaces
let extract_crc_implementations () =
  Consistbl.extract !implementations crc_implementations

(* Add C objects and options and "custom" info from a library descriptor.
   See bytecomp/bytelink.ml for comments on the order of C objects. *)

let lib_ccobjs = ref []
let lib_ccopts = ref []

let add_ccobjs origin l =
  if not !Clflags.no_auto_link then begin
    lib_ccobjs := l.lib_ccobjs @ !lib_ccobjs;
    let replace_origin =
      Misc.replace_substring ~before:"$CAMLORIGIN" ~after:origin
    in
    lib_ccopts := List.map replace_origin l.lib_ccopts @ !lib_ccopts
  end

let runtime_lib () =
  let libname =
    if !Clflags.gprofile
    then "libasmrunp" ^ ext_lib
    else "libasmrun" ^ !Clflags.runtime_variant ^ ext_lib in
  try
    if !Clflags.nopervasives then []
    else [ find_in_path !load_path libname ]
  with Not_found ->
    raise(Error(File_not_found libname))

let object_file_name name =
  let file_name =
    try
      find_in_path !load_path name
    with Not_found ->
      fatal_error "Asmlink.object_file_name: not found" in
  if Filename.check_suffix file_name ".cmx" then
    Filename.chop_suffix file_name ".cmx" ^ ext_obj
  else if Filename.check_suffix file_name ".cmxa" then
    Filename.chop_suffix file_name ".cmxa" ^ ext_lib
  else
    fatal_error "Asmlink.object_file_name: bad ext"

(* First pass: determine which units are needed *)

let missing_globals = (Hashtbl.create 17 : (string, string list ref) Hashtbl.t)

let is_required name =
  try ignore (Hashtbl.find missing_globals name); true
  with Not_found -> false

let add_required by (name, _crc) =
  try
    let rq = Hashtbl.find missing_globals name in
    rq := by :: !rq
  with Not_found ->
    Hashtbl.add missing_globals name (ref [by])

let remove_required name =
  Hashtbl.remove missing_globals name

let extract_missing_globals () =
  let mg = ref [] in
  Hashtbl.iter (fun md rq -> mg := (md, !rq) :: !mg) missing_globals;
  !mg

type file =
  | Unit of string * unit_infos * Digest.t
  | Library of string * library_infos

let read_file obj_name =
  let file_name =
    try
      find_in_path !load_path obj_name
    with Not_found ->
      raise(Error(File_not_found obj_name)) in
  if Filename.check_suffix file_name ".cmx" then begin
    (* This is a .cmx file. It must be linked in any case.
       Read the infos to see which modules it requires. *)
    let (info, crc) = read_unit_info file_name in
    Unit (file_name,info,crc)
  end
  else if Filename.check_suffix file_name ".cmxa" then begin
    let infos =
      try read_library_info file_name
      with Compilenv.Error(Not_a_unit_info _) ->
        raise(Error(Not_an_object_file file_name))
    in
    Library (file_name,infos)
  end
  else raise(Error(Not_an_object_file file_name))

let scan_file obj_name tolink = match read_file obj_name with
  | Unit (file_name,info,crc) ->
      (* This is a .cmx file. It must be linked in any case. *)
      remove_required info.ui_name;
      List.iter (add_required file_name) info.ui_imports_cmx;
      (info, file_name, crc) :: tolink
  | Library (file_name,infos) ->
      (* This is an archive file. Each unit contained in it will be linked
         in only if needed. *)
      add_ccobjs (Filename.dirname file_name) infos;
      List.fold_right
        (fun (info, crc) reqd ->
           if info.ui_force_link
             || !Clflags.link_everything
             || is_required info.ui_name
           then begin
             remove_required info.ui_name;
             List.iter (add_required (Printf.sprintf "%s(%s)"
                                        file_name info.ui_name))
               info.ui_imports_cmx;
             (info, file_name, crc) :: reqd
           end else
             reqd)
        infos.lib_units tolink


let scan_file_wasm obj_name tolink = match read_file obj_name with
  | Unit (file_name,info,crc) ->
      (* This is a .cmx file. It must be linked in any case. *)
      remove_required info.ui_name;
      List.iter (add_required file_name) info.ui_imports_cmx;
      (info, file_name, crc) :: tolink
  | Library (file_name,infos) ->
      (* This is an archive file. Each unit contained in it will be linked
         in only if needed. *)
      add_ccobjs (Filename.dirname file_name) infos;
      List.fold_right
        (fun (info, crc) reqd ->
           if info.ui_force_link
             || !Clflags.link_everything
             || is_required info.ui_name
           then begin
             remove_required info.ui_name;
             List.iter (add_required (Printf.sprintf "%s(%s)"
                                        file_name info.ui_name))
               info.ui_imports_cmx;
             (info, file_name, crc) :: reqd
           end else
             reqd)
        infos.lib_units tolink


(* Second pass: generate the startup file and link it with everything else *)

let make_startup_file ppf units_list =
  Wasmgen.reset ();
  let compile_wasm_phrase p = Wasmgen.compile_wasm_phrase ppf p in
  Location.input_name := "caml_startup"; (* set name of "current" input *)
  Compilenv.reset "_startup";
  (* set the name of the "current" compunit *)
  Emit.begin_assembly ();
  Wasmgen.setup_helper_functions ();
  let name_list =
    List.flatten (List.map (fun (info,_,_) -> info.ui_defines) units_list) in
  compile_wasm_phrase (Cmmgen.entry_point name_list);
  let units = List.map (fun (info,_,_) -> info) units_list in
  List.iter compile_wasm_phrase (Cmmgen.generic_functions false units);
  Array.iteri
    (fun i name -> compile_wasm_phrase (Cmmgen.predef_exception i name))
    Runtimedef.builtin_exceptions;
  compile_wasm_phrase (Cmmgen.global_table name_list);
  compile_wasm_phrase
    (Cmmgen.globals_map
       (List.map
          (fun (unit,_,crc) ->
               let intf_crc =
                 try
                   match List.assoc unit.ui_name unit.ui_imports_cmi with
                     None -> assert false
                   | Some crc -> crc
                 with Not_found -> assert false
               in
                 (unit.ui_name, intf_crc, crc, unit.ui_defines))
          units_list));
  compile_wasm_phrase(Cmmgen.data_segment_table ("_startup" :: name_list));
  compile_wasm_phrase(Cmmgen.code_segment_table ("_startup" :: name_list));
  let all_names = "_startup" :: "_system" :: name_list in
  compile_wasm_phrase (Cmmgen.frame_table all_names);
  if Config.spacetime then begin
    compile_wasm_phrase (Cmmgen.spacetime_shapes all_names);
  end;
  Wasmgen.turn_missing_functions_to_imports ();
  let wasm_module = !Wasmgen.wasm_module in
  Print_wat.module_ stdout 80 wasm_module;
  let s = Encode.encode wasm_module in
  let output_name = "temp_file.wasm"
  in
  let oc = open_out_bin output_name in
  output_string oc s;
  close_out oc;
  print_endline "created wasm startup file...";
  Emit.end_assembly ()

let make_shared_startup_file ppf units =
  let compile_wasm_phrase p = Wasmgen.compile_wasm_phrase ppf p in
  Location.input_name := "caml_startup";
  Compilenv.reset "_shared_startup";
  Emit.begin_assembly ();
  List.iter compile_wasm_phrase
    (Cmmgen.generic_functions true (List.map fst units));
  compile_wasm_phrase (Cmmgen.plugin_header units);
  compile_wasm_phrase
    (Cmmgen.global_table
       (List.map (fun (ui,_) -> ui.ui_symbol) units));
  (* this is to force a reference to all units, otherwise the linker
     might drop some of them (in case of libraries) *)
  Emit.end_assembly ()

let call_linker_shared file_list output_name =
  if not (Ccomp.call_linker Ccomp.Dll output_name file_list "")
  then raise(Error Linking_error)

let link_shared ppf objfiles output_name =
  Profile.record_call output_name (fun () ->
    let units_tolink = List.fold_right scan_file objfiles [] in
    List.iter
      (fun (info, file_name, crc) -> check_consistency file_name info crc)
      units_tolink;
    Clflags.ccobjs := !Clflags.ccobjs @ !lib_ccobjs;
    Clflags.all_ccopts := !lib_ccopts @ !Clflags.all_ccopts;
    let objfiles = List.rev (List.map object_file_name objfiles) @
      (List.rev !Clflags.ccobjs) in

    let startup =
      if !Clflags.keep_startup_file || !Emitaux.binary_backend_available
      then output_name ^ ".startup" ^ ext_asm
      else Filename.temp_file "camlstartup" ext_asm in
    let startup_obj = output_name ^ ".startup" ^ ext_obj in
    Asmgen.compile_unit output_name
      startup !Clflags.keep_startup_file startup_obj
      (fun () ->
         make_shared_startup_file ppf
           (List.map (fun (ui,_,crc) -> (ui,crc)) units_tolink)
      );
    call_linker_shared (startup_obj :: objfiles) output_name;
    remove_file startup_obj
  )

type fmapping = W1 | W2
let concat_wasm w1 w2 = Ast.(
  let new_wasm = ref {
    types = [];
    globals = [];
    tables = Types.[{
      ttype = TableType ({min = 0l; max = Some 0l}, AnyFuncType)
    }];
    memories = Types.[{
      (* TODO: this needs to be improved when doing GC *)
      mtype = MemoryType {min = 100l; max = Some 100l}
    }];
    funcs = [];
    start = None;
    elems = [];
    data = [];
    imports = [];
    exports = [];
  }
  in
  let w1_type_mapping = ref [] in
  let w2_type_mapping = ref [] in
  let w1_global_mapping = ref [] in
  let w2_global_mapping = ref [] in
  let w1_func_mapping = ref [] in
  let w2_func_mapping = ref [] in
  let func_mapping = ref [] in
  (* let w1_data_mapping = ref [] in
  let w2_data_mapping = ref [] in *)
  let find_type type_ =
    let w = !new_wasm in
    let result = ref (-1) in
    List.iteri (fun i t ->
      if (t = type_) then
        result := i
    ) w.types;
    !result
  in
  let add_type type_ =
    let w = !new_wasm in
    let existing_type_loc = find_type type_ in
    if existing_type_loc > -1 then
      existing_type_loc
    else (
      new_wasm := {w with types = w.types @ [type_]};
      List.length w.types
    )
  in
  let add_w1_type type_ =
    w1_type_mapping := !w1_type_mapping @ [add_type type_]
  in
  let add_w2_type type_ =
    w2_type_mapping := !w2_type_mapping @ [add_type type_]
  in
  let find_global global =
    let w = !new_wasm in
    let result = ref (-1) in
    List.iteri (fun i g ->
      if (g = global) then
        result := i
    ) w.globals;
    !result
  in
  let add_global global =
    let w = !new_wasm in
    let existing_global_loc = find_global global in
    if existing_global_loc > -1 then
      existing_global_loc
    else (
      new_wasm := {w with globals = w.globals @ [global]};
      List.length w.globals
    )
  in
  let add_w1_global global =
    w1_global_mapping := !w1_global_mapping @ [add_global global]
  in
  let add_w2_global global =
    w2_global_mapping := !w2_global_mapping @ [add_global global]
  in
  let find_import import =
    let w = !new_wasm in
    let result = ref (-1) in
    List.iteri (fun i im ->
      if im = import then
        result := i
    ) w.imports;
    !result
  in
  let add_import import =
    let w = !new_wasm in
    let existing_import = find_import import in
    if (existing_import > -1) then
      existing_import
    else (
      new_wasm := {w with imports = w.imports @ [import]};
      List.length w.imports
    )
  in
  let add_w1_import import =
    w1_func_mapping := !w1_func_mapping @ [add_import import]
  in
  let add_w2_import import =
    w2_func_mapping := !w2_func_mapping @ [add_import import]
  in
  let find_func func =
    let w = !new_wasm in
    let result = ref (-1) in
    List.iteri (fun i f ->
      if f = func then
        result := i
    ) w.funcs;
    !result
  in
  let find_func_by_name func_name =
    let w = !new_wasm in
    let result = ref (-1) in
    List.iteri (fun i (f:Ast.func) ->
      if f.name = func_name then
        result := i
    ) w.funcs;
    !result
  in
  let add_func func =
    let w = !new_wasm in
    let existing_func = find_func func in
    if (existing_func > -1)  then (
      existing_func
    )
    else (
      new_wasm := {w with funcs = w.funcs @ [func]};
      List.length w.funcs + List.length w.imports
    )
  in
  let add_w1_func func =
    w1_func_mapping := !w1_func_mapping @ [add_func func];
    func_mapping := !func_mapping @ [W1]
  in
  let add_w2_func func =
    w2_func_mapping := !w2_func_mapping @ [add_func func];
    func_mapping := !func_mapping @ [W2]
  in
  let remove_import import other_loc =
    print_endline ("move import " ^ Ast.string_of_name import.item_name ^ " to " ^ string_of_int other_loc ^ ": ");
    let w = !new_wasm in
    let import_pos = ref (-1) in
    List.iteri (fun i im ->
      if (im = import) then (
        import_pos := i
      )
    ) w.imports;
    let ip = !import_pos in
    let fix_pos fi =
      if fi < ip then
        (print_endline ("- stay the same:" ^ string_of_int fi);
        fi)
      else if fi = ip then
        (print_endline ("- move to new location:" ^ string_of_int fi ^ " becomes: " ^ string_of_int other_loc);
        other_loc)
      else
        (print_endline ("- minus one:" ^ string_of_int fi);
         fi - 1)
    in
    w1_func_mapping := List.map fix_pos !w1_func_mapping;
    w2_func_mapping := List.map fix_pos !w2_func_mapping
  in
  let remove_linking_imports () = (
    let w = !new_wasm in
    let fixed_imports = List.filter (fun i ->
      if (i.module_name = (Wasmgen.name "linking")) then (
        let func_pos = find_func_by_name (Ast.string_of_name i.item_name) in
        if func_pos > -1 then (
          remove_import i func_pos;
          false
        ) else (
          true
        )
      ) else (
          true
      )
    ) w.imports
    in
    new_wasm := {
      w with imports = fixed_imports
    }
  )
  in
  let func_with_names ast = (
    let imports_length = List.length ast.imports in
    List.mapi (fun i (func:Ast.func) ->
      print_endline ("looking for the name of: " ^ string_of_int (i + imports_length));
      let pos = Int32.of_int (i + imports_length) in
      let item = List.find (fun e -> e.edesc = FuncExport pos) ast.exports in
      print_endline ("found:" ^ Ast.string_of_name item.name);
      { func with name = Ast.string_of_name item.name }
    ) ast.funcs
  )
  in
  let rec fix_instr (ft: int list) (f: int list) result = function
    | Block (s, il) :: remaining -> fix_instr ft f (result @ [Block (s, fix_instr ft f [] il)]) remaining
    | Loop (s, il) :: remaining -> fix_instr ft f (result @ [Loop (s, fix_instr ft f [] il)]) remaining
    | If (crt, t, e) :: remaining -> fix_instr ft f (result @ [If (crt, fix_instr ft f [] t, fix_instr ft f [] e)]) remaining
    | Call v :: remaining ->
      let mi = List.nth f (Int32.to_int v) in
      fix_instr ft f (result @ [Call (Int32.of_int mi)]) remaining
    | (CallIndirect t) :: remaining ->
      let mt = List.nth ft (Int32.to_int t) in
      fix_instr ft f (result @ [CallIndirect (Int32.of_int mt)]) remaining
    | _ as item :: remaining -> (fix_instr ft f (result @ [item]) remaining)
    | [] -> result
  in
  let fix_functions () = (
    (* BY THE POWER OF GRAYSKULL FIX THE FUNCTIONS. *)
    let w = !new_wasm in
    List.iteri (fun i f -> print_endline ("-a:" ^ string_of_int i ^ ", " ^ string_of_int f)) !w1_type_mapping;
    List.iteri (fun i f -> print_endline ("-b:" ^ string_of_int i ^ ", " ^ string_of_int f)) !w2_type_mapping;
    new_wasm := {w with
      funcs = List.mapi (fun i func ->
        let func_map = List.nth !func_mapping i in
        let (ft, f) = match func_map with
        | W1 -> (!w1_type_mapping, !w1_func_mapping)
        | W2 -> (!w2_type_mapping, !w2_func_mapping)
        in
        {func with
          body = fix_instr ft f [] func.body;
          ftype = Int32.of_int (List.nth ft (Int32.to_int func.ftype))
        }
      ) w.funcs
    }
  )
  in
  print_endline "concatting wasm files...";
  print_endline "types...";
  List.iter add_w1_type w1.types;
  List.iter add_w2_type w2.types;
  print_endline "globals...";
  List.iter add_w1_global w1.globals;
  List.iter add_w2_global w2.globals;
  print_endline "imports...";
  List.iter add_w1_import w1.imports;
  List.iter add_w2_import w2.imports;
  print_endline "add functions...";
  List.iter add_w1_func (func_with_names w1);
  print_endline "add functions... 2";
  List.iter add_w2_func (func_with_names w2);
  print_endline "Remove linking imports";
  remove_linking_imports ();
  print_endline "Fix functions";
  fix_functions ();
  print_endline "mkay";


  (* ignore(w1);
  let has_export ast _name = List.filter (fun e ->
    match e with
    | ({name; edesc = FuncExport _ }) when _name = name -> true
    | _ -> false) ast.exports
  in
  let func_types = ref [] in
  let add_func_type to_ast from_ast type_ =
    let old_var = ref (-1l) in
    List.iteri (fun i t ->
      if t == type_ then (
        old_var := Int32.of_int i
      )
    ) from_ast.types;
    let to_ast = {to_ast with types = to_ast.types @ [type_]} in
    let new_var = Int32.of_int (List.length to_ast.types) in
    func_types := !func_types @ [(
      !old_var, new_var
    )];
    to_ast
  in
  let rec change_instr_vars changed_vars changed_types result = function
    | Block (s, il) :: remaining -> change_instr_vars changed_vars changed_types (result @ [Block (s, change_instr_vars changed_vars changed_types [] il)]) remaining
    | Loop (s, il) :: remaining -> change_instr_vars changed_vars changed_types (result @ [Loop (s, change_instr_vars changed_vars changed_types [] il)]) remaining
    | If (crt, t, e) :: remaining -> change_instr_vars changed_vars changed_types (result @ [If (crt, change_instr_vars changed_vars changed_types [] t, change_instr_vars changed_vars changed_types [] e)]) remaining
    | Call v :: remaining ->
      let call = List.find_opt (fun (o, _) -> o = v) changed_vars in
      let c = match call with
      | Some (_, n) -> [Call n]
      | None -> [Call v]
      in
      change_instr_vars changed_vars changed_types (result @ c) remaining
    | Const (Values.I32 v) :: (CallIndirect t) :: remaining ->
      let var = List.find_opt (fun (o, _) -> o = v) changed_vars in
      let fn_id = (match var with
      | Some (_, n) -> [Const (Values.I32 n)]
      | None -> [Const (Values.I32 v)])
      in
      let ft = List.find_opt (fun (o, _) -> o = t) changed_types in
      let call_ft = match ft with
      | Some (_, n) -> [CallIndirect n]
      | None -> [CallIndirect t]
      in
      change_instr_vars changed_vars changed_types (result @ fn_id @ call_ft) remaining
    | _ as item :: remaining -> (change_instr_vars changed_vars changed_types (result @ [item]) remaining)
    | [] -> result
  in
  List.iter(fun f -> ignore(change_instr_vars [] [] [] f.body)) w1.funcs;
  let add_imports ast imports =
    (* TODO: remove import duplicates *)
    let changed_fns = List.mapi (fun i _ -> (Int32.of_int i, Int32.of_int (i + (List.length imports)))) ast.funcs in

    {ast with
      imports = ast.imports @ imports;
      funcs = List.map (fun f -> {f with body = change_instr_vars changed_fns !func_types [] f.body}) ast.funcs
    }
  in
  let add_functions ast il funcs =
    (* we expect the funcs start from 0 so...*)
    let changed_fns = List.mapi (fun i _ ->
      (Int32.of_int (il + i), Int32.of_int (i + il + (List.length ast.funcs + List.length ast.imports)))
    ) funcs in
    let funcs = List.map (fun func ->
      let ft = List.find_opt (fun (o, _) -> o = func.ftype) !func_types in
      let ft = match ft with
      | Some (_, n) -> n
      | None -> failwith "Can't find func type in func_types"
      in
      {func with
        body = change_instr_vars changed_fns !func_types [] func.body;
        ftype = ft
      }) funcs
    in
    {ast with
      funcs = ast.funcs @ funcs
    }
  in

  (*let remove_import ast =
    ignore(ast);
    ()
  in *)
  let w1 = List.fold_left (fun cur f -> add_func_type cur w2 f) w1 w2.types in
  let w1 = add_imports w1 w2.imports in
  let w1 = add_functions w1 (List.length (w2.imports)) w2.funcs in
  List.iter (fun _ -> print_endline "WOOORD" ) (has_export w2 (Wasmgen.name "caml_curry2"));
  w1 *)
  !new_wasm
)

let call_wasm_linker file_list startup_file output_name =
  (* link the existing wasm files together, startup first... *)
  ignore(output_name);
  let existing_files = List.filter Sys.file_exists file_list
  in
  let decode file_name = (
    let ic = open_in_bin file_name in
    let len = in_channel_length ic in
    let result = really_input_string ic len in
    close_in ic;
    Decode.decode "" result
  )
  in
  let wasm_file = List.fold_left (fun wasm file -> concat_wasm (decode file) wasm) (decode startup_file) existing_files
  in
  Print_wat.module_ stdout 80 wasm_file;
  let s = Encode.encode wasm_file in
  let oc = open_out_bin output_name in
  output_string oc s;
  close_out oc

let call_linker file_list startup_file output_name =
  let main_dll = !Clflags.output_c_object
                 && Filename.check_suffix output_name Config.ext_dll
  and main_obj_runtime = !Clflags.output_complete_object
  in
  let files = startup_file :: (List.rev file_list) in
  let libunwind =
    if not Config.spacetime then []
    else if not Config.libunwind_available then []
    else String.split_on_char ' ' Config.libunwind_link_flags
  in
  let files, c_lib =
    if (not !Clflags.output_c_object) || main_dll || main_obj_runtime then
      files @ (List.rev !Clflags.ccobjs) @ runtime_lib () @ libunwind,
      (if !Clflags.nopervasives || main_obj_runtime
       then "" else Config.native_c_libraries)
    else
      files, ""
  in
  let mode =
    if main_dll then Ccomp.MainDll
    else if !Clflags.output_c_object then Ccomp.Partial
    else Ccomp.Exe
  in
  if not (Ccomp.call_linker mode output_name files c_lib)
  then raise(Error Linking_error)

(* Main entry point *)

let link ppf objfiles output_name =
  print_endline "wasmlink.link";
  Profile.record_call output_name (fun () ->
    let stdlib =
      if !Clflags.gprofile then "stdlib.p.cmxa" else "stdlib.cmxa" in
    let stdexit =
      if !Clflags.gprofile then "std_exit.p.cmx" else "std_exit.cmx" in
    let objfiles =
      if !Clflags.nopervasives then objfiles
      else if !Clflags.output_c_object then stdlib :: objfiles
      else stdlib :: (objfiles @ [stdexit]) in
    let units_tolink = List.fold_right scan_file_wasm objfiles [] in
    Array.iter remove_required Runtimedef.builtin_exceptions;
    begin match extract_missing_globals() with
      [] -> ()
    | mg -> raise(Error(Missing_implementations mg))
    end;
    List.iter
      (fun (info, file_name, crc) -> check_consistency file_name info crc)
      units_tolink;
    Clflags.ccobjs := !Clflags.ccobjs @ !lib_ccobjs;
    Clflags.all_ccopts := !lib_ccopts @ !Clflags.all_ccopts;
(* put user's opts first *)
    let startup =
      if !Clflags.keep_startup_file || !Emitaux.binary_backend_available
      then output_name ^ ".startup" ^ ext_asm
      else Filename.temp_file "camlstartup" ext_asm in
    let startup_obj = Filename.temp_file "camlstartup" ext_obj in
      Asmgen.compile_unit output_name
        startup !Clflags.keep_startup_file startup_obj
        (fun () -> make_startup_file ppf units_tolink);

      let wasm_module = call_wasm_linker (List.map object_file_name objfiles) startup_obj output_name in
      ignore(wasm_module);
      (* Wasmgen.turn_missing_functions_to_imports (); *)
      (* let wasm_module = !Wasmgen.wasm_module in
      Print_wat.module_ stdout 80 wasm_module;
      let s = Encode.encode wasm_module in
      let oc = open_out_bin output_name in
      output_string oc s;
      close_out oc; *)


      (* Misc.try_finally
          (fun () ->
            let res = call_wasm_linker (List.map object_file_name objfiles) startup_obj output_name in
            res
            )
          (fun () -> remove_file startup_obj) *)
    )

(* Error report *)

open Format

let report_error ppf = function
  | File_not_found name ->
      fprintf ppf "Cannot find file %s" name
  | Not_an_object_file name ->
      fprintf ppf "The file %a is not a compilation unit description"
        Location.print_filename name
  | Missing_implementations l ->
     let print_references ppf = function
       | [] -> ()
       | r1 :: rl ->
           fprintf ppf "%s" r1;
           List.iter (fun r -> fprintf ppf ",@ %s" r) rl in
      let print_modules ppf =
        List.iter
         (fun (md, rq) ->
            fprintf ppf "@ @[<hov 2>%s referenced from %a@]" md
            print_references rq) in
      fprintf ppf
       "@[<v 2>No implementations provided for the following modules:%a@]"
       print_modules l
  | Inconsistent_interface(intf, file1, file2) ->
      fprintf ppf
       "@[<hov>Files %a@ and %a@ make inconsistent assumptions \
              over interface %s@]"
       Location.print_filename file1
       Location.print_filename file2
       intf
  | Inconsistent_implementation(intf, file1, file2) ->
      fprintf ppf
       "@[<hov>Files %a@ and %a@ make inconsistent assumptions \
              over implementation %s@]"
       Location.print_filename file1
       Location.print_filename file2
       intf
  | Assembler_error file ->
      fprintf ppf "Error while assembling %a" Location.print_filename file
  | Linking_error ->
      fprintf ppf "Error during linking"
  | Multiple_definition(modname, file1, file2) ->
      fprintf ppf
        "@[<hov>Files %a@ and %a@ both define a module named %s@]"
        Location.print_filename file1
        Location.print_filename file2
        modname
  | Missing_cmx(filename, name) ->
      fprintf ppf
        "@[<hov>File %a@ was compiled without access@ \
         to the .cmx file@ for module %s,@ \
         which was produced by `ocamlopt -for-pack'.@ \
         Please recompile %a@ with the correct `-I' option@ \
         so that %s.cmx@ is found.@]"
        Location.print_filename filename name
        Location.print_filename  filename
        name

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )

let reset () =
  Consistbl.clear crc_interfaces;
  Consistbl.clear crc_implementations;
  implementations_defined := [];
  cmx_required := [];
  interfaces := [];
  implementations := []
