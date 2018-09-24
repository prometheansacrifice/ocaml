open Ast

type symbol_kind = 
  | Lfunction of string * int32
  | Ldata of string * ((int32 * int32 * int32) option)
  | Lsection of int32  

type symbol = {
  symbol: symbol_kind;
  flags: int32;
}

type symbol_table = symbol list


let create_symbol_table w =  
  
  let symbols = ref (
    List.mapi (fun i ({item_name;_}) -> 
      print_endline ("Import:" ^ Ast.string_of_name item_name);
      { 
        symbol = Lfunction (Ast.string_of_name item_name, Int32.of_int i);
        flags = 16l;
      }
    ) w.imports
  )
  in
  ignore(symbols)
  (* let import_counter = ref (List.length symbols) in
  let rec process instr = 
  | Call fn_name when not (List.exists (fun {name; _} -> fn_name = name) w.funcs && not (List.exists (fun s -> ) !symbols) ->
    print_endline ("Import:" ^ Ast.string_of_name item_name);
    symbols := symbols @ [{
        symbol = Limport (fn_name, Int32.of_int import_counter);
        flags = 16l}];
    import_counter := import_counter + 1;
  | Block (_, e) -> process e
  | Loop (_, e) -> process e
  | If (_, i, t, e) -> 
    process i;
    process t;
    process e
  | _ :: rest -> process rest
  | []
  in
  process w.body *)

(* let to_wasm_object_file wasm _tcmm _rt _locals =  *)

