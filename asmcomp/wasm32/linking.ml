(* open Ast *)
open Values

type symbol_kind = 
  | Lfunction of string * int32
  | Ldata of string * ((int32 * int32 * int32) option)
  | Lsection of int32  

type symbol = {
  symbol: symbol_kind;
  flags: int32;
}

type symbol_table = symbol list

let name s =
    try Utf8.decode s with Utf8.Utf8 ->
      failwith "invalid UTF-8 encoding"

let create_symbol_table m = (
  let check_duplicates symbols = Ast.(
    let used_names:string list ref = ref [] in
    List.iter (fun symbol -> 
      if (List.exists (fun f -> f = symbol.name) !used_names) then 
        failwith ("Duplicate name:" ^ symbol.name)
      else 
        used_names := !used_names @ [symbol.name]
    ) symbols
  )
  in 
  check_duplicates m.Ast.symbols;
  let fix_missing_symbols () = Ast.(
    let code_symbols = ref [] in
    let rec handle_expr instr =
      (match instr with
      | FuncSymbol symbol :: remaining ->  
        (if not (List.exists (fun s -> s.name = symbol) m.symbols) && 
            not (List.exists (fun s -> s.name = symbol) !code_symbols) then  (
          print_endline ("ADD MISSING:" ^ symbol);
          code_symbols := !code_symbols @ [{
            name = symbol;
            details = Import ([],[])
          }])
        );
        handle_expr remaining              
      | DataSymbol symbol :: remaining ->                
        (if not (List.exists (fun s -> s.name = symbol) m.symbols) && 
            not (List.exists (fun s -> s.name = symbol) !code_symbols) then  (              
          code_symbols := !code_symbols @ [{
            name = symbol;
            details = Data ({
                index = (-1l);
                relocation_offset = 0l;
                size = 0l;
                offset = 0l
            })
          }])
        );
        handle_expr remaining
      | Block (_, instr) :: remaining
      | Loop (_, instr) :: remaining ->
        handle_expr instr;
        handle_expr remaining;
      | If (_, e1 , e2) :: remaining ->
        handle_expr e1;
        handle_expr e2;
        handle_expr remaining;
      | _ :: remaining -> 
        handle_expr remaining;
      | [] -> ())
    in 
    List.iter (fun (f:Ast.func) -> handle_expr f.body) m.funcs;
    {m with symbols = m.symbols @ !code_symbols})
  in 
  let m = fix_missing_symbols () in
  check_duplicates m.Ast.symbols;
  let turn_missing_functions_to_imports () = Ast.( 
    let missing_imports = List.filter (fun symbol ->
      match symbol.details with 
      | Function
      | Import _ -> (
        let key = symbol.name in
        not (List.exists (fun (i:Ast.import) -> (Ast.string_of_name i.item_name) = key) m.imports)
        &&
        not (List.exists (fun (f:Ast.func) -> f.name = key) m.funcs)
      )
      | _ -> false    
    ) m.symbols in  
    let imports = ref [] in
    let types = ref [] in
    List.iter (fun symbol ->
      let key = symbol.name in
      let name_ = ("empty_type_" ^ key) in
      let (arg, result) = match symbol.details with 
      | Import i -> i
      | Function -> 
        print_endline "WRONG: fix me please";
        ([], []) 
      | _ -> assert false
      in 
      let empty_type:Ast.type_ = {name = name_; details = Types.FuncType (arg, result)} in
      types := !types @ [empty_type];
      imports := !imports @ [{
        module_name = name "libasmrun";
        item_name = name key;
        idesc = FuncImport name_
      }];
    ) missing_imports;
    {m with imports = m.imports @ !imports;
            types = m.types @ !types}
  )
  in
  let m = turn_missing_functions_to_imports () in
  check_duplicates m.Ast.symbols;
  let add_missing_memory_addresses () = Ast.(  
    let result = ref m in
    List.iter (fun (s:data_part segment) -> 
      List.iter (fun d -> 
        match d with 
        | Symbol symbol -> (
            let has_symbol = (List.exists (fun (x:data_part segment) ->
              x.init.name = symbol
            ) m.data) 
            in
            let has_symbol = has_symbol || List.exists (fun s -> s.name = symbol) !result.symbols in
            if not has_symbol then (
              let w = !result in       
              result := {w with symbols = w.symbols @ 
                [{
                  name = symbol;
                  details = Data ({
                      index = (-1l);
                      relocation_offset = 0l;
                      size = 0l;
                      offset = 0l
                  })
                }]
              };              
            )            
          )
        | _ -> ()
      ) s.init.detail    
    ) m.data;
    !result
  )
  in  
  let m = add_missing_memory_addresses () in
  check_duplicates m.Ast.symbols;
  let add_elems_for_funcs () = Ast.(
    let elems = ref [] in
    List.iteri (fun i _ -> 
      elems := !elems @ [{
        index = 0l;
        offset=[Const (I32 (I32.of_int_s i))];
        init=[I32.of_int_s i]
      }]
    ) m.imports;
    let no_of_imports = List.length m.imports in
    List.iteri (fun i _ -> 
      elems := !elems @ [{
        index = 0l;
        offset=[Const (I32 (I32.of_int_s (no_of_imports + i)))];
        init=[I32.of_int_s (no_of_imports + i)]
      }]
    ) m.funcs;
    let no_of_elems = Int32.of_int (List.length !elems) in
    let tables:Ast.table list = Types.[{ttype = TableType ({min = no_of_elems; max = Some no_of_elems}, AnyFuncType)}] in
    {
      m with elems = !elems;
             tables = tables;
    }
  )
  in
  let m = add_elems_for_funcs () in
  check_duplicates m.Ast.symbols;
  m
)