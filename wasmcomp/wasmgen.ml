(* open Format *)
(* open Clflags *)
open Cmm

module Source = Source
module Types = Wasm_types
module Ast = Ast
module Script = Script

(* add the VarMap stuff from wasm's parser.mly *)

let compile_wasm_phrase ppf p =
  print_endline "- handle wasm here";
  ignore(ppf);
  match p with
  | Cfunction ({fun_name; fun_args; fun_body; fun_fast; fun_dbg}) -> (
    ignore(fun_name);
    ignore(fun_args);
    ignore(fun_body);
    ignore(fun_fast);
    ignore(fun_dbg);
    ())
  | Cdata dl -> Emit.data dl
