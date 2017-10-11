[@@@ocaml.warning "-27"]

open Ast

(* let print_locals (locals:value_type list) = *)

let print_instr = function
  | Unreachable -> "(unreachable)"
  | Nop -> "(nop)"
  | _ -> "not supported"

  (* | Nop                               (* do nothing *)
  | Block of stack_type * instr list  (* execute in sequence *)
  | Loop of stack_type * instr list   (* loop header *)
  | If of stack_type * instr list * instr list  (* conditional *)
  | Br of var                         (* break to n-th surrounding label *)
  | BrIf of var                       (* conditional break *)
  | BrTable of var list * var         (* indexed break *)
  | Return                            (* break from function body *)
  | Call of var                       (* call function *)
  | CallIndirect of var               (* call function through table *)
  | Drop                              (* forget a value *)
  | Select                            (* branchless conditional *)
  | GetLocal of var                   (* read local variable *)
  | SetLocal of var                   (* write local variable *)
  | TeeLocal of var                   (* write local variable and keep value *)
  | GetGlobal of var                  (* read global variable *)
  | SetGlobal of var                  (* write global variable *)
  | Load of loadop                    (* read memory at address *)
  | Store of storeop                  (* write memory at address *)
  | CurrentMemory                     (* size of linear memory *)
  | GrowMemory                        (* grow linear memory *)
  | Const of literal                  (* constant *)
  | Test of testop                    (* numeric test *)
  | Compare of relop                  (* numeric comparison *)
  | Unary of unop                     (* unary numeric operator *)
  | Binary of binop                   (* binary numeric operator *)
  | Convert of cvtop                  (* conversion *) *)

let print_func (func:func) =
  "\t(func " ^ func.name ^ "\n"
  ^ List.fold_left (fun x y -> (x ^ "\n\t\t" ^ (print_instr y))) "\t\t" func.body ^
  "\t)"

let print (module_:module_) =
  "(module \n" ^

  ")"
