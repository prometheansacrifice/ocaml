let addi a b = a + b

let mini a b = a - b

let divi a b = a / b

let muli a b = a * b

let modi a b = a mod b

let land_ a b = a land b

let lor_ a b = a lor b

let lxor_ a b = a lxor b

let lsl_ a b = a lsl b

let lsr_ a b = a lsr b

let asr_ a b = a asr b



let absf a = abs_float a

let addf a b = a +. b

let minf a b = a -. b

let divf a b = a /. b

let mulf a b = a *. b

let foo = ref 1.
let bar = ref 2.

let absf2 () = abs_float !foo

let addf2 () = !foo +. !bar

let minf2 () = !foo -. !bar

let divf2 () = !foo /. !bar

let mulf2 () = !foo *. !bar



(* TODO: add all possible arithmetic operations that OCaml can perform *)
(* TODO: add more complex calculations *)
