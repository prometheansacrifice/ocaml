type var =
  | Cat of int
  | Dog of int
  (* | Monkey of string
  | Lion of char
  | Tiger
  | Rat *)

let a b = function
  | Cat 1 -> 222
  (* | Dog 2 -> 20
  | Dog _ -> 20
  | Monkey "ooh" -> 30
  | Lion 'r' -> 40
  | Tiger -> 50
  | Rat when b = 2 -> 99
  | Rat -> 999 *)
  | _ -> 70
