type var =
  | Cat of int
  | Dog of float
  | Monkey of string
  | Lion of char
  | Tiger
  | Rat

let a b = function
  | Cat 1 -> 10
  | Dog 2. -> 20
  | Monkey "ooh" -> 30
  | Lion 'r' -> 40
  | Tiger -> 50
  | Rat -> 60
  | _ -> 70
