type var =
  | Cat of int
  | Dog of int
  | Monkey of string
  | Lion of char
  | Tiger
  | Rat

let small_switch b = function
  | Cat 1 -> 222
  | _ -> 70

let big_switch b = function
  | Cat 1 -> 222
  | Dog 2 -> 20
  | Dog _ -> 22
  | Monkey "ooh" -> 30
  | Lion 'r' -> 40
  | Tiger -> 50
  | Rat when b = 2 -> 99
  | Rat -> 999
  | _ -> 70

let small_switch_test1 () =
  small_switch 2 (Cat 1)

let small_switch_test2 () =
  small_switch 5 (Cat 2)


let big_switch_test1 () =
  big_switch 2 (Cat 1)

let big_switch_test2 () =
  big_switch 2 (Cat 4)

let big_switch_test3 () =
  big_switch 2 (Dog 2)

let big_switch_test4 () =
  big_switch 2 (Dog 33)

let big_switch_test5 () =
  big_switch 2 (Monkey "ooh")

let big_switch_test6 () =
  big_switch 2 (Monkey "wooh")

let big_switch_test7 () =
  big_switch 2 (Lion 'r')

let big_switch_test8 () =
  big_switch 2 (Lion 'p')
