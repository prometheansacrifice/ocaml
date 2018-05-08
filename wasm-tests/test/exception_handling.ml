type error =
  | Exception1
  | Exception2
  (* | Exception3
  | Exception4 *)

exception Exception of error

let other (a: int) =
  (* print_endline (string_of_int a); *)
  raise (Exception Exception1)

let other2 (b: int) =
  raise (Exception Exception2)

(* let other3 () =
  raise (Exception Exception3)

let other4 () =
  raise (Exception Exception4) *)

let c = ref 12

let foo b = (
  let a = 12 + b in
  let otherthingy x =
    x * 12
  in
  (try
    c := (otherthingy !c) + (other2 a * b);
    !c
  with
  | Exception (Exception1) -> 300
  | Exception (Exception2) -> 500
  );
  (* print_int !c *)
)
(* | Exception (Exception3) -> 700
| Exception (Exception4) -> 900 *)
