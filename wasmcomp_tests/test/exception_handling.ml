type error =
  | Exception1
  | Exception2
  (* | Exception3
  | Exception4 *)

exception Exception of error

let other () =
  raise (Exception Exception1)

let other2 () =
  raise (Exception Exception2)

(* let other3 () =
  raise (Exception Exception3)

let other4 () =
  raise (Exception Exception4) *)

let foo () = try
  other ()
with
| Exception (Exception1) -> 300
| Exception (Exception2) -> 500
(* | Exception (Exception3) -> 700
| Exception (Exception4) -> 900 *)
