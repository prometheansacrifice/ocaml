type pl = OCaml | Reason | JavaScript

type developer = {
  name: string;
  programming_language: pl;
}

let developers: (developer list ref) = ref []

let add developer =
  developers := !developers @ [developer]

let all () =
  !developers

let create name pl =
  {
    name = name;
    programming_language = pl
  }
(*
let () = (
  create "Foo" OCaml;
  create "Foo" Reason;
  create "Foo" JavaScript;
  ()
) *)
