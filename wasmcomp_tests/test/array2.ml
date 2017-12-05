let array2 (x:int) = [|x|]

let foo =
  [|10000000;
    2;
  |]

let ala () = foo.(0)
let bla () = foo.(1)
let cla () = foo.(2)
let dla () = foo.(-1)
