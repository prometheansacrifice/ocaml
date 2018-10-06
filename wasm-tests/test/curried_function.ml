let bar () = "Once upon a time there was a 2131232323 #@$!@#@#@ ^&%%^&%^& 1"

let hello name = name

let store = ref 19

let test () = !store 

let float () = 5.4

let curried_function b c = 
    store := !store + b + c + 4;
    !store

let curried_function_2 = curried_function 5

let curried_function_3 x = curried_function_2 x

let curried_function_4 () = !store

let bar2 () = "yolo" (* size: 2 x 4 - 3 - 1 = 4 *)
let bar21 () = "yolo1" (* size: 2 x 4 - 2 - 1 *)
let bar22 () = "yolo12" (* size: 2 x 4 - 1 - 1 *)
let bar223 () = "yolo123" (* size: 2 x 4 - 0 - 1 *)
let bar224 () = "yolo1234" (* size: 3 x 4 - 3 - 1 *)

