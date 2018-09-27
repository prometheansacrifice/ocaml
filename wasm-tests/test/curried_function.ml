let curried_function b c = b + c + 4

let curried_function_2 = curried_function 5

let curried_function_3 x = curried_function_2 x
