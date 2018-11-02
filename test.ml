let a foo = foo

let b c = (
    ignore((a c) ^ "x");
    ignore((a 3) + 2);
    ignore(a '1')
)