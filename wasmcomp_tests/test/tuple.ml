type x = int
type y = float
type z = x * y

let tuple ((a, b):z) = (b, a)
