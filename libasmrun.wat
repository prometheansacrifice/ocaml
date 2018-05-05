(module $libasmrun
    (type $t0 (func (param i32) (result i32)))
    (func $caml_alloc (type $t0) (param $amount i32) (result i32)
        get_local $amount
    )
    (func $foo (type $t0) (param $amount i32) (result i32)
        get_local $amount
        call $caml_alloc
    )
    (export "caml_alloc" (func $caml_alloc))
)