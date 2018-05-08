(module $libasmrun
    (memory (;0;) 100 100)
    (data (i32.const 0) "\15\37")
    (global $mempointer_pos i32 (i32.const 0))
    (type $t0 (func (param i32) (result i32)))
    (func $caml_alloc (type $t0) (param $amount i32) (result i32)
        get_global $mempointer_pos
        i32.load
        get_local $amount
        i32.add
        get_global $mempointer_pos
        i32.store
        get_global $mempointer_pos
        i32.load
    )
    (func $foo (type $t0) (param $amount i32) (result i32)
        get_local $amount
        call $caml_alloc
    )
    (export "caml_alloc" (func $caml_alloc))
    (export "mempointer_pos" (global $mempointer_pos))
    ;; (global $_memory_pointer (mut i32) (i32.const 2000))
)