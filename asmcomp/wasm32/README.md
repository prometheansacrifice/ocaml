Compiling to WebAssembly
===

Overview
---
cmm -> typed cmm -> wasm ast -> linking -> binary

Typed CMM
---

In normal OCaml compilation, after typechecking has been completed most types are removed.
However, for WASM, including some additional type information at this stage is helpful
to generate the types required by WebAssembly.
Type enhancements to the CMM provided by OCaml are defined in [typed_cmm.ml](typed_cmm.ml).
`Typed_cmm` is used in [emit.mlp](emit.mlp) to translate that CMM to the WebAssembly AST,
which then gets transformed in actual binary code in [encode.ml](encode.ml).

Linking
---

We currently depend on LLVM for linking with other WASM code.
This is desirable for interoperability with code written in other languages.

*NOTE:* The `wasm-backend` branch currently depends on a fork of LLVM to support garbage collection.
The intention is to eventually upstream these changes into LLVM.

Contributing
---

Currently there are two active branches for WASM support in OCaml.
Ongoing work on the garbage collector is happening in [wasm-backend](https://github.com/SanderSpies/ocaml/tree/wasm-backend).
As that branch may not always be in a clean and buildable state,
new contributors may want to start with the [before_gc](https://github.com/SanderSpies/ocaml/tree/before_gc) branch,
which should be more stable.

Modules
---

The entry-point for WASM compilation is [asmcomp/asmgen.mlp-wasm](../asmgen.mlp-wasm).
The main points of interest in the pipeline are:

[Typed_cmm](Typed_cmm.ml): Adds types to the CMM representation.

[Emit](emit.mlp): Convert from CMM to WebAssembly object file AST.

[Encode](encode.ml): Write the binary. Modified from the WASM Reference Implementation.


Tests
---

```sh
make wasm-test
```

Useful Links
---

- [WebAssembly Object File Linking](https://github.com/WebAssembly/tool-conventions/blob/master/Linking.md)
- [Real World OCaml Ch. 23. The Compiler Backend: Bytecode and Native code](https://v1.realworldocaml.org/v1/en/html/the-compiler-backend-byte-code-and-native-code.html)
- [Real World OCaml Ch. 20: Memory Representation of Values](https://v1.realworldocaml.org/v1/en/html/memory-representation-of-values.html)


