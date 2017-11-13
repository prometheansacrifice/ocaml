default: build

clean:
	rm -f test/*.wasm

install:
	npm install

build:
	# build the examples
	../ocamlopt -wasm test/curried_function.ml -o test/curried_function.wasm
	../ocamlopt -wasm test/noncurried_function.ml -o test/noncurried_function.wasm
	../ocamlopt -wasm test/exception_handling.ml -o test/exception_handling.wasm
	../ocamlopt -wasm test/tuple.ml -o test/tuple.wasm
	../ocamlopt -wasm test/array.ml -o test/array.wasm
	../ocamlopt -wasm test/crud.ml -o test/crud.wasm
	../ocamlopt -wasm test/arithmetic.ml -o test/arithmetic.wasm
	# run the test
	yarn test

full: install build
