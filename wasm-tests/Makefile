default: build

clean:
	rm -f test/*.wasm test/*.o test/*.c* test/*.out

install:
	npm install

build:
	# build the examples
	# ../ocamlopt test/curried_function.ml -o test/curried_function.cmf
	# ../ocamlopt -dcmm test/curried_function.ml
	cp ../libasmrun.wasm .
	# ../boot/ocamlrun ../ocamlopt test/curried_function.ml -I ../stdlib -o test/curried_function.wasm
	# ../../wabt/bin/wasm2wat --no-check test/curried_function.wasm
	# ../ocamlopt -wasm test/noncurried_function.ml -o test/noncurried_function.wasm
	../boot/ocamlrun ../ocamlopt test/noncurried_function.ml -I ../stdlib -o test/noncurried_function.wasm
	# ../ocamlopt -wasm test/exception_handling.ml -o test/exception_handling.wasm
	# ../ocamlopt -wasm test/array2.ml -o test/array.wasm
	# ../ocamlopt -wasm test/switch.ml -o test/switch.wasm
	# ../ocamlopt -wasm test/loop.ml -o test/loop.wasm
	# # ../ocamlopt -wasm test/crud.ml -o test/crud.wasm
	# ../ocamlopt -wasm test/arithmetic.ml -o test/arithmetic.wasm
	# ../ocamlopt -c -wasm test/file_b.ml -o test/file_b.wasm
	# ../ocamlopt -I test/ -wasm file_b.cmx test/file_a.ml -o test/file_a.wasm
	# run the test
	npm test

full: install build
