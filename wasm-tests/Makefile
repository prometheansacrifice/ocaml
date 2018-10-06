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
	../boot/ocamlrun ../ocamlopt test/curried_function.ml -dcmm -I ../stdlib -o test/curried_function.wasm
	# ../../wabt/bin/wasm2wat --no-check test/curried_function.wasm
	# ../ocamlopt -wasm test/noncurried_function.ml -o test/noncurried_function.wasm
	../boot/ocamlrun ../ocamlopt test/noncurried_function.ml -I ../stdlib -o test/noncurried_function.wasm
	# ../ocamlopt -wasm test/exception_handling.ml -o test/exception_handling.wasm
	# ../boot/ocamlrun ../ocamlopt test/array2.ml -I ../stdlib -o test/array.wasm
	../boot/ocamlrun ../ocamlopt test/switch.ml -I ../stdlib -o test/switch.wasm
	../boot/ocamlrun ../ocamlopt test/loop.ml -I ../stdlib -o test/loop.wasm
	# # ../ocamlopt -wasm test/crud.ml -o test/crud.wasm
	../boot/ocamlrun ../ocamlopt test/arithmetic.ml -I ../stdlib -o test/arithmetic.wasm
	../boot/ocamlrun ../ocamlopt -c test/file_b.ml -I ../stdlib  -o test/file_b.o
	../boot/ocamlrun ../ocamlopt -I test/ -I ../stdlib file_b.cmx test/file_a.ml -o test/file_a.wasm
	# run the test
	npm test

full: install build
