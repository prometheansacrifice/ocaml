default: build

clean:
	rm -f test/*.wasm

install:
	npm install

build:
	# build the examples
	../ocamlopt -wasm test/curried_function.ml -o test/curried_function.wasm
	../ocamlopt -wasm test/noncurried_function.ml -o test/noncurried_function.wasm
	# run the test
	yarn test

full: install build
