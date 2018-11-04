#!/bin/bash

REPO_DIR=$PWD

git clone https://github.com/sanderspies/llvm llvmwasm
ls
cd ./llvmwasm/llvm/tools
git clone https://github.com/sanderspies/lld 
cd ../../

mkdir llvm-build
cd llvm-build
cmake -G "Unix Makefiles" -DCMAKE_INSTALL_PREFIX=$INSTALLDIR -DLLVM_TARGETS_TO_BUILD= -DLLVM_EXPERIMENTAL_TARGETS_TO_BUILD=WebAssembly ../llvm 
make -j 6
cd  ../../

git clone --recursive https://github.com/sanderspies/wabt
cd wabt
make
cd ../

export LLVM_HOME=$PWD/llvmwasm/llvm-build
./configure -no-pthread -no-debugger -no-curses -no-ocamldoc -no-graph -target-wasm32
make coldstart
./wabt/bin/wasm2wat --help
make wasm32
make wasm32-test
