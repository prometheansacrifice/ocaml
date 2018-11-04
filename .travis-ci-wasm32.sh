#!/bin/bash

REPO_DIR=$PWD

mkdir -p llvmwasm
cd  llvmwasm
git clone https://github.com/sanderspies/llvm
cd $REPO_DIR

mkdir -p llvmwasm/llvm/tools
cd llvmwasm/llvm/tools
git clone https://github.com/sanderspies/lld 
cd $REPO_DIR

mkdir llvm-build
cd llvm-build
cmake -G "Unix Makefiles" -DCMAKE_INSTALL_PREFIX=$INSTALLDIR -DLLVM_TARGETS_TO_BUILD= -DLLVM_EXPERIMENTAL_TARGETS_TO_BUILD=WebAssembly ../llvm 
make -j 6
cd $REPO_DIR

git clone --recursive https://github.com/sanderspies/wabt
cd wabt
make
cd ../

export LLVM_HOME=$REPO_DIR/llvmwasm/llvm-build
./configure -no-pthread -no-debugger -no-curses -no-ocamldoc -no-graph -target-wasm32
make coldstart
./wabt/bin/wasm2wat --help
make wasm32
make wasm32-test
