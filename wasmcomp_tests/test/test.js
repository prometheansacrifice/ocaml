let count = 0;
let globalInstance = null;
const importObject = {
  console: {
    log: function(e) {
      console.log('[ocaml-wasm] ', e);
    }
  },
  js: {
    tryWith: function(memPos, tryBody_, withHandler_) {
      const table = globalInstance.exports.table;
      const tryBody = table.get(tryBody_);
      const withHandler = table.get(withHandler_);
      try {
        tryBody(memPos);
      }
      catch(e) {
        withHandler(memPos, e);
      }
    },
    raise(e){
      throw e;
    },
    caml_fresh_oo_id(v) {
      count++;
    }
  }

};

function fetchAndInstantiate(url) {
  return fetch(url)
    .then(response => response.arrayBuffer())
    .then(bytes => WebAssembly.instantiate(bytes, importObject))
    .then(results => results.instance);
}

function ocamlInt(i) {
  return (i << 1) + 1;
}

function jsInt(i) {
  if (i & 1 === 0)
    throw Error('Expected an OCaml int, but got a pointer');
  return i >> 1;
}

describe('functions', () => {
  describe('noncurried function', () => {
    it('should return 20 when 5 is given', done => {
      fetchAndInstantiate("/base/test/noncurried_function.wasm").then(instance => {
        var i32 = new Uint8Array(instance.exports.memory.buffer);
        expect(i32[0]).to.equal(0);
        expect(instance.exports.caml_program()).to.equal(1);
        expect(i32[0]).to.equal(4);
        var i32 = new Uint32Array(instance.exports.memory.buffer.slice(5, 13));
        expect(i32[0]).to.equal(1792);
        expect(i32[1]).to.equal(25);
        var i32 = new Uint32Array(instance.exports.memory.buffer.slice(i32[1], i32[1] + 8));
        expect(i32[0]).to.equal(8);
        let func = instance.exports.table.get(i32[0]);
        const calculatedValue = func(ocamlInt(5));
        expect(jsInt(calculatedValue)).to.equal(20);
        done();
      })
      .catch(e => done(e))
    });
  });
  describe('curried functions', () => {
    it('should return 30 when 6 and 20 is given - direct pointer', done => {
      fetchAndInstantiate("/base/test/curried_function.wasm").then(instance => {
        var i32 = new Uint8Array(instance.exports.memory.buffer);
        expect(i32[0]).to.equal(0);
        expect(instance.exports.caml_program()).to.equal(1);
        expect(i32[0]).to.equal(4);
        var i32 = new Uint32Array(instance.exports.memory.buffer.slice(5, 13));
        expect(i32[0]).to.equal(1792);
        expect(i32[1]).to.equal(25);
        var i32 = new Uint32Array(instance.exports.memory.buffer.slice(i32[1] + 8, i32[1] + 16));
        expect(i32[0]).to.equal(8);
        let func = instance.exports.table.get(i32[0]);
        const calculatedValue = func(ocamlInt(6), ocamlInt(20));
        expect(jsInt(calculatedValue)).to.equal(30);
        done();
      })
      .catch(e => done(e))
    });

    // this function needs a change in wasmgen.ml - an extra argument needs to be added
    // to functions which have a fun_dbg list > 0
    it('should return 30 when 6 and 20 is given - caml_curry2', done => {
      fetchAndInstantiate("/base/test/curried_function.wasm").then(instance => {
        var i8 = new Uint8Array(instance.exports.memory.buffer);
        expect(i8[0]).to.equal(0);
        expect(instance.exports.caml_program()).to.equal(1);
        expect(i8[0]).to.equal(4);
        var i32_ = new Uint32Array(instance.exports.memory.buffer.slice(5, 13));
        expect(i32_[0]).to.equal(1792);
        expect(i32_[1]).to.equal(25);

        // the pointer to caml_curry2
        var i32 = new Uint32Array(instance.exports.memory.buffer.slice(i32_[1] + 0, i32_[1] + 4));
        expect(i32[0]).to.equal(9);

        // invoke caml_curry2
        let camlCurry2 = instance.exports.table.get(i32[0]);
        const allocatedMemoryAddress = camlCurry2(ocamlInt(6), i32_[1]);
        var i32 = new Uint32Array(instance.exports.memory.buffer.slice(allocatedMemoryAddress, allocatedMemoryAddress + 20));

        // calculated value is a pointer to a allocated memory block
        // var i32 = new Uint32Array(instance.exports.memory.buffer.slice(allocatedMemoryAddress + 4, allocatedMemoryAddress + 20));
        let caml_curry2_1 = instance.exports.table.get(i32[0]);

        const cv2 = caml_curry2_1(ocamlInt(20), allocatedMemoryAddress);
        expect(jsInt(cv2)).to.equal(30);

        const cv3 = caml_curry2_1(ocamlInt(10), allocatedMemoryAddress + 4);
        expect(jsInt(cv3)).to.equal(20);
        done();
      })
      .catch(e => done(e))
    });
  });

  // it('should support a simple crud', done => {
  //   fetchAndInstantiate("/base/test/crud.wasm").then(instance => {
  //     var i8 = new Uint8Array(instance.exports.memory.buffer);
  //     expect(i8[0]).to.equal(0);
  //     expect(instance.exports.caml_program()).to.equal(1);
  //     expect(i8[0]).to.equal(4);
  //     let dev = instance.exports.camlCrud__create_1211("Foo", 0);
  //     let dev2 = instance.exports.camlCrud__create_1211("Foo2", 0);
  //
  //
  //     done();
  //   })
  //   .catch(e => done(e))
  // });


});


describe('exception handling', () => {
  describe('raise + try with', () => {
    it ('should support basic exception handling', done => {
      fetchAndInstantiate("/base/test/exception_handling.wasm").then(instance => {

        expect(instance.exports.caml_program()).to.equal(1);

        globalInstance = instance;
        try {
          instance.exports.camlException_handling__other_1006();
        } catch (pointer) {
          var i32 = new Uint32Array(instance.exports.memory.buffer.slice(pointer + 0, pointer + 64));
          expect(i32[0]).to.equal(874);
          expect(i32[1]).to.equal(1);
        }
        try {
          instance.exports.camlException_handling__other2_1008();
        } catch (pointer) {
          var i32 = new Uint32Array(instance.exports.memory.buffer.slice(pointer + 0, pointer + 8));
          expect(i32[0]).to.equal(874);
          expect(i32[1]).to.equal(3);
        }

        expect(instance.exports.camlException_handling__foo_1207(55)).to.equal(ocamlInt(500));

        // expect(jsInt(instance.exports.camlException_handling__foo_1208(ocamlInt(10)))).to.equal(384);

        done();
      })
      .catch(e => { console.debug('what:', e); done(e) })
    })
  });
});

describe('tuple', () => {
  describe('tuple', () => {
    it ('should support tuples', done => {
      fetchAndInstantiate("/base/test/tuple.wasm").then(instance => {
        console.debug('A TUPLE...');
      });
    });
  });
});

describe('switch', () => {
  describe('switch', () => {
    it ('should switch statements', done => {
      fetchAndInstantiate("/base/test/switch.wasm").then(instance => {
        console.debug('A SWITCH...');
      })
      .catch(e => { done(e) })
    });
  });
});

describe('array', () => {
  describe('simple', () => {
    it ('should retrieve an array value', done => {
      fetchAndInstantiate("/base/test/array.wasm").then(instance => {
        expect(instance.exports.caml_program()).to.equal(1);
        expect(instance.exports.camlArray2__ala_1005()).to.equal(ocamlInt(10000000));
        expect(instance.exports.camlArray2__bla_1008()).to.equal(ocamlInt(2));
        try {
          instance.exports.camlArray2__cla_1011();
          done('should give an exception');
        }
        catch(pointer) {
          expect(pointer).to.equal(185);
        }
        try {
          instance.exports.camlArray2__dla_1014();
          done('should give an exception');
        }
        catch(pointer) {
          expect(pointer).to.equal(185);
        }
        done();
      })
      .catch(e => { done(e) })
    });
  });
});


describe('arithmetic', () => {
  describe('int', () => {
    it ('should support basic arithmetic', done => {
      fetchAndInstantiate("/base/test/arithmetic.wasm").then(instance => {
        expect(instance.exports.caml_program()).to.equal(1);
        expect(instance.exports.camlArithmetic__addi_1002(ocamlInt(5), ocamlInt(6))).to.equal(ocamlInt(11));

        expect(instance.exports.camlArithmetic__mini_1005(ocamlInt(10), ocamlInt(5))).to.equal(ocamlInt(5));
        expect(instance.exports.camlArithmetic__divi_1008(ocamlInt(10), ocamlInt(5))).to.equal(ocamlInt(2));
        expect(instance.exports.camlArithmetic__muli_1011(ocamlInt(10), ocamlInt(5))).to.equal(ocamlInt(50));

        expect(instance.exports.camlArithmetic__modi_1014(ocamlInt(10), ocamlInt(3))).to.equal(ocamlInt(1));
        expect(instance.exports.camlArithmetic__modi_1014(ocamlInt(99), ocamlInt(3))).to.equal(ocamlInt(0));
        expect(instance.exports.camlArithmetic__modi_1014(ocamlInt(101), ocamlInt(3))).to.equal(ocamlInt(2));

        expect(instance.exports.camlArithmetic__land__1017(ocamlInt(10), ocamlInt(3))).to.equal(ocamlInt(2));
        expect(instance.exports.camlArithmetic__lor__1020(ocamlInt(4), ocamlInt(2))).to.equal(ocamlInt(6));
        expect(instance.exports.camlArithmetic__lxor__1023(ocamlInt(10), ocamlInt(3))).to.equal(ocamlInt(9));
        expect(instance.exports.camlArithmetic__lsl__1026(ocamlInt(10), ocamlInt(1))).to.equal(ocamlInt(20));
        expect(instance.exports.camlArithmetic__lsr__1029(ocamlInt(10), ocamlInt(1))).to.equal(ocamlInt(5));
        expect(instance.exports.camlArithmetic__asr__1032(ocamlInt(10), ocamlInt(1))).to.equal(ocamlInt(5));
        done();
      })
      .catch(e => { done(e) })
    })
  });
  describe('float', () => {
    xit ('should support basic arithmetic', done => {
      fetchAndInstantiate("/base/test/arithmetic.wasm").then(instance => {
        /* floats work, but tests need to be fixed - need to figure how one can access the memory from js */
        //
        //
        //
        // expect(instance.exports.caml_program()).to.equal(1);
        //
        // let alloc = instance.exports.table.get(3);
        // let addr = alloc(8);
        //
        // const x = new Float32Array(instance.exports.memory.buffer.slice(addr, addr + 12));
        // x[0] = 5;
        // x[1] = 12;
        //
        // instance.exports.memory
        // const x21 = new Float32Array(instance.exports.memory.buffer.slice(addr, addr + 12));
        // console.debug('riiight1:', x21[0], x21[1], x21[2], x[0], x[1]);
        //
        // var pointer = instance.exports.camlArithmetic__divf_1043(addr, addr + 4);
        // const x2 = new Float32Array(instance.exports.memory.buffer.slice(pointer, pointer + 4));
        // console.debug('riiight:', x2[0]);

        done();
      })
      .catch(e => { done(e) })
    })
  });
});
