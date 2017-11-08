const importObject = {
  console: {
    log: function(e) {
      console.log('[ocaml-wasm] ', e);
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
        var i32 = new Uint32Array(instance.exports.memory.buffer.slice(1, 9));
        expect(i32[0]).to.equal(1792);
        expect(i32[1]).to.equal(21);
        var i32 = new Uint32Array(instance.exports.memory.buffer.slice(i32[1], i32[1] + 8));
        expect(i32[0]).to.equal(4);
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
        var i32 = new Uint32Array(instance.exports.memory.buffer.slice(1, 17));
        expect(i32[0]).to.equal(1792);
        expect(i32[1]).to.equal(21);
        var i32 = new Uint32Array(instance.exports.memory.buffer.slice(i32[1] + 8, i32[1] + 16));
        expect(i32[0]).to.equal(4);
        let func = instance.exports.table.get(i32[0]);
        const calculatedValue = func(ocamlInt(6), ocamlInt(20));
        expect(jsInt(calculatedValue)).to.equal(30);
        done();
      })
      .catch(e => done(e))
    });
    it('should return 30 when 6 and 20 is given - caml_curry2', done => {
      fetchAndInstantiate("/base/test/curried_function.wasm").then(instance => {
        var i32 = new Uint8Array(instance.exports.memory.buffer);
        expect(i32[0]).to.equal(0);
        expect(instance.exports.caml_program()).to.equal(1);
        expect(i32[0]).to.equal(4);
        var i32 = new Uint32Array(instance.exports.memory.buffer.slice(1, 17));
        expect(i32[0]).to.equal(1792);
        expect(i32[1]).to.equal(21);
        var i32 = new Uint32Array(instance.exports.memory.buffer.slice(i32[1] + 0, i32[1] + 8));
        expect(i32[0]).to.equal(5);
        let func = instance.exports.table.get(i32[0]);
        const calculatedValue = func(ocamlInt(1));

        var i32 = new Uint32Array(instance.exports.memory.buffer.slice(calculatedValue + 0, calculatedValue + 8));
console.debug(i32[1]);

        expect(jsInt(calculatedValue)).to.equal(30);
        done();
      })
      .catch(e => done(e))
    });
  });



});

describe('arithmetic', () => {
  describe('int', () => {

  });
});
