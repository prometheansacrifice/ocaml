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
        expect(instance.exports.caml_program()).to.equal(1);
        const calculatedValue = instance.exports.call_1(5, ocamlInt(5));
        expect(jsInt(calculatedValue)).to.equal(20);
        done();
      })
      .catch(e => done(e))
    });
  });
  describe('curried functions', () => {
    it('should return 30 when 6 and 20 is given', done => {
      fetchAndInstantiate("/base/test/curried_function.wasm").then(instance => {
        // expect(instance.exports.caml_program()).to.equal(1);
        /*
          - call .curried_function(30)
        */
        console.info("YIHAW!");

        done()
      })
      .catch(e => { console.info('yikes'); return done(e)})
      // let test = WebAssembly.compile("foo").then(done).catch(()=>{ done("Did not compile..")} );
    });

  });



});

describe('arithmetic', () => {
  describe('int', () => {

  });
});
