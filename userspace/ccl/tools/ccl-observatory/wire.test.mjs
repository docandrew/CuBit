import { test } from 'node:test';
import assert from 'node:assert/strict';
import { encodeRequest, decodeResponse, decodePresentation, decodeImageRows, decodeCompletion } from './wire.js';

test('request profile preserves uint64 IDs and bounds ASCII sources', () => {
  assert.deepEqual([...encodeRequest(1n, 0n, 'evaluate', '(+ 20 22)')], [0x85,2,1,0,2,0x69,40,43,32,50,48,32,50,50,41]);
  assert.equal(encodeRequest(18446744073709551615n, 0n, 'inspect')[2], 0x1b);
  for (const id of [0n,-1n,18446744073709551616n,1]) assert.throws(() => encodeRequest(id, 0n, 'inspect'));
  // A tab's session travels after the request id; it must be a u64.
  assert.deepEqual([...encodeRequest(1n, 300n, 'inspect')], [0x85,2,1,0x19,1,44,1,0x60]);
  for (const session of [-1n,18446744073709551616n,1]) assert.throws(() => encodeRequest(1n, session, 'inspect'));
  for (const source of ['é','x'.repeat(1025),'\0']) assert.throws(() => encodeRequest(1n,0n,'evaluate',source));
  assert.throws(() => encodeRequest(1n, 0n, 'inspect', 'x'));
});
test('response profile rejects truncation, trailing bytes and type confusion', () => {
  const good = Uint8Array.from([0x85,2,1,3,0xf5,24,42]);
  assert.deepEqual(decodeResponse(good, 1n, 'clock'), {clock:{available:true,monotonicMs:'42'}});
  for (let i = 0; i < good.length; i++) assert.throws(() => decodeResponse(good.subarray(0,i),1n,'clock'));
  assert.throws(() => decodeResponse(Uint8Array.from([...good,0]),1n,'clock'));
  assert.throws(() => decodeResponse(good,2n,'clock'));
  assert.throws(() => decodeResponse(good,1n,'inspect'));
  for (const bad of [
    [0x85,2,1,3,1,24,42], [0x85,2,1,3,0xf5,25,0,42],
    [0x9f,2,1,3,0xf5,24,42,0xff], [0x85,2,1,3,0xf5,0xfa,0,0,0,0]
  ]) assert.throws(() => decodeResponse(Uint8Array.from(bad),1n,'clock'));
});

test('monitor messages carry generation and bounded lifecycle state', () => {
  assert.deepEqual([...encodeRequest(1n,0n,'startMonitor','7')],[0x86,2,1,0,4,0x61,55,0]);
  assert.deepEqual([...encodeRequest(1n,0n,'stopMonitor','',2n)],[0x86,2,1,0,5,0x60,2]);
  assert.throws(()=>encodeRequest(1n,0n,'stopMonitor'));
  assert.throws(()=>encodeRequest(1n,0n,'startMonitor','7',2n));
  const good=Uint8Array.from([0x8f,2,1,6,0xf5,1,2,3,25,3,232,0x61,55,0xf5,0x61,55,1,0,24,64,25,7,208]);
  const result=decodeResponse(good,1n,'monitor');
  assert.equal(result.generation,'2'); assert.equal(result.runs,'3');
  assert.equal(result.state,'Waiting'); assert.equal(result.result.message,'7');
  for(let i=0;i<good.length;i++) assert.throws(()=>decodeResponse(good.subarray(0,i),1n,'monitor'));
  const bad=good.slice(); bad[5]=6;
  assert.throws(()=>decodeResponse(bad,1n,'monitor'));
});

test('list results decode typed, signed elements and reject shape confusion', () => {
  // [1,id,2,ok,"L",5,pos,fuel,elementType=Integer,[1,-2,3],total=3]
  const good = Uint8Array.from([0x8b,2,1,2,0xf5,0x61,76,5,0,10,1,0x83,1,0x21,3,3]);
  const result = decodeResponse(good, 1n, 'evaluate');
  assert.equal(result.type, 'List');
  assert.deepEqual(result.list, { elementType: 'Integer', elements: ['1', '-2', '3'], total: '3' });
  const strings = Uint8Array.from([0x8b,2,1,2,0xf5,0x61,76,5,0,10,3,0x82,0x61,97,0x62,98,99,24,100]);
  assert.deepEqual(decodeResponse(strings, 1n, 'evaluate').list, { elementType: 'String', elements: ['a', 'bc'], total: '100' });
  for (const bad of [
    [0x88,2,1,2,0xf5,0x61,76,5,0,10],                         // list type without elements
    [0x8b,2,1,2,0xf5,0x61,76,1,0,10,1,0x81,1,1],              // elements without list type
    [0x8b,2,1,2,0xf5,0x61,76,5,0,10,2,0x81,1,1],              // Boolean list holding an integer
    [0x8b,2,1,2,0xf5,0x61,76,5,0,10,6,0x81,0x20,1],           // enumeration position below zero
    [0x8b,2,1,2,0xf5,0x61,76,5,0,10,9,0x80,0],                // unknown element type
    [0x8b,2,1,2,0xf5,0x61,76,5,0x20,10,1,0x80,0],             // negative diagnostic position
    [0x8b,2,1,2,0xf5,0x61,76,5,0,10,1,0x98,65,...Array(65).fill(1),65], // more than 64 elements
    [0x8b,2,1,2,0xf5,0x61,76,5,0,10,1,0x9f,1,0xff,1],         // indefinite-length array
    [0x8b,2,1,2,0xf5,0x61,76,5,0,10,1,0x82,1,2,1],            // total below the elements carried
    [0x8b,2,1,2,0xf5,0x61,76,5,0,10,1,0x81,1,0x19,0x10,0x01], // total beyond any list (4097)
    [0x8a,2,1,2,0xf5,0x61,76,5,0,10,1,0x81,1],                // the old 10-field shape
  ]) assert.throws(() => decodeResponse(Uint8Array.from(bad), 1n, 'evaluate'));
});

test('function values travel as display text with type Function', () => {
  const good = Uint8Array.from([0x88,2,1,2,0xf5,0x61,70,6,0,10]);
  assert.equal(decodeResponse(good, 1n, 'evaluate').type, 'Function');
  assert.throws(() => decodeResponse(Uint8Array.from([0x88,2,1,2,0xf5,0x61,70,7,0,10]), 1n, 'evaluate'));
});

// A small canonical CBOR writer for the structured responses.
function cbor(value) {
  const out = [];
  const head = (major, n) => {
    n = BigInt(n);
    if (n < 24n) { out.push(major * 32 + Number(n)); return; }
    const width = n <= 255n ? 1 : n <= 65535n ? 2 : n <= 4294967295n ? 4 : 8;
    out.push(major * 32 + ({1:24,2:25,4:26,8:27})[width]);
    for (let i = width - 1; i >= 0; i--) out.push(Number((n >> BigInt(i * 8)) & 255n));
  };
  const put = v => {
    if (typeof v === 'bigint' || typeof v === 'number') { const n = BigInt(v); if (n >= 0n) head(0, n); else head(1, -1n - n); }
    else if (typeof v === 'boolean') out.push(v ? 0xf5 : 0xf4);
    else if (typeof v === 'string') { head(3, v.length); for (const c of v) out.push(c.charCodeAt(0)); }
    else if (v instanceof Uint8Array) { head(2, v.length); out.push(...v); }
    else { head(4, v.length); v.forEach(put); }
  };
  put(value);
  return Uint8Array.from(out);
}

test('present requests carry source; image rows carry an id and a row', () => {
  assert.deepEqual([...encodeRequest(1n, 0n, 'present', '7')], [0x85,2,1,0,7,0x61,55]);
  assert.deepEqual([...encodeRequest(1n, 0n, 'imageRows', '', 5n, 8n)], [0x87,2,1,0,8,0x60,5,8]);
  assert.throws(() => encodeRequest(1n, 'imageRows', '', 0n, 0n));
  assert.throws(() => encodeRequest(1n, 0n, 'imageRows', '', 5n, 512n));
  assert.throws(() => encodeRequest(1n, 0n, 'imageRows', 'x', 5n, 0n));
});

test('presentations decode text, tables and pictures, and reject confusion', () => {
  const text = decodePresentation(cbor([2,1,7,true,'Integer','42',1,0,999997,[]]), 1n);
  assert.equal(text.form, 'text'); assert.equal(text.value, '42'); assert.equal(text.type, 'Integer');
  const failure = decodePresentation(cbor([2,1,7,false,'','type error',0,3,1000000,[]]), 1n);
  assert.equal(failure.form, 'failure'); assert.equal(failure.position, '3');
  const table = decodePresentation(cbor([2,2,7,true,'List<C>','[(C 1 "x")]',2,0,10,
    ['C', true, 2, [['a','Integer',true],['s','String',false]], [['1','"x"']]]]), 2n);
  assert.equal(table.table.rowType, 'C'); assert.equal(table.table.total, '2');
  assert.deepEqual(table.table.fields[0], { name: 'a', type: 'Integer', numeric: true });
  const picture = decodePresentation(cbor([2,3,7,true,'Image','(Image 320 120 77)',3,0,10,[320,120,77]]), 3n);
  assert.deepEqual(picture.picture, { width: 320, height: 120, id: 77n });
  for (const bad of [
    [1,1,7,true,'Integer','42',1,0,10,[1]],              // text with detail
    [1,1,7,false,'Integer','42',1,0,10,[]],              // failure flag, text form
    [1,1,7,true,'Image','x',3,0,10,[0,120,77]],          // empty picture
    [1,1,7,true,'Image','x',3,0,10,[320,120,0]],         // no image id
    [1,1,7,true,'C','x',2,0,10,['C',false,1,[['a','Integer',true]],[['1','2']]]], // ragged row
    [1,1,7,true,'C','x',2,0,10,['C',false,0,[['a','Integer',true]],[['1']]]],     // rows beyond total
    [1,1,7,true,'Integer','42',4,0,10,[]],               // unknown form
  ]) assert.throws(() => decodePresentation(cbor(bad), 1n), JSON.stringify(bad, (k, v) => typeof v === 'bigint' ? String(v) : v));
  assert.throws(() => decodePresentation(cbor([2,1,7,true,'Integer','42',1,0,10,[],0]), 1n));
  assert.throws(() => decodePresentation(cbor([2,1,7,true,'Integer','42',1,0,10,[[[[1]]]]]), 1n));
  assert.throws(() => decodePresentation(cbor([2,1,7,true,'Integer','42',1,0,10,new Uint8Array(1)]), 1n));
});

test('image rows decode bands of RGB and say when an image expired', () => {
  const rows = decodeImageRows(cbor([2,4,8,true,2,3,1,2,Uint8Array.from([1,2,3,4,5,6,7,8,9,10,11,12])]), 4n);
  assert.equal(rows.rowCount, 2); assert.equal(rows.firstRow, 1); assert.equal(rows.rgb.length, 12);
  assert.deepEqual(decodeImageRows(cbor([2,4,8,false,0,0,0,0,new Uint8Array(0)]), 4n), { known: false });
  for (const bad of [
    [1,4,8,true,2,3,1,2,Uint8Array.from([1,2,3])],       // short pixels
    [1,4,8,true,2,3,2,2,new Uint8Array(12)],              // past the last row
    [1,4,8,false,2,3,0,0,new Uint8Array(0)],              // expired with a size
    [1,4,8,true,2,3,0,1,'pixels'],                        // text, not bytes
  ]) assert.throws(() => decodeImageRows(cbor(bad), 4n));
});

// Written by the native encoder (tests/ccl-remote/wire_vectors.adb) from
// real evaluations inside the CCL interpreter, with the image interface.
test('native presentation vectors decode to what the console shows', async () => {
  const { readFileSync } = await import('node:fs');
  const vectors = Object.fromEntries(JSON.parse(readFileSync(new URL('./wire-vectors.json', import.meta.url)))
    .map(v => {
      const bytes = Uint8Array.from(v.hex.match(/../g).map(h => parseInt(h, 16)));
      return [v.name, v.op === 'imageRows' ? decodeImageRows(bytes, BigInt(v.id)) :
        v.op === 'complete' ? decodeCompletion(bytes, BigInt(v.id)) : decodePresentation(bytes, BigInt(v.id), v.op)];
    }));
  assert.deepEqual(vectors['monitor-idle'].monitor, { state: 'Empty', runs: '0' });
  const names = vectors['complete-names'];
  assert.equal(names.prefixLength, 7);
  assert.deepEqual(names.candidates.map(c => c.name), ['image.plot', 'image.pixels']);
  assert.ok(names.candidates.every(c => c.origin === 'service' && c.signature.startsWith('(image.')));
  assert.equal(vectors['complete-signature'].candidates.length, 0);
  assert.equal(vectors['complete-signature'].signature, '(image.plot Object) -> Object');
  assert.deepEqual(vectors['complete-words'].candidates.map(c => [c.name, c.origin]), [['sort', 'built-in'], ['sort-by', 'built-in']]);
  assert.deepEqual([vectors.integer.form, vectors.integer.type, vectors.integer.value], ['text', 'Integer', '42']);
  assert.deepEqual([vectors.failure.ok, vectors.failure.form, vectors.failure.position], [false, 'failure', '1']);
  assert.deepEqual([vectors.string.type, vectors.string.value], ['String', 'ab']);
  const t = vectors.table.table;
  assert.equal(vectors.table.form, 'table'); assert.equal(t.rowType, 'Event'); assert.equal(t.many, true); assert.equal(t.total, '2');
  assert.deepEqual(t.fields, [{ name: 'time', type: 'Integer', numeric: true }, { name: 'level', type: 'String', numeric: false }]);
  assert.deepEqual(t.rows, [['1200', '"info"'], ['1385', '"warn"']]);
  assert.deepEqual([vectors.record.table.many, vectors.record.table.rows], [false, [['7', '"debug"']]]);
  assert.equal(vectors.picture.form, 'picture');
  assert.deepEqual([vectors.picture.picture.width, vectors.picture.picture.height], [4, 3]);
  assert.deepEqual([vectors.rows.known, vectors.rows.width, vectors.rows.height, vectors.rows.firstRow, vectors.rows.rowCount, vectors.rows.rgb.length],
    [true, 4, 3, 0, 3, 36]);
  assert.deepEqual([vectors['rows-from'].firstRow, vectors['rows-from'].rowCount], [2, 1]);
  assert.deepEqual(vectors.expired, { known: false });
  assert.deepEqual([vectors['plot-band'].width, vectors['plot-band'].rowCount], [320, 8]);
  assert.equal(vectors.gallery.form, 'gallery'); assert.equal(vectors.gallery.gallery.total, '2');
  assert.deepEqual(vectors.gallery.gallery.pictures.map(p => [p.width, p.height]), [[4, 3], [320, 120]]);
  assert.equal(vectors.gallery.gallery.pictures[0].id, vectors.picture.picture.id, 'the same pixels, the same id');
  // The picture's id is the value's own id, however many digits it has.
  for (const name of ['picture', 'plot', 'large-id']) {
    const literal = vectors[name].value.match(/^\(Image (\d+) (\d+) (\d+)\)$/);
    assert.ok(literal, vectors[name].value);
    assert.equal(vectors[name].picture.id, BigInt(literal[3]), name);
  }
});
