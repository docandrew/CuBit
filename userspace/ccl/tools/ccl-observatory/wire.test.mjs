import { test } from 'node:test';
import assert from 'node:assert/strict';
import { encodeRequest, decodeResponse } from './wire.js';

test('request profile preserves uint64 IDs and bounds ASCII sources', () => {
  assert.deepEqual([...encodeRequest(1n, 'evaluate', '(+ 20 22)')], [0x84,1,1,2,0x69,40,43,32,50,48,32,50,50,41]);
  assert.equal(encodeRequest(18446744073709551615n, 'inspect')[2], 0x1b);
  for (const id of [0n,-1n,18446744073709551616n,1]) assert.throws(() => encodeRequest(id, 'inspect'));
  for (const source of ['é','x'.repeat(1025),'\0']) assert.throws(() => encodeRequest(1n,'evaluate',source));
  assert.throws(() => encodeRequest(1n, 'inspect', 'x'));
});
test('response profile rejects truncation, trailing bytes and type confusion', () => {
  const good = Uint8Array.from([0x85,1,1,3,0xf5,24,42]);
  assert.deepEqual(decodeResponse(good, 1n, 'clock'), {clock:{available:true,monotonicMs:'42'}});
  for (let i = 0; i < good.length; i++) assert.throws(() => decodeResponse(good.subarray(0,i),1n,'clock'));
  assert.throws(() => decodeResponse(Uint8Array.from([...good,0]),1n,'clock'));
  assert.throws(() => decodeResponse(good,2n,'clock'));
  assert.throws(() => decodeResponse(good,1n,'inspect'));
  for (const bad of [
    [0x85,1,1,3,1,24,42], [0x85,1,1,3,0xf5,25,0,42],
    [0x9f,1,1,3,0xf5,24,42,0xff], [0x85,1,1,3,0xf5,0xfa,0,0,0,0]
  ]) assert.throws(() => decodeResponse(Uint8Array.from(bad),1n,'clock'));
});

test('monitor messages carry generation and bounded lifecycle state', () => {
  assert.deepEqual([...encodeRequest(1n,'startMonitor','7')],[0x85,1,1,4,0x61,55,0]);
  assert.deepEqual([...encodeRequest(1n,'stopMonitor','',2n)],[0x85,1,1,5,0x60,2]);
  assert.throws(()=>encodeRequest(1n,'stopMonitor'));
  assert.throws(()=>encodeRequest(1n,'startMonitor','7',2n));
  const good=Uint8Array.from([0x8f,1,1,6,0xf5,1,2,3,25,3,232,0x61,55,0xf5,0x61,55,1,0,24,64,25,7,208]);
  const result=decodeResponse(good,1n,'monitor');
  assert.equal(result.generation,'2'); assert.equal(result.runs,'3');
  assert.equal(result.state,'Waiting'); assert.equal(result.result.message,'7');
  for(let i=0;i<good.length;i++) assert.throws(()=>decodeResponse(good.subarray(0,i),1n,'monitor'));
  const bad=good.slice(); bad[5]=6;
  assert.throws(()=>decodeResponse(bad,1n,'monitor'));
});
