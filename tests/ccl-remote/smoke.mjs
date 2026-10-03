// Real native guest test. No evaluator or API relay runs on Linux.
import { request } from 'node:http';
import { connect } from 'node:net';
import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';
import { encodeRequest, decodeResponse, decodePresentation, decodeImageRows, decodeCompletion } from '../../userspace/ccl/tools/ccl-observatory/wire.js';

let id = 0n;
function exchange(body, { origin = 'http://127.0.0.1:8787', method = 'POST', extra = {}, split = false } = {}) {
  return new Promise((resolve,reject) => {
    const req = request({hostname:'127.0.0.1',port:18445,path:'/ccl',method,
      headers:{Origin:origin,'Content-Type':'application/cbor','Content-Length':body.length,...extra},
      agent:false, timeout:8000}, res => {
      let count = 0; const chunks = [];
      res.on('data', c => {count += c.length; if(count > 8192) res.destroy(new Error('Oversized reply')); else chunks.push(c);});
      res.on('error',reject);
      res.on('end',()=>resolve({status:res.statusCode,headers:res.headers,body:Buffer.concat(chunks)}));
    });
    req.on('timeout',()=>req.destroy(new Error('Native HTTP timeout'))); req.on('error',reject);
    if(split) { req.flushHeaders(); req.write(body.subarray(0,2)); setTimeout(()=>req.end(body.subarray(2)),50); }
    else req.end(body);
  });
}
// One browser tab's session: its definitions and streams persist on the guest.
const session = 0x5eedc0de1234n;
async function invoke(operation, source='', options={}, target=0n) {
  const queryId = ++id;
  const r = await exchange(encodeRequest(queryId, session, operation, source, target), options);
  assert.equal(r.status,200); assert.equal(r.headers['access-control-allow-origin'],'http://127.0.0.1:8787');
  assert.equal(r.headers['content-type'],'application/cbor');
  return decodeResponse(r.body,queryId,operation);
}
let snapshot;
for(let i=0;i<30;i++) {
  try { snapshot=await invoke('inspect'); break; }
  catch(e) { if(i===29) throw e; await new Promise(r=>setTimeout(r,500)); }
}
assert.ok(BigInt(snapshot.processId)>0n); assert.ok(BigInt(snapshot.networkProcessId)>0n);
assert.equal(snapshot.clock.available,true);
console.log('PASS native bindings:', snapshot.processId, snapshot.networkProcessId, snapshot.clockProcessId);
const preflight = await exchange(Buffer.alloc(0),{method:'OPTIONS',extra:{'Access-Control-Request-Method':'POST','Access-Control-Request-Headers':'content-type'}});
assert.equal(preflight.status,204);
const result = await invoke('evaluate','(+ 20 22)',{split:true});
assert.equal(result.ok,true); assert.equal(result.message,'Integer: 42');
assert.equal((await invoke('evaluate','(concat "Hello, " "CuBit!")')).message,'String: Hello, CuBit!');
assert.equal((await invoke('evaluate','(+ true 1)')).ok,false);
// The default plaintext lab endpoint has no Config inspection grant.
assert.equal((await invoke('evaluate','(config.keys "")')).ok,false);
assert.ok(BigInt((await invoke('clock')).clock.monotonicMs) >= BigInt(snapshot.clock.monotonicMs));
const before = BigInt((await invoke('clock')).clock.monotonicMs);
const inlineClock = await invoke('evaluate','(clock.monotonic-ms)');
assert.equal(inlineClock.ok,true);
const after = BigInt((await invoke('clock')).clock.monotonicMs);
const sampled = BigInt(inlineClock.message.replace(/^Integer: /,''));
assert.ok(sampled >= before && sampled <= after);
const formattingSource = await readFile(new URL('../../userspace/ccl/samples/monotonic-clock.ccl',import.meta.url),'utf8');
const formatted = await invoke('evaluate',formattingSource);
assert.equal(formatted.ok,true);
assert.match(formatted.message,/^String: [0-9]{2,}:[0-5][0-9]:[0-5][0-9]$/);
console.log('PASS native inline clock and formatter:',formatted.message);
assert.equal((await exchange(encodeRequest(++id, session, 'inspect'),{origin:'http://evil.example'})).status,400);
assert.equal((await exchange(Buffer.from([0xff]))).status,400);
console.log('PASS preflight, fragmented CBOR, integer/string evaluation, diagnostics, clock, rejection');
// A client that sends no complete header cannot monopolize the endpoint.
const start = Date.now();
await new Promise((resolve,reject)=>{
  const socket=connect(18445,'127.0.0.1',()=>socket.write('POST /ccl HTTP/1.1\r\n'));
  socket.setTimeout(8000,()=>socket.destroy(new Error('Request deadline failed')));
  socket.on('data',()=>{}); socket.on('error',reject); socket.on('close',resolve);
});
assert.ok(Date.now()-start >= 4000 && Date.now()-start < 8000);
assert.equal((await invoke('evaluate','(* 6 7)')).message,'Integer: 42');
console.log('PASS native request deadline and recovery; HTTP/CBOR guest smoke complete');

// No frontend or polling drives these invocations. Wait without sending any
// requests; native accept-deadline wakeups must advance the retained run count.
let monitor=await invoke('startMonitor',formattingSource);
assert.equal(monitor.accepted,true); assert.equal(monitor.state,'Waiting');
const generation=BigInt(monitor.generation), firstRuns=BigInt(monitor.runs);
assert.match(monitor.result.message,/^String: [0-9]{2,}:[0-5][0-9]:[0-5][0-9]$/);
await new Promise(r=>setTimeout(r,3300));
monitor=await invoke('monitor');
assert.ok(BigInt(monitor.runs)>=firstRuns+2n,'timer must run without browser requests');
assert.equal((await invoke('startMonitor','42')).accepted,false,'active source must not be replaced');
monitor=await invoke('stopMonitor','',{},generation);
assert.equal(monitor.accepted,true); assert.equal(monitor.state,'Stopped');
const stoppedRuns=monitor.runs;
await new Promise(r=>setTimeout(r,1200));
assert.equal((await invoke('monitor')).runs,stoppedRuns);
monitor=await invoke('startMonitor','(/ 1 0)');
assert.equal(monitor.state,'Faulted');
assert.equal((await invoke('stopMonitor','',{},generation)).accepted,false,'old generation must not stop replacement');
monitor=await invoke('startMonitor',formattingSource);
assert.equal(monitor.state,'Waiting');
await invoke('stopMonitor','',{},BigInt(monitor.generation));
console.log('PASS native persistent widget, idle timer wakeups, stop, stale generation and fault isolation');

// Presentations (operation 7) and image rows (8): what the Observatory's
// transcript renders, from the native interpreter and image store.
async function exchangeTyped(operation, source = '', target = 0n, row = 0n) {
  const queryId = ++id;
  const r = await exchange(encodeRequest(queryId, session, operation, source, target, row));
  assert.equal(r.status, 200);
  return operation === 'imageRows' ? decodeImageRows(r.body, queryId) : decodePresentation(r.body, queryId, operation);
}
const shown = await exchangeTyped('present', '(+ 20 22)');
assert.deepEqual([shown.form, shown.type, shown.value], ['text', 'Integer', '42']);
const failed = await exchangeTyped('present', '(+ true 1)');
assert.equal(failed.form, 'failure');
const plot = await exchangeTyped('present', '(image.plot (list 3 1 4 1 5 9 2 6))');
assert.equal(plot.form, 'picture'); assert.equal(plot.type, 'Image');
assert.deepEqual([plot.picture.width, plot.picture.height], [320, 120]);
assert.equal(plot.picture.id, BigInt(plot.value.match(/^\(Image 320 120 (\d+)\)$/)[1]));
let rows = 0, bytes = 0;
while (rows < plot.picture.height) {
  const band = await exchangeTyped('imageRows', '', plot.picture.id, BigInt(rows));
  assert.equal(band.known, true); assert.equal(band.firstRow, rows);
  rows += band.rowCount; bytes += band.rgb.length;
}
assert.equal(bytes, 320 * 120 * 3);
assert.deepEqual(await exchangeTyped('imageRows', '', 1n, 0n), { known: false });
const heat = await exchangeTyped('present', '(image.heatmap (Grid 3 2 (list 1 2 3 4 5 6)))');
assert.deepEqual([heat.picture.width, heat.picture.height], [3, 2]);
const gallery = await exchangeTyped('present', '(list (image.gradient (Size 8 4)) (image.bars (list 3 -1 2)))');
assert.equal(gallery.form, 'gallery'); assert.equal(gallery.gallery.pictures.length, 2);
assert.equal((await exchangeTyped('imageRows', '', gallery.gallery.pictures[0].id, 0n)).known, true);
console.log('PASS native presentations: text, failure, picture, gallery, image rows and expiry');

// A live picture: the native periodic slot re-runs an image entry; the
// page observes it presented (operation 9), as the web console's :watch.
let live = await invoke('startMonitor', '(image.bars (list 3 1 4 1 5))');
assert.equal(live.accepted, true);
await new Promise(r => setTimeout(r, 2300));
const observed = await exchangeTyped('presentMonitor');
assert.ok(BigInt(observed.monitor.runs) >= 2n, 'the live cell re-ran natively');
assert.equal(observed.form, 'picture');
assert.equal((await exchangeTyped('imageRows', '', observed.picture.id, 0n)).known, true);
await invoke('stopMonitor', '', {}, BigInt(live.generation));
console.log('PASS native live picture: periodic re-runs observed as a presentation');

// Completion from the guest's own catalog (operation 10), as the console's.
{
  const queryId = ++id;
  const r = await exchange(encodeRequest(queryId, session, 'complete', '(image.pl'));
  assert.equal(r.status, 200);
  const found = decodeCompletion(r.body, queryId);
  assert.deepEqual(found.candidates.map(c => c.name), ['image.plot']);
  console.log('PASS native completion from the guest catalog');
}

// A tab's session persists on the guest (Control_Wire v2): a stream bound
// in one request is read in later ones, and another tab cannot see it.
{
  assert.equal((await invoke('evaluate', '(define ticks (timer.every 50))')).ok, true);
  await new Promise(resolve => setTimeout(resolve, 400));
  const window = await invoke('evaluate', '(length (window 255 ticks))');
  assert.equal(window.ok, true, window.message);
  assert.ok(/^Integer: [0-9]+$/.test(window.message) && Number(window.message.slice(9)) >= 3, window.message);
  const latest = await exchangeTyped('present', '(- (latest ticks) (sum (first 1 (window 2 ticks))))');
  assert.equal(latest.value, '50');
  const queryId = ++id;
  const other = await exchange(encodeRequest(queryId, session + 1n, 'evaluate', '(latest ticks)'));
  assert.equal(decodeResponse(other.body, queryId, 'evaluate').ok, false);
  console.log('PASS native session streams: timer.every bound in one request, read in later ones, per tab');
}
