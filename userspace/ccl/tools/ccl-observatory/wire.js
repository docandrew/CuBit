// Narrow deterministic CBOR profile shared by the browser and wire tests.
// Unsigned wire integers stay BigInt until a range check permits conversion.
const maxU64 = (1n << 64n) - 1n;
const operations = { inspect: 1, evaluate: 2, clock: 3, startMonitor: 4, stopMonitor: 5, monitor: 6 };
export function encodeRequest(id, operation, source = '', target = 0n) {
  if (typeof id !== 'bigint' || id < 1n || id > maxU64 || !Object.hasOwn(operations, operation)) throw new Error('Invalid request identity');
  if (typeof source !== 'string' || !/^[\x09\x0a\x0d\x20-\x7e]{0,1024}$/.test(source) || (!['evaluate','startMonitor'].includes(operation) && source !== '')) throw new Error('Expected at most 1024 ASCII source bytes');
  if (typeof target !== 'bigint' || target < 0n || target > maxU64 || (operation === 'stopMonitor' ? target === 0n : target !== 0n)) throw new Error('Invalid monitor generation');
  const bytes = [];
  const head = (major, n) => {
    if (n < 24n) { bytes.push(major * 32 + Number(n)); return; }
    const width = n <= 255n ? 1 : n <= 65535n ? 2 : n <= 4294967295n ? 4 : 8;
    bytes.push(major * 32 + ({1:24,2:25,4:26,8:27})[width]);
    for (let i = width - 1; i >= 0; i--) bytes.push(Number((n >> BigInt(i * 8)) & 255n));
  };
  head(4, operations[operation] >= 4 ? 5n : 4n); head(0, 1n); head(0, id); head(0, BigInt(operations[operation]));
  head(3, BigInt(source.length));
  for (const c of source) bytes.push(c.charCodeAt(0));
  if (operations[operation] >= 4) head(0, target);
  return Uint8Array.from(bytes);
}
export function decodeResponse(bytes, id, operation) {
  if (!(bytes instanceof Uint8Array) || bytes.length > 8192) throw new Error('Oversized CBOR response');
  let offset = 0;
  const byte = () => { if (offset >= bytes.length) throw new Error('Truncated CBOR'); return bytes[offset++]; };
  const argument = ai => {
    if (ai < 24) return BigInt(ai);
    const width = ({24:1,25:2,26:4,27:8})[ai];
    if (!width) throw new Error('Indefinite/reserved CBOR not supported');
    let value = 0n;
    for (let i = 0; i < width; i++) value = (value << 8n) | BigInt(byte());
    const minimum = ({1:24n,2:256n,4:65536n,8:4294967296n})[width];
    if (value < minimum) throw new Error('Noncanonical CBOR integer');
    return value;
  };
  const scalar = () => {
    const initial = byte(), major = initial >> 5, ai = initial & 31;
    if (major === 0) return argument(ai);
    if (major === 7 && (ai === 20 || ai === 21)) return ai === 21;
    if (major === 3) {
      const n = argument(ai);
      if (n > 4096n || n > BigInt(bytes.length - offset)) throw new Error('Invalid CBOR text length');
      const text = bytes.subarray(offset, offset + Number(n)); offset += Number(n);
      if (text.some(b => b > 127)) throw new Error('Initial protocol requires ASCII results');
      return new TextDecoder('utf-8', { fatal: true }).decode(text);
    }
    throw new Error('Unsupported CBOR scalar');
  };
  const first = byte();
  if (first >> 5 !== 4) throw new Error('Expected response array');
  const count = argument(first & 31), op = operations[operation];
  if (!op || count !== BigInt(({1:12,2:8,3:5,4:15,5:15,6:15})[op])) throw new Error('Wrong response shape');
  const values = Array.from({ length: Number(count) }, scalar);
  if (offset !== bytes.length || values[0] !== 1n || values[1] !== id || values[2] !== BigInt(op)) throw new Error('Response identity/version mismatch');
  const uint = index => { if (typeof values[index] !== 'bigint') throw new Error('Expected unsigned integer'); return values[index]; };
  const flag = index => { if (typeof values[index] !== 'boolean') throw new Error('Expected Boolean'); return values[index]; };
  if (op === 1) {
    if (uint(3) === 0n || uint(3) === maxU64 || uint(4) === maxU64 || uint(5) === maxU64) throw new Error('Invalid process ID');
    return { scope: 'adapter-bindings-only', peerAuthenticated: false,
      processId: uint(3).toString(), networkProcessId: uint(4).toString(), clockProcessId: uint(5).toString(),
      clock: { available: flag(6), monotonicMs: uint(7).toString() },
      interface: { name: 'clock', version: '1.0', operation: 'clock.monotonic-ms', parameters: [],
        result: { kind: 'Integer', bits: 64 }, digestWords: [8,9,10,11].map(i => uint(i).toString()) } };
  }
  if (op === 3) return { clock: { available: flag(3), monotonicMs: uint(4).toString() } };
  const base = op >= 4 ? 9 : 3;
  const ok = flag(base), type = uint(base+2), position = uint(base+3), fuel = uint(base+4);
  if (typeof values[base+1] !== 'string' || type > 4n || position > 1025n || fuel > 4096n) throw new Error('Invalid evaluation result');
  const result = { ok, message: values[base+1], type: ['Invalid','Integer','Boolean','String','Character'][Number(type)], position: position.toString(), fuelRemaining: fuel.toString() };
  if (op < 4) return result;
  const state = uint(4), interval = uint(7);
  if (state > 5n || interval < 1000n || interval > 60000n || typeof values[8] !== 'string' || !/^[\x09\x0a\x0d\x20-\x7e]{0,1024}$/.test(values[8])) throw new Error('Invalid monitor state');
  return { accepted: flag(3), state: ['Empty','Waiting','Executing','Stopping','Stopped','Faulted'][Number(state)],
    generation: uint(5).toString(), runs: uint(6).toString(), intervalMs: interval.toString(),
    source: values[8], result, nextDeadline: uint(14).toString() };
}
