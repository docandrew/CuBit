// Narrow deterministic CBOR profile shared by the browser and wire tests.
// Unsigned wire integers stay BigInt until a range check permits conversion.
const maxU64 = (1n << 64n) - 1n;
// CCL.Sessions.Maximum_Fuel: evaluation fuel never exceeds it.
const MAX_FUEL = 16777216n;
// CCL.Language.MAX_LIST_ELEMENTS: the longest list an evaluation can hold.
const MAX_LIST_ELEMENTS = 4096n;
// CCL.Language.MAX_SOURCE_LENGTH + 1: a diagnostic position is within the program.
const MAX_SOURCE_POSITION = 8193n;
// CCL.Image_Store: ids are positive 63-bit digests; sides at most 512.
const MAX_IMAGE_ID = (1n << 63n) - 1n;
const MAX_IMAGE_SIDE = 512n;
const MAX_RESPONSE = 8192;
// Control_Wire.Protocol_Version: requests are [2, id, session, op, ...].
const PROTOCOL_VERSION = 2n;
const operations = { inspect: 1, evaluate: 2, clock: 3, startMonitor: 4, stopMonitor: 5, monitor: 6, present: 7, imageRows: 8, presentMonitor: 9, complete: 10 };
// session: this tab's random session (Control_Wire.Request.Session), or 0n
// for a fresh one the guest discards. It separates tabs; it is not a login.
export function encodeRequest(id, session, operation, source = '', target = 0n, row = 0n) {
  if (typeof id !== 'bigint' || id < 1n || id > maxU64 || !Object.hasOwn(operations, operation)) throw new Error('Invalid request identity');
  if (typeof session !== 'bigint' || session < 0n || session > maxU64) throw new Error('Invalid session');
  if (typeof source !== 'string' || !/^[\x09\x0a\x0d\x20-\x7e]{0,1024}$/.test(source) || (!['evaluate','startMonitor','present','complete'].includes(operation) && source !== '')) throw new Error('Expected at most 1024 ASCII source bytes');
  if (operation === 'imageRows') {
    // [1,id,8,"",imageId,firstRow]: an image's pixels, a band at a time.
    if (typeof target !== 'bigint' || target < 1n || target > MAX_IMAGE_ID || typeof row !== 'bigint' || row < 0n || row >= MAX_IMAGE_SIDE) throw new Error('Invalid image rows request');
  } else if (typeof target !== 'bigint' || target < 0n || target > maxU64 || (operation === 'stopMonitor' ? target === 0n : target !== 0n)) throw new Error('Invalid monitor generation');
  const bytes = [];
  const head = (major, n) => {
    if (n < 24n) { bytes.push(major * 32 + Number(n)); return; }
    const width = n <= 255n ? 1 : n <= 65535n ? 2 : n <= 4294967295n ? 4 : 8;
    bytes.push(major * 32 + ({1:24,2:25,4:26,8:27})[width]);
    for (let i = width - 1; i >= 0; i--) bytes.push(Number((n >> BigInt(i * 8)) & 255n));
  };
  const op = operations[operation];
  const monitor = (op >= 4 && op <= 6) || op === 9;
  head(4, op === 8 ? 7n : monitor ? 6n : 5n); head(0, PROTOCOL_VERSION); head(0, id); head(0, session); head(0, BigInt(op));
  head(3, BigInt(source.length));
  for (const c of source) bytes.push(c.charCodeAt(0));
  if (monitor || op === 8) head(0, target);
  if (op === 8) head(0, row);
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
    if (major === 1) return -1n - argument(ai);   // signed list elements only
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
  // An evaluation with a list result has 11 fields: the element type code, one
  // definite array of at most 64 scalar elements (a prefix), and the list's
  // full length (README, "Lists").
  const listShape = op === 2 && count === 11n;
  if (!op || (count !== BigInt(({1:12,2:8,3:5,4:15,5:15,6:15})[op]) && !listShape)) throw new Error('Wrong response shape');
  const elements = () => {
    const initial = byte();
    if (initial >> 5 !== 4) throw new Error('Expected list elements array');
    const n = argument(initial & 31);
    if (n > 64n) throw new Error('Too many list elements');
    return Array.from({ length: Number(n) }, scalar);
  };
  const values = Array.from({ length: Number(count) }, (_, i) => listShape && i === 9 ? elements() : scalar());
  if (offset !== bytes.length || values[0] !== PROTOCOL_VERSION || values[1] !== id || values[2] !== BigInt(op)) throw new Error('Response identity/version mismatch');
  const uint = index => { if (typeof values[index] !== 'bigint' || values[index] < 0n) throw new Error('Expected unsigned integer'); return values[index]; };
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
  if (typeof values[base+1] !== 'string' || type > 6n || position > MAX_SOURCE_POSITION || fuel > MAX_FUEL || listShape !== (op === 2 && type === 5n)) throw new Error('Invalid evaluation result');
  const result = { ok, message: values[base+1], type: ['Invalid','Integer','Boolean','String','Character','List','Function'][Number(type)], position: position.toString(), fuelRemaining: fuel.toString() };
  if (listShape) {
    const code = uint(8), items = values[9], total = uint(10);
    if (total < BigInt(items.length) || total > MAX_LIST_ELEMENTS) throw new Error('Invalid list length');
    const kinds = { 1n: 'Integer', 2n: 'Boolean', 3n: 'String', 4n: 'Character', 6n: 'Enumeration' };
    if (!Object.hasOwn(kinds, code)) throw new Error('Invalid list element type');
    const valid = item =>
      code === 1n ? typeof item === 'bigint' :
      code === 2n ? typeof item === 'boolean' :
      code === 3n ? typeof item === 'string' :
      code === 4n ? typeof item === 'string' && item.length === 1 :
      typeof item === 'bigint' && item >= 0n;
    if (!items.every(valid)) throw new Error('List element does not match its declared type');
    result.list = { elementType: kinds[code], elements: items.map(item => typeof item === 'bigint' ? item.toString() : item), total: total.toString() };
  }
  if (op < 4) return result;
  const state = uint(4), interval = uint(7);
  if (state > 5n || interval < 1000n || interval > 60000n || typeof values[8] !== 'string' || !/^[\x09\x0a\x0d\x20-\x7e]{0,1024}$/.test(values[8])) throw new Error('Invalid monitor state');
  return { accepted: flag(3), state: ['Empty','Waiting','Executing','Stopping','Stopped','Faulted'][Number(state)],
    generation: uint(5).toString(), runs: uint(6).toString(), intervalMs: interval.toString(),
    source: values[8], result, nextDeadline: uint(14).toString() };
}

// Strict structured decoding for operations 7 and 8: definite arrays, nested
// within a bound, canonical integers, ASCII text, and (operation 8 only)
// one byte string of pixels. Tables nest four deep: response, detail,
// fields, field.
function structured(bytes, allowBytes) {
  if (!(bytes instanceof Uint8Array) || bytes.length > MAX_RESPONSE) throw new Error('Oversized CBOR response');
  let offset = 0;
  const byte = () => { if (offset >= bytes.length) throw new Error('Truncated CBOR'); return bytes[offset++]; };
  const argument = ai => {
    if (ai < 24) return BigInt(ai);
    const width = ({24:1,25:2,26:4,27:8})[ai];
    if (!width) throw new Error('Indefinite/reserved CBOR not supported');
    let value = 0n;
    for (let i = 0; i < width; i++) value = (value << 8n) | BigInt(byte());
    if (value < ({1:24n,2:256n,4:65536n,8:4294967296n})[width]) throw new Error('Noncanonical CBOR integer');
    return value;
  };
  const item = depth => {
    const initial = byte(), major = initial >> 5, ai = initial & 31;
    if (major === 0) return argument(ai);
    if (major === 1) return -1n - argument(ai);
    if (major === 7 && (ai === 20 || ai === 21)) return ai === 21;
    if (major === 2 || major === 3) {
      const n = argument(ai);
      if (n > BigInt(bytes.length - offset)) throw new Error('Invalid CBOR string length');
      const data = bytes.subarray(offset, offset + Number(n)); offset += Number(n);
      if (major === 2) { if (!allowBytes) throw new Error('Unexpected byte string'); return Uint8Array.from(data); }
      if (data.some(b => b > 127)) throw new Error('Initial protocol requires ASCII results');
      return new TextDecoder('utf-8', { fatal: true }).decode(data);
    }
    if (major === 4) {
      if (depth >= 4) throw new Error('CBOR nested too deeply');
      const n = argument(ai);
      if (n > BigInt(bytes.length - offset)) throw new Error('Invalid CBOR array length');
      return Array.from({ length: Number(n) }, () => item(depth + 1));
    }
    throw new Error('Unsupported CBOR item');
  };
  const value = item(0);
  if (offset !== bytes.length) throw new Error('Trailing CBOR bytes');
  if (!Array.isArray(value)) throw new Error('Expected response array');
  return value;
}
const isUint = v => typeof v === 'bigint' && v >= 0n;
const isText = v => typeof v === 'string';

// [1,id,7,ok,typeText,valueText,form,position,fuel,detail]: the result as
// CCL.Presentations describes it, the same description the native console
// renders. Forms: 0 failure, 1 text, 2 table, 3 picture, 4 gallery.
// Operation 9 (presentMonitor) is the same presentation of the native
// periodic program's last result, then its state and completed runs.
export function decodePresentation(bytes, id, operation = 'present') {
  const v = structured(bytes, false);
  const op = operation === 'presentMonitor' ? 9n : 7n;
  if (v.length !== (op === 9n ? 12 : 10) || v[0] !== PROTOCOL_VERSION || v[1] !== id || v[2] !== op) throw new Error('Response identity/version mismatch');
  let monitor;
  if (op === 9n) {
    const [state, runs] = v.splice(10, 2);
    if (!isUint(state) || state > 5n || !isUint(runs)) throw new Error('Invalid monitor state');
    monitor = { state: ['Empty','Waiting','Executing','Stopping','Stopped','Faulted'][Number(state)], runs: runs.toString() };
  }
  const [, , , ok, typeText, valueText, form, position, fuel, detail] = v;
  if (typeof ok !== 'boolean' || !isText(typeText) || !isText(valueText) || !isUint(form) || form > 4n ||
      !isUint(position) || position > MAX_SOURCE_POSITION || !isUint(fuel) || fuel > MAX_FUEL || !Array.isArray(detail)) throw new Error('Invalid presentation');
  const result = { ok, type: typeText, value: valueText, position: position.toString(), fuelRemaining: fuel.toString(),
    form: ['failure', 'text', 'table', 'picture', 'gallery'][Number(form)] };
  if (monitor) result.monitor = monitor;
  if (ok !== (form !== 0n)) throw new Error('Presentation form contradicts its status');
  if (form <= 1n) { if (detail.length !== 0) throw new Error('Unexpected presentation detail'); return result; }
  if (form === 2n) {
    const [rowType, many, total, fields, rows] = detail;
    if (detail.length !== 5 || !isText(rowType) || typeof many !== 'boolean' || !isUint(total) || total > MAX_LIST_ELEMENTS ||
        !Array.isArray(fields) || fields.length < 1 || fields.length > 16 || !Array.isArray(rows) || BigInt(rows.length) > total) throw new Error('Invalid table');
    for (const f of fields) if (!Array.isArray(f) || f.length !== 3 || !isText(f[0]) || !isText(f[1]) || typeof f[2] !== 'boolean') throw new Error('Invalid table field');
    for (const r of rows) if (!Array.isArray(r) || r.length !== fields.length || !r.every(isText)) throw new Error('Invalid table row');
    result.table = { rowType, many, total: total.toString(), fields: fields.map(([name, type, numeric]) => ({ name, type, numeric })), rows };
    return result;
  }
  const picture = ([width, height, image]) => {
    if (!isUint(width) || width < 1n || width > MAX_IMAGE_SIDE || !isUint(height) || height < 1n ||
        height > MAX_IMAGE_SIDE || !isUint(image) || image < 1n || image > MAX_IMAGE_ID) throw new Error('Invalid picture');
    return { width: Number(width), height: Number(height), id: image };
  };
  if (form === 4n) {
    const [total, pictures] = detail;
    if (detail.length !== 2 || !isUint(total) || total > MAX_LIST_ELEMENTS || !Array.isArray(pictures) ||
        BigInt(pictures.length) > total || pictures.some(p => !Array.isArray(p) || p.length !== 3)) throw new Error('Invalid gallery');
    result.gallery = { total: total.toString(), pictures: pictures.map(picture) };
    return result;
  }
  const [width, height, image] = detail;
  if (detail.length !== 3 || !isUint(width) || width < 1n || width > MAX_IMAGE_SIDE || !isUint(height) || height < 1n ||
      height > MAX_IMAGE_SIDE || !isUint(image) || image < 1n || image > MAX_IMAGE_ID) throw new Error('Invalid picture');
  result.picture = { width: Number(width), height: Number(height), id: image };
  return result;
}

// [1,id,8,known,width,height,firstRow,rowCount,rgb]: rowCount rows of the
// image from firstRow, three bytes a pixel. An unknown id is expired, never
// other pixels.
export function decodeImageRows(bytes, id) {
  const v = structured(bytes, true);
  if (v.length !== 9 || v[0] !== PROTOCOL_VERSION || v[1] !== id || v[2] !== 8n) throw new Error('Response identity/version mismatch');
  const [, , , known, width, height, first, count, rgb] = v;
  if (typeof known !== 'boolean' || ![width, height, first, count].every(isUint) || !(rgb instanceof Uint8Array)) throw new Error('Invalid image rows');
  if (!known) { if (width !== 0n || height !== 0n || count !== 0n || rgb.length !== 0) throw new Error('Invalid expired image'); return { known }; }
  if (width < 1n || width > MAX_IMAGE_SIDE || height < 1n || height > MAX_IMAGE_SIDE || first >= height ||
      count < 1n || first + count > height || BigInt(rgb.length) !== count * width * 3n) throw new Error('Invalid image rows');
  return { known, width: Number(width), height: Number(height), firstRow: Number(first), rowCount: Number(count), rgb };
}

// [1,id,10,prefixLength,[[name,origin,signature]...],beyond,signature]: what
// completes the text before the caret, as CCL.Completions finds it for the
// native console. Origins: 0 service operation, 1 built-in, 2 form.
export function decodeCompletion(bytes, id) {
  const v = structured(bytes, false);
  if (v.length !== 7 || v[0] !== PROTOCOL_VERSION || v[1] !== id || v[2] !== 10n) throw new Error('Response identity/version mismatch');
  const [, , , prefix, candidates, beyond, signature] = v;
  if (!isUint(prefix) || prefix > 1024n || !Array.isArray(candidates) || candidates.length > 10 ||
      typeof beyond !== 'boolean' || !isText(signature)) throw new Error('Invalid completion');
  for (const c of candidates) if (!Array.isArray(c) || c.length !== 3 || !isText(c[0]) || !isUint(c[1]) || c[1] > 2n || !isText(c[2])) throw new Error('Invalid completion candidate');
  return { prefixLength: Number(prefix), beyond, signature,
    candidates: candidates.map(([name, origin, sig]) => ({ name, origin: ['service', 'built-in', 'form'][Number(origin)], signature: sig })) };
}
