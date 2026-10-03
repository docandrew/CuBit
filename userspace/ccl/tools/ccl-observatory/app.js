import * as THREE from 'three';
import { OrbitControls } from '/OrbitControls.js';
import { encodeRequest, decodeResponse, decodePresentation, decodeImageRows, decodeCompletion } from '/wire.js';
import { card } from '/console.js';
import { balance } from '/highlight.js';

const $ = id => document.getElementById(id);
// No relay credentials, browser-side evaluator, or synthetic observations.
const disconnected = 'Connect to the isolated CuBit lab guest first. Nothing was executed.';
let snapshot = null, selected = 'control';
let connected = false, busy = false, paused = false, requestId = 0n, observedAt = 0;
let monitor = null;
function updateControls() {
  $('refresh').disabled = busy;
  $('refresh').textContent = connected ? '↻ Refresh' : 'Connect to CuBit';
  $('execute').disabled = !connected || busy;
  $('invoke').disabled = !connected || busy || !snapshot?.clock.available;
  $('pause').disabled = !connected;
  const active = monitor && ['Waiting','Executing','Stopping'].includes(monitor.state);
  $('start-monitor').disabled = !connected || busy || !monitor || active;
  $('stop-monitor').disabled = !connected || busy || !active;
}
const journal = [];
const objects = new Map();
const viewport = $('viewport');
let renderer, scene, camera, controls, root, pendingFrame = false;
let hovered = null, pointerDown = null;
const ray = new THREE.Raycaster();
const pointer = new THREE.Vector2();
const colors = { control: 0x72e5df, clock: 0xb49bff, network: 0xf1bd7c };

function record(text) {
  journal.unshift(`${new Date().toLocaleTimeString()}  ${text}`);
  journal.length = Math.min(journal.length, 12);
  $('journal').replaceChildren(...journal.map(text => {
    const li = document.createElement('li'); li.textContent = text; return li;
  }));
}
function requestRender() {
  if (!renderer || pendingFrame || document.hidden) return;
  pendingFrame = true;
  requestAnimationFrame(() => {
    pendingFrame = false;
    renderer.render(scene, camera);
    for (const item of objects.values()) {
      const p = item.mesh.position.clone().add(new THREE.Vector3(0, -1.15, 0)).project(camera);
      item.label.style.left = `${(p.x + 1) * viewport.clientWidth / 2}px`;
      item.label.style.top = `${(1 - p.y) * viewport.clientHeight / 2}px`;
      item.label.hidden = p.z > 1 || p.z < -1;
    }
  });
}
function setupMap() {
  try {
    renderer = new THREE.WebGLRenderer({ antialias: true, alpha: true });
    renderer.setPixelRatio(Math.min(devicePixelRatio, 2));
    renderer.setClearColor(0x000000, 0);
    viewport.prepend(renderer.domElement);
    scene = new THREE.Scene();
    camera = new THREE.PerspectiveCamera(40, 1, .1, 100);
    camera.position.set(11, 10, 16);
    controls = new OrbitControls(camera, renderer.domElement);
    controls.target.set(0, .8, 0); controls.minDistance = 5; controls.maxDistance = 35;
    controls.maxPolarAngle = Math.PI * .48;
    controls.addEventListener('change', requestRender);
    controls.update();
    scene.add(new THREE.AmbientLight(0x94b4d4, 2.2));
    const light = new THREE.DirectionalLight(0xe9fcff, 3); light.position.set(5, 12, 8); scene.add(light);
    // A reference plane, not network links or fabricated observations.
    const grid = new THREE.GridHelper(36, 36, 0x244958, 0x152c3a); grid.position.y = -1.3; scene.add(grid);
    root = new THREE.Group(); scene.add(root);
    new ResizeObserver(() => {
      renderer.setSize(viewport.clientWidth, viewport.clientHeight);
      camera.aspect = viewport.clientWidth / viewport.clientHeight; camera.updateProjectionMatrix();
      requestRender();
    }).observe(viewport);
    renderer.domElement.addEventListener('pointerdown', e => { pointerDown = [e.clientX, e.clientY]; });
    renderer.domElement.addEventListener('pointermove', e => {
      const rect = viewport.getBoundingClientRect();
      pointer.set((e.clientX - rect.left) / rect.width * 2 - 1, -(e.clientY - rect.top) / rect.height * 2 + 1);
      ray.setFromCamera(pointer, camera);
      const hit = ray.intersectObjects([...objects.values()].map(o => o.mesh), false)[0];
      hovered = hit ? hit.object.userData.id : null;
      const card = $('hover-card'); card.hidden = !hovered;
      if (hovered) {
        const item = objects.get(hovered);
        card.textContent = `${item.name} · process ${item.pid} · click for evidence`;
        card.style.left = `${Math.max(8, Math.min(e.clientX - rect.left + 14, rect.width - 255))}px`;
        card.style.top = `${Math.max(35, Math.min(e.clientY - rect.top, rect.height - 70))}px`;
      }
      renderer.domElement.style.cursor = hovered ? 'pointer' : 'grab';
    });
    renderer.domElement.addEventListener('pointerup', e => {
      if (hovered && pointerDown && Math.hypot(e.clientX - pointerDown[0], e.clientY - pointerDown[1]) < 5) select(hovered);
      pointerDown = null;
    });
    renderer.domElement.addEventListener('pointerleave', () => { $('hover-card').hidden = true; hovered = null; });
    document.addEventListener('visibilitychange', requestRender);
  } catch (error) {
    $('map-empty').querySelector('h2').textContent = '3D rendering is unavailable.';
    $('map-empty').querySelector('p').textContent = 'The preview layout and expression editor remain available. No guest is connected.';
    $('reset').disabled = true;
    renderer = null;
  }
}
function clearGraph() {
  objects.clear(); $('map-labels').replaceChildren();
  if (root) {
    root.traverse(o => { o.geometry?.dispose(); o.material?.dispose(); });
    root.clear();
  }
}
function buildGraph(data) {
  const oldIds = [...objects.values()].map(o => o.pid).join(',');
  const entries = [{ id: 'control', name: 'CCL control', pid: data.processId, at: [0, .4, 0] }];
  if (data.networkProcessId !== '0') entries.push({ id: 'network', name: 'Network service', pid: data.networkProcessId, at: [-4.5, .2, 1.5] });
  if (data.clockProcessId !== '0') entries.push({ id: 'clock', name: 'Clock service', pid: data.clockProcessId, at: [4, 1.2, -2] });
  if (oldIds === entries.map(o => o.pid).join(',')) return;
  clearGraph();
  for (const entry of entries) {
    const label = document.createElement('div'); label.className = 'map-label';
    label.textContent = entry.name;
    const subtitle = document.createElement('small'); subtitle.textContent = `PID ${entry.pid}`; label.append(subtitle);
    $('map-labels').append(label);
    const geometry = entry.id === 'control' ? new THREE.IcosahedronGeometry(1.25, 1) :
      entry.id === 'clock' ? new THREE.OctahedronGeometry(.8) : new THREE.BoxGeometry(1.25, 1.25, 1.25);
    const material = new THREE.MeshStandardMaterial({ color: colors[entry.id], metalness: .55, roughness: .3, emissive: colors[entry.id], emissiveIntensity: .12 });
    const mesh = new THREE.Mesh(geometry, material); mesh.position.set(...entry.at); mesh.userData.id = entry.id;
    if (root) {
      root.add(mesh);
      const outline = new THREE.LineSegments(new THREE.EdgesGeometry(geometry), new THREE.LineBasicMaterial({ color: colors[entry.id], transparent: true, opacity: .7 }));
      outline.position.copy(mesh.position); outline.scale.setScalar(1.07); root.add(outline);
      const ring = new THREE.Mesh(new THREE.TorusGeometry(entry.id === 'control' ? 1.8 : 1.2, .014, 5, 80), new THREE.MeshBasicMaterial({ color: colors[entry.id], transparent: true, opacity: .42 }));
      ring.rotation.x = Math.PI / 2; ring.position.set(entry.at[0], -1.24, entry.at[2]); root.add(ring);
      if (entry.id !== 'control') {
        const points = [new THREE.Vector3(0, .4, 0), new THREE.Vector3(entry.at[0] / 2, -0.4, entry.at[2] / 2), mesh.position.clone()];
        const curve = new THREE.CatmullRomCurve3(points);
        root.add(new THREE.Line(new THREE.BufferGeometry().setFromPoints(curve.getPoints(40)), new THREE.LineBasicMaterial({ color: colors[entry.id], transparent: true, opacity: .65 })));
      }
    } else label.hidden = true;
    objects.set(entry.id, { ...entry, mesh, label });
  }
  $('objects').replaceChildren(...[...objects.values()].map(item => {
    const button = document.createElement('button'); button.dataset.object = item.id;
    const symbol = document.createElement('span'); symbol.className = 'object-symbol'; symbol.textContent = '◇';
    const text = document.createElement('span'); text.textContent = item.name;
    const small = document.createElement('small'); small.textContent = `native process ${item.pid}`; text.append(small);
    button.append(symbol, text); button.onclick = () => select(item.id); return button;
  }));
  $('process-count').textContent = String(entries.length);
  if (renderer) $('map-empty').hidden = true;
  select(objects.has(selected) ? selected : 'control'); requestRender();
}
function property(name, value) {
  const dt = document.createElement('dt'); dt.textContent = name;
  const dd = document.createElement('dd'); dd.textContent = value;
  $('properties').append(dt, dd);
}
function select(id) {
  selected = id;
  const item = objects.get(id); if (!item || !snapshot) return;
  document.querySelectorAll('[data-object]').forEach(b => b.classList.toggle('active', b.dataset.object === id));
  $('object-title').textContent = item.name;
  $('object-kind').textContent = id === 'control' ? 'Development application' : 'Native service binding';
  $('object-description').textContent = {
    control: 'Evaluates bounded CCL expressions inside the guest. Each submission is independent.',
    clock: 'Monotonic time read through the adapter’s manifest-requested clock endpoint. Not calendar time.',
    network: 'Native network service binding reported by the control adapter.'
  }[id];
  $('properties').replaceChildren();
  property('Process', item.pid);
  property('Identity assurance', 'Not cryptographically authenticated');
  property('Evidence', id === 'control' ? 'Guest-reported process ID' : id === 'clock' ?
    (snapshot.clock.available ? 'Clock IPC call succeeded' : 'Registered provider; clock call failed') : 'Guest-reported provider');
  property('Scope', 'Adapter’s own bindings only');
  if (id !== 'control') property('Manifest request', id === 'clock' ? 'Clock endpoint · slot 25 · read/write' : 'Network endpoint · slot 11 · read/write');
  $('type-card').hidden = id !== 'clock';
  if (id === 'clock') {
    $('operation-name').textContent = snapshot.interface.operation;
    $('digest').textContent = snapshot.interface.digestWords.map(w => BigInt(w).toString(16).padStart(16, '0')).join(' ');
  }
  $('evidence-source').textContent = 'Process IDs and samples: native response. Manifest labels: adapter definition. Schema: bundled Clock v1, not live reflection.';
  if (renderer) {
    for (const o of objects.values()) o.mesh.material.emissiveIntensity = o.id === id ? .3 : .06;
    requestRender();
  }
}
function fail(message) {
  connected = false;
  document.body.classList.add('stale');
  $('status-light').className = ''; $('connection').textContent = 'Unavailable / stale';
  $('footer-status').textContent = message;
  $('monitor-state').textContent = 'Unknown / stale';
  updateControls();
}
function validateSnapshot(data) {
  const decimal = value => typeof value === 'string' && /^\d{1,20}$/.test(value) && BigInt(value) <= 18446744073709551615n;
  if (data.scope !== 'adapter-bindings-only' || data.peerAuthenticated !== false ||
      ![data.processId, data.networkProcessId, data.clockProcessId].every(decimal) ||
      typeof data.clock?.available !== 'boolean' || (data.clock.available && !decimal(data.clock.monotonicMs)) ||
      data.interface?.name !== 'clock' || data.interface?.version !== '1.0' ||
      data.interface.operation !== 'clock.monotonic-ms' || data.interface.result?.kind !== 'Integer' ||
      data.interface.result.bits !== 64 || !Array.isArray(data.interface.parameters) || data.interface.parameters.length !== 0 ||
      !Array.isArray(data.interface.digestWords) || data.interface.digestWords.length !== 4 || !data.interface.digestWords.every(decimal)) {
    throw new Error('Unsupported or invalid observation schema; no topology rendered');
  }
}
// This tab's session on the guest: its definitions and streams persist
// between entries. A random 64-bit id kept for the tab; it separates tabs
// and is not a credential (the lab adapter has no login yet).
const session = (() => {
  try {
    const kept = sessionStorage.getItem('ccl-session');
    if (kept && /^[1-9][0-9]{0,19}$/.test(kept) && BigInt(kept) < (1n << 64n)) return BigInt(kept);
  } catch { /* storage unavailable: a new session */ }
  const words = crypto.getRandomValues(new Uint32Array(2));
  const id = ((BigInt(words[0]) << 32n) | BigInt(words[1])) || 1n;
  try { sessionStorage.setItem('ccl-session', id.toString()); } catch { /* kept for this page only */ }
  return id;
})();
async function call(operation, source = '', target = 0n, row = 0n) {
  if (busy) throw new Error('A native request is already in flight.');
  if (!connected && operation !== 'inspect') throw new Error(disconnected);
  if (location.origin !== 'http://127.0.0.1:8787') throw new Error('Open this lab frontend at http://127.0.0.1:8787/');
  const id = ++requestId, body = encodeRequest(id, session, operation, source, target, row);
  const start = performance.now();
  busy = true; updateControls();
  try {
    const response = await fetch('http://127.0.0.1:18445/ccl', {
      method: 'POST', headers: { 'Content-Type': 'application/cbor' }, body,
      credentials: 'omit', cache: 'no-store', redirect: 'error', signal: AbortSignal.timeout(8000)
    });
    if (!response.ok || response.headers.get('content-type') !== 'application/cbor') throw new Error(`Native request rejected (HTTP ${response.status})`);
    const reader = response.body.getReader(), bytes = new Uint8Array(8192);
    let used = 0;
    try {
      for (;;) {
        const { value, done } = await reader.read(); if (done) break;
        if (value.length > bytes.length - used) throw new Error('Oversized native response');
        bytes.set(value, used); used += value.length;
      }
    } finally { await reader.cancel(); }
    const received = bytes.subarray(0, used);
    const result = operation === 'present' || operation === 'presentMonitor' ? decodePresentation(received, id, operation) :
      operation === 'imageRows' ? decodeImageRows(received, id) :
      operation === 'complete' ? decodeCompletion(received, id) : decodeResponse(received, id, operation);
    $('latency').textContent = `${Math.round(performance.now() - start)} ms`;
    if (!['imageRows', 'presentMonitor', 'complete'].includes(operation)) record(`${operation} #${id} · native CBOR response`);
    return result;
  } catch (error) {
    fail(`Native connection unavailable: ${error.message}`); throw error;
  } finally { busy = false; updateControls(); }
}
async function refresh() {
  if (busy) return;
  try {
    const data = await call('inspect'); validateSnapshot(data);
    snapshot = data; connected = true; observedAt = Date.now(); buildGraph(data);
    document.body.classList.remove('stale'); $('status-light').className = 'live';
    $('connection').textContent = 'Receiving native observations';
    $('clock-value').textContent = data.clock.available ? data.clock.monotonicMs : 'Unavailable';
    $('freshness').textContent = new Date(observedAt).toLocaleTimeString();
    $('footer-status').textContent = 'Live guest · plaintext lab connection · peer NOT authenticated';
    $('session-mode').textContent = 'Native interpreter · fuel 1,000,000';
    updateControls();
    select(selected);
    renderMonitor(await call('monitor'));
  } catch (error) { fail(error.message); }
}
function renderMonitor(data) {
  monitor = data;
  $('monitor-state').textContent = data.state;
  $('monitor-value').textContent = data.runs === '0' ? 'No completed invocation.' : data.result.message;
  $('monitor-value').classList.toggle('error', data.state === 'Faulted');
  $('monitor-detail').textContent = `Generation ${data.generation} · ${data.runs} completed runs · ${data.intervalMs} ms after completion · fuel ${data.result.fuelRemaining} remaining on last run. Runs in CuBit, not this browser; not saved across reboot.`;
  $('monitor-source').textContent = data.source;
  updateControls();
}
async function monitorAction(operation) {
  if (busy) return;
  try {
    const data = await call(operation, operation === 'startMonitor' ? $('source').value : '',
      operation === 'stopMonitor' ? BigInt(monitor.generation) : 0n);
    renderMonitor(data);
    $('monitor-notice').textContent = data.accepted ?
      (operation === 'startMonitor' ? 'Loaded inside CuBit. Editing the source above does not alter the running widget.' : 'Stop acknowledged by CuBit.') :
      'Request not accepted: the native slot is busy or its generation changed. Refreshed its current state.';
  } catch (error) { $('monitor-notice').textContent = error.message; }
}
$('start-monitor').onclick = () => monitorAction('startMonitor');
$('stop-monitor').onclick = () => monitorAction('stopMonitor');
// The transcript: each entry as the native CCL console shows it, from the
// same CCL.Presentations description (wire operation 7).
const actions = {
  insert(text) {
    const editor = $('source'), at = editor.selectionStart;
    editor.setRangeText(text, at, editor.selectionEnd, 'end'); editor.focus();
  },
  replace(text) { $('source').value = text; $('source').focus(); },
  rows: (image, row) => call('imageRows', '', image, row),
};
// Live cells, as the native console's :watch. The lab guest has one native
// periodic slot (1 s); the page only observes it (operation 9).
let liveCard = null, liveSource = '', liveTimer = 0;
async function unwatch() {
  clearInterval(liveTimer); liveTimer = 0;
  if (monitor && ['Waiting','Executing','Stopping'].includes(monitor.state)) {
    try { renderMonitor(await call('stopMonitor', '', BigInt(monitor.generation))); } catch (error) { /* reported by call */ }
  }
  liveCard = null;
}
actions.unwatch = unwatch;
async function pollLive() {
  if (busy || !liveCard) return;
  try {
    const result = await call('presentMonitor');
    if (result.monitor.runs === '0') return;
    const next = card(liveSource, result, 0, actions, { runs: result.monitor.runs });
    liveCard.replaceWith(next); liveCard = next;
    if (result.monitor.state !== 'Waiting' && result.monitor.state !== 'Executing') { clearInterval(liveTimer); liveTimer = 0; }
  } catch (error) { clearInterval(liveTimer); liveTimer = 0; }
}
async function watch() {
  const last = $('transcript').lastElementChild;
  if (!last) { $('outcome').textContent = 'Nothing to watch yet: run an expression first'; return; }
  await unwatch();
  liveSource = last.querySelector('.ccl-source').textContent;
  const started = await call('startMonitor', liveSource);
  renderMonitor(started);
  if (!started.accepted) { $('outcome').textContent = 'The native periodic slot did not accept this entry'; return; }
  liveCard = last; liveTimer = setInterval(pollLive, 1000);
  $('outcome').textContent = 'Live inside CuBit, every second  |  :unwatch or click LIVE to stop';
}
async function evaluate(event) {
  event.preventDefault();
  if (busy) return;
  const source = $('source').value;
  if (/^:watch( +[0-9]+)? *$/.test(source.trim())) { $('source').value = ''; await watch(); return; }
  if (source.trim() === ':unwatch') { $('source').value = ''; await unwatch(); $('outcome').textContent = 'Live cell stopped'; return; }
  if (!/^[\x09\x0a\x0d\x20-\x7e]{0,1024}$/.test(source)) {
    $('outcome').textContent = 'This CCL version accepts up to 1024 ASCII bytes.'; $('outcome').className = 'error'; return;
  }
  try {
    const started = performance.now();
    const result = await call('present', source);
    $('transcript').append(card(source, result, Math.round(performance.now() - started), actions));
    $('transcript').lastElementChild.scrollIntoView({ block: 'nearest' });
    $('outcome').textContent = result.ok ? '' : `${result.value}`;
    $('outcome').className = result.ok ? 'success' : 'error';
    if (result.ok) $('source').value = '';
    if (!result.ok && /^\d{1,4}$/.test(result.position) && Number(result.position) > 0) {
      const pos = Math.min(source.length, Number(result.position) - 1);
      $('source').focus(); $('source').setSelectionRange(pos, Math.min(source.length, pos + 1));
    }
  } catch (error) { $('outcome').textContent = error.message; $('outcome').className = 'error'; }
}
$('command-form').addEventListener('submit', evaluate);
// Completion, as the native console's: what the guest's CCL.Completions
// says completes the text before the caret (operation 10).
let completion = null, completionTimer = 0;
function closeCompletion() { completion = null; $('completion').hidden = true; }
function showCompletion() {
  const list = $('completion');
  if (!completion || completion.candidates.length === 0) { list.hidden = true; return; }
  list.replaceChildren(...completion.candidates.map((c, i) => {
    const row = document.createElement('div');
    row.className = `completion-row${i === completion.selected ? ' selected' : ''}`;
    row.setAttribute('role', 'option');
    const typed = document.createElement('span'); typed.className = 'typed'; typed.textContent = c.name.slice(0, completion.prefixLength);
    const rest = document.createElement('span'); rest.className = `rest ${c.origin}`; rest.textContent = c.name.slice(completion.prefixLength);
    const tag = document.createElement('small'); tag.textContent = c.origin;
    row.append(typed, rest, tag);
    row.title = c.signature || c.origin;
    row.onmousedown = e => { e.preventDefault(); acceptCompletion(i); };
    return row;
  }));
  if (completion.beyond) { const more = document.createElement('div'); more.className = 'muted'; more.textContent = 'more - keep typing to narrow'; list.append(more); }
  list.hidden = false;
}
function acceptCompletion(index) {
  const c = completion?.candidates[index];
  if (!c) return;
  const editor = $('source'), at = editor.selectionStart;
  const after = editor.value[at];
  editor.setRangeText(c.name.slice(completion.prefixLength) + (after === undefined || !/[\s)]/.test(after) ? ' ' : ''), at, at, 'end');
  closeCompletion(); editor.focus(); requestCompletion();
}
async function requestCompletion() {
  const editor = $('source'), at = editor.selectionStart, after = editor.value[at];
  if (!connected || busy || editor.selectionStart !== editor.selectionEnd || (after !== undefined && !/[\s)]/.test(after))) { closeCompletion(); return; }
  try {
    const found = await call('complete', editor.value.slice(0, at));
    completion = { ...found, selected: 0 };
    $('signature').textContent = found.signature;
    showCompletion();
  } catch (error) { closeCompletion(); }
}
$('source').addEventListener('input', () => { clearTimeout(completionTimer); completionTimer = setTimeout(requestCompletion, 150); });
$('source').addEventListener('keydown', e => {
  if (!completion || $('completion').hidden) {
    if (e.key === ' ' && e.ctrlKey) { e.preventDefault(); requestCompletion(); }
    return;
  }
  const n = completion.candidates.length;
  if (e.key === 'ArrowDown' || e.key === 'ArrowUp') {
    completion.selected = (completion.selected + (e.key === 'ArrowDown' ? 1 : n - 1)) % n; showCompletion();
  } else if ((e.key === 'Tab' || e.key === 'Enter') && !e.shiftKey) {
    acceptCompletion(completion.selected);
  } else if (e.key === 'Escape') {
    closeCompletion();
  } else return;
  e.preventDefault(); e.stopImmediatePropagation();
});
// As the native console: Enter runs a complete form and continues an open
// one on a new, indented line; Shift+Enter breaks the line; Ctrl+Enter runs.
$('source').addEventListener('keydown', e => {
  if (e.key !== 'Enter') return;
  const editor = $('source'), before = editor.value.slice(0, editor.selectionStart);
  if (e.ctrlKey) { evaluate(e); return; }
  if (e.shiftKey) return;
  const state = balance(editor.value);
  if (state === 'open') {
    e.preventDefault();
    let depth = 0;
    for (const c of before) depth += c === '(' ? 1 : c === ')' ? -1 : 0;
    editor.setRangeText('\n' + '  '.repeat(Math.max(0, Math.min(depth, 16))), editor.selectionStart, editor.selectionEnd, 'end');
  } else if (state !== 'empty') evaluate(e);
});
$('refresh').onclick = refresh;
$('pause').onclick = () => {
  paused = !paused; $('pause').textContent = paused ? 'Resume updates' : 'Pause updates';
  if (!paused) refresh();
};
$('reset').onclick = () => { if (controls) { camera.position.set(11, 10, 16); controls.target.set(0, .8, 0); controls.update(); requestRender(); } };
$('invoke').onclick = async () => {
  try {
    const data = await call('clock');
    $('outcome').textContent = data.clock?.available ? `Integer: ${data.clock.monotonicMs}\nclock.monotonic-ms · actual native IPC response` : 'Clock endpoint did not return a valid sample';
    $('outcome').className = data.clock?.available ? 'success' : 'error';
  } catch (error) { $('outcome').textContent = error.message; $('outcome').className = 'error'; }
};
setupMap();
updateControls();
setInterval(() => {
  if (observedAt) $('freshness').textContent = `${Math.floor((Date.now() - observedAt) / 1000)}s ago${paused ? ' · paused' : ''}`;
  if (connected && Date.now() - observedAt > 8000) {
    document.body.classList.add('stale'); $('status-light').className = '';
    $('connection').textContent = 'Observation is stale';
  }
  if (connected && !paused && !document.hidden && !busy) refresh();
}, 2000);
record('Opened locally · click Connect to contact the lab guest');
