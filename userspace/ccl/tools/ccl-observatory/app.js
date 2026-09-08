import * as THREE from 'three';
import { OrbitControls } from '/OrbitControls.js';

const $ = id => document.getElementById(id);
const token = new URLSearchParams(location.hash.slice(1)).get('session') || '';
// Keep the development credential in memory, not storage or subsequent URLs.
history.replaceState(null, '', '/');
let snapshot = null, connection = null, selected = 'control', busy = false;
let paused = false, observedAt = 0, healthy = false;
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
    $('map-empty').querySelector('p').textContent = 'The live object list, inspector, and CCL controls still work.';
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
    network: 'The native netstack carries the guest’s outbound TCP connection to the local development relay.'
  }[id];
  $('properties').replaceChildren();
  property('Process', item.pid);
  property('Identity assurance', 'Not cryptographically authenticated');
  property('Evidence', id === 'control' ? 'Guest-reported process ID' : id === 'clock' ?
    (snapshot.clock.available ? 'Clock IPC call succeeded' : 'Registered provider; clock call failed') : 'Registered provider + active TCP relay');
  property('Scope', 'Adapter’s own bindings only');
  if (id !== 'control') property('Manifest request', id === 'clock' ? 'Clock endpoint · slot 25 · read/write' : 'Network endpoint · slot 11 · read/write');
  if (id === 'network') property('Destination', '10.0.2.2:9440 · adapter configuration, not enforced destination policy');
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
  healthy = false; document.body.classList.add('stale');
  $('status-light').className = ''; $('connection').textContent = 'Unavailable / stale';
  $('footer-status').textContent = message;
}
function validateSnapshot(data) {
  const decimal = value => typeof value === 'string' && /^\d{1,20}$/.test(value) && BigInt(value) <= 18446744073709551615n;
  if (data.scope !== 'adapter-bindings-only' || data.transport !== 'plaintext-development-relay' || data.peerAuthenticated !== false ||
      ![data.processId, data.networkProcessId, data.clockProcessId].every(decimal) ||
      typeof data.clock?.available !== 'boolean' || (data.clock.available && !decimal(data.clock.monotonicMs)) ||
      data.interface?.name !== 'clock' || data.interface?.version !== '1.0' ||
      data.interface.operation !== 'clock.monotonic-ms' || data.interface.result?.kind !== 'Integer' ||
      data.interface.result.bits !== 64 || !Array.isArray(data.interface.parameters) || data.interface.parameters.length !== 0 ||
      !Array.isArray(data.interface.digestWords) || data.interface.digestWords.length !== 4 || !data.interface.digestWords.every(decimal)) {
    throw new Error('Unsupported or invalid observation schema; no topology rendered');
  }
}
async function call(operation, source = '') {
  if (!token) throw new Error('Open the session URL printed by the local relay. No credential is stored in the browser.');
  if (busy) throw new Error('A native request is already in progress');
  busy = true; $('execute').disabled = true; $('invoke').disabled = true;
  try {
    const response = await fetch('/api/request', {
      method: 'POST', headers: { 'Content-Type': 'application/json', 'X-CuBit-Session': token },
      body: JSON.stringify({ operation, source }), signal: AbortSignal.timeout(5000)
    });
    const envelope = await response.json();
    if (!response.ok) throw new Error(envelope.error || 'Control request failed');
    if (connection !== null && connection !== envelope.connection) {
      snapshot = null; clearGraph(); record('New native connection; prior observations discarded');
    }
    connection = envelope.connection;
    $('latency').textContent = `${envelope.roundTripMs} ms`;
    $('footer-status').textContent = `Native connection ${connection} · request ${envelope.request} · plaintext development`;
    if (operation !== 'inspect') record(`#${envelope.request} ${operation} · ${envelope.roundTripMs} ms`);
    return envelope.result;
  } catch (error) { fail(error.message); throw error; }
  finally { busy = false; $('execute').disabled = false; $('invoke').disabled = false; }
}
async function refresh() {
  if (busy) return;
  try {
    const data = await call('inspect'); validateSnapshot(data);
    snapshot = data; buildGraph(data); observedAt = Date.now(); healthy = true;
    document.body.classList.remove('stale'); $('status-light').className = 'live';
    $('connection').textContent = 'Receiving native observations';
    $('clock-value').textContent = data.clock.available ? data.clock.monotonicMs : 'Unavailable';
    select(selected);
  } catch (error) { fail(error.message); }
}
async function evaluate(event) {
  event.preventDefault();
  const source = $('source').value;
  if (!/^[\x09\x0a\x0d\x20-\x7e]{0,1024}$/.test(source)) {
    $('outcome').textContent = 'This CCL version accepts up to 1024 ASCII bytes.'; $('outcome').className = 'error'; return;
  }
  try {
    const result = await call('evaluate', source);
    if (typeof result.ok !== 'boolean' || typeof result.message !== 'string' || typeof result.type !== 'string') throw new Error('Invalid evaluation response');
    $('outcome').textContent = `${result.message}\n${result.type} · fuel remaining ${result.fuelRemaining}`;
    $('outcome').className = result.ok ? 'success' : 'error';
    if (!result.ok && /^\d{1,4}$/.test(result.position) && Number(result.position) > 0) {
      const pos = Math.min(source.length, Number(result.position) - 1);
      $('source').focus(); $('source').setSelectionRange(pos, Math.min(source.length, pos + 1));
    }
  } catch (error) { $('outcome').textContent = error.message; $('outcome').className = 'error'; }
}
$('command-form').addEventListener('submit', evaluate);
$('source').addEventListener('keydown', e => { if (e.ctrlKey && e.key === 'Enter') evaluate(e); });
$('refresh').onclick = refresh;
$('pause').onclick = () => { paused = !paused; $('pause').textContent = paused ? 'Resume updates' : 'Pause updates'; if (!paused) refresh(); };
$('reset').onclick = () => { if (controls) { camera.position.set(11, 10, 16); controls.target.set(0, .8, 0); controls.update(); requestRender(); } };
$('invoke').onclick = async () => {
  try {
    const data = await call('clock');
    $('outcome').textContent = data.clock?.available ? `Integer: ${data.clock.monotonicMs}\nclock.monotonic-ms · actual native IPC response` : 'Clock endpoint did not return a valid sample';
    $('outcome').className = data.clock?.available ? 'success' : 'error';
  } catch (error) { $('outcome').textContent = error.message; $('outcome').className = 'error'; }
};
setupMap(); refresh();
setInterval(() => {
  $('freshness').textContent = observedAt ? `${Math.floor((Date.now() - observedAt) / 1000)}s ago${paused ? ' / paused' : ''}` : 'Never';
  if (observedAt && Date.now() - observedAt > 6000 && healthy) fail('Observations are stale; current state is unknown');
}, 1000);
setInterval(() => { if (!paused && !document.hidden) refresh(); }, 2000);
