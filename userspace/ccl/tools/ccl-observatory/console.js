// The Observatory's REPL transcript: the same presentation the native CCL
// console draws (CCL.Presentations over the wire, operation 7), with the
// same interactions. Everything from CuBit is data: built with text nodes,
// never markup.
import { runs } from '/highlight.js';
import { unitOf, humanize } from '/units.js';

const RAINBOW = 6;
// Rows of a table shown at once, as the native console.
const MAXIMUM_TABLE_ROWS = 20;

// The CCL a table click writes, exactly as the native console writes it
// (CCL_Console_View.Row_Query): the entry's own source, wrapped.
const ROW_NAME = 'row';
export function fieldAccess(field) { return `(field ${ROW_NAME} ${field})`; }
export function rowQuery(operation, rowType, body, source) {
  return `(${operation} (fn ((${ROW_NAME} ${rowType})) ${body}) ${source})`;
}

function element(tag, className, text) {
  const node = document.createElement(tag);
  if (className) node.className = className;
  if (text !== undefined) node.textContent = text;
  return node;
}

export function highlighted(source) {
  const fragment = document.createDocumentFragment();
  for (const run of runs(source)) {
    const span = element('span', `tok-${run.cls}${run.cls === 'delimiter' ? ` depth-${run.depth % RAINBOW}` : ''}`, run.text);
    fragment.append(span);
  }
  return fragment;
}

function grouped(value) { return BigInt(value).toLocaleString('en-US'); }

// One transcript card. actions: { insert(text), replace(text), rows(id, first) }
// live: { runs } when the card is a live cell (the native periodic slot).
export function card(source, result, elapsedMs, actions, live = null) {
  const root = element('article', `ccl-card ${result.ok ? 'ok' : 'failed'}`);
  const top = element('div', 'ccl-card-top');
  const code = element('pre', 'ccl-source');
  code.append(highlighted(source));
  code.title = 'Click to edit this entry again';
  code.onclick = () => actions.replace(source);
  top.append(code);
  if (live) {
    const mark = element('button', 'ccl-live', `LIVE 1s  x${live.runs}`);
    mark.title = 'Re-runs inside CuBit every second; click to stop';
    mark.onclick = () => actions.unwatch();
    top.append(mark);
  }
  if (result.type) top.append(element('span', 'ccl-badge', result.type));
  root.append(top);
  const body = element('div', 'ccl-result');
  const meta = element('span', 'ccl-meta',
    `${result.table?.many ? `${grouped(result.table.total)} rows  ` : ''}` +
    `${result.gallery ? `${grouped(result.gallery.total)} images  ` : ''}${elapsedMs} ms  ` +
    `${grouped(1000000n - BigInt(result.fuelRemaining))} fuel`);
  if (result.form === 'failure') {
    body.append(element('div', 'ccl-failure', `! ${result.value}`));
    const position = Number(result.position);
    if (position > 0 && position <= source.length) {
      // The diagnostic's place in the source, underlined.
      const marked = [...code.querySelectorAll('span')];
      let offset = 0;
      for (const span of marked) {
        if (offset + span.textContent.length >= position) { span.classList.add('ccl-diagnostic'); break; }
        offset += span.textContent.length;
      }
    }
  } else if (result.form === 'table') {
    body.append(table(source, result.table, actions));
  } else if (result.form === 'picture') {
    body.append(picture(result.picture, actions));
  } else if (result.form === 'gallery') {
    const strip = element('div', 'ccl-gallery');
    result.gallery.pictures.forEach(p => {
      const figure = picture(p, actions, true);
      figure.title = 'Click to insert this image';
      figure.onclick = () => actions.insert(`(Image ${p.width} ${p.height} ${p.id})`);
      strip.append(figure);
    });
    body.append(strip);
  } else {
    const value = element('pre', 'ccl-value');
    value.append(element('span', 'ccl-equals', '= '));
    if (result.type === 'String') value.append(element('span', 'tok-text', result.value));
    else value.append(highlighted(result.value));
    value.title = 'Click to insert this value at the caret';
    value.onclick = () => actions.insert(result.type === 'String' ? JSON.stringify(result.value) : result.value);
    body.append(value);
  }
  body.append(meta);
  root.append(body);
  return root;
}

function table(source, t, actions) {
  const node = element('table', 'ccl-table');
  const head = node.createTHead().insertRow();
  for (const field of t.fields) {
    const th = element('th', field.numeric ? 'numeric' : '', field.name);
    th.title = `${field.name} : ${field.type}  |  click to sort the rows by this field`;
    th.onclick = () => actions.replace(rowQuery('sort-by', t.rowType, fieldAccess(field.name), source));
    head.append(th);
  }
  const body = node.createTBody();
  t.rows.slice(0, MAXIMUM_TABLE_ROWS).forEach(row => {
    const tr = body.insertRow();
    row.forEach((cell, k) => {
      const td = tr.insertCell();
      td.className = t.fields[k].numeric ? 'numeric' : '';
      const unit = unitOf(t.fields[k].type);
      td.append(unit ? element('span', 'number', humanize(unit, cell)) : highlighted(cell));
      td.title = `${t.fields[k].name} : ${t.fields[k].type} = ${cell}  |  click to insert  |  Ctrl+click: rows with this value`;
      td.onclick = event => {
        if (event.ctrlKey) actions.replace(rowQuery('where', t.rowType, `(= ${fieldAccess(t.fields[k].name)} ${cell})`, source));
        else actions.insert(cell);
      };
    });
  });
  const hidden = BigInt(t.total) - BigInt(Math.min(t.rows.length, MAXIMUM_TABLE_ROWS));
  if (hidden > 0n) node.createCaption().textContent = `+${grouped(hidden)} more rows`;
  return node;
}

// Pixels by content id: immutable, so cached for the page's lifetime.
const pixelCache = new Map();
function picture(p, actions, thumbnail = false) {
  const figure = element('figure', thumbnail ? 'ccl-picture thumbnail' : 'ccl-picture');
  const canvas = element('canvas');
  canvas.width = p.width; canvas.height = p.height;
  // Whole-number enlargement while it fits, as the native console; a
  // gallery's thumbnails fit 160 x 120.
  const [boxWidth, boxHeight] = thumbnail ? [160, 120] : [640, 280];
  const scale = Math.max(1, Math.min(8, Math.floor(boxWidth / p.width), Math.floor(boxHeight / p.height)));
  canvas.style.width = `${p.width * scale}px`; canvas.style.height = `${p.height * scale}px`;
  const caption = element('figcaption', '', `${p.width} x ${p.height} pixels${scale > 1 ? `  |  shown at ${p.width * scale} x ${p.height * scale}` : ''}`);
  figure.append(canvas, caption);
  const context = canvas.getContext('2d');
  const paint = image => context.putImageData(image, 0, 0);
  const key = p.id.toString();
  if (pixelCache.has(key)) { paint(pixelCache.get(key)); return figure; }
  const image = context.createImageData(p.width, p.height);
  (async () => {
    for (let row = 0; row < p.height;) {
      const band = await actions.rows(p.id, BigInt(row));
      if (!band.known) { caption.textContent = 'image expired: no longer in the guest\'s image store'; caption.className = 'expired'; return; }
      if (band.width !== p.width || band.height !== p.height || band.firstRow !== row) throw new Error('Image rows do not match the picture');
      for (let i = 0; i < band.rowCount * band.width; i++) {
        const at = (row * band.width + i) * 4;
        image.data[at] = band.rgb[i * 3]; image.data[at + 1] = band.rgb[i * 3 + 1];
        image.data[at + 2] = band.rgb[i * 3 + 2]; image.data[at + 3] = 255;
      }
      row += band.rowCount;
      paint(image);
    }
    pixelCache.set(key, image);
  })().catch(error => { caption.textContent = error.message; caption.className = 'expired'; });
  return figure;
}
