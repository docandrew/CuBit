import { test } from 'node:test';
import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { humanize, unitOf } from './units.js';

// CCL.Units's own outputs (tests/ccl-console/units_vectors.adb): the browser
// shows every unit exactly as the native console does.
test('units match CCL.Units', () => {
  const vectors = JSON.parse(readFileSync(new URL('./units-vectors.json', import.meta.url)));
  assert.ok(vectors.length > 10);
  for (const v of vectors) assert.equal(humanize(unitOf(v.type), v.raw), v.shown, `${v.type} ${v.raw}`);
});
