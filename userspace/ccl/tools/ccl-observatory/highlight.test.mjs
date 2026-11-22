import { test } from 'node:test';
import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { classify, balance, runs } from './highlight.js';

// Written by the native CCL.Highlighting (tests/ccl-console/vectors.adb).
const vectors = JSON.parse(readFileSync(new URL('./highlight-vectors.json', import.meta.url)));

test('the browser highlighter agrees with the native one, mark for mark', () => {
  assert.ok(vectors.length >= 10);
  for (const v of vectors) {
    const marks = classify(v.source).map(m => [m.cls, m.depth]);
    assert.deepEqual(marks, v.marks, v.source);
    assert.equal(balance(v.source), v.balance, v.source);
  }
});

test('runs cover the source exactly', () => {
  for (const v of vectors) assert.equal(runs(v.source).map(r => r.text).join(''), v.source);
});
