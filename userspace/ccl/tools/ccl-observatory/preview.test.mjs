import { test } from 'node:test';
import assert from 'node:assert/strict';
import { request } from 'node:http';
import { createHash } from 'node:crypto';
import { startPreview } from './preview.mjs';

test('static preview exposes only its loopback asset allowlist', async t => {
  const server = await startPreview(0);
  t.after(() => new Promise(resolve => server.close(resolve)));
  assert.equal(server.address().address, '127.0.0.1');
  const port = server.address().port;
  const get = (path, options = {}) => new Promise((resolve, reject) => {
    const req = request({ hostname: '127.0.0.1', port, path, ...options }, res => {
      const chunks = [];
      res.on('data', chunk => chunks.push(chunk));
      res.on('end', () => resolve({ status: res.statusCode, headers: res.headers, body: Buffer.concat(chunks).toString() }));
    });
    req.on('error', reject); req.end();
  });
  await t.test('HTML and full ES module dependency chain are available', async () => {
    for (const path of ['/', '/app.js', '/style.css', '/three.module.js', '/three.core.js', '/OrbitControls.js', '/LICENSE-three.txt']) {
      const res = await get(path);
      assert.equal(res.status, 200, path);
      assert.ok(res.body.length > 0, path);
      assert.equal(res.headers['cache-control'], 'no-store');
      assert.equal(res.headers['x-content-type-options'], 'nosniff');
    }
    const res = await get('/');
    const map = res.body.match(/<script type="importmap">([^<]+)<\/script>/)[1];
    const hash = createHash('sha256').update(map).digest('base64');
    assert.ok(res.headers['content-security-policy'].includes(`'sha256-${hash}'`));
    assert.ok(res.headers['content-security-policy'].includes('connect-src http://127.0.0.1:18445;'));
    assert.match(res.body, /PLAINTEXT/);
  });
  await t.test('HEAD and localhost work, with no HEAD body', async () => {
    const res = await get('/', { method: 'HEAD', headers: { Host: `localhost:${port}` } });
    assert.equal(res.status, 200);
    assert.equal(res.body, '');
    assert.ok(Number(res.headers['content-length']) > 0);
  });
  await t.test('repository files, traversal, and the retired API are not served', async () => {
    for (const path of ['/package.json', '/preview.mjs', '/.git/config', '/../flake.nix', '/%2e%2e/flake.nix', '/x/../', '//', '/api/request']) {
      assert.equal((await get(path)).status, 404, path);
    }
    assert.equal((await get('/api/request', { method: 'POST' })).status, 405);
    assert.equal((await get('/', { method: 'PUT' })).status, 405);
  });
  await t.test('foreign Host headers are rejected', async () => {
    for (const host of ['attacker.example', `attacker.example:${port}`, '127.0.0.1:1', `127.0.0.1:${port}.attacker.example`]) {
      assert.equal((await get('/', { headers: { Host: host } })).status, 403);
    }
  });
});
