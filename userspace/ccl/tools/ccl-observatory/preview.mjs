// Linux-hosted, static-only preview. This is not a CuBit control gateway.
import { createServer } from 'node:http';
import { readFile } from 'node:fs/promises';
import { createHash } from 'node:crypto';
import { pathToFileURL } from 'node:url';

const assets = new Map([
  ['/', ['index.html', 'text/html; charset=utf-8']],
  ['/app.js', ['app.js', 'text/javascript; charset=utf-8']],
  ['/wire.js', ['wire.js', 'text/javascript; charset=utf-8']],
  ['/style.css', ['style.css', 'text/css; charset=utf-8']],
  ['/three.module.js', ['node_modules/three/build/three.module.js', 'text/javascript; charset=utf-8']],
  ['/three.core.js', ['node_modules/three/build/three.core.js', 'text/javascript; charset=utf-8']],
  ['/OrbitControls.js', ['node_modules/three/examples/jsm/controls/OrbitControls.js', 'text/javascript; charset=utf-8']],
  ['/LICENSE-three.txt', ['node_modules/three/LICENSE', 'text/plain; charset=utf-8']],
]);

export async function startPreview(port = 8787) {
  // Read only the explicitly published assets, never arbitrary repository paths.
  const content = new Map(await Promise.all([...assets].map(async ([route, [path, type]]) =>
    [route, { type, bytes: await readFile(new URL(path, import.meta.url)) }])));
  const html = content.get('/').bytes.toString('utf8');
  const importMap = html.match(/<script type="importmap">([^<]+)<\/script>/)[1];
  const hash = createHash('sha256').update(importMap).digest('base64');
  const server = createServer((request, response) => {
    response.setHeader('Cache-Control', 'no-store');
    response.setHeader('X-Content-Type-Options', 'nosniff');
    response.setHeader('Referrer-Policy', 'no-referrer');
    response.setHeader('Content-Security-Policy',
      `default-src 'none'; script-src 'self' 'sha256-${hash}'; style-src 'self' 'unsafe-inline'; connect-src http://127.0.0.1:18445; img-src 'self'; base-uri 'none'; frame-ancestors 'none'; form-action 'none'`);
    const reply = (status, message, extra = {}) => {
      response.writeHead(status, { 'Content-Type': 'text/plain; charset=utf-8', ...extra });
      response.end(request.method === 'HEAD' ? undefined : message);
    };
    // Loopback binding alone does not prevent hostile DNS rebinding.
    const authority = request.headers.host;
    const localPort = server.address().port;
    if (authority !== `127.0.0.1:${localPort}` && authority !== `localhost:${localPort}`) {
      reply(403, 'Loopback preview hosts only.\n'); return;
    }
    if (request.method !== 'GET' && request.method !== 'HEAD') {
      reply(405, 'Static preview only; no control operations.\n', { Allow: 'GET, HEAD' }); return;
    }
    // Do not decode/normalize untrusted paths into allowed asset names.
    const path = request.url.split('?')[0];
    if (path === '/favicon.ico') { response.writeHead(204); response.end(); return; }
    const asset = content.get(path);
    if (!asset) { reply(404, 'Not a preview asset. No CuBit transport is attached.\n'); return; }
    response.writeHead(200, { 'Content-Type': asset.type, 'Content-Length': asset.bytes.length });
    response.end(request.method === 'HEAD' ? undefined : asset.bytes);
  });
  server.requestTimeout = 5000;
  server.headersTimeout = 5000;
  await new Promise((resolve, reject) => {
    server.once('error', reject);
    server.listen(port, '127.0.0.1', resolve);
  });
  return server;
}

if (process.argv[1] && import.meta.url === pathToFileURL(process.argv[1]).href) {
  const port = process.env.CCL_PREVIEW_PORT ?? '8787';
  if (!/^[0-9]{1,5}$/.test(port) || Number(port) < 1 || Number(port) > 65535) {
    console.error('CCL_PREVIEW_PORT must be an integer from 1 to 65535.');
    process.exitCode = 1;
  } else {
    try {
      await startPreview(Number(port));
      console.log(`CuBit Observatory: http://127.0.0.1:${port}/`);
      console.log('Static assets only; Connect sends CBOR directly to the CuBit guest on loopback port 18445.');
      console.log('Restart this server after editing assets. Ctrl+C to stop.');
    } catch (error) {
      console.error(`Preview could not start: ${error.message}`);
      if (error.code === 'ENOENT') console.error('Install pinned assets with npm ci --ignore-scripts first (inside nix develop).');
      process.exitCode = 1;
    }
  }
}
