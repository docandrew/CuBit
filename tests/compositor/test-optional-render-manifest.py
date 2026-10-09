#!/usr/bin/env python3
"""Check required/optional render metadata with an explicitly selected compiler.

Run in Nix. Does not modify compiler/schema sources or apply the adjacent patch.
Each invocation retains its input, assembly, metadata and diagnostics in a new
output directory. Only wire bytes and rejection status are asserted here;
procmgr's optional launch behavior requires separate native integration tests.
"""
import argparse
import hashlib
import json
from pathlib import Path
import struct
import subprocess

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('--compiler', type=Path, required=True)
parser.add_argument('--schema', type=Path, required=True)
parser.add_argument('--output', type=Path, required=True, help='new evidence directory')
args = parser.parse_args()
compiler, schema, output = (p.resolve() for p in (args.compiler, args.schema, args.output))
output.mkdir(parents=True, exist_ok=False)

def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()

inputs = {str(p): digest(p) for p in (compiler, schema, Path(__file__).resolve())}
catalog = output / 'catalog.ccl'
catalog.write_text('(service-catalog v1 (application-slots 24 62))\n')
base = '(executable-manifest v1 (identity "test") (version "1") %s)'
cases = [
    ('required', base % '(request-render read-write render)', 0),
    ('optional', base % '(request-render-optional read-write render)', 1),
    ('typed-required', '(Executable_Manifest "test" "1" requests => [(Request.Render "render")])', 0),
    ('typed-optional', '(Executable_Manifest "test" "1" requests => [(Request.Optional_Render "render")])', 1),
    ('duplicate', base % '(request-render read-write r) (request-render-optional read-write s)', None),
    ('bad-rights', base % '(request-render-optional read r)', None),
    ('typed-duplicate', '(Executable_Manifest "test" "1" requests => [(Request.Render "r"), (Request.Optional_Render "s")])', None),
]
results = {}
for name, source, mode in cases:
    manifest = output / f'{name}.ccl'
    manifest.write_text(source + '\n')
    result = subprocess.run([str(compiler), str(catalog), str(manifest), '--schema', str(schema)], capture_output=True, text=True)
    (output / f'{name}.stderr').write_text(result.stderr)
    if mode is None:
        assert result.returncode == 1 and not result.stdout, (name, result.returncode, result.stderr)
    else:
        assert result.returncode == 0, (name, result.stderr)
        asm, obj, caps = (output / f'{name}.{suffix}' for suffix in ('S', 'o', 'caps'))
        asm.write_text(result.stdout)
        subprocess.run(['gcc', '-c', str(asm), '-o', str(obj)], check=True)
        subprocess.run(['objcopy', '--dump-section', f'.cubit.caps={caps}', str(obj)], check=True)
        assert caps.read_bytes() == struct.pack('<IHHBBHIQ', 0x43424954, 1, 1, 11, 3, 24, mode, 0), name
    results[name] = 'PASS'
assert all(digest(Path(p)) == h for p, h in inputs.items())
(output / 'result.json').write_text(json.dumps({'inputs': inputs, 'cases': results}, indent=2) + '\n')
print('PASS required/optional keyword and typed wire metadata; duplicate and rights rejection')
