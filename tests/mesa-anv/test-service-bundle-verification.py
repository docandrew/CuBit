#!/usr/bin/env python3
"""Check a real link bundle, then exercise isolated verifier corruption cases."""
import argparse
import hashlib
import importlib.util
import json
from pathlib import Path
import tempfile

root = Path(__file__).resolve().parents[2]
spec = importlib.util.spec_from_file_location('bundle', root / 'tools/verify_mesa_service_bundle.py')
module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(module)
parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('bundle', type=Path)
args = parser.parse_args()
prefix, flags = module.verify(args.bundle)
assert prefix[-1] == 'cpp'
assert '-Wl,--whole-archive' in flags
assert not any('--wrap=' in flag for flag in flags)

with tempfile.TemporaryDirectory(prefix='cubit-bundle-verifier-') as name:
    directory = Path(name)
    files = {key: directory / key for key in ('input.a', 'bridge.o', 'link-check.app', 'link-args.json')}
    contents = {key: b'fixture' for key in files}
    contents['link-args.json'] = b'[]'
    for key, path in files.items():
        path.write_bytes(contents[key])
    sha = lambda key: hashlib.sha256(contents[key]).hexdigest()
    data = {'status': 'LINK_PASS', 'executed': False,
            'inputs_sha256': {str(files['input.a']): sha('input.a')},
            'objects_sha256': {str(files['bridge.o']): sha('bridge.o')},
            'link_check_sha256': sha('link-check.app'),
            'link_args_sha256': sha('link-args.json'), 'link_prefix': ['fixture']}
    manifest = directory / 'inputs.json'
    manifest.write_text(json.dumps(data))
    assert module.verify(directory) == (['fixture'], [])
    for key, path in files.items():
        path.write_bytes(b'changed')
        try:
            module.verify(directory)
        except ValueError:
            pass
        else:
            raise AssertionError('accepted corrupted ' + key)
        path.write_bytes(contents[key])
    for field, value in [('status', 'BUILDING'), ('executed', True)]:
        manifest.write_text(json.dumps({**data, field: value}))
        try:
            module.verify(directory)
        except ValueError:
            pass
        else:
            raise AssertionError('accepted invalid ' + field)
print('PASS: real native bundle, fixture baseline, six rejection cases; no GPU execution')
