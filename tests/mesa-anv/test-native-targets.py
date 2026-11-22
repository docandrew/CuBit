"""Test combined archive selection against real Meson target metadata."""
import argparse
import json
from pathlib import Path
import sys

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / 'tools'))
import native_mesa_targets as policy

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('build', type=Path)
args = parser.parse_args()
build = args.build.resolve()
targets = json.loads((build / 'meson-info/intro-targets.json').read_text())
selected = policy.archives(targets, build)
for excluded in ('src/loader/libloader.a', 'src/gallium/auxiliary/pipe-loader/libpipe_loader_static.a'):
    assert build / excluded not in selected
assert all(build / required in selected for required in policy.REQUIRED)
for required in policy.REQUIRED:
    missing = [item for item in targets if str(build / required) not in item['filename']]
    try:
        policy.archives(missing, build)
    except ValueError as error:
        assert 'missing required' in str(error)
    else:
        raise AssertionError('accepted missing ' + required)
try:
    policy.archives(targets + [{'type': 'static library', 'filename': [str(build.parent / 'outside.a')]}], build)
except ValueError:
    pass
else:
    raise AssertionError('accepted outside build root')
print(f'PASS {len(selected)} engine archives; missing-engine and outside-root controls rejected')
