#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../.."
python3 - <<'PY'
from pathlib import Path
import hashlib,json,shutil
sources=list(Path('userspace/ccl/src').glob('*.ad?'))
sources += [Path('userspace/runtime/gnat')/name for name in ('cubit.ads','cubit-metric_protocol.ads','cubit-metric_records.ads','cubit-metric_records.adb')]
hashes={str(p):hashlib.sha256(p.read_bytes()).hexdigest() for p in sources}
for p in sources:
    target=Path('tests/observatory-metrics/build')/('ccl-source' if 'ccl/src' in str(p) else 'source')/p.name
    target.parent.mkdir(parents=True,exist_ok=True)
    shutil.copyfile(p,target)
    assert hashlib.sha256(target.read_bytes()).hexdigest()==hashes[str(p)],'source changed during snapshot'
assert all(hashlib.sha256(Path(p).read_bytes()).hexdigest()==h for p,h in hashes.items()),'source changed during snapshot'
Path('tests/observatory-metrics/build/ccl-inputs.json').write_text(json.dumps(hashes,indent=2))
PY
cd kernel
alr exec -- gprbuild -q -p -P ../tests/observatory-metrics/ccl.gpr
../tests/observatory-metrics/build/ccl/ccl_tests
