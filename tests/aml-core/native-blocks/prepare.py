"""Copy only the handle declaration needed by the historical import fixture."""
from pathlib import Path
import hashlib
import json

root = Path(__file__).resolve().parents[3]
original = root / 'userspace/runtime/gnat/cubit-handles.ads'
expected = hashlib.sha256(original.read_bytes()).hexdigest()
out = root / 'tests/aml-core/build/handles'
out.mkdir(parents=True, exist_ok=True)
target = out / original.name
target.write_bytes(original.read_bytes())
assert hashlib.sha256(original.read_bytes()).hexdigest() == expected
assert hashlib.sha256(target.read_bytes()).hexdigest() == expected
(out / 'inputs.json').write_text(json.dumps({str(original.relative_to(root)): expected}, indent=2) + '\n')
