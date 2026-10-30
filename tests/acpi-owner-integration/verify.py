from pathlib import Path
import hashlib
import json

here = Path(__file__).resolve().parent
root = here.parent.parent
pins = json.loads((here / 'source-inputs.json').read_text())
directories = {Path(name).parent for name in pins if Path(name).suffix in {'.ads', '.adb'} and not name.startswith('userspace/runtime/')}
actual = {str(path.relative_to(root)) for directory in directories for path in (root / directory).iterdir()
          if path.suffix in {'.ads', '.adb'}}
expected = {name for name in pins if Path(name).suffix in {'.ads', '.adb'} and not name.startswith('userspace/runtime/')}
assert actual == expected, ('source inventory drift', actual ^ expected)
for name, expected_hash in pins.items():
    path = root / name
    assert path == path.resolve(strict=True) and path.is_file(), name
    assert hashlib.sha256(path.read_bytes()).hexdigest() == expected_hash, name
print(f'Unified owner suite verified {len(pins)} exact source/recipe inputs; no compiler run.')
