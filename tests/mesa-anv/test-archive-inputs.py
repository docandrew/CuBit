"""Actual GNU ar regression: thin-member changes must invalidate inventory."""
import hashlib
import importlib.util
from pathlib import Path
import subprocess
import sys
import tempfile

root = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(root / 'tools'))
spec = importlib.util.spec_from_file_location('builder', root / 'tools/build_mesa_service_bundle.py')
builder = importlib.util.module_from_spec(spec)
spec.loader.exec_module(builder)

def sha(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()

with tempfile.TemporaryDirectory(prefix='mesa-archive-inputs-') as name:
    directory = Path(name)
    objects = directory / 'objects with spaces'
    objects.mkdir()
    source = directory / 'unit.c'
    source.write_text('int cubit_fixture(void) { return 7; }\n')
    obj = objects / 'unit.o'
    subprocess.run(['cc', '-c', str(source), '-o', str(obj)], check=True)
    thin, normal = directory / 'thin.a', directory / 'normal.a'
    subprocess.run(['ar', 'crsT', 'thin.a', str(obj.relative_to(directory))], cwd=directory, check=True)
    subprocess.run(['ar', 'crs', 'normal.a', str(obj.relative_to(directory))], cwd=directory, check=True)
    tracked = {}
    def track(path):
        path = Path(path).resolve()
        tracked[path] = sha(path)
    builder.track_archive(thin, track)
    assert set(tracked) == {thin, obj}, tracked
    before = dict(tracked)
    original = obj.read_bytes()
    obj.write_bytes(original + b'changed')
    assert sha(thin) == before[thin], 'fixture did not isolate external-member mutation'
    assert sha(obj) != before[obj], 'thin member mutation escaped inventory'
    # Negative control: the old archive-only algorithm cannot detect this.
    assert all(sha(path) == value for path, value in {thin: before[thin]}.items())
    obj.write_bytes(original)
    tracked.clear()
    builder.track_archive(normal, track)
    assert set(tracked) == {normal}, tracked
    # Missing external members fail closed rather than producing a partial bundle.
    obj.unlink()
    try:
        builder.track_archive(thin, track)
    except (FileNotFoundError, subprocess.CalledProcessError):
        pass
    else:
        raise AssertionError('accepted missing thin member')
print('PASS: thin external member, path with spaces, normal archive, missing member; old inventory negative control detected')
