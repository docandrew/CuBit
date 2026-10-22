"""Closed hosted source selection; no compiler execution."""
from pathlib import Path
import hashlib
import shlex


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def canonical_file(path):
    path = Path(path)
    assert path.is_absolute() and path.is_file(), path
    assert path == path.resolve(strict=True), path
    return path


def inventory(root, sources):
    root = root.resolve(strict=True)
    directories = {root / Path(item['path']).parent for item in sources.values()}
    actual = {}
    for directory in directories:
        assert directory == directory.resolve(strict=True)
        assert not any(x in directory.relative_to(root).parts for x in ['kernel', 'native', 'runtime'])
        for path in directory.iterdir():
            if path.suffix.lower() not in ['.ads', '.adb']:
                continue
            canonical_file(path)
            assert path.name not in actual, ('duplicate', path.name)
            actual[path.name] = {'path': str(path.relative_to(root)), 'sha256': digest(path)}
    assert actual == sources, ('unexpected, missing or changed source', set(actual) ^ set(sources))
    return {name: canonical_file(root / item['path']) for name, item in sources.items()}


def generated(path, obj, mains):
    path = Path(path)
    if not path.is_absolute():
        # GPR explicitly compiles binder files in this invocation's object directory.
        assert path.parent == Path('.'), path
        path = obj / path
    canonical_file(path)
    assert path.parent == obj.resolve(strict=True), path
    assert path.name in {'b__' + Path(m).stem + ext for m in mains for ext in ['.ads', '.adb']}, path
    return path


def compiler_lines(text, gcc, pinned, obj, mains):
    selected = []
    for line in text.splitlines():
        if ' -c ' not in line or ' -x ' not in line:
            continue
        argv = shlex.split(line)
        if '-c' not in argv or '-x' not in argv:
            continue
        assert argv[argv.index('-x') + 1] == 'ada'
        assert argv[0] == gcc, argv[0]
        operands = [Path(x) for x in argv[1:] if not x.startswith('-') and Path(x).suffix in ['.ads', '.adb']]
        assert len(operands) == 1, operands
        source = operands[0]
        if source.name.startswith('b__'):
            source = generated(source, obj, mains)
        else:
            canonical_file(source)
            assert source.name in pinned and source == pinned[source.name], source
        selected.append(str(source))
    return selected


def ali_source(name, pinned, obj, mains, runtime):
    assert Path(name).name == name, name
    if name in pinned:
        return 'local', canonical_file(pinned[name])
    if name.startswith('b__'):
        return 'generated', generated(Path(name), obj, mains)
    assert name in runtime, ('unaccounted ALI dependency', name)
    item = runtime[name]
    path = canonical_file(Path(item['path']))
    assert digest(path) == item['sha256'], path
    return 'runtime', path
