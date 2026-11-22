"""Synthetic source-selection checks; never launches an Ada compiler."""
from pathlib import Path
import tempfile, shlex
from source_guards import inventory, compiler_lines, ali_source, digest
checks = 0

def check(condition):
    global checks
    assert condition
    checks += 1

def denied(action):
    try:
        action()
    except (AssertionError, FileNotFoundError):
        check(True)
    else:
        raise AssertionError('invalid source accepted')

with tempfile.TemporaryDirectory(prefix='capture-guards-') as tmp:
    root = Path(tmp); src = root / 'source with spaces'; src.mkdir(); obj = root / 'obj'; obj.mkdir()
    a = src / 'a.adb'; a.write_text('procedure A is begin null; end A;')
    closure = {'a.adb': {'path': str(a.relative_to(root)), 'sha256': digest(a)}}
    pinned = inventory(root, closure); check(pinned['a.adb'] == a)
    command = lambda p: '/pinned/gcc -c -x ada ' + shlex.quote(str(p))
    check(compiler_lines(command(a), '/pinned/gcc', pinned, obj, ['a.adb']) == [str(a)])
    stranger = root / 'a.adb'; stranger.write_text(a.read_text())
    denied(lambda: compiler_lines(command(stranger), '/pinned/gcc', pinned, obj, ['a.adb']))
    denied(lambda: compiler_lines(command(a).replace('/pinned/gcc','/wrong/gcc'), '/pinned/gcc', pinned, obj, ['a.adb']))
    unknown = src / 'extra.adb'; unknown.write_text('extra')
    denied(lambda: inventory(root, closure)); unknown.unlink()
    other = root / 'other'; other.mkdir(); (other/'a.adb').write_text('duplicate'); b=other/'b.ads';b.write_text('b')
    duplicate = dict(closure);duplicate['b.ads']={'path':str(b.relative_to(root)),'sha256':digest(b)}
    denied(lambda: inventory(root,duplicate))
    a.write_text('changed');denied(lambda: inventory(root,closure));a.write_text('procedure A is begin null; end A;')
    binder = obj / 'b__a.adb';binder.write_text('generated')
    check(compiler_lines(command('b__a.adb'),'/pinned/gcc',pinned,obj,['a.adb'])==[str(binder)])
    fake=root/'b__a.adb';fake.write_text('outside')
    denied(lambda: compiler_lines(command(fake),'/pinned/gcc',pinned,obj,['a.adb']))
    wrong=obj/'b__other.adb';wrong.write_text('other')
    denied(lambda: compiler_lines(command(wrong),'/pinned/gcc',pinned,obj,['a.adb']))
    link=src/'link.adb';link.symlink_to(a);denied(lambda: inventory(root,closure));link.unlink()
    check(ali_source('a.adb',pinned,obj,['a.adb'],{})[0]=='local')
    check(ali_source('b__a.adb',pinned,obj,['a.adb'],{})[0]=='generated')
    denied(lambda: ali_source('unknown.ads',pinned,obj,['a.adb'],{}))
    runtime=root/'system.ads';runtime.write_text('runtime');rp={'system.ads':{'path':str(runtime),'sha256':digest(runtime)}}
    check(ali_source('system.ads',pinned,obj,['a.adb'],rp)[0]=='runtime')
    runtime.write_text('changed');denied(lambda: ali_source('system.ads',pinned,obj,['a.adb'],rp))
    denied(lambda: ali_source('../a.adb',pinned,obj,['a.adb'],rp))
    manifest=root/'manifest.json';manifest.write_text('{}');saved=digest(manifest);manifest.write_text('{"changed":true}')
    check(digest(manifest)!=saved)
print('source guard checks', checks)
