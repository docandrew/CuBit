"""Exercise the real normal-desktop disk target using only temporary images.

Run from the repository root in Nix under the shared build lock after
`make -C kernel desktop-metrics`. Uses existing staged desktop dependencies.
"""
import hashlib
from pathlib import Path
import subprocess
import tempfile

root=Path(__file__).resolve().parents[2]
def digest(p):
    return hashlib.file_digest(p.open('rb'),'sha256').hexdigest()
def run(*args):
    return subprocess.run(args,check=True,capture_output=True,text=True,cwd=root)

with tempfile.TemporaryDirectory(prefix='cubit-metrics-session-disk-') as tmp:
    d=Path(tmp);base=d/'base.img';out=d/'desktop.img'
    with base.open('wb') as f: f.truncate(32*1024*1024)
    run('mke2fs','-q','-t','ext2','-F',str(base))
    original=digest(base)
    run('make','-C','kernel','prepare-desktop-disk',
        f'DESKTOP_BASE_DISK={base}',f'DESKTOP_SCRATCH_DISK={out}')
    assert digest(base)==original,'base image was modified'
    for name,source in (
        ('desktop.svc',root/'userspace/services/desktop/build-metrics/desktop.svc'),
        ('metrics.svc',root/'kernel/isodir/boot/metrics.svc'),
        ('init.ccl',root/'tests/headless/init-desktop-session.ccl')):
        extracted=d/name
        run('debugfs','-R',f'dump /{name} {extracted}',str(out))
        assert digest(extracted)==digest(source),(name,'wrong staged payload')
    startup=(d/'init.ccl').read_text()
    collector='(start "metrics.svc" (priority 2))'
    desktop='(start "desktop.svc" (priority 4))'
    assert startup.count(collector)==startup.count(desktop)==1
    assert startup.index(collector)<startup.index(desktop)
    run('e2fsck','-fn',str(out))
    print('PASS normal desktop disk: metrics-enabled Desktop, collector and startup extracted byte-for-byte; base unchanged')
