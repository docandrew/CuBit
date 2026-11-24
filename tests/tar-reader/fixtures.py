#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later
"""fixtures.py DIR: interoperability archives for tests/tar-reader, made
at test time with Python's tarfile in its three formats (POSIX ustar, GNU,
pax) and with git archive (pax global header), plus each one's expected
walk (EXPECTED.txt beside ARCHIVE.tar). Nothing here is committed output;
the archives hold only the text below."""
import io
import os
import subprocess
import sys
import tarfile
import tempfile

LONG = 'deep/' + 'x' * 120 + '/' + 'y' * 120 + '/file.txt'
FILES = [('pkg', None), ('pkg/a.txt', b'hello\n'), ('pkg/empty', b''),
         ('pkg/big.bin', bytes(range(256)) * 9), (LONG, b'long name\n')]


def expected(entries, data):
    lines = []
    for name, kind, size in entries:
        lines.append(('D %s' % name) if kind == 'D' else ('F %s %d' % (name, size)) if kind == 'F'
                     else 'S %s' % name)
    lines.append('END %d' % data)
    return '\n'.join(lines) + '\n'


def make_tarfile(path, fmt):
    entries, data = [], 0
    with tarfile.open(path, 'w', format=fmt) as tar:
        for name, content in FILES:
            if fmt == tarfile.USTAR_FORMAT and name == LONG:  # no ustar split fits it
                continue
            info = tarfile.TarInfo(name)
            info.mtime = 1700000000
            if content is None:
                info.type = tarfile.DIRTYPE
                tar.addfile(info)
                entries.append((name, 'D', 0))
            else:
                info.size = len(content)
                tar.addfile(info, io.BytesIO(content))
                entries.append((name, 'F', len(content)))
                data += len(content)
    return entries, data


def main():
    out = sys.argv[1]
    os.makedirs(out, exist_ok=True)
    for label, fmt in (('ustar', tarfile.USTAR_FORMAT), ('gnu', tarfile.GNU_FORMAT), ('pax', tarfile.PAX_FORMAT)):
        entries, data = make_tarfile(os.path.join(out, label + '.tar'), fmt)
        open(os.path.join(out, label + '.expected'), 'w').write(expected(entries, data))
    # A symlink: refused by default, skipped on request.
    with tarfile.open(os.path.join(out, 'link.tar'), 'w', format=tarfile.PAX_FORMAT) as tar:
        info = tarfile.TarInfo('pkg/link')
        info.type = tarfile.SYMTYPE
        info.linkname = '../../outside'
        tar.addfile(info)
        info = tarfile.TarInfo('pkg/after.txt')
        info.size = 3
        tar.addfile(info, io.BytesIO(b'ok\n'))
    open(os.path.join(out, 'link.expected'), 'w').write('REFUSED SPECIAL_ENTRY\n')
    open(os.path.join(out, 'link.skip-expected'), 'w').write('S pkg/link\nF pkg/after.txt 3\nEND 3\n')
    # git archive: a pax global header carrying the commit id, then ustar.
    with tempfile.TemporaryDirectory() as repo:
        env = dict(os.environ, GIT_AUTHOR_NAME='t', GIT_AUTHOR_EMAIL='t@example.invalid',
                   GIT_COMMITTER_NAME='t', GIT_COMMITTER_EMAIL='t@example.invalid',
                   GIT_AUTHOR_DATE='2026-01-01T00:00:00Z', GIT_COMMITTER_DATE='2026-01-01T00:00:00Z')
        run = lambda *a: subprocess.run(['git', '-C', repo, *a], check=True, env=env, capture_output=True)
        run('init', '-q')
        os.makedirs(os.path.join(repo, 'src'))
        open(os.path.join(repo, 'README'), 'w').write('readme\n')
        open(os.path.join(repo, 'src', 'main.adb'), 'w').write('procedure Main is begin null; end Main;\n')
        run('add', '.')
        run('-c', 'commit.gpgsign=false', 'commit', '-q', '-m', 'fixture')
        run('archive', '--format=tar', '--prefix=proj/', '-o', os.path.join(out, 'git.tar'), 'HEAD')
    open(os.path.join(out, 'git.expected'), 'w').write(
        'D proj\nF proj/README 7\nD proj/src\nF proj/src/main.adb 40\nEND 47\n')
    # For the unpack checks: a duplicate entry (a write fails late, so
    # what was made must be removed) and a traversal after a good file.
    with tarfile.open(os.path.join(out, 'dup.tar'), 'w', format=tarfile.PAX_FORMAT) as tar:
        for name in ('d/a.txt', 'd/b.txt', 'd/a.txt'):
            info = tarfile.TarInfo(name)
            info.size = 2
            tar.addfile(info, io.BytesIO(b'x\n'))
    with tarfile.open(os.path.join(out, 'traverse.tar'), 'w', format=tarfile.PAX_FORMAT) as tar:
        for name in ('ok.txt', '../evil.txt'):
            info = tarfile.TarInfo(name)
            info.size = 2
            tar.addfile(info, io.BytesIO(b'x\n'))
    # Truncated: the pax archive cut in the middle of big.bin's data.
    raw = open(os.path.join(out, 'pax.tar'), 'rb').read()
    cut = raw.index(bytes(range(256))) + 1024
    open(os.path.join(out, 'cut.tar'), 'wb').write(raw[:cut])
    open(os.path.join(out, 'cut.expected'), 'w').write(
        'D pkg\nF pkg/a.txt 6\nF pkg/empty 0\nF pkg/big.bin 2304\nREFUSED TRUNCATED\n')


if __name__ == '__main__':
    main()
