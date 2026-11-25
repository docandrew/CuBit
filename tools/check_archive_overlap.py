# SPDX-License-Identifier: GPL-3.0-or-later
"""Refuse an archive that carries a copy of the runtime (BUG-009).

usage: check_archive_overlap.py --runtime RUNTIME.a ARCHIVE.a...

One allocator, and one copy of every runtime unit, per process: linked from
the runtime (libgnat-user.a), never from another archive the same program
links. An archive linked before the runtime that carries its own copy of a
runtime unit wins the link, and then nothing rebuilds it when the runtime
changes: BUG-009, where the font archive's CuAlloc kept the kernel's old
16 MiB commit step after the kernel refused it, and Desktop died at start.

Fails, naming each member and symbol, when ARCHIVE defines a global symbol
(text, data, bss, read-only data) the runtime also defines. Weak and
undefined symbols do not count: they resolve to the runtime's definition.
"""
import argparse
import collections
import os
import subprocess
import sys

STRONG = set('TDBRG')


def definitions(nm, archive):
    """Global strong symbols an archive defines, and the members defining them."""
    listed = subprocess.run([nm, '-A', '--defined-only', archive], check=True, capture_output=True, text=True).stdout
    defined = collections.defaultdict(set)
    for line in listed.splitlines():
        parts = line.rsplit(' ', 2)
        if len(parts) != 3 or parts[1] not in STRONG:
            continue
        location, symbol = parts[0], parts[2]
        member = location.split(':')[-2] if location.count(':') >= 2 else location
        defined[symbol].add(os.path.basename(member))
    return defined


def main():
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument('--runtime', required=True)
    parser.add_argument('--nm', default=os.path.join(os.environ.get('CUBIT_BINUTILS', '/usr'), 'bin', 'nm'))
    parser.add_argument('archives', nargs='+')
    args = parser.parse_args()
    runtime = definitions(args.nm, args.runtime)
    failed = False
    for archive in args.archives:
        defined = definitions(args.nm, archive)
        overlap = sorted(s for s in defined if s in runtime)
        if overlap:
            failed = True
            members = collections.defaultdict(list)
            for symbol, found in defined.items():
                if symbol in runtime:
                    for member in found:
                        members[member].append(symbol)
            print('check_archive_overlap: %s carries %d of the runtime\'s symbols (%s):'
                  % (archive, len(overlap), args.runtime), file=sys.stderr)
            for member in sorted(members):
                shown = sorted(members[member])
                print('  %s: %s%s' % (member, ', '.join(shown[:4]), ' ...' if len(shown) > 4 else ''), file=sys.stderr)
    if failed:
        sys.exit('check_archive_overlap: one copy of each runtime unit per process, from the runtime (BUG-009)')
    print('check_archive_overlap: %s: no runtime unit (%s)' % (', '.join(args.archives), args.runtime))


if __name__ == '__main__':
    main()
