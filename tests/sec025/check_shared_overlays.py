#!/usr/bin/env python3
"""SEC-025: every Import/Address overlay is reviewed for peer-writable memory.

A peer (another process, a device) can rewrite granted, lent, ring, DMA or
published pages at any time; a decoder that reads one of their words twice
can be steered past its own checks (display.svc's damage record, 054c64a70).
An overlay passes when it is
  * Volatile with Async_Writers (the prover then enforces one read), or
  * annotated: a comment "SEC-025: <class> <why>" on its declaration or in
    the three lines above it (classes: snapshot, single-read, own, device),
    or
  * in the reviewed baseline (tests/sec025/overlay-baseline.txt), keyed by
    file and the overlay's own text (not its line), so moved code passes and
    a new or changed overlay is flagged.
Usage: check_shared_overlays.py [--write-baseline]
"""
import hashlib
import pathlib
import re
import sys

ROOT = pathlib.Path(__file__).resolve().parents[2]
BASELINE = ROOT / 'tests/sec025/overlay-baseline.txt'
SCAN = ['userspace', 'shared']
SKIP_PARTS = {'build', 'obj', 'gnatprove'}
ANNOTATION = re.compile(r'--.*SEC-025:\s*(snapshot|single-read|own|device)\b')
# An object declaration whose aspects name both Import and Address.
DECL = re.compile(r'(?is)\b[A-Za-z_][A-Za-z0-9_]*(?:\s*,\s*[A-Za-z_][A-Za-z0-9_]*)*\s*:\s*'
                  r'(?:aliased\s+)?(?:constant\s+)?[^;:=]*?\bwith\b[^;]*?;')


def strip_comments(text):
    return re.sub(r'--[^\n]*', lambda m: ' ' * len(m.group(0)), text)


def overlays(path):
    raw = path.read_text(errors='replace')
    code = strip_comments(raw)
    for m in DECL.finditer(code):
        decl = m.group(0)
        aspects = decl[decl.lower().index('with'):]
        if not (re.search(r'(?i)\bImport\b', aspects) and re.search(r'(?i)\bAddress\s*=>', aspects)):
            continue
        line = raw.count('\n', 0, m.start()) + 1
        norm = ' '.join(decl.split())
        yield line, norm, raw, m.start(), m.end()


def key(path, norm):
    return hashlib.sha1(f'{path}\0{norm}'.encode()).hexdigest()[:16]


def main():
    write = '--write-baseline' in sys.argv
    baseline = {}
    if BASELINE.exists():
        for row in BASELINE.read_text().splitlines():
            if row and not row.startswith('#'):
                path, digest, _count = row.split('\t')
                baseline[(path, digest)] = int(_count)
    seen, flagged, rows = {}, [], {}
    for top in SCAN:
        for path in sorted((ROOT / top).rglob('*.ad[bs]')):
            if SKIP_PARTS & set(path.relative_to(ROOT).parts):
                continue
            rel = str(path.relative_to(ROOT))
            for line, norm, raw, start, end in overlays(path):
                low = norm.lower()
                if 'volatile' in low and 'async_writers' in low:
                    continue
                before = raw[:start].splitlines()[-3:]
                if any(ANNOTATION.search(l) for l in before) or ANNOTATION.search(raw[start:end]):
                    continue
                k = (rel, key(rel, norm))
                seen[k] = seen.get(k, 0) + 1
                rows[k] = seen[k]
                if seen[k] > baseline.get(k, 0):
                    flagged.append(f'{rel}:{line}: {norm[:110]}')
    if write:
        with BASELINE.open('w') as out:
            out.write('# SEC-025 reviewed overlays (tests/sec025/check_shared_overlays.py):\n'
                      '# file<TAB>digest of file and overlay text<TAB>count. Reviewed against\n'
                      '# /home/doc/cubit-build-tmp/sec025/inventory.md; add new overlays by\n'
                      '# annotation (SEC-025: <class> <why>), not here.\n')
            for (path, digest), count in sorted(rows.items()):
                out.write(f'{path}\t{digest}\t{count}\n')
        print(f'SEC-025-OVERLAYS: baseline written, {sum(rows.values())} overlays')
        return 0
    if flagged:
        for f in flagged:
            print(f'SEC-025-OVERLAYS: unreviewed overlay {f}')
        print(f'SEC-025-OVERLAYS: FAIL {len(flagged)} unreviewed overlays: make them Volatile with '
              'Async_Writers, or annotate "--  SEC-025: <snapshot|single-read|own|device> <why>"')
        return 1
    print(f'SEC-025-OVERLAYS: PASS {sum(seen.values())} baseline overlays, none unreviewed')
    return 0


if __name__ == '__main__':
    sys.exit(main())
