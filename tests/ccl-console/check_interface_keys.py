#!/usr/bin/env python3
"""CCL interfaces whose types are CCL source (TYPE_SOURCE) carry schema keys
precomputed from it: SHA-256 of the source, and of the source, "#" and each
bound type's name. A change to the source without new keys fails here."""
import hashlib
import pathlib
import re
import sys

ROOT = pathlib.Path(__file__).resolve().parents[2]
SRC = ROOT / 'userspace/ccl/src'
CHECKS = [
    (SRC / 'ccl-interfaces-processes.ads',
     {'DIGEST': None, 'PROCESS_KEY': 'Process', 'PROCESSES_KEY': 'List-Process'}),
    (SRC / 'ccl-interfaces-files.ads',
     {'DIGEST': None, 'KIND_KEY': 'File_Kind', 'PLACE_KEY': 'Place', 'CHILD_KEY': 'Child',
      'METADATA_KEY': 'File_Metadata', 'LISTING_KEY': 'List-File_Metadata'}),
    (SRC / 'ccl-interfaces-console.ads',
     {'DIGEST': None, 'NOTATION_KEY': 'Notation', 'THEME_KEY': 'Theme', 'STATS_KEY': 'Console_Stats'}),
    (SRC / 'ccl-interfaces-images.ads',
     {'DIGEST': None, 'IMAGE_KEY': 'Image', 'SIZE_KEY': 'Size', 'SERIES_KEY': 'List-Integer',
      'GRID_KEY': 'Grid', 'IMAGES_KEY': 'List-Image', 'SCALED_KEY': 'Scaled'}),
    (SRC / 'ccl-interfaces-logs.ads',
     {'DIGEST': None, 'SCHEMA_KEY': 'List-LogEntry', 'SEVERITY_KEY': 'Severity'})]


def constant(text, name):
    body = re.search(name + r' : constant [\w.]+ :=\s*\[([^\]]*)\]', text).group(1)
    return ''.join(re.findall(r'16#([0-9A-F_]+)#', body)).replace('_', '').lower()


def main():
    failures = 0
    for path, keys in CHECKS:
        text = path.read_text()
        source = re.search(r'TYPE_SOURCE : constant String :=\s*(.*?);\n', text, re.S).group(1)
        source = ''.join(part.replace('""', '"') for part in re.findall(r'"((?:[^"]|"")*)"', source))
        for name, type_name in keys.items():
            expected = hashlib.sha256(source.encode() + (b'#' + type_name.encode() if type_name else b'')).hexdigest()
            if constant(text, name) != expected:
                print('FAIL: %s %s is not SHA-256 of its TYPE_SOURCE' % (path.name, name))
                failures += 1
    if not failures:
        print('PASS: interface schema keys match their CCL type source')
    return 1 if failures else 0


if __name__ == '__main__':
    sys.exit(main())
