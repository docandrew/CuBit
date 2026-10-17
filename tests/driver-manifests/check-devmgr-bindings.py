#!/usr/bin/env python3
"""Every startup name devmgr answers for a child is one that child's manifest
declares (CAP-002). devmgr answers by name; a name the manifest does not
declare answers nothing, and a required entry left unanswered fails the
launch (intel-gpu's "ready" vs "device-manager", 2026-10-13). Static: reads
devmgr's source and compiles each child's manifest with ccl-manifest.
Run from the Nix shell after `make -C kernel ccl-manifest`."""
import pathlib
import re
import subprocess
import sys
import tempfile

ROOT = pathlib.Path(__file__).resolve().parents[2]
TOOL = ROOT / 'userspace/ccl/build/manifest/ccl-manifest'
# argv[1]: another devmgr main.adb (a mutation check).
DEVMGR = pathlib.Path(sys.argv[1]) if len(sys.argv) > 1 else ROOT / 'userspace/services/devmgr/main.adb'
CATALOGS = {'.drv': 'driver-services.ccl', '.svc': 'driver-services.ccl',
            '.app': 'native-runtime-services.ccl'}

source = DEVMGR.read_text()
children = dict(re.findall(r'(\w+) := spawnFromBootStorage \("([\w.-]+)"', source))
# DL.Give (child, "name", ...), DL.Give_Placed, Give_Ports, Give_Line, and
# Give_Registers (child, ..., Name => "name").
answers = re.findall(r'\b(?:DL\.Give|DL\.Give_Placed|Give_Ports|Give_Line) \((\w+), "([^"]+)"',
                     source)
answers += re.findall(r'Give_Registers\s*\((\w+),[^;]*?Name => "([^"]+)"', source, re.S)


def manifest_of(binary):
    stem, suffix = binary.rsplit('.', 1)
    for place in ('userspace/services', 'userspace/apps'):
        path = ROOT / place / stem / 'manifest.ccl'
        if path.exists():
            return path, CATALOGS['.' + suffix]
    raise SystemExit(f'no manifest for {binary}')


def declared(binary):
    manifest, catalog = manifest_of(binary)
    with tempfile.TemporaryDirectory(prefix='devmgr-bindings.') as scratch:
        startup = pathlib.Path(scratch) / 'startup.ads'
        result = subprocess.run([TOOL, ROOT / 'userspace/ccl/catalogs' / catalog, manifest,
                                 '--startup-output', startup],
                                capture_output=True, text=True, timeout=10)
        if result.returncode != 0:
            raise SystemExit(f'{manifest}: {result.stderr}')
        return set(re.findall(r'To_Name \("([^"]+)"\)', startup.read_text()))


failures = []
checked = 0
cache = {}
for child, name in answers:
    if child not in children:
        continue  # a helper's formal parameter (child, Name)
    binary = children[child]
    names = cache.setdefault(binary, declared(binary))
    checked += 1
    if name not in names:
        failures.append(f'{binary}: devmgr answers "{name}", manifest declares {sorted(names)}')
if failures:
    print('\n'.join(failures))
    sys.exit(1)
assert checked >= 15, checked
print(f'DEVMGR-BINDINGS: PASS {checked} answers over {len(cache)} manifests')
