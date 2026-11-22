#!/usr/bin/env python3
"""The test console object (Test_Console_ABI) never ships.

Only test builds' logstore may hold it: the test variant of its manifest
(tests/headless/logstore-test/manifest.ccl) declares it, and only test
startup plans approve it, (test-console approve-declared). This check
fails if anything shipped declares, approves or contains it:

- a manifest outside tests/ that names Kernel_Object.Test_Console;
- a shipped startup plan or settings file that says test-console: the
  repository's init.ccl and system.ccl, and every startup and settings
  artifact a system image (images/*.ccl) names through images/artifacts.ccl;
- an executable a system image names whose .cubit.startup or .cubit.caps
  carries the "test-console" binding, or the variant logstore-test.svc
  itself (checked when the artifact is built; --require-built makes a
  missing one a failure).

Positive controls keep it honest: the variant manifest must declare it,
and the detectors must find it in the samples below.
"""
import argparse
import pathlib
import re
import subprocess
import sys
import tempfile

root = pathlib.Path(__file__).resolve().parents[2]
BINDING = b'test-console'
VARIANT = root / 'tests/headless/logstore-test/manifest.ccl'


def code(text):
    """CCL text without its comments (# to the end of a line, outside strings)."""
    out = []
    for line in text.splitlines():
        result, quoted = [], False
        for ch in line:
            if ch == '"':
                quoted = not quoted
            if ch == '#' and not quoted:
                break
            result.append(ch)
        out.append(''.join(result))
    return '\n'.join(out)


def declares(text):
    return re.search(r'Kernel_Object\s*\.\s*Test_Console\b', code(text)) is not None


def approves(text):
    return re.search(r'\(\s*test-console\b', code(text)) is not None


def sections_name_it(path):
    with tempfile.TemporaryDirectory() as scratch:
        for section in ('.cubit.startup', '.cubit.caps'):
            out = pathlib.Path(scratch) / (section.strip('.') + '.bin')
            done = subprocess.run(['objcopy', '--dump-section', f'{section}={out}', str(path),
                                   str(pathlib.Path(scratch) / 'copy')],
                                  capture_output=True)
            if done.returncode == 0 and out.exists() and BINDING in out.read_bytes():
                return True
    return False


def artifacts():
    text = code((root / 'images/artifacts.ccl').read_text())
    return {name: root / path for name, path in
            re.findall(r'\(artifact\s+"([^"]+)"\s+repository\s+"([^"]+)"\)', text)}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--require-built', action='store_true',
                        help='fail when an image names an executable that is not built')
    args = parser.parse_args()
    failures = []

    # Positive controls.
    if not declares(VARIANT.read_text()):
        failures.append(f'{VARIANT.relative_to(root)}: the variant no longer declares the test console')
    if not approves('(start "logstore.svc" (test-console approve-declared))'):
        failures.append('detector: an approval was not found')
    if declares('# Kernel_Object.Test_Console in a comment') or approves('# (test-console approve-declared)'):
        failures.append('detector: a comment was taken for a declaration')

    for manifest in sorted(root.glob('userspace/**/manifest*.ccl')) + sorted(root.glob('images/**/*.ccl')):
        if declares(manifest.read_text()):
            failures.append(f'{manifest.relative_to(root)}: a shipped manifest declares the test console')

    shipped_plans = [root / 'init.ccl', root / 'system.ccl']
    catalog = artifacts()
    executables = {}
    for image in sorted(root.glob('images/*.ccl')):
        text = code(image.read_text())
        if '(system-image' not in text:
            continue
        # A -test twin (images/laptop-build-test.ccl) is the QEMU test of
        # its shipped image (tests/device-builds/run-live.sh checks it
        # differs only in the pairing display and ccl-remote's test
        # variant); the shipped image itself is checked here.
        if image.name.endswith('-test.ccl'):
            twin = image.with_name(image.name[:-len('-test.ccl')] + '.ccl')
            if not twin.exists():
                failures.append(f'{image.relative_to(root)}: a test image without its shipped twin')
            continue
        for kind, name in re.findall(r'\((startup|settings)\s+"([^"]+)"\)', text):
            if name not in catalog:
                failures.append(f'{image.relative_to(root)}: {kind} "{name}" is not in images/artifacts.ccl')
            else:
                shipped_plans.append(catalog[name])
        for name in re.findall(r'\(file\s+\S+\s+"([^"]+)"', text):
            if name in catalog:
                executables.setdefault(catalog[name], image)
    for plan in shipped_plans:
        if plan.exists() and approves(plan.read_text()):
            failures.append(f'{plan.relative_to(root)}: a shipped startup plan approves the test console')

    checked = 0
    for path, image in sorted(executables.items()):
        if path.name in ('logstore-test.svc', 'ccl-remote-test.app'):
            failures.append(f'{image.relative_to(root)}: ships {path.name}')
            continue
        if not path.exists():
            if args.require_built:
                failures.append(f'{image.relative_to(root)}: {path.relative_to(root)} is not built')
            continue
        if path.read_bytes()[:4] != b'\x7fELF':
            continue
        checked += 1
        if sections_name_it(path):
            failures.append(f'{image.relative_to(root)}: {path.relative_to(root)} carries the test console binding')
    variant = root / 'kernel/isodir/boot/logstore-test.svc'
    if variant.exists() and not sections_name_it(variant):
        failures.append('logstore-test.svc: the variant carries no test console binding (detector)')
    remote_variant = root / 'kernel/isodir/boot/ccl-remote-test.app'
    if remote_variant.exists() and not sections_name_it(remote_variant):
        failures.append('ccl-remote-test.app: the variant carries no test console binding (detector)')

    for failure in failures:
        print('test-console: FAIL ' + failure)
    print(f'test-console: {len(shipped_plans)} shipped plans/settings, {checked} shipped executables checked'
          f'{"" if variant.exists() else " (logstore-test.svc not built)"}')
    print('test-console: ' + ('FAIL' if failures else 'PASS'))
    return 1 if failures else 0


if __name__ == '__main__':
    sys.exit(main())
