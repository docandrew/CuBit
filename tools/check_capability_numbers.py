#!/usr/bin/env python3
"""CAP-002 build check: no capability is ever named by a number.

Programs name capabilities only through handles (CuBit.Handles in Ada,
struct cubit_endpoint in C, cubit::Endpoint in Rust) obtained from the
startup table, an accepted message or an offer. The type system already
refuses an integer where a handle is expected; this check refuses the ways
around it and the retired slot vocabulary, and runs first in `make world`.

    check_capability_numbers.py [--root ROOT] [--self-test]

Exit status 0 when clean; 1 with one line per finding otherwise.
"""
import argparse
import pathlib
import re
import subprocess
import sys

# Retired slot vocabulary: any use is a finding.
RETIRED = [
    (re.compile(r'\bCapabilitySlot\b'), 'CapabilitySlot (integer capability slot type)'),
    (re.compile(r'\bCAP_SLOT_[A-Z_]+'), 'CAP_SLOT_* (fixed capability slot number)'),
    (re.compile(r'\bREPLY_CAP_SLOT\b'), 'REPLY_CAP_SLOT (the reply is thread state now)'),
    (re.compile(r'\bCCL_Manifest_Bindings\b'), 'CCL_Manifest_Bindings (generated slot numbers)'),
    (re.compile(r'\bsaveReplyCap\s*\(\s*[0-9A-Z_]'), 'saveReplyCap (slot) (use CuBit.Handles.IPC.Save_Reply)'),
    (re.compile(r'\brestoreReplyCap\b'), 'restoreReplyCap (RESTORE is retired)'),
    (re.compile(r'\bFind_Endpoint_Capability\b'), 'Find_Endpoint_Capability (no table scans)'),
    (re.compile(r'\bSYSCALL_POLICY_MINT_CAPABILITY\b'), 'POLICY_MINT_CAPABILITY (retired)'),
    (re.compile(r'\bSYSCALL_INSPECT_CAPABILITY\b'), 'raw INSPECT_CAPABILITY (use Describe)'),
    (re.compile(r'\bEndpointSlot\b'), 'EndpointSlot (Rust slot reference)'),
    (re.compile(r'\bpub const SLOT_[A-Z_]+\b'), 'Rust SLOT_* binding'),
]

# Handles may be built from words only inside the runtime's handle units,
# the libc's startup reader and the C ABI conversions.
FORGERY = [
    (re.compile(r'Unchecked_Conversion\s*\([^)]*CuBit\.Handles\.', re.S),
     'Unchecked_Conversion to or from a handle type'),
    (re.compile(r'\.opaque\s*=[^=]'), 'C code writing a handle word'),
    (re.compile(r'\(\s*cubit_endpoint\s*\)\s*\{'), 'C code building a handle from a word'),
]

# A literal system call number outside the glue that defines them.
LITERAL_SYSCALL = re.compile(r'\bsyscall\s*\(\s*[0-9]', re.I)

# Catalog declarations that laid out slots.
CATALOG_RETIRED = re.compile(r'\((application-slots|fixed-binding)\b')

SOURCE_SUFFIXES = {'.ads', '.adb', '.c', '.h', '.rs', '.ccl', '.S'}
# Patches are applied to sources, so the retired vocabulary is refused in
# them too.
PATCH_SUFFIXES = {'.patch', '.diff'}
SKIP_PARTS = {'build', '.git', 'target', 'node_modules', '__pycache__', 'runtime-src'}

# Paths (relative to the root, as posix prefixes) allowed to hold handle
# words: the runtime's handle units, the libc's startup table reader and the
# C ABI headers. The kernel side is the authority and is not checked here.
GLUE = (
    'userspace/runtime/gnat/cubit-handles',
    'userspace/runtime/gnat/cubit-kernel_calls',
    'userspace/runtime/gnat/cubit-messages',
    'userspace/libc/ada/cubit-libc_startup',
    'userspace/c/cubit_handles.h',
    'userspace/mesa/anv/cubit_handles.h',
    'userspace/rust/cubit/src/',
    'userspace/rust/std/',
    'tests/support/handles/',
)
# Recorded evidence: patches kept as the record of an earlier experiment,
# named one by one (never a pattern), each with why it may keep the old
# vocabulary. They are never applied to this tree.
RECORDED_EVIDENCE = {
    # The compositor startup diagnostics, measured on the pre-handle tree
    # with review.json and evidence/ beside it.
    'tests/compositor/startup-diagnostics/diagnostic.patch',
}

# Trees outside the check: the kernel (it defines capabilities), imported
# third-party sources, and fixture data.
EXCLUDED = (
    'kernel/',
    'shared/',
    'tools/check_capability_numbers.py',
    'docs/',
    'userspace/ports/doom/src',
    'userspace/ports/sameboy/src',
)


def relevant(path, root):
    rel = path.relative_to(root).as_posix()
    if path.suffix not in SOURCE_SUFFIXES | PATCH_SUFFIXES or SKIP_PARTS & set(path.parts):
        return None
    if rel.startswith(EXCLUDED) or rel in RECORDED_EVIDENCE:
        return None
    return rel


def strip_comments(text, suffix):
    if suffix in {'.ads', '.adb'}:
        return re.sub(r'--[^\n]*', '', text)
    if suffix == '.ccl':
        return re.sub(r'#[^\n]*', '', text)
    text = re.sub(r'/\*.*?\*/', '', text, flags=re.S)
    return re.sub(r'//[^\n]*', '', text)


# A security test that forges a word on purpose says so on that line.
FORGED_ON_PURPOSE = 'capability-check: forged on purpose'


def hosted_test(rel):
    """Hosted tests run on Linux against mocks; only tests that run on
    CuBit (their paths name native fixtures or apps) hold real handles."""
    return rel.startswith('tests/') and '/native' not in rel


def check_text(rel, text, suffix):
    findings = []
    if suffix in PATCH_SUFFIXES:
        # Only the retired vocabulary: a patch mixes languages.
        for pattern, what in RETIRED:
            for match in pattern.finditer(text):
                findings.append((rel, text.count('\n', 0, match.start()) + 1, what))
        return findings
    exempt = {number for number, line in enumerate(text.splitlines(), 1)
              if FORGED_ON_PURPOSE in line}
    code = strip_comments(text, suffix)
    glue = rel.startswith(GLUE) or hosted_test(rel)
    for pattern, what in RETIRED:
        for match in pattern.finditer(code):
            findings.append((rel, code.count('\n', 0, match.start()) + 1, what))
    if not glue:
        for pattern, what in FORGERY:
            for match in pattern.finditer(code):
                findings.append((rel, code.count('\n', 0, match.start()) + 1, what))
        if suffix in {'.ads', '.adb'}:
            for match in LITERAL_SYSCALL.finditer(code):
                findings.append((rel, code.count('\n', 0, match.start()) + 1,
                                 'literal system call number (name it)'))
    if suffix == '.ccl':
        for match in CATALOG_RETIRED.finditer(code):
            findings.append((rel, code.count('\n', 0, match.start()) + 1,
                             f'catalog {match.group(1)} (slots are retired)'))
    return [f for f in findings if f[1] not in exempt]


def candidates(root):
    """Tracked and unignored files in a checkout; every file elsewhere.
    Walking a checkout would also read ignored build and port trees."""
    try:
        out = subprocess.run(
            ['git', '-C', str(root), 'ls-files', '-z', '--cached', '--others',
             '--exclude-standard'],
            check=True, capture_output=True).stdout
    except (OSError, subprocess.CalledProcessError):
        return sorted(root.rglob('*'))
    return sorted(root / name for name in out.decode(errors='replace').split('\0') if name)


def scan(root):
    findings = []
    # An exemption names a file that exists: none outlives its evidence.
    for rel in sorted(RECORDED_EVIDENCE):
        if not (root / rel).is_file():
            findings.append((rel, 0, 'recorded-evidence exemption names no file'))
    for path in candidates(root):
        if not path.is_file():
            continue
        rel = relevant(path, root)
        if rel is None:
            continue
        try:
            text = path.read_text(errors='replace')
        except OSError:
            continue
        findings.extend(check_text(rel, text, path.suffix))
    return findings


def self_test():
    bad = [
        ('tests/x/fix.patch', '+   X := capCall (CAP_SLOT_DISPLAY, M, Wait_Forever);', '.patch'),
        ('userspace/apps/x/main.adb', 'Ignore := capCall (CapabilitySlot (15), M, Wait_Forever);', '.adb'),
        ('userspace/apps/x/main.adb', 'Ignore := syscall (128, 15, M\'Address);', '.adb'),
        ('userspace/apps/x/main.adb',
         'function To is new Ada.Unchecked_Conversion (Unsigned_64, CuBit.Handles.Endpoint);', '.adb'),
        ('userspace/apps/x/main.c', 'cubit_endpoint e; e.opaque = 15;', '.c'),
        ('userspace/apps/x/main.c', 'return (cubit_endpoint){15};', '.c'),
        ('userspace/ccl/catalogs/x.ccl', '(service-catalog v1 (application-slots 24 62))', '.ccl'),
        ('userspace/apps/x/lib.rs', 'pub const SLOT_CLOCK : u64 = 25;', '.rs'),
    ]
    good = [
        ('userspace/apps/x/main.adb', 'Tag := capCall (Startup.Clock, M, Wait_Forever);', '.adb'),
        ('userspace/apps/x/main.adb', '--  Old CapabilitySlot comments are fine.', '.adb'),
        ('userspace/runtime/gnat/cubit-handles-kernel.ads',
         'function To_Endpoint (W : Unsigned_64) return Endpoint is ((Word => W));', '.ads'),
        ('userspace/apps/x/main.adb', 'Now := syscall (SYSCALL_GETTIME);', '.adb'),
        ('userspace/apps/x/main.adb',
         'R := syscall (128, 16#DEAD#, 0);  -- capability-check: forged on purpose', '.adb'),
        ('tests/x/hosted_test.c', 'cubit_endpoint e = (cubit_endpoint){15};', '.c'),
    ]
    failures = 0
    for rel, text, suffix in bad:
        if not check_text(rel, text, suffix):
            print(f'self-test: not caught: {text}')
            failures += 1
    for rel, text, suffix in good:
        if check_text(rel, text, suffix):
            print(f'self-test: false finding: {text}')
            failures += 1
    print(f'self-test: {len(bad)} rejected, {len(good)} accepted, {failures} wrong')
    return failures == 0


def main():
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument('--root', type=pathlib.Path,
                        default=pathlib.Path(__file__).resolve().parents[1])
    parser.add_argument('--self-test', action='store_true')
    args = parser.parse_args()
    if args.self_test:
        return 0 if self_test() else 1
    findings = scan(args.root.resolve())
    for rel, line, what in findings:
        print(f'{rel}:{line}: {what}')
    if findings:
        print(f'check_capability_numbers: {len(findings)} finding(s): '
              'capabilities are named by handles, never numbers (docs/capability-handles.md)')
        return 1
    print('check_capability_numbers: clean')
    return 0


if __name__ == '__main__':
    sys.exit(main())
