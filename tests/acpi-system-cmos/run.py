#!/usr/bin/env python3
"""Pinned hosted CMOS gate. Hardware and capability syscalls are modeled."""
from pathlib import Path
import argparse, hashlib, json, os, re, shutil, subprocess, tempfile

suite = Path(__file__).resolve().parent
root = suite.parents[1]
parser = argparse.ArgumentParser()
parser.add_argument('--mode', choices=['checked', 'release'], default='release')
parser.add_argument('--validate-only', action='store_true')
args = parser.parse_args()
sha = lambda p: hashlib.sha256(p.read_bytes()).hexdigest()
catalog = json.loads((suite / 'sources.json').read_text())
pins = {str(suite / 'sources.json'): sha(suite / 'sources.json')}
for row in catalog['inputs']:
    rel = Path(row['path'])
    assert not rel.is_absolute() and '..' not in rel.parts
    p = root / rel
    assert p.is_file() and not p.is_symlink() and sha(p) == row['sha256'], p
    pins[str(p)] = sha(p)
tools = {}
for name, row in catalog['tools'].items():
    p = Path(row['path']); assert p.is_file() and sha(p) == row['sha256'], p
    tools[name] = p; pins[str(p)] = sha(p)
if args.validate_only:
    print(json.dumps(dict(validated=True, inputs=len(catalog['inputs'])))); raise SystemExit
assert os.environ.get('TMPDIR') == '/home/doc/cubit-build-tmp', 'Export TMPDIR before Nix'
out = Path(tempfile.mkdtemp(prefix='acpi-system-cmos-', dir=os.environ['TMPDIR']))
runs = []; references = []; negatives = []
def verify():
    for p, h in pins.items(): assert sha(Path(p)) == h, p

def save(done):
    cases = [r for r in runs if r.get('test')]
    (out / 'RESULT.json').write_text(json.dumps(dict(
        completed=done, profile=args.mode, pins=pins, runs=runs,
        passed=sum(r['passed'] for r in cases), total=39,
        percent=100 * sum(r['passed'] for r in cases) / 39,
        checks=sum(r.get('checks', 0) for r in cases),
        acpica_references=references, iasl_negative=negatives,
        hardware=False, native_executed=False,
        scope='Actual pure policies, serialized port template, provider, mux, Core/Field_IO and Boot_RTC bodies; modeled port/capability/RTC/FACS callbacks. ACPICA simulated SystemMemory value reference only. No whole-service or hardware proof.'), indent=2) + '\n')

def run(name, argv, seconds, test=False, rejection=False):
    verify()
    argv = list(map(str, argv))
    x = subprocess.run(['timeout', '--kill-after=2s', str(seconds) + 's', 'prlimit',
                        '--as=25769803776', '--stack=134217728', *argv],
                       cwd=out, capture_output=True, text=True, timeout=seconds + 3)
    text = x.stdout + x.stderr; log = out / (name + '.log'); log.write_text(text)
    passed = (x.returncode != 0 and x.returncode not in [124, 137] and '6100' in text) if rejection else x.returncode == 0 and (not test or 'PASS' in text)
    match = re.search(r'checks\s+(\d+)', text, re.I)
    row = dict(name=name, argv=argv, status=x.returncode, passed=passed, test=test,
               log=str(log), checks=int(match[1]) if match else 0)
    runs.append(row); save(False)
    assert passed, log
    if test: print(name, 'PASS', flush=True)
    return text

common = out / 'common'; rtc = out / 'rtc'; common.mkdir(); rtc.mkdir()
for row in catalog['inputs']:
    p = root / row['path']
    if p.suffix not in ['.ads', '.adb']: continue
    destinations = [rtc] if row['lane'] == 'rtc' else [common]
    if row['lane'] == 'tests' and p.name == 'cmos_boot_rtc_tests.adb': destinations = [rtc]
    for directory in destinations:
        target = directory / p.name
        assert not target.exists() or sha(target) == sha(p), target
        shutil.copy2(p, target); pins[str(target)] = sha(target)

flags = '"-gnata","-gnato"' if args.mode == 'checked' else '"-gnatp"'
mains = ['cmos_indices_tests', 'cmos_transaction_tests', 'cmos_provider_tests',
         'cmos_port_tests', 'cmos_boot_policy_tests', 'cmos_mux_tests', 'cmos_aml_tests']
for lane, names in [('common', mains), ('rtc', ['rtc_original_tests', 'cmos_boot_rtc_tests'])]:
    project = out / (lane + '.gpr')
    naming = 'package Naming is for Body ("ACPI_Target_Heap") use "acpi_target_heap_host.adb";end Naming;' if lane == 'common' else ''
    # The existing mux fixture has redundant/unused with/use clauses. Retain
    # every other warning and warnings-as-errors in this hosted gate.
    project.write_text('project ' + lane + ' is\nfor Source_Dirs use ("' + lane + '");\nfor Object_Dir use "' + lane + '-obj";\nfor Exec_Dir use "' + lane + '-bin";\nfor Main use (' + ','.join('"' + n + '.adb"' for n in names) + ');\n' + naming + '\npackage Compiler is for Default_Switches ("Ada") use ("-O2","-gnat2022","-gnatwa","-gnatwe","-gnatwU","-gnatwR",' + flags + ');end Compiler;\nend ' + lane + ';\n')
    pins[str(project)] = sha(project)
    run('build-' + lane, [tools['gprbuild'], '-P', project, '-j2'], 240)
    for p in (out / (lane + '-bin')).iterdir(): pins[str(p)] = sha(p)
for main in mains[:5]: run(main, [out / 'common-bin' / main], 30, test=True)
for mode in ['normal', 'no-pool', 'ambiguous', 'unknown', 'reentry', 'no-cmos', 'cmos-unknown', 'cmos-reentry']:
    run('mux-' + mode, [out / 'common-bin/cmos_mux_tests', mode], 30, test=True)
for main in ['rtc_original_tests', 'cmos_boot_rtc_tests']:
    run(main, [out / 'rtc-bin' / main], 30, test=True)

original = (suite / 'cmos-fields.asl').read_text()
for width in [32, 64]:
    name = 'w' + str(width); text = original.replace('"DSDT", 2', '"DSDT", ' + ('1' if width == 32 else '2'))
    source = out / (name + '.asl'); source.write_text(text); pins[str(source)] = sha(source)
    run('iasl-' + name, [tools['iasl'], '-p', out / name, source], 10)
    table = out / (name + '.aml'); pins[str(table)] = sha(table)
    for mode in ['read', 'write', 'protected', 'century', 'cmos-unknown', 'no-cmos', 'lock-denied']:
        run(name + '-' + mode, [out / 'common-bin/cmos_aml_tests', table, mode], 30, test=True)
    reference = out / (name + '-reference.asl')
    reference.write_text(text.replace('SystemCMOS', 'SystemMemory').replace(' Method (RDTM', ' Method (INIT, 0) { Store (0x23, HOR) Store (0x23, RWB0) }\n Method (RDTM', 1)); pins[str(reference)] = sha(reference)
    run('iasl-reference-' + name, [tools['iasl'], '-p', out / (name + '-reference'), reference], 10)
    ref_aml = out / (name + '-reference.aml'); pins[str(ref_aml)] = sha(ref_aml)
    result = run('acpica-' + name, [tools['acpiexec'], '-b', 'execute INIT;execute RDTM;execute WRT0 0xA5', ref_aml], 8)
    match = '0000000000000023' in result and '00000000000000A5' in result
    references.append(dict(width=width, passed=match, scope='Simulated SystemMemory value semantics; no physical CMOS equivalence.'))
    save(False); assert match
    for code, access in [(0, 'AnyAcc'), (2, 'WordAcc'), (3, 'DWordAcc'), (4, 'QWordAcc'), (5, 'BufferAcc')]:
        bad = out / (name + '-' + access + '.asl'); bad.write_text(text.replace('ByteAcc', access)); pins[str(bad)] = sha(bad)
        run('iasl-reject-' + name + '-' + access, [tools['iasl'], '-p', out / bad.stem, bad], 10, rejection=True)
        negatives.append(dict(width=width, access=access, passed=True, diagnostic=6100))
        # Explicit invalid-field probe. Never describe this as accepted ASL.
        data = bytearray(table.read_bytes()); pattern = b'CMOS\x11'; assert data.count(pattern) == 1
        offset = data.index(pattern) + 4; data[offset] = 0x10 | code; data[9] = 0; data[9] = (-sum(data)) & 255
        assert sum(data) % 256 == 0
        invalid = out / (bad.stem + '-invalid.aml'); invalid.write_bytes(data); pins[str(invalid)] = sha(invalid)
        (out / (bad.stem + '-patch.json')).write_text(json.dumps(dict(original_sha256=sha(table), result_sha256=sha(invalid), flag_offset=offset, before=17, after=16 | code, checksum_offset=9), indent=2) + '\n')
        run(name + '-reject-' + access, [out / 'common-bin/cmos_aml_tests', invalid, 'bad-access'], 30, test=True)
verify(); save(True)
print(out / 'RESULT.json')
