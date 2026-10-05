"""ACPICA Index-reference lifetime oracle and optional strict CuBit differential checks.

Reference-only mode establishes fixtures; it does not validate CuBit.
Each method runs in a fresh interpreter so mutations cannot leak between cases.
"""
import argparse
import hashlib
import json
from pathlib import Path
import re
import subprocess

# Method-returned Index references must retain their backing object.
EXPECTED = {'RIDX': 3, 'RPKG': 7, 'RGLB': 9, 'RCHN': 3, 'RIWR': 3, 'RSTR': 97, 'RSMU': 99}

KNOWN = 'ACPI Error: 4 (0x4) Outstanding cache allocations (20260408/uttrack-902)'
DIAGNOSTIC = r'^.*(?:ACPI (?:Error|Exception):|AE_[A-Z_]+|Failed at AML Offset).*$'
INTEGER = r'^\s*\[Integer\] = ([0-9A-Fa-f]+)\s*$'


def run(args, log):
    result = subprocess.run(list(map(str, args)), text=True,
                            stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                            timeout=30)
    log.write_text(result.stdout)
    if result.returncode:
        raise RuntimeError(f'Command failed ({result.returncode}); see {log}')
    return result.stdout


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--tools', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    mode = parser.add_mutually_exclusive_group(required=True)
    mode.add_argument('--reference-only', action='store_true')
    mode.add_argument('--runner', type=Path)
    args = parser.parse_args()
    tools = args.tools.resolve()
    output = args.output.resolve()
    output.mkdir(parents=True, exist_ok=True)
    # Never leave a stale success report behind after a failed rerun.
    report_path = output / 'report.json'
    report_path.unlink(missing_ok=True)
    tracked = [Path(__file__).resolve(), tools / 'iasl', tools / 'acpiexec']
    if args.runner:
        args.runner = args.runner.resolve()
        tracked.append(args.runner)
    hashes = {str(path): digest(path) for path in tracked}
    report = {'mode': 'differential' if args.runner else 'reference-only',
              'input_sha256': hashes, 'cases': {}}
    for revision in (1, 2):
        source = output / f'reference-{revision}.asl'
        source.write_text(f'DefinitionBlock("", "DSDT", {revision}, "CUBIT", "REFLIFE", 1) {{\n' + ASL_BODY)
        run([tools / 'iasl', '-oa', source], output / f'compile-{revision}.log')
        aml = source.with_suffix('.aml')
        if bytes([0x88]) not in aml.read_bytes()[36:]:
            raise RuntimeError('Compiled fixture contains no Index opcode')
        control = run([tools / 'acpiexec', '-b', 'execute CTRL', aml],
                      output / f'control-{revision}.log')
        diagnostics = re.findall(DIAGNOSTIC, control, re.M)
        values = re.findall(INTEGER, control, re.M)
        if diagnostics not in ([], [KNOWN]) or len(values) != 1 or int(values[0], 16) != 1:
            raise RuntimeError('Invalid ACPICA control evaluation')
        for method, expected in EXPECTED.items():
            key = f'{revision}-{method}'
            reference = run([tools / 'acpiexec', '-b', 'execute ' + method, aml],
                            output / f'{key}-reference.log')
            values = re.findall(INTEGER, reference, re.M)
            errors = re.findall(DIAGNOSTIC, reference, re.M)
            if errors != diagnostics or len(values) != 1 or int(values[0], 16) != expected:
                raise RuntimeError(f'Unexpected reference behavior: {key}')
            if KNOWN in errors and reference.index(KNOWN) < reference.rfind('[Integer]'):
                raise RuntimeError(f'Baseline diagnostic occurred during evaluation: {key}')
            result = {'reference_integer': expected, 'baseline_diagnostics': errors,
                      'asl_sha256': digest(source), 'aml_sha256': digest(aml)}
            if args.runner:
                actual = run([args.runner, aml, method, '--result-object'],
                             output / f'{key}-actual.log')
                if actual.strip() != f'INTEGER {expected}':
                    raise RuntimeError(f'CuBit mismatch: {key}: {actual!r}')
                result['actual_integer'] = expected
            report['cases'][key] = result
    if hashes != {str(path): digest(path) for path in tracked}:
        raise RuntimeError('Inputs changed during validation')
    report_path.write_text(json.dumps(report, indent=2) + '\n')
    print(f"Index-reference {report['mode']}: {len(report['cases'])} PASS")



ASL_BODY = '\n Name (GBUF, Buffer (2) {1,2})\n Method (CTRL,0) { Return (One) }\n Method (MIDX,0) { Local0 = Buffer(2){3,4} Return (Index(Local0,0)) }\n Method (RIDX,0) { Local0 = MIDX() Return (DerefOf(Local0)) }\n Method (MPKG,0) { Local0 = Package(1){7} Return (Index(Local0,0)) }\n Method (RPKG,0) { Local0 = MPKG() Return (DerefOf(Local0)) }\n Method (MGLB,0) { Return (Index(GBUF,0)) }\n Method (RGLB,0) { Local0 = MGLB() Store(9,Index(GBUF,0)) Return(DerefOf(Local0)) }\n Method (CHRN,0) { Local0=Buffer(32){99} Return(SizeOf(Local0)) }\n Method (RCHN,0) { Local0=MIDX() If(LEqual(CHRN(),32)){Return(DerefOf(Local0))} Return(Zero) }\n Method (WIDX,1) { Store(9,Arg0) }\n Method (RIWR,0) { Local0=MIDX() WIDX(Local0) Return(DerefOf(Local0)) }\n Name (STR0, "abc")\n Method (MSTR,0) { Local0 = "abc" Return (Index(Local0,0)) }\n Method (RSTR,0) { Local0 = MSTR() Return (DerefOf(Local0)) }\n Method (RSMU,0) { Local0 = Index(STR0,0) Store(99,Index(STR0,0)) Return(DerefOf(Local0)) }\n}\n'

if __name__ == '__main__':
    main()
