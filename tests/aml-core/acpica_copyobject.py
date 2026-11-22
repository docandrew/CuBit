"""ACPICA CopyObject oracle and optional strict CuBit differential checks.

Reference-only mode establishes fixtures; it does not validate CuBit.
Each method runs in a fresh interpreter so mutations cannot leak between cases.
"""
import argparse
import hashlib
import json
from pathlib import Path
import re
import subprocess

# ALIA checks that mutating the captured expression result leaves the named
# destination unchanged (ACPICA copies when attaching to a namespace node).
EXPECTED = {
    'TEXT': 2, 'INTG': 1, 'BUFF': 1, 'NEST': 1, 'PKGM': 3,
    'SELF': 3, 'UNIN': 42, 'RETN': 42, 'ARGC': 2, 'STRC': 1,
    'EMPT': 0, 'ZPKG': 3, 'RVAL': 1, 'ALIA': 1,
    'ARGR': 1, 'LOCR': 2, 'INXR': 2,
    'REPT': 1, 'ORIG': 1, 'BASE': 9, 'SRCO': 9,
}
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
        source = output / f'copy-{revision}.asl'
        source.write_text(f'DefinitionBlock("", "DSDT", {revision}, "CUBIT", "COPYOBJ", 1) {{\n' + ASL_BODY)
        run([tools / 'iasl', '-oa', source], output / f'compile-{revision}.log')
        aml = source.with_suffix('.aml')
        if bytes([0x9D]) not in aml.read_bytes()[36:]:
            raise RuntimeError('Compiled fixture contains no CopyObject opcode')
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
    print(f"CopyObject {report['mode']}: {len(report['cases'])} PASS")



ASL_BODY = '\n Name (DST0, 0)\n Name (DSTS, "22")\n Name (SRCB, Buffer (3) {1,2,3})\n Name (RPKG, Package (2) {SRCB, SRCB})\n Name (SRCP, Package (2) {Buffer (2) {1,2}, Package (1) {3}})\n Method (CTRL,0) { Return (One) }\n Method (TEXT,0) { CopyObject("hello",DST0) Return(ObjectType(DST0)) }\n Method (INTG,0) { CopyObject(42,DSTS) Return(ObjectType(DSTS)) }\n Method (BUFF,0) { CopyObject(SRCB,DST0) Store(9,Index(SRCB,0)) Return(DerefOf(Index(DST0,0))) }\n Method (NEST,0) { CopyObject(SRCP,DST0) Store(9,Index(DerefOf(Index(SRCP,0)),0)) Local1=DerefOf(Index(DST0,0)) Return(DerefOf(Index(Local1,0))) }\n Method (PKGM,0) { CopyObject(SRCP,DST0) Store(9,Index(DerefOf(Index(SRCP,1)),0)) Local1=DerefOf(Index(DST0,1)) Return(DerefOf(Index(Local1,0))) }\n Method (SELF,0) { CopyObject(SRCB,SRCB) Return(SizeOf(SRCB)) }\n Method (UNIN,0) { CopyObject(42,Local0) Return(Local0) }\n Method (RETN,0) { Return(CopyObject(42,Local0)) }\n Method (ARGT,1) { CopyObject(42,Arg0) Return(Arg0) }\n Method (ARGC,0) { Local0="old" ARGT(Local0) Return(ObjectType(Local0)) }\n Method (STRC,0) { CopyObject(DSTS,Local0) Store("new",DSTS) If(LEqual(Local0,"22")) {Return(One)} Return(Zero) }\n\n Method (EMPT,0) { CopyObject(Buffer(0){},DST0) Return(SizeOf(DST0)) }\n Method (ZPKG,0) { CopyObject(Package(3){One},DST0) Return(SizeOf(DST0)) }\n Method (RVAL,0) { Local0=CopyObject(SRCB,DST0) Store(9,Index(SRCB,0)) Return(DerefOf(Index(Local0,0))) }\n Method (ALIA,0) { Local0=CopyObject(SRCB,DST0) Store(9,Index(Local0,0)) Return(DerefOf(Index(DST0,0))) }\n\n Method (ARGR,0) { ARGT(RefOf(DSTS)) Return(ObjectType(DSTS)) }\n Method (LOCR,0) { Local0=RefOf(DSTS) CopyObject(42,Local0) Return(ObjectType(DSTS)) }\n Method (INXR,0) { Local0=Package(1){"old"} ARGT(Index(Local0,0)) Return(ObjectType(DerefOf(Index(Local0,0)))) }\n Method (REPT,0) {\n  CopyObject(RPKG,DST0)\n  Store(9,Index(DerefOf(Index(DST0,0)),0))\n  Local1 = DerefOf(Index(DST0,1))\n  Return(DerefOf(Index(Local1,0)))\n }\n Method (ORIG,0) {\n  CopyObject(RPKG,DST0)\n  Store(9,Index(DerefOf(Index(DST0,0)),0))\n  Return(DerefOf(Index(SRCB,0)))\n }\n Method (BASE,0) {\n Store(9,Index(DerefOf(Index(RPKG,0)),0))\n Local1 = DerefOf(Index(RPKG,1))\n Return(DerefOf(Index(Local1,0)))\n }\n Method (SRCO,0) {\n Store(9,Index(DerefOf(Index(RPKG,0)),0))\n Return(DerefOf(Index(SRCB,0)))\n }\n}\n'

if __name__ == '__main__':
    main()
