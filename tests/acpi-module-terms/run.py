"""Hosted module-TermList regressions, with original per-case 12s limits."""
from pathlib import Path
import argparse
import hashlib
import json
import re
import subprocess
import sys
import time

sys.path.insert(0, str(Path(__file__).parent))
from generate import generate
from clock_policy import without_clock

ROOT = Path(__file__).resolve().parents[2]
sha = lambda p: hashlib.sha256(Path(p).read_bytes()).hexdigest()


def build_runner(output, profile, no_clock=False):
    catalog = json.loads((ROOT / 'tests/acpi-aslts-corpus/sources.json').read_text())
    directories = [ROOT / 'tests/acpi-aslts-corpus'] + [ROOT / name for name in catalog['source_dirs']]
    directories = list(dict.fromkeys(directories))
    folder = output / 'build'
    folder.mkdir()
    main = 'aslts_corpus_runner.adb'
    if no_clock:
        derived = folder / 'source'
        derived.mkdir()
        main = 'module_no_clock_runner.adb'
        (derived / main).write_text(without_clock((ROOT / 'tests/acpi-aslts-corpus/aslts_corpus_runner.adb').read_text()))
        directories.insert(0, derived)
    flags = ['-O3', '-gnatp'] if profile == 'shipping' else ['-O1', '-gnata', '-gnato']
    flags += ['-gnat2022', '-gnatwe', '-g']
    arguments = ', '.join(json.dumps(flag) for flag in flags)
    project = folder / 'module_runner.gpr'
    project.write_text('project Module_Runner is\n for Source_Dirs use (' +
        ', '.join(json.dumps(str(path)) for path in directories) + ');\n'
        ' for Object_Dir use "obj"; for Exec_Dir use "bin";\n'
        ' for Main use (' + json.dumps(main) + ');\n package Compiler is\n'
        ' for Default_Switches ("Ada") use (' + arguments + ');\n'
        ' for Switches (' + json.dumps(main) + ') use (' + arguments + ', "-gnatwa");\n'
        ' end Compiler;\nend Module_Runner;\n')
    sources = {str(path): sha(path) for directory in directories for path in directory.iterdir()
               if path.is_file() and path.suffix in ['.ads', '.adb']}
    argv = ['timeout', '--kill-after=2s', '240s', 'gprbuild', '-q', '-p', '-j2', '-P', str(project)]
    process = subprocess.run(argv, capture_output=True, text=True, timeout=245)
    (folder / 'build.log').write_text(process.stdout + process.stderr)
    assert process.returncode == 0 and 'warning:' not in (process.stdout + process.stderr).lower(), str(folder / 'build.log')
    for path, expected in sources.items():
        assert sha(path) == expected, path
    (folder / 'INPUTS.json').write_text(json.dumps(sources, indent=2) + '\n')
    return folder / 'bin' / Path(main).stem


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--output', type=Path, required=True)
    parser.add_argument('--fixtures', type=Path)
    parser.add_argument('--runner', type=Path)
    parser.add_argument('--iasl', type=Path)
    parser.add_argument('--acpiexec', type=Path)
    parser.add_argument('--no-clock', action='store_true',
                        help='Use the unavailable-clock default; Timer must be refused')
    parser.add_argument('--reference', action='store_true')
    parser.add_argument('--baseline-observation', action='store_true',
                        help='Record expected pre-fix failures; never reports the suite passed')
    parser.add_argument('--profile', choices=['shipping', 'checked'], default='checked')
    parser.add_argument('--start', type=int, default=0)
    parser.add_argument('--limit', type=int)
    parser.add_argument('--only', nargs='+')
    args = parser.parse_args()
    assert not (args.reference and args.no_clock), 'No-clock is a CuBit host-policy control'
    output = args.output.resolve()
    output.mkdir(parents=True, exist_ok=False)
    catalog = json.loads((ROOT / 'tests/acpi-aslts-corpus/sources.json').read_text())
    def tool(name, explicit):
        record = catalog['tools'][name]
        path = explicit.resolve() if explicit else Path(record['path'])
        assert path.is_file(), (name, str(path), 'Use the pinned Nix development environment')
        if not explicit:
            assert sha(path) == record['sha256'], (name, 'catalog tool hash')
        return path
    if args.fixtures:
        cases = json.loads((args.fixtures / 'FIXTURES.json').read_text())['cases']
    else:
        cases = generate(output / 'fixtures', tool('iasl', args.iasl))
    if args.reference:
        cases = [case for case in cases if case['expected'] == 'PASS']
        runner = tool('acpiexec', args.acpiexec)
    else:
        runner = args.runner.resolve() if args.runner else build_runner(output, args.profile, args.no_clock)
    if args.only:
        cases = [case for case in cases if case['name'] in args.only]
    runner_hash = sha(runner)
    cases = cases[args.start:args.start + args.limit if args.limit is not None else None]
    rows = []
    for case in cases:
        assert sha(case['aml']) == case['aml_sha256'], case['name']
        if args.reference:
            executable = [str(runner), '-b', 'execute MN00; execute ERRS; execute RMRC', case['aml']]
        else:
            executable = [str(runner), case['aml']]
        argv = ['timeout', '--kill-after=2s', '12s', 'prlimit', '--as=25769803776',
                '--stack=134217728', *executable]
        began = time.monotonic()
        process = subprocess.run(argv, cwd=output, capture_output=True, text=True, timeout=17)
        log = output / (case['name'] + '.log')
        log.write_text(process.stdout + process.stderr)
        if args.reference:
            values = [int(value, 16) for value in re.findall(r'\[Integer\]\s*=\s*([0-9A-Fa-f]+)', log.read_text())]
            prefix = log.read_text().split('Evaluating ', 1)[0]
            load_errors = [line for line in prefix.splitlines()
                           if line.startswith(('ACPI Error:', 'Firmware Error'))]
            result = dict(report=values, namespace_load_errors=load_errors)
            status = 'PASS' if values == [0, 0, 1] and not load_errors and 'failed with status' not in log.read_text() else 'REFERENCE_REFUSED_OR_MISMATCH'
            matched = process.returncode == 0 and status == 'PASS'
        else:
            records = [json.loads(line[len('ASLTS_RESULT '):]) for line in log.read_text().splitlines()
                       if line.startswith('ASLTS_RESULT ')]
            result = records[-1] if len(records) == 1 else None
            status = 'TIMEOUT' if process.returncode in [124, 137] else result['status'] if result else 'NO_RESULT'
            expected = 'UNSUPPORTED' if args.no_clock and case['name'] == 'timer' else case['expected']
            execution = ['UNSUPPORTED'] if args.no_clock and case['name'] == 'timer' else case['execution']
            matched = process.returncode == 0 and status == expected
            if matched and execution:
                matched = result['execution'] in execution
        row = dict(case=case['name'], expected=('UNSUPPORTED' if args.no_clock and case['name'] == 'timer' else case['expected']), actual=status, matched=matched,
            result=result, argv=argv, exit=process.returncode, seconds=time.monotonic() - began,
            log=str(log), log_sha256=sha(log), aml_sha256=case['aml_sha256'])
        rows.append(row)
        (output / 'PARTIAL.json').write_text(json.dumps(dict(terminal=False, cases=rows), indent=2) + '\n')
        print(case['name'], status, 'matched', matched, flush=True)
        assert sha(runner) == runner_hash
    passed = sum(row['matched'] for row in rows)
    result = dict(terminal=True, cases=rows, passed=passed, executed=len(rows),
        percent_expected=100 * passed / len(rows) if rows else None,
        baseline_observation=args.baseline_observation,
        clock_provider='unavailable default' if args.no_clock else 'existing bounded owner clock',
        suite_passed=bool(rows) and passed == len(rows) and not args.baseline_observation,
        runner=str(runner), runner_sha256=runner_hash,
        scope='Own synthetic fixtures only; reference covers positives, strict malformed/quota refusals counted separately. No original ASLTS denominator, firmware fixture, timeout/fuel/quota change or production proof claim.')
    (output / 'RESULT.json').write_text(json.dumps(result, indent=2) + '\n')
    print('OBSERVATION' if args.baseline_observation else 'RESULT', passed, '/', len(rows), result['percent_expected'], flush=True)
    return 0 if args.baseline_observation or result['suite_passed'] else 1


if __name__ == '__main__':
    raise SystemExit(main())
