"""Check module missing-name cursor/effects and called-method error boundaries."""
from pathlib import Path
import argparse
import hashlib
import json
import re
import shutil
import subprocess
import time
from generate import generate

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[1]
SEEDS = ROOT/'tests/acpi-module-name-recovery'
sha = lambda p: hashlib.sha256(Path(p).read_bytes()).hexdigest()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--output', type=Path, required=True)
    parser.add_argument('--profile', choices=['shipping', 'checked'], required=True)
    parser.add_argument('--fixtures', type=Path, help='Reuse an exact own COMPILED.json seed set')
    parser.add_argument('--reference', action='store_true', help='Also run the 13 own tables through ACPICA')
    args = parser.parse_args()
    output = args.output.resolve()
    output.mkdir(parents=True, exist_ok=False)
    catalog_path = ROOT/'tests/acpi-aslts-corpus/sources.json'
    catalog = json.loads(catalog_path.read_text())
    spec = json.loads((HERE/'cases.json').read_text())
    inputs = {str(p): sha(p) for p in HERE.iterdir() if p.is_file()}
    inputs[str(catalog_path)] = sha(catalog_path)
    for name in ['flake.nix', 'flake.lock']:
        inputs[str(ROOT/name)] = sha(ROOT/name)
    tools = {}
    for name in ['iasl', 'acpiexec']:
        tool = Path(catalog['tools'][name]['path'])
        assert sha(tool) == catalog['tools'][name]['sha256']
        inputs[str(tool)] = sha(tool)
        tools[name] = tool
    for name in ['gprbuild', 'gcc']:
        tool = shutil.which(name)
        assert tool, name
        inputs[tool] = sha(tool)
    commands, rows, reference_rows = [], [], []
    failure = None

    def write(name, value):
        (output/name).write_text(json.dumps(value, indent=2)+'\n')

    def verify():
        for path, digest in inputs.items():
            assert sha(path) == digest, path

    def command(argv, cap, log, stdin=None):
        began = time.monotonic()
        process = subprocess.run(argv, capture_output=True, text=True, timeout=cap,
                                 cwd=output, input=stdin)
        log.write_text(process.stdout+process.stderr)
        record = dict(argv=argv, stdin=stdin, exit=process.returncode,
                      seconds=time.monotonic()-began, log=str(log), log_sha256=sha(log))
        commands.append(record)
        write('COMMANDS.json', commands)
        assert process.returncode == 0, record
        return log.read_text()

    try:
        compiled = {}
        if args.fixtures:
            path = args.fixtures.resolve()/'COMPILED.json'
            inputs[str(path)] = sha(path)
            reused = json.loads(path.read_text())
        else:
            (output/'seeds').mkdir()
            reused = {}
        for name, expected in spec['units'].items():
            source = SEEDS/'fixtures'/name
            assert sha(source) == expected
            inputs[str(source)] = expected
            if args.fixtures:
                item = reused[name]
                assert item['source_sha256'] == expected
                assert sha(item['aml']) == item['aml_sha256']
            else:
                prefix = output/'seeds'/Path(name).stem
                text = command(['timeout', '--kill-after=2s', '20s', str(tools['iasl']), '-oa',
                                '-p', str(prefix), str(source)], 25, prefix.with_suffix('.log'))
                assert '0 Errors, 0 Warnings' in text
                item = dict(source_sha256=expected, aml=str(prefix.with_suffix('.aml')),
                            aml_sha256=sha(prefix.with_suffix('.aml')))
            compiled[name] = item
            inputs[item['aml']] = item['aml_sha256']
        write('COMPILED.json', compiled)
        cases = generate(compiled[spec['base']]['aml'], compiled[spec['provider']]['aml'], output/'fixtures')
        assert [c['name'] for c in cases] == [c['name'] for c in spec['cases']]
        for path in (output/'fixtures').iterdir():
            inputs[str(path)] = sha(path)
        directories = [SEEDS]+[ROOT/path for path in catalog['source_dirs']]
        for directory in directories:
            for path in directory.iterdir():
                if path.is_file() and path.suffix in ['.ads', '.adb']:
                    inputs[str(path)] = sha(path)
        flags = ['-O3', '-gnatp'] if args.profile == 'shipping' else ['-O1', '-gnata', '-gnato']
        flags += ['-gnat2022', '-g', '-gnatwe']
        project = output/'name_terms.gpr'
        switches = ', '.join(json.dumps(flag) for flag in flags)
        project.write_text('project Name_Terms is\n for Source_Dirs use ('+
            ', '.join(json.dumps(str(d)) for d in directories)+');\n'+
            ' for Object_Dir use "obj"; for Exec_Dir use "bin";\n'+
            ' for Main use ("module_name_recovery_runner.adb");\n package Compiler is\n'+
            ' for Default_Switches ("Ada") use ('+switches+');\n'+
            ' for Switches ("module_name_recovery_runner.adb") use ('+switches+', "-gnatwa");\n'+
            ' end Compiler;\nend Name_Terms;\n')
        inputs[str(project)] = sha(project)
        write('INPUTS.json', inputs)
        verify()
        text = command(['timeout', '--kill-after=2s', '240s', 'gprbuild', '-q', '-p', '-j2', '-P',
                        str(project)], 245, output/'build.log')
        assert 'warning:' not in text.lower()
        runner = output/'bin/module_name_recovery_runner'
        inputs[str(runner)] = sha(runner)
        write('INPUTS.json', inputs)
        verify()
        for case, expected in zip(cases, spec['cases']):
            if args.reference:
                reference = expected['reference']
                script = 'execute \\_LID\nexecute \\CONS\nexecute \\SIDE\nexecute \\SINK\nquit\n'
                text = command(['timeout', '--kill-after=2s', '12s', 'prlimit', '--as=25769803776',
                    '--stack=134217728', str(tools['acpiexec']), '-fv', '0', *case['tables']],
                    17, output/(case['name']+'-reference.log'), script)
                starts = list(re.finditer(r'^Evaluating (.+)$', text, re.M))
                observations = []
                for index, match in enumerate(starts):
                    stop = starts[index+1].start() if index+1 < len(starts) else len(text)
                    section = text[match.start():stop]
                    observations.append(dict(path=match[1], integers=[int(x, 16) for x in
                        re.findall(r'\[Integer\]\s*=\s*([0-9A-Fa-f]+)', section)], failures=
                        re.findall(r'failed with status (\S+)', section)))
                assert len(observations) == 4
                probe, cons, side, sink = observations
                matched = (probe['integers'] == reference['probe_integers'] and
                    probe['failures'] == reference['probe_failures'] and
                    cons['integers'] == [reference['cons']] and not cons['failures'] and
                    side['integers'] == [reference['side']] and not side['failures'] and
                    sink['integers'] == [reference['side']] and not sink['failures'])
                reference_rows.append(dict(case=case['name'], matched=matched, observations=observations))
                assert matched, reference_rows[-1]
            text = command(['timeout', '--kill-after=2s', '12s', 'prlimit', '--as=25769803776',
                            '--stack=134217728', str(runner), *case['tables']],
                           17, output/(case['name']+'.log'))
            module = re.findall(r'^MODULE outcome=(\S+) failure=(\S+) completed=\s*(\d+)', text, re.M)
            probe = re.findall(r'^PROBE outcome=(\S+) access=(\S+)', text, re.M)
            recovery = re.findall(r'^RECOVERY count=\s*(\d+) kind=(\S+)', text, re.M)
            model = re.findall(r'^MODEL reads=\s*(\d+) writes=\s*(\d+)', text, re.M)
            expected_probe = [] if expected['probe'] is None else [(expected['probe'], 'AVAILABLE')]
            matched = (len(module) == len(recovery) == len(model) == 1 and
                module[0][0] == expected['module'] and int(module[0][2]) == expected['completed'] and
                ('failure' not in expected or module[0][1] == expected['failure']) and
                probe == expected_probe and int(model[0][1]) == expected['writes'] and
                int(recovery[0][0]) == expected['recoveries'] and
                (not expected['recoveries'] or recovery[0][1] == 'MODULE_PARSED_NAME_UNKNOWN'))
            rows.append(dict(case=case['name'], matched=matched, module=module, probe=probe,
                recovery=recovery, model=model, log=str(output/(case['name']+'.log')),
                log_sha256=sha(output/(case['name']+'.log'))))
            write('PARTIAL.json', dict(terminal=False, cases=rows, reference_cases=reference_rows))
            print(case['name'], 'PASS' if matched else 'FAIL',
                  f"{sum(r['matched'] for r in rows)}/{len(rows)}", flush=True)
            assert matched, rows[-1]
            verify()
    except Exception as error:
        failure = repr(error)
        print('STOPPED', failure, flush=True)
    write('RESULT.json', dict(terminal=True, failure=failure, profile=args.profile,
        expected_matches=sum(r['matched'] for r in rows), executed=len(rows), planned=len(spec['cases']),
        percent=100*sum(r['matched'] for r in rows)/len(rows) if rows else None,
        reference_cases=reference_rows, commands=commands, cases=rows,
        scope='Own hosted module continuation/effects and method propagation guards. Called-method abort does not establish full catalog rollback parity. Synthetic owner; not full ASLTS, native hardware or SPARK closure.'))
    return 1 if failure else 0


if __name__ == '__main__':
    raise SystemExit(main())
