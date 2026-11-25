"""Own module-name recovery regressions with an in-process memory owner."""
from pathlib import Path
import argparse, hashlib, json, re, subprocess, time

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[1]
sha = lambda p: hashlib.sha256(Path(p).read_bytes()).hexdigest()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--output', type=Path, required=True)
    parser.add_argument('--profile', choices=['shipping', 'checked'], required=True)
    parser.add_argument('--fixtures', type=Path, help='Reuse an exact previously compiled own fixture set')
    args = parser.parse_args()
    output = args.output.resolve()
    output.mkdir(parents=True, exist_ok=False)
    inputs = {str(p): sha(p) for p in HERE.rglob('*') if p.is_file() and '__pycache__' not in p.parts}
    catalog = json.loads((ROOT/'tests/acpi-aslts-corpus/sources.json').read_text())
    spec = json.loads((HERE/'cases.json').read_text())
    iasl = Path(catalog['tools']['iasl']['path'])
    assert sha(iasl) == catalog['tools']['iasl']['sha256']
    inputs[str(iasl)] = sha(iasl)
    commands, rows = [], []
    failure = None

    def write(name, value):
        (output/name).write_text(json.dumps(value, indent=2)+'\n')

    def verify():
        for path, digest in inputs.items():
            assert sha(path) == digest, path

    def command(argv, cap, log):
        began = time.monotonic()
        process = subprocess.run(argv, capture_output=True, text=True, timeout=cap, cwd=output)
        log.write_text(process.stdout+process.stderr)
        record = dict(argv=argv, exit=process.returncode, seconds=time.monotonic()-began,
                      log=str(log), log_sha256=sha(log))
        commands.append(record)
        write('COMMANDS.json', commands)
        assert process.returncode == 0, record
        return log.read_text()

    try:
        if args.fixtures:
            fixture_root = args.fixtures.resolve()
            compiled = json.loads((fixture_root/'COMPILED.json').read_text())
            assert set(compiled) == set(spec['units'])
            for name, item in compiled.items():
                assert item['source_sha256'] == spec['units'][name]
                assert sha(item['aml']) == item['aml_sha256']
        else:
            fixture_root = output/'fixtures'
            fixture_root.mkdir()
            compiled = {}
            for name, expected in spec['units'].items():
                source = HERE/'fixtures'/name
                assert sha(source) == expected
                prefix = fixture_root/Path(name).stem
                text = command(['timeout', '--kill-after=2s', '20s', str(iasl), '-oa', '-p', str(prefix), str(source)],
                               25, fixture_root/(Path(name).stem+'.log'))
                assert '0 Errors, 0 Warnings' in text
                aml = prefix.with_suffix('.aml')
                compiled[name] = dict(source_sha256=expected, aml=str(aml), aml_sha256=sha(aml))
            (fixture_root/'COMPILED.json').write_text(json.dumps(compiled, indent=2)+'\n')
        for item in compiled.values():
            inputs[item['aml']] = item['aml_sha256']
        directories = [HERE] + [ROOT/path for path in catalog['source_dirs']]
        for directory in directories:
            for path in directory.iterdir():
                if path.is_file() and path.suffix in ['.ads', '.adb']:
                    inputs[str(path)] = sha(path)
        flags = ['-O3', '-gnatp'] if args.profile == 'shipping' else ['-O1', '-gnata', '-gnato']
        flags += ['-gnat2022', '-g', '-gnatwe']
        project = output/'recovery.gpr'
        switches = ', '.join(json.dumps(flag) for flag in flags)
        project.write_text('project Recovery is\n for Source_Dirs use (' +
                          ', '.join(json.dumps(str(d)) for d in directories) + ');\n'
                          ' for Object_Dir use "obj"; for Exec_Dir use "bin";\n'
                          ' for Main use ("module_name_recovery_runner.adb");\n package Compiler is\n'
                          ' for Default_Switches ("Ada") use (' + switches + ');\n'
                          ' for Switches ("module_name_recovery_runner.adb") use (' + switches + ', "-gnatwa");\n'
                          ' end Compiler;\nend Recovery;\n')
        inputs[str(project)] = sha(project)
        write('INPUTS.json', inputs)
        verify()
        text = command(['timeout', '--kill-after=2s', '240s', 'gprbuild', '-q', '-p', '-j2', '-P', str(project)],
                       245, output/'build.log')
        assert 'warning:' not in text.lower()
        runner = output/'bin/module_name_recovery_runner'
        inputs[str(runner)] = sha(runner)
        write('INPUTS.json', inputs)
        verify()
        for case in spec['cases']:
            argv = ['timeout', '--kill-after=2s', '12s', 'prlimit', '--as=25769803776', '--stack=134217728',
                    str(runner), *[compiled[name]['aml'] for name in case['tables']]]
            text = command(argv, 17, output/(case['name']+'.log'))
            module = re.findall(r'^MODULE outcome=(\S+) failure=(\S+) completed=\s*(\d+)', text, re.M)
            probe = re.findall(r'^PROBE outcome=(\S+) access=(\S+)', text, re.M)
            recovery = re.findall(r'^RECOVERY count=\s*(\d+) kind=(\S+)', text, re.M)
            model = re.findall(r'^MODEL reads=\s*(\d+) writes=\s*(\d+)', text, re.M)
            matched = (len(module) == len(probe) == len(recovery) == len(model) == 1
                       and module[0][0] == 'MODULES_COMPLETED' and int(module[0][2]) == 3
                       and probe[0] == (case['probe'], 'AVAILABLE')
                       and int(model[0][1]) == case['writes']
                       and int(recovery[0][0]) == case['recoveries']
                       and (case['recoveries'] == 0 or recovery[0][1] == 'MODULE_FIELD_REGION_UNKNOWN_NAME'))
            rows.append(dict(case=case['name'], matched=matched, expected=case, module=module,
                             probe=probe, recovery=recovery, model=model,
                             log=str(output/(case['name']+'.log')), log_sha256=sha(output/(case['name']+'.log'))))
            write('PARTIAL.json', dict(terminal=False, cases=rows))
            print(case['name'], 'PASS' if matched else 'FAIL', probe, recovery, model, flush=True)
            assert matched, rows[-1]
            verify()
    except Exception as error:
        failure = repr(error)
        print('STOPPED', failure, flush=True)
    write('RESULT.json', dict(terminal=True, failure=failure, profile=args.profile, expected_matches=sum(r['matched'] for r in rows),
                             executed=len(rows), planned=len(spec['cases']), commands=commands, cases=rows,
                             scope='Own hosted semantic regressions; synthetic memory owner only; not native hardware, full ASLTS or SPARK closure'))
    return 1 if failure else 0


if __name__ == '__main__':
    raise SystemExit(main())
