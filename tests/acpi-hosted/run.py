#!/usr/bin/env python3
"""Portable Linux-hosted ACPI checks. Requires the repository's Nix shell."""
from pathlib import Path
import argparse, json, math, os, re, resource, subprocess, sys, tempfile, time
root = Path(__file__).resolve().parents[2]
parser = argparse.ArgumentParser()
parser.add_argument('--mode', choices=['release', 'checked'], default='release')
parser.add_argument('--group', choices=['focused', 'features', 'fixtures', 'adapters', 'compile-original', 'original-tests', 'corrections', 'capacity', 'static-buffer', 'static-buffer-oracle', 'service-api', 'mixed-comparison', 'tail-conditionals', 'to-buffer', 'delay', 'delay-policy', 'mid', 'to-string', 'explicit-format', 'match', 'resource-templates', 'buffer-fields', 'notify-alias', 'metadata', 'buffer-sizing', 'buffer-dynamic', 'table-fields', 'capture', 'package-store', 'integer-ownership', 'service-milestone', 'runtime-packages', 'breakpoint', 'scalar-ledger', 'all'], default='focused')
parser.add_argument('--test-timeout', type=float, default=180.0, help='maximum seconds per test (default:180)')
parser.add_argument('--skip-main', action='append', default=[], help='explicitly omit a previously validated main; recorded in report')
parser.add_argument('--ledger-selection', nargs='+', help='Explicit scalar-ledger closure labels only')
args = parser.parse_args()
if args.ledger_selection and args.group != 'scalar-ledger':
    parser.error('--ledger-selection requires --group scalar-ledger')
if not math.isfinite(args.test_timeout) or args.test_timeout <= 0:
    parser.error('--test-timeout must be positive')
if args.group == 'scalar-ledger':
    if args.skip_main: parser.error('scalar-ledger uses explicit closure selections, not skip-main')
    command = [sys.executable, '-B', str(root / 'tests/aml-scalar-ledger-count/run.py'), '--mode', args.mode]
    if args.ledger_selection: command += ['--selection', *args.ledger_selection]
    os.execv(sys.executable, command)

if args.group == 'breakpoint':
    if args.skip_main: parser.error('breakpoint requires its complete focused selection')
    os.execv(sys.executable, [sys.executable, '-B', str(root / 'tests/aml-breakpoint/run.py'), '--mode', args.mode])

if args.group == 'runtime-packages':
    if args.skip_main: parser.error('runtime-packages requires its complete registered selection')
    os.execv(sys.executable, [sys.executable, '-B', str(root / 'tests/aml-runtime-packages/run.py'), '--mode', args.mode])

if args.group == 'service-milestone':
    if args.skip_main: parser.error('service-milestone requires its complete selection')
    os.execv(sys.executable, [sys.executable, '-B', str(root / 'tests/acpi-service-milestone/run.py'), '--mode', args.mode])

if args.group == 'integer-ownership':
    if args.skip_main:
        parser.error('integer-ownership requires its complete wide selection; --skip-main is not supported')
    os.execv(sys.executable, [sys.executable, '-B', str(root / 'tests/aml-integer-ownership/run.py'), '--selection', 'wide', '--mode', args.mode])

if args.group == 'package-store':
    if args.skip_main:
        parser.error('package-store requires all seven mains; --skip-main is not supported')
    os.execv(sys.executable, [sys.executable, '-B', str(root / 'tests/aml-package-store/run.py'), '--mode', args.mode])

if args.group == 'capture':
    if args.skip_main:
        parser.error('capture requires its complete selection; --skip-main is not supported')
    os.execv(sys.executable, [sys.executable, '-B', str(root / 'tests/aml-capture/run.py'), '--mode', args.mode])
if args.group == 'table-fields':
    if args.skip_main:
        parser.error('table-fields requires all five mains and all twelve comparisons; --skip-main is not supported')
    os.execv(sys.executable, [sys.executable, '-B', str(root / 'tests/aml-table-fields/run.py'),
             '--mode', args.mode, '--test-timeout', str(args.test_timeout)])
if args.group == 'buffer-dynamic':
    if args.skip_main:
        parser.error('buffer-dynamic requires the real owner main and all twenty comparisons; --skip-main is not supported')
    os.execv(sys.executable, [sys.executable, '-B', str(root / 'tests/aml-buffer-dynamic/run.py'),
             '--mode', args.mode, '--test-timeout', str(args.test_timeout)])
if args.group == 'buffer-sizing':
    if args.skip_main:
        parser.error('buffer-sizing requires all five mains and thirteen adapters; --skip-main is not supported')
    os.execv(sys.executable, [sys.executable, '-B', str(root / 'tests/aml-buffer-sizing/run.py'),
             '--mode', args.mode, '--test-timeout', str(args.test_timeout)])
if args.group == 'metadata':
    if args.skip_main:
        parser.error('metadata requires all ten mains; --skip-main is not supported')
    os.execv(sys.executable, [sys.executable, '-B', str(root / 'tests/aml-metadata/run.py'),
             '--mode', args.mode, '--test-timeout', str(args.test_timeout)])
if args.group == 'notify-alias':
    if args.skip_main:
        parser.error('notify-alias requires all six mains; --skip-main is not supported')
    # Replace this dispatcher so TERM reaches the suite directly.
    os.execv(sys.executable, [sys.executable, str(root / 'tests/aml-notify/run.py'),
             '--mode', args.mode, '--test-timeout', str(args.test_timeout)])
resource.setrlimit(resource.RLIMIT_STACK, (64 * 1024 * 1024, resource.getrlimit(resource.RLIMIT_STACK)[1]))
mode = args.mode
focused = [f'tests/aml-containers/containers_{mode}.gpr', f'tests/aml-debug/revision_{mode}.gpr', f'tests/aml-concatenate/owner_{mode}.gpr']
groups = {'focused': focused,
          'buffer-fields': [],
          'resource-templates': [f'tests/aml-resource/{mode}.gpr'],
          'match': [f'tests/aml-match/{mode}.gpr'],
          'explicit-format': [f'tests/aml-explicit-format/{mode}.gpr'],
          'to-string': [f'tests/aml-tostring/{mode}.gpr'],
          'mid': [f'tests/aml-mid/{mode}.gpr'],
          'delay-policy': [f'tests/aml-delay/policy_{mode}.gpr'],
          'to-buffer': [f'tests/aml-tobuffer/{mode}.gpr'],
          'delay': [f'tests/aml-delay/delay_{mode}.gpr'],
          'tail-conditionals': [f'tests/aml-conditionals/tail_{mode}.gpr'],
          'mixed-comparison': [f'tests/aml-mixed/mixed_{mode}.gpr'],
          'features': [f'tests/acpi-hosted/integration_{mode}.gpr'],
          'fixtures': [f'tests/acpi-hosted/core_{mode}.gpr', f'tests/acpi-hosted/service_{mode}.gpr'],
          'adapters': [f'tests/acpi-hosted/adapter_original_{mode}.gpr', f'tests/acpi-hosted/adapter_owned_{mode}.gpr', f'tests/acpi-hosted/adapter_service_{mode}.gpr'],
          'compile-original': ['tests/aml-core/aml.gpr'],
          'original-tests': ['tests/aml-core/aml.gpr'],
          'corrections': [f'tests/acpi-hosted/corrections_{mode}.gpr'],
          'capacity': [f'tests/acpi-capacity/capacity_{mode}.gpr'],
          'static-buffer': [f'tests/aml-static-buffer/static_buffer_{mode}.gpr'],
          'static-buffer-oracle': [f'tests/aml-static-buffer/oracle_{mode}.gpr'],
          'service-api': [f'tests/acpi-hosted/service_api_{mode}.gpr', f'tests/acpi-hosted/service_api_integration_{mode}.gpr']}
projects = sum((groups[g] for g in ['focused','features','fixtures','adapters','capacity','static-buffer']), []) if args.group == 'all' else groups[args.group]
log_root = root / 'tests/acpi-hosted/results'
log_root.mkdir(parents=True, exist_ok=True)
logs = Path(tempfile.mkdtemp(prefix=mode + '-' + args.group + '-', dir=log_root))
print('Run logs:', logs, flush=True)
report = {'mode': mode, 'group': args.group, 'test_timeout_seconds': args.test_timeout,
          'state': 'running', 'outcomes': [],
          'profile_note': 'compile-original always uses aml.gpr existing strict profile, independent of --mode'}
def save():
    (logs / 'results.json').write_text(json.dumps(report, indent=2) + '\n')
def run(cmd, label, timeout=None, cwd=root):
    row = {'command': cmd, 'label': label, 'state': 'running', 'timeout_seconds': timeout, 'cwd': str(cwd)}
    report['outcomes'].append(row)
    save()
    started = time.monotonic()
    try:
        with (logs / (label + '.log')).open('w') as output:
            completed = subprocess.run(cmd, cwd=cwd, stdout=output, stderr=subprocess.STDOUT, timeout=timeout)
        row.update(state='passed' if completed.returncode == 0 else 'failed', exit_code=completed.returncode)
        if completed.returncode:
            raise RuntimeError(label + ' failed: ' + str(completed.returncode))
    except subprocess.TimeoutExpired:
        row.update(state='timeout', exit_code=None)
        raise
    except BaseException as exc:
        if row['state'] == 'running':
            row.update(state='failed', error=repr(exc))
        raise
    finally:
        row['elapsed_seconds'] = time.monotonic() - started
        save()
save()
try:
    if args.group == 'buffer-fields':
        command = ['python3', str(root / 'tests/aml-buffer-fields/run.py'),
                   '--mode', mode, '--test-timeout', str(args.test_timeout)]
        for main in args.skip_main:
            command.extend(['--skip-main', main])
        # The suite bounds each child and kills its process group on timeout.
        # A second deadline here could kill only the supervisor, orphaning them.
        run(command, 'buffer-fields-suite')
    for name in projects:
        project = root / name
        text = project.read_text()
        if args.group == 'delay':
            run(['python3', str(root / 'tests/aml-delay/verify.py')],
                project.stem + '-reference-verification', timeout=30)
        if args.group == 'tail-conditionals':
            run(['python3', str(root / 'tests/aml-conditionals/verify.py')],
                project.stem + '-reference-verification', timeout=30)
        compile_only = args.group == 'compile-original'
        mains_match = re.search(r'for Main use\s*\((.*?)\);', text, re.S)
        mains = re.findall(r'"([^"]+)"', mains_match[1]) if mains_match else []
        if project.stem.startswith('adapter_'):
            # Explicitly compile every callback adapter, even projects with mains.
            run(['gprbuild', '-p', '-j1', '-P', str(project), '-u', 'timer_verification.adb'], project.stem + '-timer-build')
        if (mains or compile_only) and args.group != 'original-tests':
            run(['gprbuild', '-p', '-j1', '-P', str(project)], project.stem + '-build')
        if args.group == 'static-buffer-oracle':
            run(['python3', str(root / 'tests/aml-static-buffer/compare.py'), '--mode', mode],
                project.stem + '-cached-comparison', timeout=36 * 30 + 60)
        if args.group == 'mixed-comparison':
            run(['python3', str(root / 'tests/aml-mixed/compare.py'), '--mode', mode],
                project.stem + '-cached-comparison', timeout=60 * 30 + 60)
        if args.group == 'to-buffer':
            run(['python3', str(root / 'tests/aml-tobuffer/compare.py'), '--mode', mode],
                project.stem + '-cached-comparison', timeout=88 * 30 + 60)
        if args.group == 'resource-templates':
            run(['python3', str(root / 'tests/aml-resource/compare.py'),
                 '--runner', str(root / 'tests/aml-resource' / ('build-' + mode) / 'resource_runner'),
                 '--output', str(logs / 'resource-cached-results')],
                project.stem + '-cached-resource-comparison', timeout=72 * 30 + 60)
        if args.group == 'match':
            run(['python3', str(root / 'tests/aml-match/compare.py'), '--mode', mode,
                 '--runner', str(root / 'tests/aml-match' / ('build-' + mode) / 'match_runner'),
                 '--output', str(logs / 'match-cached-results')],
                project.stem + '-cached-match-comparison', timeout=48 * 30 + 60)
        if args.group == 'explicit-format':
            run(['python3', str(root / 'tests/aml-explicit-format/compare.py'), '--mode', mode,
                 '--runner', str(root / 'tests/aml-explicit-format' / ('build-' + mode) / 'format_runner'),
                 '--output', str(logs / 'explicit-format-cached-results')],
                project.stem + '-cached-explicit-format-comparison', timeout=70 * 30 + 60)
        if args.group == 'to-string':
            run(['python3', str(root / 'tests/aml-tostring/compare.py'), '--mode', mode,
                 '--runner', str(root / 'tests/aml-tostring' / ('build-' + mode) / 'to_string_runner'),
                 '--output', str(logs / 'tostring-cached-results')],
                project.stem + '-cached-tostring-comparison', timeout=72 * 30 + 60)
        if args.group == 'mid':
            run(['python3', str(root / 'tests/aml-mid/compare.py'),
                 '--runner', str(root / 'tests/aml-mid' / ('build-' + mode) / 'mid_runner'),
                 '--output', str(logs / 'mid-cached-results')],
                project.stem + '-cached-mid-comparison', timeout=56 * 30 + 60)
        if compile_only:
            continue
        directory = re.search(r'for Exec_Dir use\s*"([^"]+)"', text) or re.search(r'for Object_Dir use\s*"([^"]+)"', text)
        if not directory:
            raise RuntimeError('Project needs an explicit executable directory: ' + name)
        for main in mains:
            if main.endswith('_runner.adb'):
                continue
            if main in args.skip_main:
                report['outcomes'].append({'label': main, 'state': 'skipped', 'reason': 'explicit --skip-main; prior evidence required'})
                save()
                continue
            exe = project.parent / directory[1] / Path(main).stem
            if main == 'integer_oracle_tests.adb':
                run(['python3', str(root / 'tests/aml-core/integer_oracle.py')], project.stem + '-integer-fixture')
            try:
                run([str(exe)], project.stem + '-' + exe.name, timeout=args.test_timeout, cwd=root / 'userspace' if main == 'integer_oracle_tests.adb' else root)
            except (RuntimeError, subprocess.TimeoutExpired):
                if args.group != 'original-tests':
                    raise
    if any(row['state'] not in ('passed', 'skipped') for row in report['outcomes']):
        raise RuntimeError('One or more independent tests failed; see all outcomes')
    report['state'] = 'passed'
except BaseException as exc:
    report.update(state='failed', error=repr(exc))
    raise
finally:
    save()
print('Hosted run passed:', logs, flush=True)
