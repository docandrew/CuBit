"""Replay twelve immutable-table field observations with endpoint-only comparison scope."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import resource
import signal
import subprocess
import sys
from compare import compare, parse
from verify import verify as verify_reference

D = Path(__file__).resolve().parent

def interrupted(signum, frame):
    raise InterruptedError('replay supervisor interrupted')

signal.signal(signal.SIGTERM, interrupted)

def digest(p):
    return hashlib.sha256(Path(p).read_bytes()).hexdigest()

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--runner', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    parser.add_argument('--source-manifest', type=Path, required=True,
                        help='JSON absolute source paths to SHA256 digests')
    args = parser.parse_args()
    args.runner = args.runner.resolve(strict=True)
    args.output.mkdir(parents=True, exist_ok=False)
    verify_reference(D)
    reference = json.loads((D/'reference.json').read_bytes())
    def resolve_reference(name):
        relative = Path(name)
        if relative.is_absolute() or '..' in relative.parts:
            raise ValueError('unsafe reference path')
        target = D / relative
        if any(part.is_symlink() for part in [target, *target.parents]):
            raise ValueError('reference symlink rejected')
        if not target.resolve().is_relative_to(D.resolve()):
            raise ValueError('reference escape')
        return str(target.resolve(strict=True))
    guards = {resolve_reference(name): h for name, h in reference['inputs'].items()}
    for row in reference['rows']:
        if 'aml' in row:
            row['aml'] = resolve_reference(row['aml'])
    sources = json.loads(args.source_manifest.read_bytes())
    for path, expected in sources.items():
        if path in guards and guards[path] != expected:
            raise ValueError('conflicting input hash: '+path)
        guards[path] = expected
    for p in [D/'reference.json', D/'compare.py', D/'verify.py', Path(__file__)]:
        if str(p.resolve()) not in sources:
            raise ValueError('harness source missing from reviewed source manifest')
    for p in [args.source_manifest, args.runner]:
        path = str(p.resolve()); expected = digest(p)
        if path in guards and guards[path] != expected:
            raise ValueError('conflicting generated input hash: '+path)
        guards[path] = expected
    def verify():
        for path, expected in guards.items():
            if not Path(path).is_absolute() or digest(path) != expected:
                raise RuntimeError('input drift: '+path)
    verify()
    (args.output/'attempt-inputs.json').write_text(json.dumps(guards, indent=2)+'\n')
    def limits():
        signal.pthread_sigmask(signal.SIG_UNBLOCK, {signal.SIGTERM, signal.SIGINT})
        resource.setrlimit(resource.RLIMIT_AS, (1024*1024*1024,)*2)
        resource.setrlimit(resource.RLIMIT_STACK, (64*1024*1024,)*2)
        resource.setrlimit(resource.RLIMIT_FSIZE, (1024*1024,)*2)
    results = []
    for index, row in enumerate(reference['rows']):
        result = {'key': row['key'], 'selection': row['selection']}
        results.append(result)
        if row['selection'] != 'compare':
            result['classification'] = row['selection']
            continue
        verify()
        stdout = args.output/f'{index:03}-stdout.log'
        stderr = args.output/f'{index:03}-stderr.log'
        with stdout.open('xb') as out, stderr.open('xb') as err:
            process = None
            # Block interruption until Popen returns and its child handle is owned.
            blocked = signal.pthread_sigmask(signal.SIG_BLOCK, {signal.SIGTERM, signal.SIGINT})
            try:
                process = subprocess.Popen([str(args.runner), row['aml']], stdout=out, stderr=err,
                                           start_new_session=True, preexec_fn=limits)
                signal.pthread_sigmask(signal.SIG_SETMASK, blocked)
                code = process.wait(timeout=30)
            except subprocess.TimeoutExpired:
                if process is not None:
                    try: os.killpg(process.pid, signal.SIGKILL)
                    except ProcessLookupError: pass
                    process.wait()
                code = 'timeout'
            except BaseException:
                if process is not None:
                    try: os.killpg(process.pid, signal.SIGKILL)
                    except ProcessLookupError: pass
                    process.wait()
                raise
            finally:
                signal.pthread_sigmask(signal.SIG_SETMASK, blocked)
        verify()
        result['returncode'] = code
        try:
            if code != 0: raise ValueError('runner process failed')
            if stderr.read_bytes(): raise ValueError('unexpected stderr')
            observed = parse(stdout.read_bytes())
            compare(observed, row)
            result.update(classification='matched', observed=observed,
                          comparison_scope=row['comparison_scope'],
                          acpica_error=row['phases']['TEST'].get('acpica_error'))
        except ValueError as error:
            result.update(classification='failed', reason=str(error))
        (args.output/'report.json').write_text(json.dumps(results, indent=2)+'\n')
        print(row['key'], result['classification'], flush=True)
    verify()
    summary = {key: sum(r['classification']==key for r in results)
               for key in ('matched','failed','compiler_rejected')}
    (args.output/'report.json').write_text(json.dumps(results, indent=2)+'\n')
    (args.output/'summary.json').write_text(json.dumps(summary, indent=2)+'\n')
    print(json.dumps(summary))
    return int(summary['failed'] != 0 or summary['matched'] != 12)

if __name__ == '__main__':
    sys.exit(main())
