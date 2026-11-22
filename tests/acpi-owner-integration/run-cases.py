from pathlib import Path
import json
import subprocess
import sys

here = Path(__file__).resolve().parent
out = Path(sys.argv[1]).resolve(); profile = sys.argv[2]
assert profile in {'checked', 'release'}
matrix = json.loads((here / 'matrix.json').read_text())
for width in (1, 2):
    for entry in matrix:
        for index, case in enumerate(entry['cases']):
            args = [str(out / profile / 'bin' / entry['main'])]
            fixture = str(out / f'{entry["fixture"]}-{width}.aml')
            if entry['main'] == 'bootstrap_driver_tests': args += [str(case), fixture]
            else:
                args += [fixture]
                if case is not None: args.append(str(case))
            log = out / f'{profile}-{width}-{entry["main"]}-{index}.log'
            with log.open('wb') as stream:
                result = subprocess.run(['timeout', '--kill-after=2s', '30'] + args, stdout=stream, stderr=subprocess.STDOUT)
            assert log.stat().st_size <= 1024 * 1024, log
            if result.returncode:
                print(log.read_text()); raise SystemExit(result.returncode)
            with (out / 'status.tsv').open('a') as stream:
                stream.write(f'{profile}\t{width}\t{entry["main"]}\t{index}\tpassed\n')
        print(f'{profile} AML{width}: {entry["main"]} {len(entry["cases"])} cases passed', flush=True)
