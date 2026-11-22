"""Independent callback corruption checks for native benchmark reports."""
from js_benchmark_report import EXPECTED, native_report

lines = [f'CUBITSHELL-PERF: CuBitBrowserPerfJS:{name},{sample},{sample + 0.125},{checksum}'
         for name, checksum in EXPECTED.items() for sample in range(5)]
lines.append('CUBITSHELL-PERF: CuBitBrowserPerfJSDone')
report = native_report('\n'.join(lines))
assert len(report['results']) == 6
assert all(row['median_ms'] == 2.125 for row in report['results'])
corruptions = [[], lines[:-1], lines[1:], lines + [lines[-1]],
               [lines[1], lines[0]] + lines[2:],
               lines[:-1] + ['CUBITSHELL-PERF: CuBitBrowserPerfJSFailed']]
for index in range(30):
    corruptions.append(lines[:index] + lines[index+1:])
    corruptions.append(lines[:index] + [lines[index]] + lines[index:])
for bad in ('nan', 'inf', '-1'):
    corruptions.append([lines[0].replace(',0.125,', f',{bad},')] + lines[1:])
corruptions.append([lines[0].rsplit(',', 1)[0] + ',0'] + lines[1:])
corruptions.append([lines[0].replace(':integer,', ':unknown,')] + lines[1:])
for serial in corruptions:
    try:
        native_report('\n'.join(serial))
    except ValueError:
        pass
    else:
        raise AssertionError('invalid benchmark accepted')
print(f'PASS native benchmark report: complete run and {len(corruptions)} corruptions')
