"""Validate native title callbacks; incomplete runs never become timings."""
import math
import re
import statistics

EXPECTED = {
    'integer': sum((i * 17) ^ (i >> 3) for i in range(100000)) & 0xffffffff,
    'typed-array': 32 * sum((i * 2654435761) & 0xffffffff for i in range(16384)) & 0xffffffff,
    'objects': 20 * sum(range(2000)),
    'json': 8 * sum(range(1000)),
    'regexp': 2 * sum(range(2000)),
    'sort': 4 * sum((i + 1) * i for i in range(4096)) & 0xffffffff,
}


def native_report(serial):
    markers = re.findall(r'CUBITSHELL-PERF: (CuBitBrowserPerfJS[^\r\n]*)', serial)
    if not markers or markers[-1] != 'CuBitBrowserPerfJSDone':
        raise ValueError('benchmark did not complete')
    wanted = [(name, sample) for name in EXPECTED for sample in range(5)]
    if len(markers) != len(wanted) + 1:
        raise ValueError('missing, duplicate or failed benchmark callbacks')
    samples = {name: [] for name in EXPECTED}
    for marker, (name, sample) in zip(markers[:-1], wanted):
        fields = marker.removeprefix('CuBitBrowserPerfJS:').split(',')
        if len(fields) != 4 or fields[0] != name or fields[1] != str(sample):
            raise ValueError('out-of-order or malformed benchmark callback')
        elapsed = float(fields[2])
        if not math.isfinite(elapsed) or elapsed < 0 or int(fields[3]) != EXPECTED[name]:
            raise ValueError('invalid clock sample or checksum')
        samples[name].append(elapsed)
    return {
        'version': 'penny-js-v1', 'warmups': 1, 'repeats': 5,
        'clock': 'page performance.now(); milliseconds rounded to 0.001 in title callback',
        'results': [dict(id=name, checksum=EXPECTED[name], samples_ms=values,
                         median_ms=statistics.median(values), minimum_ms=min(values),
                         maximum_ms=max(values)) for name, values in samples.items()],
    }
