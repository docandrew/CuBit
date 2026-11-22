"""Check restart boundaries against the misleading apparent-reclamation case."""
from pathlib import Path
import contextlib
import importlib.util
import io
import json
import sys
import tempfile

source = Path(sys.argv[1]) if len(sys.argv) > 1 else Path(__file__).with_name('performance_report.py')
spec = importlib.util.spec_from_file_location('report_under_test', source)
module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(module)
oracle = 'CUBITSHELL-MEMORY: PASS charge/release 2097152 bytes\n'
def sample(ms, owned, peak):
    return f'CUBITSHELL-MEMORY: ms={ms} owned_bytes={owned} sampled_peak_bytes={peak} windows=1\n'
first = oracle + sample(0, 250, 250) + sample(400000, 900, 950)
second = oracle + sample(0, 250, 250)
with tempfile.TemporaryDirectory() as directory:
    serial, destination = Path(directory)/'serial.log', Path(directory)/'memory.json'
    def report(text):
        serial.write_text(text)
        with contextlib.redirect_stdout(io.StringIO()):
            result = module.memory_report(serial, destination)
        assert result == json.loads(destination.read_text())
        return result
    one = report(first)
    assert one['run_count'] == 1 and one['first_last_same_startup_run']
    two = report(first + second)
    assert two['run_count'] == 2 and not two['first_last_same_startup_run']
    assert two['runs'][0]['last_owned_bytes'] == 900
    assert two['runs'][1]['first_owned_bytes'] == 250
    invalid = [sample(0, 250, 250), first + sample(0, 250, 950), first + oracle,
               oracle, oracle + sample(0, 250, 200), first + sample(400001, 250, 300),
               first + 'CUBITSHELL-MEMORY: unavailable\n']
    for text in invalid:
        try:
            report(text)
        except AssertionError:
            pass
        else:
            raise AssertionError('invalid memory report accepted')
print('PASS memory reporting: restart isolation, stable JSON, seven invalid traces rejected')
