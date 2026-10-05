"""Run exact Desktop output-retirement callers with controlled FFI; run inside Nix.

This tests actual caller routines, not kernel grants or renderer fence truth.
The native fixture separately exercises real output leases and grant retirement in CuBit.
"""
import hashlib
import json
from pathlib import Path
import subprocess
import tempfile
import output_retirement_fixture
import async_lease_fixture

ROOT = Path(__file__).resolve().parents[2]
HERE = Path(__file__).resolve().parent


def main():
    output_retirement_fixture.self_test()
    async_lease_fixture.self_test()
    build = HERE / 'build'
    build.mkdir(exist_ok=True)
    work = Path(tempfile.mkdtemp(prefix='output-retirement-', dir=build))
    inputs = {}

    def read(path):
        data = path.read_bytes()
        inputs[str(path)] = hashlib.sha256(data).hexdigest()
        return data.decode()

    source = read(ROOT / 'userspace/services/desktop/main.adb')
    start = '   procedure closeOutput (Output : Output_Index) is'
    stop = '   function validDisplayInfo'
    assert source.count(start) == 1 and source.count(stop) == 1
    for name in ('compositor_output_retirement.ads', 'compositor_output_retirement.adb', 'compositor_lease_request.ads', 'compositor_lease_request.adb'):
        (work / name).write_text(read(ROOT / 'userspace/lib/compositor' / name))
    routines = source[source.index(start):source.index(stop)]
    template = read(HERE / 'output_controller_fixture.inc')
    assert template.count('@DESKTOP_ROUTINES@') == 1
    (work / 'output_controller_tests.adb').write_text(template.replace('@DESKTOP_ROUTINES@\n', routines))
    (work / 'check.gpr').write_text('''project Check is
 for Source_Dirs use (".");
 for Object_Dir use "obj";
 for Exec_Dir use ".";
 for Main use ("output_controller_tests.adb");
 package Compiler is
  for Default_Switches ("Ada") use ("-gnat2022", "-gnata", "-gnato", "-O2");
 end Compiler;
end Check;
''')
    subprocess.run(['gprbuild', '-q', '-p', '-P', str(work/'check.gpr')], check=True)
    for mode in range(20):
        subprocess.run([str(work/'output_controller_tests'), str(mode)], check=True)
    start = '            for Output in Output_Index loop\n               if LR.Token'
    stop = '            if not matched then\n            for P of presentations loop'
    assert source.count(start) == 1 and source.count(stop) == 1
    template = read(HERE / 'lease_routing_fixture.inc')
    assert template.count('@DESKTOP_ROUTINES@') == 1
    (work / 'routing_tests.adb').write_text(template.replace('@DESKTOP_ROUTINES@\n', source[source.index(start):source.index(stop)]))
    project = (work / 'check.gpr').read_text().replace('output_controller_tests.adb', 'routing_tests.adb')
    (work / 'routing.gpr').write_text(project)
    subprocess.run(['gprbuild', '-q', '-p', '-P', str(work/'routing.gpr')], check=True)
    subprocess.run([str(work/'routing_tests')], check=True)
    for path, digest in inputs.items():
        assert hashlib.sha256(Path(path).read_bytes()).hexdigest() == digest, path
    (work/'inputs.json').write_text(json.dumps(inputs, indent=2)+'\n')
    print('PASS exact Desktop output-retirement callers:', work)


if __name__ == '__main__':
    main()
