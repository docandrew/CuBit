"""Run exact Desktop source-loan callers with controlled FFI; run inside Nix.

This tests actual caller routines, not kernel grants or renderer fence truth.
The native fixture separately exercises real acquisitions/returns in CuBit.
"""
import hashlib
import json
from pathlib import Path
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parents[2]
HERE = Path(__file__).resolve().parent


def main():
    build = HERE / 'build'
    build.mkdir(exist_ok=True)
    work = Path(tempfile.mkdtemp(prefix='source-retirement-', dir=build))
    inputs = {}

    def read(path):
        data = path.read_bytes()
        inputs[str(path)] = hashlib.sha256(data).hexdigest()
        return data.decode()

    source = read(ROOT / 'userspace/services/desktop/main.adb')
    start = '   procedure acquireSourceLoan\n'
    assert source.count(start) == 1
    for name in ('compositor_source_loans.ads', 'compositor_source_loans.adb',
                 'compositor_surface_state.ads', 'compositor_surface_state.adb'):
        (work / name).write_text(read(ROOT / 'userspace/lib/compositor' / name))
    for kind, stop in [('caller', '   procedure retirePublicationBuffer\n'),
                       ('surface', '   procedure requestClose (target : Unsigned_64);')]:
        assert source.count(stop) == 1
        routines = source[source.index(start):source.index(stop)]
        template = read(HERE / f'source_{kind}_fixture.inc')
        assert template.count('@DESKTOP_ROUTINES@') == 1
        name = f'source_{kind}_tests'
        (work / f'{name}.adb').write_text(template.replace('@DESKTOP_ROUTINES@\n', routines))
        (work / 'check.gpr').write_text(f'''project Check is
 for Source_Dirs use (".");
 for Object_Dir use "obj";
 for Exec_Dir use ".";
 for Main use ("{name}.adb");
 package Compiler is
  for Default_Switches ("Ada") use ("-gnat2022", "-gnata", "-gnato", "-O2");
 end Compiler;
end Check;
''')
        subprocess.run(['gprbuild', '-q', '-p', '-P', str(work/'check.gpr')], check=True)
        subprocess.run([str(work/name)], check=True)
    for path, digest in inputs.items():
        assert hashlib.sha256(Path(path).read_bytes()).hexdigest() == digest, path
    (work/'inputs.json').write_text(json.dumps(inputs, indent=2)+'\n')
    print('PASS exact Desktop source-retirement callers:', work)


if __name__ == '__main__':
    main()
