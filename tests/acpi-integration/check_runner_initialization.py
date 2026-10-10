"""Original runner lifecycle regression; ACPICA oracle for forward buffer aliases."""
from pathlib import Path
import subprocess, json, hashlib, re, argparse, resource
root = Path(__file__).resolve().parents[2]
runner = root / 'tests/aml-core/build/table_runner'
output = Path(__file__).resolve().parent / 'runner-fixtures'
output.mkdir(exist_ok=True)
(output / 'report.json').unlink(missing_ok=True)
# Match the checked hosted harness's explicit stack budget.
required_stack = 64 * 1024 * 1024
_, hard_stack = resource.getrlimit(resource.RLIMIT_STACK)
if hard_stack != resource.RLIM_INFINITY and hard_stack < required_stack:
    raise RuntimeError('checked table_runner needs a 64 MiB hosted stack budget')
resource.setrlimit(resource.RLIMIT_STACK, (required_stack, hard_stack))
parser = argparse.ArgumentParser()
parser.add_argument('--tools', required=True, type=Path, help='Directory containing pinned iasl and acpiexec')
tools = parser.parse_args().tools.resolve()
results = []
def run(argv, stem):
    p = subprocess.run([str(a) for a in argv], text=True, capture_output=True, timeout=60)
    (output / (stem + '.stdout')).write_text(p.stdout)
    (output / (stem + '.stderr')).write_text(p.stderr)
    assert p.returncode == 0, (argv, p.returncode, p.stderr)
    return p
for revision in (1, 2):
    stem = f'alias{revision}'
    source = output / (stem + '.asl')
    source.write_text('''DefinitionBlock ("", "DSDT", REV, "CUBIT", "ALIAS", 1) {
 Name (PKG0, Package (2) {BUF0, BUF0})
 Name (BUF0, Buffer (1) {7})
 Method (MAIN, 0) {
   Store (0x2A, Index (BUF0, Zero))
   Return (DerefOf (Index (DerefOf (Index (PKG0, One)), Zero)))
 }
}'''.replace('REV', str(revision)))
    run([tools/'iasl', '-p', output/stem, source], stem+'-compile')
    oracle = run([tools/'acpiexec', '-b', 'execute MAIN', output/(stem+'.aml')], stem+'-oracle')
    assert re.search(r'\[Integer\]\s*=\s*0*2A\b', oracle.stdout), oracle.stdout
    actual = run([runner, output/(stem+'.aml'), 'MAIN', '--result-object'], stem+'-actual')
    assert actual.stdout.strip() == 'INTEGER 42' and not actual.stderr.strip(), actual
    results.append({'revision': revision, 'forward_alias': 42})
    # Missing/unsupported entries are intentionally encoded without an ASL
    # compiler resolving/rejecting the absent name before the service sees it.
    payload = bytes([8])+b'PKG0'+bytes([0x12,14,3])+b'FUTRMISSMTHD'
    payload += bytes([8])+b'FUTR'+bytes([0x0A,42])
    payload += bytes([0x14,8])+b'MTHD'+bytes([0,0xA4,0])
    data = bytearray(36)+payload
    data[:4] = b'DSDT'; data[4:8] = len(data).to_bytes(4,'little'); data[8] = revision
    data[10:16] = b'CUBIT '; data[16:24] = b'DIAGNOST'; data[9] = -sum(data)%256
    path=output/f'diagnostics{revision}.aml';path.write_bytes(data)
    expected='acpi member initialization: bound= 1 missing= 1 unsupported= 1'
    for attempt in (1,2):
        p=run([runner,path,'PKG0','--object'],f'diagnostics{revision}-object{attempt}')
        assert p.stdout.strip()=='PACKAGE 3\nINTEGER 42\nNULL\nNULL',p.stdout
        assert p.stderr.strip()==expected,p.stderr
    p=run([runner,path,'MTHD','--result-object'],f'diagnostics{revision}-method')
    assert p.stdout.strip()=='INTEGER 0' and p.stderr.strip()==expected,(p.stdout,p.stderr)
    results.append({'revision':revision,'diagnostics':{'bound':1,'missing':1,'unsupported':1},'repeat_runs':2})
sha=lambda p:hashlib.sha256(p.read_bytes()).hexdigest()
(output/'report.json').write_text(json.dumps({'status':'PASS','results':results,'runner_sha256':sha(runner),'tools':{str(tools/n):sha(tools/n) for n in ['iasl','acpiexec']},'artifacts':{p.name:sha(p) for p in output.iterdir() if p.is_file() and p.name!='report.json'}},indent=2)+'\n')
print('Runner initialization PASS: 2 ACPICA forward-alias comparisons and 6 diagnostic runs')
