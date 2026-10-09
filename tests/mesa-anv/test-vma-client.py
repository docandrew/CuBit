import argparse,hashlib,json,os,pathlib,runpy,subprocess,tempfile
assert os.environ.get('IN_NIX_SHELL')
parser=argparse.ArgumentParser(description='Hosted common ANV VA regression and negative mutation; not GPU execution')
parser.add_argument('build',type=pathlib.Path)
parser.add_argument('mesa_source',type=pathlib.Path)
args=parser.parse_args()
root=pathlib.Path(__file__).resolve().parents[2]
build=args.build.resolve()
mesa_source=args.mesa_source.resolve()
fixture=root/'tests/mesa-anv/vma-client-test.c'
source=mesa_source/'src/intel/vulkan/anv_device.c'
text=source.read_text()
start=text.index('static struct util_vma_heap *\nanv_vma_heap_for_flags(')
end=text.index('\nVkResult anv_AllocateMemory(',start)
extracted=text[start:end]
assert 'anv_vma_alloc(' in extracted and 'anv_vma_free(' in extracted
out=pathlib.Path(tempfile.mkdtemp(prefix='cubit-vma-client-',dir='/tmp'))
(out/'actual-vma.inc').write_text(extracted)
helpers=runpy.run_path(str(root/'tests/mesa-anv/test-native-memory-policy.py'))
entry,=[e for e in json.loads((build/'compile_commands.json').read_text()) if e['file'].endswith('/vulkan/anv_kmd_backend.c')]
cmd=helpers['compiler_command'](entry)
objects=[]
for src in [fixture,mesa_source/'src/util/vma.c']:
    obj=out/(src.stem+'.o')
    subprocess.run(cmd+['-UNDEBUG','-I'+str(out),'-c',str(src),'-o',str(obj)],cwd=entry['directory'],check=True)
    objects.append(str(obj))
binary=out/'test'
subprocess.run(['cc','-Wl,--gc-sections',*objects,'-o',str(binary)],check=True)
subprocess.run([str(binary)],check=True,timeout=60)
# Prove this fixture detects silently ignoring requested client addresses.
needle='if (client_address) {'
assert extracted.count(needle)==1
(out/'actual-vma.inc').write_text(extracted.replace(needle,'if (false) {'))
mutant=out/'mutant.o'
subprocess.run(cmd+['-UNDEBUG','-I'+str(out),'-c',str(fixture),'-o',str(mutant)],cwd=entry['directory'],check=True)
mutant_binary=out/'mutant'
subprocess.run(['cc','-Wl,--gc-sections',str(mutant),objects[1],'-o',str(mutant_binary)],check=True)
mutation=subprocess.run([str(mutant_binary)],capture_output=True,text=True,timeout=60)
assert mutation.returncode!=0 and 'Assertion' in mutation.stderr, mutation
(out/'mutation.stderr').write_text(mutation.stderr)
(out/'actual-vma.inc').write_text(extracted)
(out/'evidence.json').write_text(json.dumps({'source':str(source),'sha256':hashlib.sha256(source.read_bytes()).hexdigest(),'scope':'actual extracted common ANV functions and actual util/vma.c; hosted only','status':'PASS','ignored_client_address_mutation':'detected'},indent=2)+'\n')
print(out)
