"""Hosted controller-only method _DEP observation; no native or hardware validation."""
from pathlib import Path
import argparse, hashlib, json, os, re, resource, shutil, tempfile
from hosted_capture import capture
parser=argparse.ArgumentParser();parser.add_argument('--output-root',type=Path,default=Path(os.environ.get('TMPDIR',tempfile.gettempdir())))
options=parser.parse_args();here=Path(__file__).resolve().parent;root=here.parents[1];sha=lambda p:hashlib.sha256(p.read_bytes()).hexdigest()
resource.setrlimit(resource.RLIMIT_STACK,(64*1024*1024,)*2);resource.setrlimit(resource.RLIMIT_CORE,(0,0))
out=Path(tempfile.mkdtemp(prefix='evaluated-dependencies-',dir=options.output_root));print(out,flush=True)
source=out/'source';names=json.loads((here/'sources.json').read_text());assert len(names)==len(set(names)) and all(not Path(n).is_absolute() and '..' not in Path(n).parts for n in names)
pins={}
for n in names:
    src=root/n;assert not src.is_symlink();digest=sha(src);dst=source/n;dst.parent.mkdir(parents=True,exist_ok=True);shutil.copy2(src,dst);assert sha(dst)==sha(src)==digest;pins[n]=digest
for n in ('run.py','hosted_capture.py','sources.json'):shutil.copy2(here/n,out/n)
(out/'inputs.json').write_text(json.dumps(pins,indent=2)+'\n')
files=sorted(n for n in pins if n.endswith(('.ads','.adb')));assert len(set(Path(n).name for n in files))==len(files)
tools={'gprbuild':Path(shutil.which('gprbuild')).resolve(strict=True)}
for name in ('iasl','acpiexec'):
    found=shutil.which(name,path=os.environ.get('ACPICA_BIN',os.environ.get('PATH','')))
    if found is None:raise SystemExit('Missing '+name+'; select ACPICA_BIN or PATH')
    tools[name]=Path(found).resolve(strict=True)
(out/'tools.json').write_text(json.dumps({n:{'path':str(p),'sha256':sha(p)} for n,p in tools.items()},indent=2)+'\n')
commands=[]
def run(command,label,seconds):
    command=[str(x) for x in command];result=capture(command,source,out/(label+'.log'),seconds)
    commands.append({'command':command,'result':result,'log':label+'.log','timeout_seconds':seconds})
    (out/'commands.json').write_text(json.dumps(commands,indent=2)+'\n');print(label,result,flush=True)
    if result!=0:
        print((out/(label+'.log')).read_text(),flush=True);raise SystemExit(1)
oracle={}
for revision in (1,2):
    prefix=out/f'dependencies-{revision}'
    run([tools['iasl'],'-ve','-r',revision,'-p',prefix,source/'tests/acpi-evaluated-dependencies/dependencies.asl'],f'iasl-{revision}',5)
    data=prefix.with_suffix('.aml').read_bytes();assert data[8]==revision and sum(data)%256==0
    batch='evaluate HITS; execute DYN0._DEP; evaluate HITS'
    run([tools['acpiexec'],'-l','-di','-dr','-b',batch,prefix.with_suffix('.aml')],f'acpica-{revision}',5)
    text=(out/f'acpica-{revision}.log').read_text()
    assert not re.search(r'Evaluation of .* failed|Aborting method|Could not resolve symbol',text)
    integers=[int(n,16) for n in re.findall(r'^\s*\[Integer\] = ([0-9A-F]+)',text,re.M)]
    refs=re.findall(r'Name ([A-Z0-9_]{4}) Device',text);counts=[int(n) for n in re.findall(r'\[Package\] Contains ([0-9]+) Elements:',text)]
    assert integers==[0,1] and refs==['GPO0','I2C0','GPO0'] and counts==[3]
    oracle[revision]={'integers':integers,'device_references':refs,'package_counts':counts,'aml_sha256':sha(prefix.with_suffix('.aml')),'shutdown_cache_note':'Outstanding cache allocations' in text}
runtime={}
for profile,extra in [('checked',['-gnata','-gnato']),('release',['-gnatp'])]:
    p=out/(profile+'.gpr');dirs=sorted({str((source/n).parent) for n in files})
    text='project '+profile.title()+' is\n for Source_Dirs use ('+', '.join('"'+d+'"' for d in dirs)+');\n'
    text+=' for Source_Files use ('+', '.join('"'+Path(n).name+'"' for n in files)+');\n'
    text+=' for Main use ("evaluated_dependency_tests.adb");\n for Object_Dir use "'+profile+'/obj";\n for Exec_Dir use "'+profile+'/bin";\n'
    text+=' package Compiler is\n for Default_Switches ("Ada") use ('+', '.join('"'+f+'"' for f in ['-gnat2022','-O1','-gnatwa','-gnatwe']+extra)+');\n end Compiler;\n end '+profile.title()+';\n'
    p.write_text(text);run([tools['gprbuild'],'-p','-j1','-P',p],profile+'-build',180)
    runtime[profile]={}
    for revision in (1,2):
        label=f'{profile}-runtime-{revision}';binary=out/profile/'bin/evaluated_dependency_tests'
        run([binary,out/f'dependencies-{revision}.aml'],label,30)
        match=re.fullmatch(r'Evaluated dependency checks (\d+)\n',(out/(label+'.log')).read_text());assert match
        runtime[profile][revision]={'checks':int(match[1]),'binary_sha256':sha(binary)}
assert all(sha(source/n)==digest==sha(root/n) for n,digest in pins.items())
summary={'inputs_unchanged':True,'runtime':runtime,'acpica_reference':oracle,'new_proof':False,'native_validated':False,'hardware_validated':False,'full_goal_complete':False}
(out/'complete.json').write_text(json.dumps(summary,indent=2)+'\n');print(json.dumps(summary,indent=2),flush=True)
