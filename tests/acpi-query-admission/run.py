"""Hosted query admission policy and compile-only native input adapter checks."""
from pathlib import Path
import argparse, hashlib, json, os, re, resource, shutil, tempfile
from hosted_capture import capture
parser=argparse.ArgumentParser()
parser.add_argument('--proof',action='store_true')
parser.add_argument('--output-root',type=Path,default=Path(os.environ.get('TMPDIR',tempfile.gettempdir())))
options=parser.parse_args();here=Path(__file__).resolve().parent;root=here.parents[1]
resource.setrlimit(resource.RLIMIT_STACK,(64*1024*1024,)*2);resource.setrlimit(resource.RLIMIT_CORE,(0,0))
sha=lambda p:hashlib.sha256(p.read_bytes()).hexdigest()
out=Path(tempfile.mkdtemp(prefix='query-admission-',dir=options.output_root));print(out,flush=True)
source=out/'source';names=json.loads((here/'sources.json').read_text())
assert len(set(names))==len(names) and all(not Path(n).is_absolute() and '..' not in Path(n).parts for n in names)
pins={}
for name in names:
    src=root/name;assert not src.is_symlink();digest=sha(src);dst=source/name;dst.parent.mkdir(parents=True,exist_ok=True)
    shutil.copy2(src,dst);assert sha(dst)==sha(src)==digest;pins[name]=digest
(out/'inputs.json').write_text(json.dumps(pins,indent=2)+'\n')
for name in ('run.py','hosted_capture.py','sources.json'):shutil.copy2(here/name,out/name)
tools={n:Path(shutil.which(n)).resolve(strict=True) for n in ('gprbuild',)+(('gnatprove',) if options.proof else ())}
(out/'tools.json').write_text(json.dumps({n:{'path':str(p),'sha256':sha(p)} for n,p in tools.items()},indent=2)+'\n')
commands=[]
def run(command,label,seconds):
    command=[str(x) for x in command];result=capture(command,source,out/(label+'.log'),seconds)
    commands.append({'command':command,'result':result,'log':label+'.log','timeout_seconds':seconds})
    (out/'commands.json').write_text(json.dumps(commands,indent=2)+'\n');print(label,result,flush=True)
    if result!=0:
        print((out/(label+'.log')).read_text(),flush=True);raise SystemExit(1)
def project(name,flags,main=True):
    dirs=sorted({str((source/n).parent) for n in names});p=out/(name+'.gpr')
    text='project '+name.title()+' is\n for Source_Dirs use ('+', '.join('"'+d+'"' for d in dirs)+');\n'
    text+=' for Source_Files use ('+', '.join('"'+Path(n).name+'"' for n in names)+');\n'
    if main:text+=' for Main use ("query_admission_tests.adb");\n'
    text+=' for Object_Dir use "'+name+'/obj";\n for Exec_Dir use "'+name+'/bin";\n'
    text+=' package Compiler is\n for Default_Switches ("Ada") use ('+', '.join('"'+f+'"' for f in flags)+');\n end Compiler;\n end '+name.title()+';\n'
    p.write_text(text);return p
runtime={}
for profile,extra in [('checked',['-gnata','-gnato']),('release',['-gnatp'])]:
    p=project(profile,['-gnat2022','-O1','-gnatwa','-gnatwe']+extra)
    run([tools['gprbuild'],'-p','-j1','-P',p],profile+'-build',180)
    if profile=='checked':
        for unit in ('native_query_input_model.adb','acpi_native_query_input.adb'):
            run([tools['gprbuild'],'-p','-j1','-c','-u','-P',p,unit],unit+'-compile',180)
    binary=out/profile/'bin/query_admission_tests';run([binary],profile+'-runtime',30)
    match=re.fullmatch(r'Admission checks (\d+)\n',(out/(profile+'-runtime.log')).read_text());assert match
    runtime[profile]={'checks':int(match[1]),'binary_sha256':sha(binary)}
proof=None
if options.proof:
    p=project('proof',['-gnat2022'],False)
    run([tools['gnatprove'],'-P',p,'-u','acpi_query_admission.adb','--mode=all','--level=2','--timeout=5','-j1','--report=all'],'proof',180)
    summary=out/'proof/obj/gnatprove/gnatprove.out';shutil.copy2(summary,out/'gnatprove.out');text=summary.read_text()
    row=next(l for l in text.splitlines() if l.startswith('Total '));columns=[c.strip() for c in re.split(r'\s{2,}',row.strip())]
    assert len(columns)==6 and int(columns[1])>0 and columns[-2:]==['.','.']
    assert not re.search(r'flow analyzed \([1-9]|[1-9][0-9]* pragma Assume',text)
    diagnostics=re.findall(r'^.*: (?:low|medium|high|warning|error):.*$',(out/'proof.log').read_text(),re.M);assert not diagnostics,diagnostics
    proof={'reported_checks':int(columns[1]),'unproved':0,'justified':0,'scope':'ACPI_Query_Admission body and declared contracts under identity/reference/codec dependency contracts'}
assert all(sha(source/n)==digest==sha(root/n) for n,digest in pins.items())
complete={'inputs_unchanged':True,'runtime':runtime,'proof':proof,'native_adapter_compile_checks':2,'actual_syscall_executed':False,'native_validated':False,'hardware_validated':False,'full_goal_complete':False}
(out/'complete.json').write_text(json.dumps(complete,indent=2)+'\n');print(json.dumps(complete,indent=2),flush=True)
