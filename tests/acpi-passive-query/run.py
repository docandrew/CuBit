"""Hosted passive ACPI queries; disjoint outputs, no native/kernel/image build."""
from pathlib import Path
import argparse, hashlib, json, os, re, resource, shutil, tempfile
from hosted_capture import capture
args = argparse.ArgumentParser()
args.add_argument('--proof', action='store_true', help='also prove concrete loan/session/pump model')
args.add_argument('--proof-only', action='store_true')
args.add_argument('--output-root', type=Path, default=Path(os.environ.get('TMPDIR', tempfile.gettempdir())))
options = args.parse_args()
here = Path(__file__).resolve().parent
root = here.parents[1]
resource.setrlimit(resource.RLIMIT_STACK, (64*1024*1024,)*2)
resource.setrlimit(resource.RLIMIT_CORE, (0,0))
sha = lambda p: hashlib.sha256(p.read_bytes()).hexdigest()
names = json.loads((here/'sources.json').read_text())
assert len(names) == len(set(names)) and all(not Path(n).is_absolute() and '..' not in Path(n).parts for n in names)
options.output_root.mkdir(parents=True, exist_ok=True)
out = Path(tempfile.mkdtemp(prefix='acpi-passive-query-',dir=options.output_root)); print(out,flush=True)
snapshot = out/'source'
pins = {}
for name in names + ['tests/acpi-passive-query/fixtures/passive.asl']:
    src = root/name
    assert not src.is_symlink()
    digest = sha(src); dst = snapshot/name; dst.parent.mkdir(parents=True,exist_ok=True)
    shutil.copy2(src,dst); assert sha(src) == sha(dst) == digest; pins[name] = digest
if options.proof or options.proof_only:
    for name in ('query_pump_proof_model.ads','query_pump_proof_model.adb'):
        rel = 'tests/acpi-passive-query/proof/'+name; src = root/rel; dst = snapshot/rel
        dst.parent.mkdir(parents=True,exist_ok=True); shutil.copy2(src,dst); pins[rel] = sha(src); assert sha(dst) == pins[rel]
assert all(sha(root/name)==digest for name,digest in pins.items())
(out/'inputs.json').write_text(json.dumps(pins,indent=2)+'\n')
for name in ('run.py','hosted_capture.py','sources.json'): shutil.copy2(here/name,out/name)
tools = {}
for name in ('gprbuild','gnatbind','gnatlink','iasl','acpiexec') + (('gnatprove',) if options.proof or options.proof_only else ()):
    search_path = os.environ.get('PATH', '')
    if name in ('iasl', 'acpiexec') and os.environ.get('ACPICA_BIN'):
        search_path = os.environ['ACPICA_BIN']
    tool = shutil.which(name, path=search_path)
    if tool is None: raise SystemExit('Missing '+name+'; run inside nix develop with ACPICA installed')
    tools[name] = Path(tool).resolve(strict=True)
(out/'tools.json').write_text(json.dumps({n:{'path':str(p),'sha256':sha(p)} for n,p in tools.items()},indent=2)+'\n')
commands=[]
def run(command,label,seconds):
    command=[str(x) for x in command]; result=capture(command,snapshot,out/(label+'.log'),seconds)
    commands.append({'command':command,'result':result,'log':label+'.log','timeout_seconds':seconds})
    (out/'commands.json').write_text(json.dumps(commands,indent=2)+'\n'); print(label,result,flush=True)
    if result != 0:
        print((out/(label+'.log')).read_text(),flush=True); raise SystemExit(1)
def project(name,files,flags,main=False):
    dirs=sorted({str((snapshot/f).parent) for f in files}); p=out/(name+'.gpr')
    text='project '+name.title()+' is\n for Source_Dirs use ('+', '.join('"'+d+'"' for d in dirs)+');\n'
    text+=' for Source_Files use ('+', '.join('"'+Path(f).name+'"' for f in files)+');\n'
    if main: text+=' for Main use ("passive_member_tests.adb");\n'
    text+=' for Object_Dir use "'+name+'/obj";\n for Exec_Dir use "'+name+'/bin";\n'
    text+=' package Compiler is\n for Default_Switches ("Ada") use ('+', '.join('"'+f+'"' for f in flags)+');\n end Compiler;\nend '+name.title()+';\n'
    p.write_text(text);return p
runtime={};oracle={}
if not options.proof_only:
    fixture=snapshot/'tests/acpi-passive-query/fixtures/passive.asl'
    batch='evaluate DEV0._HID; evaluate DEV0._CID; evaluate DEV0._UID; evaluate DEV0._ADR; evaluate DEV0._CRS; evaluate DEV0._STA; evaluate ZSTA._STA; evaluate ST32._STA; evaluate ALIA._STA; evaluate DUPL._HID; evaluate DUPL._CID; evaluate MCID._CID; evaluate LCID._CID; evaluate ACID._CID; evaluate HITS; execute METH._STA; evaluate HITS'
    for revision in (1,2):
        prefix=out/('passive-'+str(revision));run([tools['iasl'],'-ve','-r',revision,'-p',prefix,fixture],f'iasl-{revision}',5)
        aml=prefix.with_suffix('.aml'); data=aml.read_bytes()
        assert data[8]==revision and int.from_bytes(data[4:8],'little')==len(data) and sum(data)%256==0
        run([tools['acpiexec'],'-l','-di','-dr','-b',batch,aml],f'acpica-{revision}',5)
        text=(out/f'acpica-{revision}.log').read_text()
        assert not re.search(r'Evaluation of .* failed|Aborting method|Could not resolve symbol',text)
        values=[int(v,16) for v in re.findall(r'^\s*\[Integer\] = ([0-9A-F]+)',text,re.M)]
        assert values==[0x500CD041,0x10000,15,0,0x80000001,9,0x500CD041,0,15,1]
        assert all(s in text for s in ('INTC1057','ELAN06FA','PCI\\\\VEN_1234&DEV_ABCD','COMPLETE_IDENTIFIER_FINAL_Z','86 09 00 01 00 00 00 FD 00 10 00 00 79 00'))
        oracle[revision]={'integers':values,'aml_sha256':sha(aml),'shutdown_cache_note':'Outstanding cache allocations' in text}
    for profile,flags in [('checked',['-gnata','-gnato']),('release',['-gnatp'])]:
        p=project(profile,names,['-gnat2022','-O1','-gnatwa','-gnatwe']+flags,True)
        run([tools['gprbuild'],'-p','-j1','-P',p],profile+'-build',180)
        if profile=='checked':
            for unit in ('grants','replies','polling'):
                run([tools['gprbuild'],'-p','-j1','-c','-u','-P',p,'acpi_runtime_query_'+unit+'.adb'],'runtime-'+unit+'-compile',180)
        runtime[profile]={}
        for revision in (1,2):
            label=profile+'-runtime-'+str(revision);binary=out/profile/'bin/passive_member_tests'
            run([binary,out/f'passive-{revision}.aml'],label,30)
            match=re.fullmatch(r'Passive checks (\d+)\n',(out/(label+'.log')).read_text()); assert match
            runtime[profile][revision]={'checks':int(match[1]),'binary_sha256':sha(binary)}
    assert len({r['checks'] for rows in runtime.values() for r in rows.values()})==1
proof=None
if options.proof or options.proof_only:
    pure=[n for n in names if re.fullmatch(r'shared/acpi/acpi_(protocol\.ads|passive_query(?:-pages|-replies|-resource_records)?\.ad[bs])',n) or re.search(r'/(acpi_observer_loans|acpi_query_sessions|acpi_query_pump)\.ad[bs]$',n)]
    assert len(pure)==15
    pure += ['tests/acpi-passive-query/proof/query_pump_proof_model.'+suffix for suffix in ('ads','adb')]
    p=project('proof',pure,['-gnat2022'])
    run([tools['gnatprove'],'-P',p,'-u','query_pump_proof_model.adb','--mode=all','--level=2','--timeout=5','-j1','--report=all'],'proof',180)
    summary=out/'proof/obj/gnatprove/gnatprove.out'; shutil.copy2(summary,out/'gnatprove.out'); text=summary.read_text()
    total=next(l for l in text.splitlines() if l.startswith('Total ')).strip().split('  ')
    columns=[c.strip() for c in total if c.strip()];assert len(columns)==6 and columns[-2:]==['.','.'] and int(columns[1])>0
    assert 'and not proved' not in text and not re.search(r'flow analyzed \([1-9]|[1-9][0-9]* pragma Assume',text)
    diagnostics=re.findall(r'^.*: (?:low|medium|high|warning|error):.*$',(out/'proof.log').read_text(),re.M)
    assert len(diagnostics)==12 and all(re.fullmatch(r'query_pump_proof_model.ads:\d+:\d+: warning: "Owner" is not modified, could be IN',d) for d in diagnostics)
    proof={'scope':'concrete loan/session/pump safety and declared contracts, model callbacks/dependency contracts','checks':int(columns[1]),'model_mode_warnings':diagnostics,'unproved':0}
assert all(sha(snapshot/name)==digest for name,digest in pins.items())
assert all(sha(root/name)==digest for name,digest in pins.items())
(out/'complete.json').write_text(json.dumps({'inputs_unchanged':True,'runtime':runtime,'acpica_reference':oracle,'proof':proof,'actual_syscall_executed':False,'native_validated':False,'hardware_validated':False},indent=2)+'\n')
print(json.dumps({'runtime':runtime,'proof':proof},indent=2),flush=True)
