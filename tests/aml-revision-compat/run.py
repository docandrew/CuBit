from pathlib import Path
import json,hashlib,subprocess,os,shutil,tempfile,sys
suite=Path(__file__).resolve().parent;root=suite.parents[1];sha=lambda p:hashlib.sha256(p.read_bytes()).hexdigest();catalog=json.loads((suite/'sources.json').read_text());pins={str(root/p):h for p,h in catalog['inputs'].items()};pins.update({str(p):sha(p)for p in [Path(__file__),suite/'sources.json',suite/'run.sh']})
def verify():
 for p,h in pins.items():assert sha(Path(p))==h,p
for name in ['gnatprove','gprbuild']:
 p=Path(shutil.which(name)).resolve();assert str(p).startswith('/nix/store/');pins[str(p)]=sha(p)
verify()
if sys.argv[1:]!=['--prove']:
 for rel,h in catalog.get('accepted_prior_proof',{}).get('source_pins',{}).items():
  assert sha(root/rel)==h, 'Accepted proof inputs changed; update the catalog and run --prove: '+rel
if sys.argv[1:]==['--validate-only']:print(json.dumps({'validated':True,'inputs':len(catalog['inputs']),'executed':False}));raise SystemExit(0)
assert sys.argv[1:] in [[],['--prove']] and os.environ.get('TMPDIR')=='/home/doc/cubit-build-tmp';out=Path(tempfile.mkdtemp(prefix='aml-revision-compat-',dir=os.environ['TMPDIR']));rows=[];proofs=[]
def save(accepted=False):
 (out/'RESULT.json').write_text(json.dumps({'accepted':accepted,'pins':pins,'commands':rows,'proofs':proofs,'proofs_reused':not bool(proofs),'accepted_prior_proof':catalog.get('accepted_prior_proof'),'scope':'Actual Read_Integer decoder exact Revision/error framing and AoRTE/termination only, with standard Interfaces.Shift_Left effects/AoRTE trust. Not full executor/service/native/ASLTS proof.'},indent=2)+'\n')
def run(label,args,seconds,env=None):
 verify();x=subprocess.run(['timeout','--kill-after=2s',str(seconds)+'s',*map(str,args)],cwd=out,env=env,capture_output=True,text=True,timeout=seconds+3);p=out/(label+'.log');p.write_text(x.stdout+x.stderr);rows.append({'label':label,'command':list(map(str,args)),'status':x.returncode,'log_sha256':sha(p)});save();assert x.returncode==0,(label,p);verify();return p.read_text()
# Accepted unchanged decoder proofs are reused by default. Explicit --prove
# is for edited proof inputs or an operator-requested new proof run.
proof_project=out/'proof.gpr';proof_project.write_text('project Proof is\n for Source_Dirs use ("'+str(root/'userspace/services/acpi/lib')+'","'+str(root/'shared/firmware')+'"); for Object_Dir use external("ACPI_REVISION_PROOF_OBJECTS"); package Compiler is for Default_Switches("Ada")use("-gnat2022");end Compiler;\nend Proof;\n')
for level in ([1,2] if sys.argv[1:]==['--prove'] else []):
 obj=out/('proof-'+str(level));env={**os.environ,'ACPI_REVISION_PROOF_OBJECTS':str(obj)};run('proof-'+str(level),['gnatprove','-P',proof_project,'-u','aml_decode.adb','--limit-subp=aml_decode.adb:6','--mode=all','--level='+str(level),'--timeout=5','--prover=cvc5','-j2','--report=all','--checks-as-errors=on'],180,env);p=obj/'gnatprove/aml_decode.spark';d=json.loads(p.read_text());assert d['stop_reason']=='STOP_REASON_NONE'and set(d['spark'].values())=={'all'};assert not d['pragma_assume']and not d['skip_proof']and not d['skip_flow_proof'];entity=next(int(k)for k,v in d['entities'].items()if v['name']=='AML_Decode.Read_Integer');assert len(d['flow'])==1 and len(d['proof'])==17 and all(x['severity']=='info'and x['entity']==entity for x in d['flow']+d['proof']);claims=[]
 for claim in d['assumptions']:
  for q in claim['assumptions']:
   e=next(v for k,v in d['entities'].items()if int(k)==q['arg']);assert e['name']=='Interfaces.Shift_Left'and q['predicate']in ['CLAIM_EFFECTS','CLAIM_AORTE'];claims.append(q['predicate'])
 assert set(claims)=={'CLAIM_EFFECTS','CLAIM_AORTE'};pins[str(p)]=sha(p);proofs.append({'level':level,'checks':18,'flow':1,'prover':17,'result':str(p),'standard_intrinsic_trust':'Interfaces.Shift_Left effects/AoRTE','unproved':0});save()
for profile in ['checked','release']:
 flags=['-O1','-gnat2022','-gnatwa','-gnatwe']+(['-gnata','-gnato']if profile=='checked'else['-gnatp']);project=out/(profile+'.gpr');project.write_text('project '+profile+' is\n for Source_Dirs use ("'+str(root/'userspace/services/acpi/lib')+'","'+str(root/'shared/firmware')+'","'+str(root/'tests/aml-core')+'"); for Main use ("decode_tests.adb"); for Object_Dir use "'+profile+'-obj"; for Exec_Dir use "'+profile+'-bin"; package Compiler is for Default_Switches("Ada")use('+','.join(json.dumps(f)for f in flags)+');end Compiler;\nend '+profile+';\n');run(profile+'-build',['gprbuild','-q','-p','-j2','-P',project],60)
 for p in(out/(profile+'-obj')).glob('*.ali'):
  if p.name.startswith('b__'):continue
  lines=p.read_text().splitlines();assert all('A '+f in lines for f in flags if f.startswith('-gnat'));pins[str(p)]=sha(p)
 binary=out/(profile+'-bin/decode_tests');pins[str(binary)]=sha(binary);log=run(profile+'-tests',[binary],10);assert'AML-DECODE-CHECK: PASS 789825'in log
verify();save(True);print(json.dumps({'accepted':True,'out':str(out),'proof_checks_per_level':18,'levels':([1,2] if sys.argv[1:]==['--prove'] else []),'unchanged_proofs_reused':sys.argv[1:]==[],'checked_decode_checks':789825,'release_decode_checks':789825,'standard_intrinsic_trust':True}))
