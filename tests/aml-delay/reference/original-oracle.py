from pathlib import Path
import subprocess,resource,signal,os,json,time,hashlib
p=Path(__file__).resolve().parent;t=Path('/nix/store/72fphm0m9k2f8zd066ayxkqi44ia9ql5-acpica-tools-20260408/bin');out=p/'delay-oracle';out.mkdir();rows=[]
resource.setrlimit(resource.RLIMIT_AS,(1024*1024*1024,)*2);resource.setrlimit(resource.RLIMIT_STACK,(64*1024*1024,)*2)
cases=[('integer','', 'One'),('string','','"0"'),('buffer','','Buffer(){0}'),('emptybuffer','','Buffer(){}'),('named','','NVAL'),('refof','','RefOf(NVAL)'),('heldref','Store(RefOf(NVAL),Local0)','Local0'),('packageindex','','Index(PVAL,0)'),('heldpackage','Store(Index(PVAL,0),Local0)','Local0'),('byteindex','','Index(BVAL,0)'),('heldbyte','Store(Index(BVAL,0),Local0)','Local0'),('deref','','DerefOf(RefOf(NVAL))'),('uninit','','Local0'),('emptyindex','','Index(EMP0,0)')]
def run(tag,cmd):
 with (out/(tag+'.out')).open('wb') as o,(out/(tag+'.err')).open('wb') as e:
  c=subprocess.Popen(list(map(str,cmd)),stdout=o,stderr=e,start_new_session=True)
  try:rc=c.wait(30)
  except subprocess.TimeoutExpired:os.killpg(c.pid,signal.SIGKILL);c.wait();rc='timeout'
 return rc
for rev in (1,2):
 for name,prep,expr in cases:
  tag=name+str(rev);f=out/(tag+'.asl');f.write_text('DefinitionBlock("","DSDT",'+str(rev)+',"CUBIT","DELAY",1){ Name(NVAL,Zero) Name(BVAL,Buffer(){0}) Name(PVAL,Package(){Zero}) Name(EMP0,Package(1){}) Method(TEST,0){'+prep+' Sleep('+expr+') Return(0x55)} Method(TST1,0){'+prep+' Stall('+expr+') Return(0x66)}}')
  rc=run(tag+'-compile',[t/'iasl','-p',out/tag,f]);row=dict(case=name,revision=rev,compile=rc)
  if rc==0:row['execution']=run(tag+'-execute',[t/'acpiexec','-b','execute TEST; execute TST1',out/(tag+'.aml')])
  rows.append(row);(out/'outcomes.json').write_text(json.dumps(rows,indent=2));print(row,flush=True)
(out/'audit.json').write_text(json.dumps({str(f.relative_to(out)):hashlib.sha256(f.read_bytes()).hexdigest() for f in out.iterdir() if f.is_file()},indent=2))
