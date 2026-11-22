from pathlib import Path
import subprocess,resource,signal,os,json,time,hashlib
p=Path(__file__).resolve().parent;t=Path('/nix/store/72fphm0m9k2f8zd066ayxkqi44ia9ql5-acpica-tools-20260408/bin');out=p/'oracle';out.mkdir();rows=[]
resource.setrlimit(resource.RLIMIT_AS,(128*1024*1024,)*2);resource.setrlimit(resource.RLIMIT_STACK,(64*1024*1024,)*2)
common='Name(MARK,Zero) Name(BUF0,Buffer(){0,0,0,0}) Method(SRC0,0){Add(MARK,1,MARK) Return("ABCD")} Method(SRCP,0){Add(MARK,1,MARK) Return(Package(){One})} Method(IDX0,0){Add(MARK,10,MARK) Return(Zero)} Method(IDXP,0){Add(MARK,10,MARK) Return(Package(){One})} Method(IDXE,0){Add(MARK,10,MARK) Return(Buffer(){})} Method(LEN0,0){Add(MARK,100,MARK) Return(One)} Method(LENE,0){Add(MARK,100,MARK) Return(Buffer(){})} Method(LENP,0){Add(MARK,100,MARK) Return(Package(){One})} Method(TGT0,0){Add(MARK,10000,MARK) Return(Zero)} Method(SEEN,0){Return(MARK)}'
cases=[('ordering','sourcebad','Mid(SRCP(),IDX0(),LEN0(),Index(BUF0,TGT0())) Return(MARK)'),('ordering','indexbad','Mid(SRC0(),IDXP(),LEN0(),Index(BUF0,TGT0())) Return(MARK)'),('ordering','countempty','Mid(SRCP(),IDXP(),LENE(),Index(BUF0,TGT0())) Return(MARK)'),('ordering','countpackage','Mid(SRC0(),IDXE(),LENP(),Index(BUF0,TGT0())) Return(MARK)')]
def run(tag,cmd):
 start=time.monotonic()
 with (out/(tag+'.out')).open('wb') as o,(out/(tag+'.err')).open('wb') as e:
  c=subprocess.Popen(list(map(str,cmd)),stdout=o,stderr=e,start_new_session=True)
  try:rc=c.wait(5)
  except subprocess.TimeoutExpired:os.killpg(c.pid,signal.SIGKILL);c.wait();rc='timeout'
 return dict(returncode=rc,seconds=time.monotonic()-start)
for rev in (1,2):
 for group,name,body in cases:
  tag=name+str(rev);f=out/(tag+'.asl');f.write_text('DefinitionBlock("","DSDT",'+str(rev)+',"CUBIT","MIDPROBE",1){'+common+' Method(TEST,0){'+body+'}}')
  result=run(tag+'-compile',[t/'iasl','-oa','-p',out/tag,f]);row=dict(group=group,case=name,revision=rev,compile=result)
  if result['returncode']==0:row['execution']=run(tag+'-execute',[t/'acpiexec','-b','execute TEST; execute SEEN',out/(tag+'.aml')])
  rows.append(row);(out/'outcomes.json').write_text(json.dumps(rows,indent=2));print(row,flush=True)
(out/'audit.json').write_text(json.dumps({str(f.relative_to(out)):hashlib.sha256(f.read_bytes()).hexdigest() for f in out.iterdir() if f.is_file()},indent=2))
(p/'tools.json').write_text(json.dumps({str(t/n):hashlib.sha256((t/n).read_bytes()).hexdigest() for n in ('iasl','acpiexec')},indent=2))
