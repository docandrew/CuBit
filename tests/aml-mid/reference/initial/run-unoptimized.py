from pathlib import Path
import subprocess,resource,signal,os,json,time,hashlib
p=Path(__file__).resolve().parent;t=Path('/nix/store/72fphm0m9k2f8zd066ayxkqi44ia9ql5-acpica-tools-20260408/bin');out=p/'unoptimized';out.mkdir();rows=[]
resource.setrlimit(resource.RLIMIT_AS,(128*1024*1024,)*2);resource.setrlimit(resource.RLIMIT_STACK,(64*1024*1024,)*2)
cases=[]
def case(group,name,body):cases.append((group,name,body))
for name,expr in [('string','Mid("ABCDE",1,3)'),('buffer','Mid(Buffer(){0x10,0,0xFE,0x33},1,2)'),('emptystring','Mid("",0,1)'),('emptybuffer','Mid(Buffer(){},0,1)'),('outside','Mid("ABC",3,1)'),('hugecount','Mid("ABC",1,Ones)'),('hugeindex','Mid("ABC",Ones,1)'),('zero','Mid("ABC",1,0)'),('integer','Mid(0x1122334455667788,1,Ones)')]:case('range',name,'Return('+expr+')')
for name,prep,expr in [('coerce','', 'Mid("ABCDE","1",Buffer(){2})'),('directindex','','Mid(Index(PKG0,0),1,2)'),('heldindex','Store(Index(PKG0,0),Local0)','Mid(Local0,1,2)'),('methodindex','','Mid(GETI(),1,2)'),('heldref','Store(RefOf(BUF0),Local0)','Mid(Local0,1,2)'),('deref','','Mid(DerefOf(RefOf(BUF0)),1,2)'),('package','','Mid(PKG0,0,1)'),('uninit','','Mid(Local0,0,1)')]:case('resolve',name,prep+' Return('+expr+')')
def bundle(exprs):
 return 'Store(Package('+str(len(exprs))+'){'+','.join('Zero' for x in exprs)+'},Local6) '+''.join('Store('+x+',Index(Local6,'+str(i)+')) ' for i,x in enumerate(exprs))+'Return(Local6)'
for name,target in [('namedint','INT0'),('namedstr','STR0'),('namedbuf','BUF1'),('member','Index(PKG0,0)'),('byte','Index(BUF1,0)')]:case('targets',name,'Store(Mid("ABCD",1,2,'+target+'),Local0) '+bundle(['Local0',target,'ObjectType('+target+')','MARK']))
case('targets','same','Store(Mid(BUF0,1,2,BUF0),Local0) Store(0x77,Index(Local0,0)) '+bundle(['Local0','BUF0']))
case('targets','argref','Return(HELP(RefOf(STR0)))')
case('order','once','Store(Mid(SRC0(),IDX0(),LEN0(),BUF1),Local0) '+bundle(['Local0','BUF1','MARK']))
case('order','badindex','Mid(SRC0(),BAD0(),LEN0(),BUF1) Return(MARK)')
case('order','badsource','Mid(BAD0(),IDX0(),LEN0(),BUF1) Return(MARK)')
common='Name(MARK,Zero) Name(INT0,Zero) Name(STR0,"QQQQ") Name(BUF0,Buffer(){0x10,0x20,0x30,0x40}) Name(BUF1,Buffer(){1,2,3,4}) Name(PKG0,Package(){Buffer(){0x41,0x42,0x43,0x44}}) Method(GETI,0){Return(Index(PKG0,0))} Method(SRC0,0){Add(MARK,1,MARK) Return("ABCDE")} Method(IDX0,0){Add(MARK,10,MARK) Return(One)} Method(LEN0,0){Add(MARK,100,MARK) Return(2)} Method(BAD0,0){Add(MARK,1000,MARK) Return(Package(){One})} Method(HELP,1){Store(Mid(Buffer(){0x41,0x42,0x43},1,2,Arg0),Local0) '+bundle(['Local0','STR0','ObjectType(STR0)'])+'} Method(SEEN,0){Return(MARK)}'
def run(tag,cmd):
 start=time.monotonic()
 with (out/(tag+'.out')).open('wb') as o,(out/(tag+'.err')).open('wb') as e:
  c=subprocess.Popen(list(map(str,cmd)),stdout=o,stderr=e,start_new_session=True)
  try:rc=c.wait(5)
  except subprocess.TimeoutExpired:os.killpg(c.pid,signal.SIGKILL);c.wait();rc='timeout'
 return dict(returncode=rc,seconds=time.monotonic()-start)
for rev in (1,2):
 for group,name,body in cases:
  if group!='range' and name not in ('coerce','namedint','namedstr','namedbuf','member','byte'):continue
  tag=name+str(rev);f=out/(tag+'.asl');f.write_text('DefinitionBlock("","DSDT",'+str(rev)+',"CUBIT","MIDPROBE",1){'+common+' Method(TEST,0){'+body+'}}')
  result=run(tag+'-compile',[t/'iasl','-oa','-p',out/tag,f]);row=dict(group=group,case=name,revision=rev,compile=result)
  if result['returncode']==0:row['execution']=run(tag+'-execute',[t/'acpiexec','-b','execute TEST; execute SEEN',out/(tag+'.aml')])
  rows.append(row);(out/'outcomes.json').write_text(json.dumps(rows,indent=2));print(row,flush=True)
(out/'audit.json').write_text(json.dumps({str(f.relative_to(out)):hashlib.sha256(f.read_bytes()).hexdigest() for f in out.iterdir() if f.is_file()},indent=2))
(p/'tools.json').write_text(json.dumps({str(t/n):hashlib.sha256((t/n).read_bytes()).hexdigest() for n in ('iasl','acpiexec')},indent=2))
