from pathlib import Path
import subprocess,resource,json,os,signal
p=Path(__file__).resolve().parent;t=Path('/nix/store/72fphm0m9k2f8zd066ayxkqi44ia9ql5-acpica-tools-20260408/bin/iasl');out=p/'index-disassembly';out.mkdir();rows=[]
resource.setrlimit(resource.RLIMIT_AS,(128*1024*1024,)*2);resource.setrlimit(resource.RLIMIT_STACK,(64*1024*1024,)*2)
for group in ('index-observation',):
 for f in sorted((p/group).glob('*.aml')):
  tag=group+'-'+f.stem
  with (out/(tag+'.out')).open('wb') as o,(out/(tag+'.err')).open('wb') as e:
   c=subprocess.Popen([str(t),'-p',str(out/tag),'-d',str(f)],stdout=o,stderr=e,start_new_session=True)
   try:rc=c.wait(5)
   except subprocess.TimeoutExpired:os.killpg(c.pid,signal.SIGKILL);c.wait();rc='timeout'
  row=dict(source=str(f.relative_to(p)),returncode=rc)
  if rc==0:
   s=(out/(tag+'.dsl')).read_text();start=s.index('Method (TEST,');body=s[start:];row['TEST_has_Mid']='Mid (' in body
   if f.stem.startswith('argref'):
    a=s.index('Method (HELP,');b=s.index('Method (SEEN,');row['HELP_has_Mid']='Mid (' in s[a:b]
  rows.append(row)
(out/'outcomes.json').write_text(json.dumps(rows,indent=2));print('disassembled',len(rows),'failed',sum(x['returncode']!=0 for x in rows))
