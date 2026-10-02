"""Generate isolated fault variants from an authenticated copy of metricsvc.
No production collector source is changed. Run in Nix under the build lock.
"""
from pathlib import Path
import hashlib,json,subprocess,sys
root=Path(__file__).resolve().parents[3]
mode=sys.argv[1]
assert mode in ('envelope','row','stall','full')
d=Path(__file__).parent/'build'/mode
src=d/'source';src.mkdir(parents=True,exist_ok=True)
files=list((root/'userspace/services/metricsvc').glob('*.ad?'))
manifest=root/'userspace/services/metricsvc/manifest.ccl';files.append(manifest)
hashes={str(p):hashlib.sha256(p.read_bytes()).hexdigest() for p in files}
for p in files:(src/p.name).write_bytes(p.read_bytes())
assert all(hashlib.sha256(p.read_bytes()).hexdigest()==hashes[str(p)] for p in files)
(d/'inputs.json').write_text(json.dumps(hashes,indent=2))
p=src/'main.adb';s=p.read_text();needle='   Store : Metric_Store.Store;';assert s.count(needle)==1
s=s.replace(needle,needle+'\n   Nonempty_Queries : Natural := 0;')
needle='                           declare\n                              Shared : Summary_Page'
assert s.count(needle)==1
injection='                           if Written > 0 then\n                              Nonempty_Queries := Nonempty_Queries + 1;\n                              if Nonempty_Queries > 12 then\n                                 debugPrint ("TEST: FAIL observer queried after fault" & ASCII.LF);\n                              end if;\n                           end if;\n                           if Nonempty_Queries = 12 then\n                              debugPrint ("TEST: viewer fault injected MODE" & ASCII.LF);\n                              ACTION\n                           end if;\n'
action={'full':'null;', 'row':'Rows (0) (Row_Flags) := 4;', 'envelope':'null;',
        'stall':'Ignore := syscall (SYSCALL_SLEEP, 3000);'}[mode]
if mode=='full':
    injection="""                           if Written > 0 then
                              for I in Row_Index loop
                                 Rows (I) := Rows (0);
                                 Rows (I) (Row_Key) := Unsigned_64 (I + 1);
                              end loop;
                              Written := Rows_Per_Page;
                              Next := Metric_Store.Series_Slots;
                              debugPrint ("TEST: full summary page rows=16" & ASCII.LF);
                           end if;
"""
s=s.replace(needle,injection.replace('MODE',mode).replace('ACTION',action)+needle)
needle='         Ignore := reply (From, Response);';assert s.count(needle)==1
extra='         if Known and then Op = Query_Summaries and then Nonempty_Queries = 12 then\n            ACTION\n            debugPrint ("TEST: viewer fault reply MODE" & ASCII.LF);\n         end if;\n'
action='Response.words (3) := 1;' if mode=='envelope' else 'null;'
s=s.replace(needle,extra.replace('MODE',mode).replace('ACTION',action)+needle);p.write_text(s)
base=(root/'tests/compositor/metrics-fault/fault.gpr').read_text()
base=base.replace('Desktop_Metrics_Fault','Observatory_Fault').replace('"build"','"obj"').replace('(".", "build/generated")','("source")').replace('desktop-metrics-fault.svc','metrics.svc')
base=base.replace('   for Main use', '   for Exec_Dir use ".";\n   for Main use')
base=base.replace('../../../userspace/',str(root/'userspace')+'/').replace('"build/manifest.o"','"'+str(d/'manifest.o')+'"')
(d/'fault.gpr').write_text(base)
def run(*args,**kwargs):subprocess.run(args,cwd=root/'kernel',check=True,**kwargs)
with (d/'manifest.S').open('w') as f:
    run(str(root/'userspace/ccl/build/manifest/ccl-manifest'),str(root/'userspace/ccl/catalogs/native-runtime-services.ccl'),str(src/'manifest.ccl'),stdout=f)
run('alr','exec','--','gcc','-c',str(d/'manifest.S'),'-o',str(d/'manifest.o'))
run('alr','exec','--','gprbuild','-p','-P',str(d/'fault.gpr'))
assert all(hashlib.sha256(p.read_bytes()).hexdigest()==hashes[str(p)] for p in files)
print('BUILT fault collector',mode,d/'metrics.svc')
