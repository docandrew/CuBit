#!/usr/bin/env python3
"""Actual CCL/ELF regression for the CMOS named ACPI hardware window."""
from pathlib import Path
import argparse,json,hashlib,subprocess,tempfile,os,shutil,resource,time
H=Path(__file__).resolve().parent;ROOT=H.parents[2]
sha=lambda p:hashlib.sha256(Path(p).read_bytes()).hexdigest()
a=argparse.ArgumentParser();a.add_argument('--root',type=Path,default=ROOT);a.add_argument('--output',type=Path);v=a.parse_args()
if not os.environ.get('IN_NIX_SHELL'):raise SystemExit('Use pinned nix develop; export TMPDIR=/home/doc/cubit-build-tmp first')
out=v.output.resolve() if v.output else Path(tempfile.mkdtemp(prefix='acpi-cmos-windows-',dir=os.environ['TMPDIR']));out.mkdir(parents=True,exist_ok=True);catalog=json.loads((H/'sources.json').read_text());pins={}
for p,digest in catalog['inputs'].items():
 q=v.root/p
 if sha(q)!=digest:raise SystemExit('source drift: '+p)
 pins[str(q)]=digest
for q in H.iterdir():
 if q.is_file():pins[str(q)]=sha(q)
compiler=Path(shutil.which('gprbuild')).resolve();gcc=Path(shutil.which('gcc')).resolve()
for p in [compiler,gcc]:
 if not str(p).startswith('/nix/store/'):raise SystemExit('Nix tools required')
 pins[str(p)]=sha(p)
soft,hard=resource.getrlimit(resource.RLIMIT_STACK);target=128*1024*1024
if hard!=-1 and hard<target:raise SystemExit('128MiB stack required for actual CCL compiler witness')
resource.setrlimit(resource.RLIMIT_STACK,(target,hard));runs=[]
def run(label,argv,seconds=180,expected=0):
 started=time.monotonic();log=out/(label+'.log')
 with log.open('w') as f:r=subprocess.run(list(map(str,argv)),stdout=f,stderr=subprocess.STDOUT,timeout=seconds)
 runs.append(dict(label=label,argv=list(map(str,argv)),returncode=r.returncode,elapsed=time.monotonic()-started,log=str(log),log_sha256=sha(log)))
 if r.returncode!=expected:raise SystemExit('unexpected result: '+str(log))
for profile in ['checked','release']:
 d=out/'manifest-admission';d.mkdir(exist_ok=True);shutil.copy2(H/'manifest_sections.adb',d/'manifest_sections.adb');dirs=[d]+[v.root/p for p in catalog['source_dirs']];switches=['-O1','-gnat2022','-gnatwa','-gnatwe','-gnata'if profile=='checked'else'-gnatp'];gpr='project Manifest_Admission is\nfor Source_Dirs use ('+','.join('"'+str(p)+'"'for p in dirs)+');\nfor Source_Files use ('+','.join('"'+p+'"'for p in ['manifest_sections.adb',*catalog['compiler_source_files']])+');\nfor Object_Dir use "obj-'+profile+'";for Exec_Dir use "bin-'+profile+'";for Main use ("manifest_sections.adb");\npackage Compiler is for Default_Switches("Ada") use ('+','.join('"'+x+'"'for x in switches)+');end Compiler;\nend Manifest_Admission;\n';project=d/'manifest_admission.gpr';project.write_text(gpr);(out/'manifest-output1').mkdir(exist_ok=True)
 run(profile+'-build',[compiler,'-p','-P',project,'-j1'],600)
 run(profile+'-sections',[d/('bin-'+profile)/'manifest_sections',v.root/'userspace/services/acpi/native/manifest.ccl',v.root/'userspace/ccl/catalogs/native-runtime-services.ccl',v.root/'userspace/ccl/interfaces/executable-manifest.ccl',out/'manifest-output1'],30)
 if 'CMOS_MANIFEST_WINDOW_CHECKS 148 PASS'not in(out/(profile+'-sections.log')).read_text():raise SystemExit('missing148checks')
# Use the checked profile in both extracted-source control executables.
p=out/'manifest-admission/manifest_admission.gpr';s=p.read_text().replace('obj-release','obj').replace('bin-release','bin').replace('"-gnatp"','"-gnata"');p.write_text(s)
run('prepare-elf',['python3',H/'prepare-elf.py',out,v.root],30)
run('assemble',[gcc,'-c','-x','assembler',out/'elf-path/manifest.S','-o',out/'elf-path/manifest.o'],30)
for mode in ['candidate','old-zero-map-mutant']:run(mode+'-build',[compiler,'-p','-P',out/'elf-path'/mode/'elf_path.gpr','-j1'],300)
run('elf-cases',['python3',H/'check-elf.py',out],60)
run('old-branch-killed',[out/'elf-path/old-zero-map-mutant/bin/procmgr_manifest_path_tests',out/'elf-path/normal.o','allow'],10,1)
if 'actual ELF/manifest resource route'not in(out/'old-branch-killed.log').read_text():raise SystemExit('mutant failed for unrelated reason')
for p,digest in pins.items():
 if sha(p)!=digest:raise SystemExit('post-run input drift: '+p)
result=dict(completed=True,base=catalog['base'],checks_per_profile=148,profiles=['checked','release'],elf_cases=6,old_branch_mutant_compiled_and_killed=True,runs=runs,input_hashes=pins,native_executed=False,scope=catalog['scope']);(out/'RESULT.json').write_text(json.dumps(result,indent=2)+'\n');print('CMOS_WINDOWS checked148/148 release148/148 ELF6/6 mutant1/1 100% '+str(out/'RESULT.json'))
