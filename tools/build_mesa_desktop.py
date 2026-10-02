"""Explicit opt-in link, in Nix under build.lock. Does not stage the result.
Usage: build-desktop.py EXISTING_MESA_BUILD [none|init|draw|text]
The production gpr default remains legacy; reuse its built manifest/assets.
"""
from pathlib import Path
import argparse, os, subprocess, sys
from desktop_build_variant import directory
root=Path(__file__).resolve().parents[1]
parser=argparse.ArgumentParser(description=__doc__)
parser.add_argument('mesa', type=Path)
parser.add_argument('fault', nargs='?', default='none', choices=('none','init','draw','text'))
parser.add_argument('--metrics', choices=('off','on'), default='off')
parser.add_argument('--scenario-output', action='store_true')
args=parser.parse_args()
mesa=args.mesa.resolve()
fault=args.fault
scenario={**os.environ, 'CUBIT_COMPOSITOR':'mesa'}
build_directory=directory(args.metrics, scenario)
if args.scenario_output and (fault!='none' or os.environ.get('CUBIT_COMPOSITOR_ALLOC_TRACE')=='1'):
    raise SystemExit('fault/allocator instrumentation cannot replace a production scenario')
if not (mesa/'compile_commands.json').is_file():
    raise SystemExit(f'Mesa build missing compile_commands.json: {mesa}')
out=root/'tests/compositor/build'
out.mkdir(parents=True,exist_ok=True)
desktop=root/'userspace/services/desktop'
def run(args,cwd=root):
    subprocess.run([str(x) for x in args],cwd=cwd,check=True,
                   env={**os.environ,'CUBIT_STACK_SIZE':'16777216'})
flags=[] if fault=='none' else ['-DCUBIT_MESA_FAIL_TEXT_PARTIAL' if fault=='text' else '-DCUBIT_MESA_FAIL_'+fault.upper()]
obj=out/('softpipe-'+fault+'.o')
run(['python3',root/'tests/mesa-software/compile-native-probe.py',mesa,
     root/'userspace/lib/compositor/softpipe.c',obj,*flags])
trace = os.environ.get('CUBIT_COMPOSITOR_ALLOC_TRACE') == '1'
trace_args = []
if trace:
    trace_obj = out/'allocation-trace.o'
    run(['python3',root/'tests/mesa-software/compile-native-probe.py',mesa,
         root/'tests/compositor/allocation-trace.c',trace_obj])
    trace_args = [trace_obj, *(f'-Wl,--wrap={name}' for name in
                  ('malloc','calloc','realloc','aligned_alloc','posix_memalign'))]
run(['alr','exec','--','gprbuild','-p','-c','-b','-P',desktop/'desktop.gpr',
     '-XCUBIT_COMPOSITOR=mesa', '-XCUBIT_COMPOSITOR_METRICS='+args.metrics],root/'kernel')
d=desktop/build_directory
bexch=(d/'main.bexch').read_text()
objects=bexch.split('[BOUND OBJECT FILES]\n')[1].split('\n[')[0].splitlines()
libs=['src/gallium/drivers/softpipe/libsoftpipe.a','src/gallium/auxiliary/libgallium.a',
      'src/compiler/nir/libnir.a','src/compiler/libcompiler.a','src/util/libmesa_util.a',
      'src/util/blake3/libblake3.a','src/util/libmesa_util_clflush.a',
      'src/util/libmesa_util_clflushopt.a','src/util/libmesa_util_simd.a',
      'src/c11/impl/libmesa_util_c11.a']
result=out/('desktop-mesa-'+fault+('-metrics' if args.metrics=='on' else '')+('-trace' if trace else '')+'.svc')
if args.scenario_output:
    result=d/'desktop.svc'
manifest=desktop/('build-metrics-manifest' if args.metrics=='on' else 'build')/'manifest.o'
temporary=result.with_suffix('.svc.pending')
run([root/'userspace/libc/cubit-c++',d/'b__main.o',*objects,obj,*trace_args,
     desktop/'build/wallpaper.o',desktop/'build/wallpaper_cubie.o',
     root/'userspace/rust/build/font-native/libcubit_fonts.a','-Wl,--start-group',
     *(mesa/x for x in libs),root/'userspace/runtime/adalib/libgnat-user.a','-Wl,--end-group',
     '--manifest',manifest,'-o',temporary])
os.replace(temporary,result)
print(result)
