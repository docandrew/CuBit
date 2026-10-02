"""Run inside Nix with coordination/build.lock held. Reuse built Mesa archives."""
import json
from pathlib import Path
import subprocess
import sys
root = Path(__file__).resolve().parents[2]
build = Path(sys.argv[1]).resolve()
out = root / 'tests/compositor/build/native'
out.mkdir(parents=True, exist_ok=True)
commands = json.loads((build / 'compile_commands.json').read_text())
entry, = [x for x in commands if x['file'].endswith('/st_manager.c')]
source = (Path(entry['directory']) / entry['file']).resolve().parents[3]
def run(args):
    subprocess.run([str(x) for x in args], cwd=root, check=True)
def compile(src, name, *flags):
    run(['python3', root/'tests/mesa-software/compile-native-probe.py', build,
         root/src, out/name, *flags])
# The native glyph oracle uses the same archive as Desktop, never a stale seed.
run(['make', '-C', root/'userspace/rust', 'fonts-native'])
compile('userspace/lib/compositor/softpipe.c', 'softpipe.o')
compile('tests/compositor/native-report.c', 'report.o', '-Wno-missing-prototypes')
compile('tests/mesa-software/native-softpipe.c', 'baseline.o',
        '-Dmain=compositor_baseline_main', '-Wno-missing-prototypes',
        '-I'+str(source/'src/gallium/drivers'), '-I'+str(source/'src/gallium/winsys/sw'))
subprocess.run(['alr', 'exec', '--', 'gprbuild', '-p', '-c', '-b', '-P',
                str(root/'tests/compositor/native.gpr')], cwd=root/'kernel', check=True)
bexch = (out/'native_main.bexch').read_text()
objects = bexch.split('[BOUND OBJECT FILES]\n')[1].split('\n[')[0].splitlines()
libs = ['src/gallium/drivers/softpipe/libsoftpipe.a',
        'src/gallium/winsys/sw/null/libws_null.a', 'src/gallium/auxiliary/libgallium.a',
        'src/compiler/nir/libnir.a', 'src/compiler/libcompiler.a', 'src/util/libmesa_util.a',
        'src/util/blake3/libblake3.a', 'src/util/libmesa_util_clflush.a',
        'src/util/libmesa_util_clflushopt.a', 'src/util/libmesa_util_simd.a',
        'src/c11/impl/libmesa_util_c11.a']
run([root/'userspace/libc/cubit-c++', out/'b__native_main.o', *objects,
     out/'softpipe.o', out/'report.o', out/'baseline.o',
     '-Wl,--start-group', *(build/x for x in libs),
     root/'userspace/rust/build/font-native/libcubit_fonts.a',
     root/'userspace/runtime/adalib/libgnat-user.a', '-Wl,--end-group',
     '--manifest', build/'native-manifest.o', '-o', out/'compositor-probe.app'])
print(out/'compositor-probe.app')
