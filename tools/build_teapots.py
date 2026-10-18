#!/usr/bin/env python3
"""Build CuBit Teapots (userspace/apps/teapots) and stage teapots.app.

Called from `make -C kernel teapots` under build.lock, inside Nix, after the
runtime, libc and CCL manifest compiler are built. Like Desktop's Vulkan
build: gprbuild compiles and binds the Ada units, the C glue is compiled with
the libc cross compiler, and the program is linked against the verified
combined Mesa bundle (ANV) the Desktop uses. A CuBit ELF, not a Linux-hosted
demonstration; linking proves nothing about GPU execution.
"""
import argparse
import os
from pathlib import Path
import subprocess
import sys


def run(command, cwd, env):
    command = [str(part) for part in command]
    subprocess.run(command, cwd=cwd, env=env, check=True)


def main():
    root = Path(__file__).resolve().parents[1]
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--output', type=Path, default=root / 'kernel/isodir/boot/teapots.app')
    parser.add_argument('--map', type=Path, help='also write the GNU ld map (notice inventory)')
    args = parser.parse_args()
    if not os.environ.get('IN_NIX_SHELL'):
        parser.error('Use the pinned Nix environment')
    sys.path.insert(0, str(root / 'tools'))
    from ensure_native_mesa import ensure
    from verify_mesa_service_bundle import verify
    cache = Path(os.environ.get('CUBIT_MESA_CACHE') or root / 'userspace/mesa/build/combined')
    dependency = ensure(root, cache)
    bundle, mesa = dependency / 'bundle', dependency / 'source'
    prefix, flags = verify(bundle)
    if len(prefix) != 3 or prefix[0] != 'bash' or prefix[2] not in ('c', 'cpp'):
        raise ValueError('Unsupported native bundle compiler prefix')
    c_prefix = [prefix[0], prefix[1], 'c']
    app = root / 'userspace/apps/teapots'
    build = app / 'build'
    generated = build / 'generated'
    generated.mkdir(parents=True, exist_ok=True)
    # 16 MiB main stack, as Desktop: Mesa's compiler recurses deeply.
    env = {**os.environ, 'CUBIT_STACK_SIZE': '16777216', 'NIX_HARDENING_ENABLE': ''}

    # Manifest and the generated Startup binding.
    with (build / 'manifest.S').open('w') as assembly:
        subprocess.run([str(root / 'userspace/ccl/build/manifest/ccl-manifest'),
                        str(root / 'userspace/ccl/catalogs/native-runtime-services.ccl'),
                        str(app / 'manifest.ccl'), '--schema',
                        str(root / 'userspace/ccl/interfaces/executable-manifest.ccl'),
                        '--startup-output', str(generated / 'startup.ads')],
                       stdout=assembly, check=True, env=env)
    run([*c_prefix, '-c', build / 'manifest.S', '-o', build / 'manifest.o'], build, env)

    # Ada: compile and bind only; the link is below.
    run(['alr', 'exec', '--', 'gprbuild', '-q', '-p', '-c', '-b', '-P', app / 'teapots.gpr'],
        root / 'kernel', env)

    # Shaders: GLSL -> SPIR-V words in a C header.
    for stage in ('vert', 'frag'):
        run(['glslangValidator', '-V', '--vn', 'teapot_%s_spv' % stage,
             app / 'shaders' / ('teapot.' + stage), '-o', generated / ('teapot_%s.h' % stage)],
            build, env)
    (generated / 'teapot_shaders.h').write_text(
        '#include "teapot_vert.h"\n#include "teapot_frag.h"\n')

    # C glue, against the Mesa source's Vulkan headers.
    objects = []
    for source in (root / 'userspace/lib/ui/gpu_canvas_vk.c', app / 'teapot_vk.c'):
        obj = build / (source.stem + '.o')
        run([*c_prefix, '-std=c11', '-O2', '-Wall', '-Wextra', '-Werror',
             '-I' + str(mesa / 'include'), '-I' + str(root / 'userspace/lib/ui'),
             '-I' + str(generated), '-c', source, '-o', obj], build, env)
        objects.append(obj)

    exchange = (build / 'main.bexch').read_text()
    bound = exchange.split('[BOUND OBJECT FILES]\n', 1)[1].split('\n[', 1)[0].splitlines()
    binary = build / 'teapots.app'
    map_flags = ['-Wl,-Map,%s,--cref' % args.map.resolve()] if args.map else []
    run([*prefix, '--manifest', build / 'manifest.o', build / 'b__main.o', *bound, *objects,
         root / 'userspace/rust/build/font-native/libcubit_fonts.a', *flags, *map_flags,
         '-o', binary], build, env)
    if subprocess.check_output(['nm', '-u', str(binary)], text=True).strip():
        raise ValueError('teapots.app has unresolved symbols')
    args.output.parent.mkdir(parents=True, exist_ok=True)
    temporary = args.output.with_suffix('.pending')
    temporary.write_bytes(binary.read_bytes())
    os.chmod(temporary, 0o755)
    os.replace(temporary, args.output)
    print(args.output)


if __name__ == '__main__':
    main()
