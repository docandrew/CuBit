#!/usr/bin/env python3
"""Build an isolated runtime-dispatch Desktop from explicit local inputs.

Requires Nix and an existing verified Mesa bundle. Does not patch Desktop,
metadata, shared staging or the installed image. Output must be a new directory.
Caller must keep selected inputs stable; shared inputs require build.lock.
Prebuilt runtime and font inputs are recorded, not rebuilt here.
"""
import argparse
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import subprocess
import shlex


def software_recipe(build, bundle, mesa_source, link_flags):
    """Require one verified, jointly configured Gallium/ANV build."""
    build = build.resolve()
    inventory = json.loads((bundle / 'inputs.json').read_text())['inputs_sha256']
    metadata = build / 'meson-info/meson-info.json'
    commands = build / 'compile_commands.json'
    for path in (metadata, commands):
        actual = hashlib.sha256(path.read_bytes()).hexdigest()
        if inventory.get(str(path)) != actual:
            raise ValueError('Software Mesa build input is not in verified bundle: ' + str(path))
    directories = json.loads(metadata.read_text())['directories']
    if Path(directories['build']).resolve() != build or Path(directories['source']).resolve() != mesa_source.resolve():
        raise ValueError('Software Mesa source/build configuration mismatch')
    for relative in ('src/gallium/drivers/softpipe/libsoftpipe.a', 'src/intel/vulkan/libvulkan_intel.a'):
        archive = build / relative
        if str(archive) not in link_flags or str(archive) not in inventory:
            raise ValueError('Combined Mesa bundle lacks selected-build archive: ' + relative)
    entries = [entry for entry in json.loads(commands.read_text()) if entry['file'].endswith('/st_manager.c')]
    if len(entries) != 1:
        raise ValueError('Expected one verified Gallium frontend compiler recipe')
    entry = entries[0]
    cwd = Path(entry['directory']).resolve()
    if cwd != build:
        raise ValueError('Unexpected Gallium compiler working directory')
    source = (cwd / entry['file']).resolve()
    if not source.is_relative_to(mesa_source.resolve()):
        raise ValueError('Gallium compiler source escapes selected Mesa source')
    arguments = entry.get('arguments') or shlex.split(entry['command'])
    filtered, index, removed_source = [], 0, 0
    while index < len(arguments):
        value = arguments[index]
        if value in ('-o', '-MF', '-MQ', '-MT'):
            if index + 1 >= len(arguments):
                raise ValueError('Incomplete Gallium compiler option')
            index += 2
            continue
        if value in ('-c', '-MD', '-MMD', '-MP'):
            index += 1
            continue
        if not value.startswith('-') and (cwd / value).resolve() == source:
            removed_source += 1
        else:
            filtered.append(value)
        index += 1
    if removed_source != 1 or not filtered:
        raise ValueError('Gallium compiler recipe does not identify exactly one input')
    return filtered, cwd, (metadata, commands)


def verify_runtime(source, bundle, link_flags):
    """Require the Ada compile runtime to equal the bundle's linked runtime.

    Bundle verification establishes recorded hashes; this binds the separate
    compositor source snapshot to those exact inputs before any compilation.
    """
    inventory = json.loads((bundle / 'inputs.json').read_text())['inputs_sha256']
    candidates = [Path(flag).resolve() for flag in link_flags
                  if str(flag).endswith('/userspace/runtime/adalib/libgnat-user.a')]
    if len(candidates) != 1:
        raise ValueError('Expected exactly one recorded Mesa link runtime')
    archive = candidates[0]
    runtime = archive.parent.parent
    checked = {}
    for name, expected in inventory.items():
        path = Path(name)
        if not path.is_relative_to(runtime):
            continue
        relative = path.relative_to(runtime)
        if relative.parts[0] not in ('gnat', 'adalib'):
            continue  # Build intermediates are not consumed by Desktop's runtime.
        if path != archive and path.suffix not in ('.ads', '.adb', '.ali'):
            continue
        copied = source / 'userspace/runtime' / relative
        if not copied.is_file() or hashlib.sha256(copied.read_bytes()).hexdigest() != expected:
            raise ValueError('Compositor/Mesa runtime input mismatch: ' + str(relative))
        checked[str(relative)] = expected
    if str(archive) not in inventory or 'adalib/libgnat-user.a' not in checked:
        raise ValueError('Linked runtime archive is absent from Mesa inventory')
    if not any(name.endswith('.ali') for name in checked) or not any(name.endswith('.ads') for name in checked):
        raise ValueError('Mesa inventory lacks compiler runtime metadata')
    return checked


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    for name in ('source-root', 'toolchain-root', 'bundle', 'mesa-source',
                 'manifest-compiler', 'schema', 'catalog', 'output'):
        parser.add_argument('--' + name, type=Path, required=True)
    parser.add_argument("--input-overlay", choices=("off", "on"), default="off")
    parser.add_argument("--timing", choices=("off", "on"), default="off")
    parser.add_argument("--metrics", choices=("off", "on"), default="off")
    parser.add_argument('--software-mesa-build', type=Path, help='verified combined softpipe/ANV native build')
    parser.add_argument('--software-fault', choices=('none', 'init', 'draw', 'text-partial'), default='none',
                        help='test-only failure injection into the compositor Mesa bridge')
    args = parser.parse_args()
    if not os.environ.get('IN_NIX_SHELL'):
        parser.error('Run in the pinned Nix development environment')
    source, toolchain, bundle, mesa, compiler, schema, catalog, out = (
        getattr(args, name).resolve() for name in
        ('source_root', 'toolchain_root', 'bundle', 'mesa_source', 'manifest_compiler',
         'schema', 'catalog', 'output'))
    if out == source or source in out.parents:
        parser.error('Output must be outside the input source tree')
    verifier = toolchain / 'tools/verify_mesa_service_bundle.py'
    spec = importlib.util.spec_from_file_location('mesa_bundle', verifier)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    prefix, flags = module.verify(bundle)
    if len(prefix) != 3 or prefix[0] != 'bash' or prefix[2] not in ('c', 'cpp'):
        raise ValueError('Unsupported native bundle compiler prefix')
    runtime_inputs = verify_runtime(source, bundle, flags)
    c_prefix = [prefix[0], prefix[1], 'c']
    software = software_recipe(args.software_mesa_build, bundle, mesa, flags) if args.software_mesa_build else None
    if (source / 'userspace/services/desktop/desktop_mesa_software_renderer.ads').exists() and software is None:
        raise ValueError('Unified Mesa source requires --software-mesa-build')
    if args.software_fault != 'none' and software is None:
        raise ValueError('Software fault injection requires the unified Mesa bridge')
    fault_flags = [] if args.software_fault == 'none' else ['-D' + {
        'init': 'CUBIT_MESA_FAIL_INIT', 'draw': 'CUBIT_MESA_FAIL_DRAW',
        'text-partial': 'CUBIT_MESA_FAIL_TEXT_PARTIAL'}[args.software_fault] + '=1']
    out.mkdir(parents=True, exist_ok=False)
    inputs, copies, commands = {}, {}, []
    result = {'status': 'INCOMPLETE', 'backend': 'vulkan-runtime-dispatch',
              'gpu_drawing_enabled': True, 'hardware_validated': False,
              'executed': False, 'matched_runtime_inputs': runtime_inputs, 'scope': 'Isolated build; no staging or installation'}

    def digest(path):
        return hashlib.sha256(path.read_bytes()).hexdigest()

    def record(path):
        inputs[str(path)] = digest(path)

    def copy(path, relative):
        if path.is_symlink():
            raise ValueError('Resolve input symlink explicitly before building: ' + str(path))
        data = path.read_bytes()
        record(path)
        destination = out / relative
        destination.parent.mkdir(parents=True, exist_ok=True)
        destination.write_bytes(data)
        copies[str(relative)] = hashlib.sha256(data).hexdigest()

    env = {**os.environ, 'CUBIT_STACK_SIZE': '16777216', 'NIX_HARDENING_ENABLE': ''}
    # Keep a single declared build variant regardless of ambient scenarios.
    env.update(CUBIT_COMPOSITOR='vulkan', CUBIT_COMPOSITOR_TIMING=args.timing,
               CUBIT_COMPOSITOR_METRICS=args.metrics, CUBIT_COMPOSITOR_STORAGE='production',
               CUBIT_DISPLAY_TEST_MODE='production', CUBIT_INPUT_OVERLAY=args.input_overlay)

    def run(command, cwd=out, **kw):
        command = list(map(str, command))
        commands.append({'argv': command, 'cwd': str(cwd)})
        return subprocess.run(command, cwd=cwd, env=env, check=True, **kw)

    try:
        for path in (Path(__file__).resolve(), verifier, compiler, schema, catalog):
            record(path)
        if software:
            for path in software[2]: record(path)
        for header in sorted((mesa / 'include').rglob('*.h')):
            record(header)
        for directory in ('userspace/services/desktop', 'userspace/lib/compositor',
                          'userspace/lib/display', 'userspace/lib/image', 'userspace/lib/theme', 'userspace/lib/ui',
                          'userspace/ccl/src', 'userspace/allocator/src',
                          'userspace/services/display/production'):
            for path in sorted((source / directory).rglob('*')):
                if (path.is_file() and path.suffix in ('.ads', '.adb', '.gpr', '.c', '.h', '.vert', '.frag')
                        and not any(p.startswith('build') for p in path.relative_to(source / directory).parts)):
                    copy(path, path.relative_to(source))
        for directory in ('userspace/runtime/gnat', 'userspace/runtime/adalib'):
            for path in sorted((source / directory).rglob('*')):
                if path.is_file():
                    copy(path, path.relative_to(source))
        for relative in ('userspace/runtime/ada_source_path', 'userspace/runtime/ada_object_path',
                         'userspace/runtime/runtime.xml', 'userspace/runtime/target_properties',
                         'userspace/mesa/mesa_service.ads', 'userspace/mesa/mesa_service.adb',
                         'userspace/mesa/service-device.h', 'userspace/services/desktop/manifest.ccl',
                         'userspace/rust/build/font-native/libcubit_fonts.a'):
            copy(source / relative, Path(relative))
        shader_script = Path('tests/compositor/build-vulkan-affine-shaders.py')
        copy(toolchain / shader_script, shader_script)
        desktop = out / 'userspace/services/desktop'
        if 'Configure_Renderer' not in (out / 'userspace/services/desktop/backend-vulkan/desktop_compositor.ads').read_text():
            raise ValueError('Selected source lacks runtime backend dispatch')
        metadata = desktop / ('build-metrics-manifest' if args.metrics == 'on' else 'build')
        generated = metadata / 'generated'
        generated.mkdir(parents=True, exist_ok=True)
        selected_manifest = desktop / 'manifest.ccl'
        if args.metrics == 'on':
            base = selected_manifest.read_text().rstrip()
            if not base.startswith('#') and not base.startswith('(executable-manifest v1'):
                raise ValueError('Metrics overlay requires a reviewed keyword Desktop manifest')
            if '(executable-manifest v1' not in base or not base.endswith(')') or '(request-service metrics ' in base:
                raise ValueError('Unexpected Desktop manifest; review metrics overlay')
            selected_manifest = metadata / 'manifest.ccl'
            selected_manifest.write_text(base[:-1] + '\n  (request-service metrics read-write metrics))\n')
        with (metadata / 'manifest.S').open('w') as assembly:
            run([compiler, catalog, selected_manifest, '--schema', schema,
                 '--ada-output', generated / 'ccl_manifest_bindings.ads'], stdout=assembly)
        run([*c_prefix, '-c', metadata / 'manifest.S', '-o', metadata / 'manifest.o'])
        run(['alr', 'exec', '--', 'gprbuild', '-q', '-p', '-c', '-b', '-P',
             desktop / 'desktop.gpr', '-XCUBIT_COMPOSITOR=vulkan',
             '-XCUBIT_COMPOSITOR_METRICS=' + args.metrics], toolchain / 'kernel')
        shader_out = out / 'generated'
        run(['python3', out / shader_script, shader_out])
        # All C bridge objects are rebuilt, not inherited from a previous test.
        objects = []
        for name in ('vulkan_context', 'vulkan_submission_native', 'vulkan_affine',
                     'vulkan_backdrop', 'vulkan_checker', 'vulkan_sources', 'vulkan_device_storage',
                     'vulkan_upload_buffer', 'vulkan_upload_record', 'vulkan_targets',
                     'vulkan_owned_image', 'vulkan_owned_target_binding'):
            obj = out / (name + '.o')
            run([*c_prefix, '-std=c11', '-O2', '-Wall', '-Wextra', '-Werror',
                 '-I' + str(mesa / 'include'), '-I' + str(shader_out), '-c',
                 out / 'userspace/lib/compositor' / (name + '.c'), '-o', obj])
            objects.append(obj)
        if software:
            obj = out / 'softpipe.o'
            run([*software[0], *fault_flags, '-c', out / 'userspace/lib/compositor/softpipe.c', '-o', obj], software[1])
            objects.append(obj)
        directory = desktop / ('build-vulkan' + ('-input-overlay' if args.input_overlay == 'on' else '') + ('-timing' if args.timing == 'on' else '') + ('-metrics' if args.metrics == 'on' else ''))
        exchange = (directory / 'main.bexch').read_text()
        bound = exchange.split('[BOUND OBJECT FILES]\n', 1)[1].split('\n[', 1)[0].splitlines()
        binary = out / 'desktop-vulkan-compositor.svc'
        run([*prefix, '--manifest', metadata / 'manifest.o', directory / 'b__main.o',
             *bound, *objects,
             out / 'userspace/rust/build/font-native/libcubit_fonts.a', *flags, '-o', binary])
        if subprocess.check_output(['nm', '-u', str(binary)], text=True).strip():
            raise ValueError('Linked Desktop contains unresolved symbols')
        if software:
            defined = {line.split()[-1] for line in subprocess.check_output(['nm', '--defined-only', str(binary)], text=True).splitlines() if line.split()}
            if not {'cubit_mesa_create', 'cubit_mesa_fill', 'cubit_mesa_draw', 'softpipe_create_screen'}.issubset(defined):
                raise ValueError('Unified compositor lacks software Mesa symbols')
        module.verify(bundle)
        for path, expected in inputs.items():
            if digest(Path(path)) != expected:
                raise ValueError('Build input changed: ' + path)
        for relative, expected in copies.items():
            if digest(out / relative) != expected:
                raise ValueError('Copied input changed: ' + relative)
        for generated_input in (selected_manifest, generated / 'ccl_manifest_bindings.ads', metadata / 'manifest.S'):
            copies[str(generated_input.relative_to(out))] = digest(generated_input)
        manifest = out / 'compositor-sources.json'
        manifest.write_text(json.dumps(copies, indent=2) + '\n')
        result.update(software_renderer='mesa-softpipe' if software else 'builtin-cpu', status='LINKED', binary=binary.name, binary_bytes=binary.stat().st_size,
                      binary_sha256=digest(binary), source_manifest=manifest.name,
                      source_manifest_sha256=digest(manifest), software_fallback=True,
                      gpu_drawing_enabled_meaning='Runtime capability, not hardware validation',
                      build_variant={'software_fault': args.software_fault, 'input_overlay': args.input_overlay, 'timing': args.timing, 'metrics': args.metrics, 'storage': 'production'})
        print(binary, flush=True)
    finally:
        (out / 'build-inputs.json').write_text(json.dumps(inputs, indent=2) + '\n')
        (out / 'build-commands.json').write_text(json.dumps(commands, indent=2) + '\n')
        (out / 'compositor-result.json').write_text(json.dumps(result, indent=2) + '\n')


if __name__ == '__main__':
    main()
