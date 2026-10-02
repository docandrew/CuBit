#!/usr/bin/env python3
"""Link a real CuBit instance probe against completed native Mesa archives.

This is a link check, not execution. No fake backend or unresolved-symbol
allowlist is provided. Outputs/logs are isolated and failures are propagated.
"""
from pathlib import Path
import argparse
import json
import subprocess
import sys
import tempfile
import importlib.util
import hashlib

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('build', type=Path)
parser.add_argument('--retain-transport', action='store_true',
                    help='retain every backend callback and link the real native Ada FFI')
parser.add_argument('--snapshot-discovery', action='store_true',
                    help='test real native initialization with synthetic read-only replies; NO GPU')
parser.add_argument('--state-table-probe', action='store_true',
                    help='exercise actual ANV CPU state-table allocation/growth; NO GPU')
parser.add_argument('--reservation-probe', action='store_true',
                    help='exercise native owned virtual reservation syscalls; NO GPU')
modes = parser.add_mutually_exclusive_group()
modes.add_argument('--no-provider', action='store_true',
                    help='link the authority-free native lifecycle regression with its manifest')
modes.add_argument('--authorized-discovery', action='store_true',
                   help='link real manifest-authorized Mesa discovery (no logical device or GPU work)')
parser.add_argument('--logical-device', action='store_true',
                    help='extend authorized discovery with one real Mesa device lifecycle')
parser.add_argument('--transfer-smoke', action='store_true',
                    help='submit a real Vulkan fill/barrier/fence and check mapped readback')
parser.add_argument('--triangle-smoke', action='store_true',
                    help='compile/draw/read back a real offscreen Vulkan triangle')
parser.add_argument('--present-triangle', action='store_true',
                    help='opt-in native Desktop window after verified triangle completion')
parser.add_argument('--triangle-cycles', type=int, default=1, choices=range(1, 17),
                    help='complete render/consume/cleanup cycles on one device (default 1)')
parser.add_argument('--shader-dir', type=Path,
                    help='validated output of build-triangle-shaders.py')
args = parser.parse_args()
if args.snapshot_discovery and (not args.no_provider or not args.retain_transport):
    parser.error('--snapshot-discovery requires --no-provider --retain-transport')
if args.state_table_probe and (not args.no_provider or not args.retain_transport):
    parser.error('--state-table-probe requires --no-provider --retain-transport')
if args.reservation_probe and (not args.no_provider or not args.retain_transport):
    parser.error('--reservation-probe requires --no-provider --retain-transport')
if args.triangle_cycles != 1 and not args.triangle_smoke:
    parser.error('--triangle-cycles requires --triangle-smoke')
if args.present_triangle and not args.triangle_smoke:
    parser.error('--present-triangle requires --triangle-smoke')
if args.triangle_smoke and (not args.logical_device or args.transfer_smoke or not args.shader_dir):
    parser.error('--triangle-smoke requires --logical-device and --shader-dir; excludes transfer')
if args.shader_dir and not args.triangle_smoke:
    parser.error('--shader-dir requires --triangle-smoke')
if args.transfer_smoke and not args.logical_device:
    parser.error('--transfer-smoke requires --logical-device')
if args.logical_device and not args.authorized_discovery:
    parser.error('--logical-device requires --authorized-discovery')
if args.authorized_discovery and not args.retain_transport:
    parser.error('--authorized-discovery requires --retain-transport')
root = Path(__file__).resolve().parents[2]
build = args.build.resolve()
metadata = json.loads((build / "meson-info/meson-info.json").read_text())
if Path(metadata['directories']['build']).resolve() != build:
    raise SystemExit("Configured Mesa build directory does not match input")
source = Path(metadata['directories']['source']).resolve()
if 'cubit_mesa_build_id_for_address(addr)' not in (source / 'src/util/build_id.c').read_text():
    raise SystemExit('Missing native static build-ID adaptation; prepare and rebuild Mesa')
# Prepared trees contain copies of the owned transport. Reject a mixed snapshot
# before compiling glue against newer headers than those used by the archive.
# This checks copied transport inputs, not every patched upstream source file.
transport_source = source / 'src/intel/vulkan'
owned_transport = root / 'userspace/mesa/anv'
transport_inputs = {}
for required in ('anv_cubit_memory.c', 'anv_cubit_memory.h',
                 'anv_cubit_physical.c', 'anv_cubit_physical.h',
                 'native_gpu_mapping.c', 'native_gpu_mapping.h'):
    if not (transport_source / required).is_file():
        raise SystemExit('Missing prepared transport input: ' + required)
for owned in sorted(owned_transport.iterdir()):
    prepared = transport_source / owned.name
    if owned.suffix not in ('.c', '.h') or not prepared.is_file():
        continue
    content = owned.read_bytes()
    if content != prepared.read_bytes():
        raise SystemExit('Stale prepared transport input: ' + owned.name +
                         '; prepare and rebuild matching native Mesa sources')
    transport_inputs[owned.name] = hashlib.sha256(content).hexdigest()
wrapper = root / "tests/mesa-anv/native-compiler.sh"
out = Path(tempfile.mkdtemp(prefix="native-instance-link.", dir=root / "tests/mesa-anv/target"))
archive = build / "src/intel/vulkan/libvulkan_intel.a"
if not archive.is_file():
    raise SystemExit("Build native static ANV before running this check")
# This isolated cross-build uses external host generators. All archives below
# are target libraries. Start-group resolves their circular static references.
targets = json.loads((build / "meson-info/intro-targets.json").read_text())
archives = sorted({Path(filename).resolve()
                   for target in targets if target["type"] == "static library"
                   for filename in target["filename"]})
if not archives or any(not path.is_relative_to(build) or path.suffix != ".a"
                       for path in archives):
    raise SystemExit("Invalid configured archive inventory")
missing = [path for path in archives if not path.is_file()]
if missing:
    raise SystemExit("Run build-cubit.py first; missing archives:\n" +
                     "\n".join(map(str, missing)))
obj = out / "main.o"
if args.authorized_discovery or args.snapshot_discovery or args.state_table_probe:
    spec = importlib.util.spec_from_file_location('policy', root / 'tests/mesa-anv/test-native-memory-policy.py')
    policy = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(policy)
    entries = json.loads((build / 'compile_commands.json').read_text())
    entry, = [e for e in entries if e['file'].endswith('/vulkan/anv_kmd_backend.c')]
if args.authorized_discovery:
    subprocess.run(policy.compiler_command(entry) +
                   (['-DCUBIT_TEST_LOGICAL_DEVICE=1'] if args.logical_device else []) +
                   (['-DCUBIT_TEST_TRANSFER=1'] if args.transfer_smoke else []) +
                   (['-DCUBIT_TEST_TRIANGLE=1',
                     '-DCUBIT_TEST_TRIANGLE_CYCLES=' + str(args.triangle_cycles),
                     '-I' + str(args.shader_dir.resolve())]
                    if args.triangle_smoke else []) +
                   (['-DCUBIT_TEST_PRESENT_TRIANGLE=1'] if args.present_triangle else []) +
                   ['-I' + str(root / 'userspace/mesa/anv'), '-c',
                    str(root / 'tests/mesa-anv/native-authorized-discovery.c'),
                    '-o', str(obj)], cwd=entry['directory'], check=True)
else:
    subprocess.run(["bash", str(wrapper), "c", "-c",
                *(['-DCUBIT_TEST_SNAPSHOT_PROBE'] if args.snapshot_discovery else []),
                *(['-DCUBIT_TEST_STATE_TABLE_PROBE'] if args.state_table_probe else []),
                *(['-DCUBIT_TEST_RESERVATION_PROBE'] if args.reservation_probe else []),
                "-I" + str(source / "include"),
                "-I" + str(build / "src/intel/vulkan"),
                "-I" + str(build / "src/vulkan/util"),
                "-I" + str(source / "src/vulkan/util"),
                str(root / "tests/mesa-anv" / ("native-instance-no-provider.c"
                    if args.no_provider else "native-instance-link.c")),
                "-o", str(obj)], check=True)
manifest_args = []
if args.no_provider or args.authorized_discovery:
    assembly = out / 'manifest.S'
    with assembly.open('w') as destination:
        subprocess.run([str(root / 'userspace/ccl/build/manifest/ccl-manifest'),
                        str(root / 'userspace/ccl/catalogs/native-runtime-services.ccl'),
                        str(root / 'tests/mesa-anv' / ('native-triangle-present.ccl'
                            if args.present_triangle else 'native-authorized-discovery.ccl'
                            if args.authorized_discovery else 'native-instance-no-provider.ccl')),
                        *(['--ada-output', str(out / 'ccl_manifest_bindings.ads')]
                          if args.authorized_discovery else [])],
                       stdout=destination, check=True)
    manifest = out / 'manifest.o'
    subprocess.run(['bash', str(wrapper), 'c', '-c', str(assembly),
                    '-o', str(manifest)], check=True)
    manifest_args = ['--manifest', str(manifest)]
native_objects = []
init_trace_flags = []
if args.reservation_probe:
    reservation_probe = out / 'native-owned-reservation.o'
    subprocess.run(['bash', str(wrapper), 'c', '-c',
                   str(root / 'tests/mesa-anv/native-owned-reservation.c'),
                   '-o', str(reservation_probe)], check=True)
    native_objects.append(str(reservation_probe))
if args.state_table_probe:
    state_probe = out / 'native-state-table.o'
    subprocess.run(policy.compiler_command(entry) + ['-c',
                   str(root / 'tests/mesa-anv/native-state-table.c'),
                   '-o', str(state_probe)], cwd=entry['directory'], check=True)
    native_objects.append(str(state_probe))
if args.authorized_discovery or args.snapshot_discovery:
    init_trace = out / 'native-init-trace.o'
    subprocess.run(policy.compiler_command(entry) + ['-c',
                   str(root / 'tests/mesa-anv/native-init-trace.c'),
                   '-o', str(init_trace)], cwd=entry['directory'], check=True)
    native_objects.append(str(init_trace))
    init_trace_flags = ['-Wl,--wrap=' + name for name in (
        'anv_physical_device_init_common', 'anv_cubit_init_sync_types',
        'anv_init_wsi', 'anv_physical_device_init_va_ranges',
        'anv_physical_device_init_properties', 'anv_shader_init_uuid',
        'brw_compiler_create', 'isl_device_init', 'build_id_find_nhdr_for_addr',
        'anv_device_alloc_bo', 'cubit_intel_create_buffer', 'anv_vma_alloc')]
if args.snapshot_discovery:
    snapshot = out / 'native-snapshot-discovery.o'
    subprocess.run(policy.compiler_command(entry) + ['-c',
                   str(root / 'tests/mesa-anv/native-snapshot-discovery.c'),
                   '-o', str(snapshot)], cwd=entry['directory'], check=True)
    native_objects.append(str(snapshot))
for unit in ('native_build_id', 'native_build_id_link'):
    native_object = out / (unit + '.o')
    subprocess.run(['bash', str(wrapper), 'c', '-c',
                    str(root / 'userspace/mesa/anv' / (unit + '.c')),
                    '-o', str(native_object)], check=True)
    native_objects.append(str(native_object))
if args.authorized_discovery:
    for unit in ('mesa_discovery_slot', 'mesa_probe_log',
                 *(['mesa_triangle_surface'] if args.present_triangle else [])):
        subprocess.run(['gnatmake', '-q', '-c', '-gnatA', '-gnat2022', '-O2',
                    '-mno-red-zone', '-fno-pic',
                    '--RTS=' + str(root / 'userspace/runtime'),
                    '-I' + str(out),
                    '-I' + str(root / 'tests/mesa-anv'),
                    str(root / 'tests/mesa-anv' / (unit + '.adb'))],
                   cwd=out, check=True)
        native_objects.append(str(out / (unit + '.o')))
if args.present_triangle:
    presenter = out / 'native_gpu_presenter.o'
    subprocess.run(['bash', str(wrapper), 'c', '-c',
                    str(root / 'userspace/mesa/anv/native_gpu_presenter.c'),
                    '-o', str(presenter)], check=True)
    native_objects.append(str(presenter))
retain_flags = []
if args.retain_transport:
    for unit in ('native_gpu_buffers', 'native_gpu_memory', 'native_gpu_query',
                 *(['native_gpu_presentation'] if args.present_triangle else [])):
        subprocess.run(['gnatmake', '-q', '-c', '-gnatA', '-gnat2022', '-O2',
                        '-mno-red-zone', '-fno-pic',
                        '--RTS=' + str(root / 'userspace/runtime'),
                        '-I' + str(root / 'userspace/mesa/anv'),
                        str(root / 'userspace/mesa/anv' / (unit + '.adb'))],
                       cwd=out, check=True)
        native_objects.append(str(out / (unit + '.o')))
    retain_flags = ['-Wl,--undefined=' + name for name in (
        'anv_cubit_transport_backend', 'cubit_mesa_query_device_defaults',
        'cubit_gpu_native_query_call', 'cubit_mesa_init_memory_types',
        'anv_cubit_physical_device_create',
        'anv_cubit_install_discovery',
        'anv_cubit_attach_owned_session')]
    archives.append(root / 'userspace/runtime/adalib/libgnat-user.a')
# Preserve Mesa's link_whole policy (src/vulkan/runtime/meson.build and
# src/intel/vulkan/meson.build). The aggregate Intel archive already contains
# the ANV, per-generation and runtime objects. Ordinary archive extraction
# loses implementations referenced only weakly by generated dispatch tables.
# Put the aggregate FIRST so its constituent archives below are only searched
# for genuinely unresolved dependencies, not extracted twice.
# These are regression assertions, NOT a per-command retention allowlist.
required_dispatch = [
    'vk_common_GetPhysicalDeviceProperties2',
    # vk_device_init adds these common implementations through another weak
    # entrypoint table. The triangle needs them even when ANV itself has no
    # strong reference to their archive members.
    'vk_common_CreateFramebuffer', 'vk_common_DestroyFramebuffer',
    'vk_common_CreatePipelineLayout', 'vk_common_DestroyPipelineLayout',
    'vk_common_QueueSubmit',
    'vk_common_CmdCopyImageToBuffer',
]
command = ["bash", str(wrapper), "cpp", *manifest_args, str(obj), *native_objects, *retain_flags, *init_trace_flags,
           "-Wl,--build-id=sha1", "-Wl,--start-group",
           "-Wl,--whole-archive", str(archive), "-Wl,--no-whole-archive",
           *[str(path) for path in archives if path != archive.resolve()], "-Wl,--end-group",
           "-o", str(out / ('mesa-triangle-window-repeat.app' if args.present_triangle and args.triangle_cycles > 1
                           else 'mesa-triangle-repeat.app' if args.triangle_smoke and args.triangle_cycles > 1
                           else 'mesa-triangle-window.app' if args.present_triangle
                           else 'mesa-triangle.app' if args.triangle_smoke
                           else 'mesa-transfer.app' if args.transfer_smoke
                           else 'mesa-logical-device.app' if args.logical_device
                           else 'mesa-authorized-discovery.app' if args.authorized_discovery
                           else "mesa-no-provider.app" if args.no_provider else "mesa-instance.app"))]
with (out / "link.log").open("w") as log:
    result = subprocess.run(command, stdout=log, stderr=subprocess.STDOUT)
print("Native instance link:", "PASS (not executed)" if result.returncode == 0 else "FAIL",
      "artifacts:", out, flush=True)
if result.returncode == 0:
    # A successful static link alone does not establish dispatch availability.
    # Check these required roots in the final ELF, not merely in an archive.
    defined = {line.split()[-1] for line in subprocess.check_output(
        ['nm', '--defined-only', command[-1]], text=True).splitlines() if line.split()}
    absent = sorted(set(required_dispatch) - defined)
    if absent:
        raise SystemExit('Missing required static dispatch implementations: ' + ', '.join(absent))
    def digest(path):
        with Path(path).open('rb') as stream:
            return hashlib.file_digest(stream, 'sha256').hexdigest()
    (out / 'inputs.json').write_text(json.dumps({
        'build': str(build), 'source': str(source),
        'synthetic_snapshot_discovery': args.snapshot_discovery,
        'cpu_state_table_probe': args.state_table_probe,
        'native_reservation_probe': args.reservation_probe,
        'whole_archives': [str(archive)],
        'required_dispatch_symbols': required_dispatch,
        'triangle_cycles': args.triangle_cycles if args.triangle_smoke else 0,
        'present_triangle': args.present_triangle,
        'transport_sha256': transport_inputs,
        'archives_sha256': {str(path): digest(path) for path in archives},
        'objects_sha256': {str(path): digest(path) for path in
                          [obj, *native_objects, *([manifest] if manifest_args else [])]},
        'executable': command[-1], 'executable_sha256': digest(command[-1]),
        'executed': False,
    }, indent=2) + '\n')
sys.exit(result.returncode)
