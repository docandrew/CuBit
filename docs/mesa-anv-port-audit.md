# Mesa ANV: first CuBit OS-boundary audit

## Native cross-build probe, 2026-09-28

Repeatable preparation is now `bash tests/mesa-anv/prepare-cubit-source.sh
PRISTINE_SOURCE NEW_DESTINATION` inside Nix (one command). It checks 26.2.3,
applies the CuBit platform, ANV-header and runtime-finalization patches to a
new copy, and refuses existing destinations. Fresh preparation `dhBEVo` and
finalization test `68wg8o74` passed; resulting finalizer sources match the
reviewed prepared tree. Native configuration requires the finalizer declaration.

`tests/mesa-anv/runtime-finalize.patch` extracts the existing upstream scratch,
engine-prefetch and workaround sequence into `intel_device_info_finalize_runtime`.
The Linux discovery path calls the same helper at its original location; no
CuBit discovery or successful submission is fabricated. Apply this patch to
the prepared 26.2.3 source in addition to the existing patches. It requires
initialized defaults, validated topology, and completed backend/hwconfig/memory
setup; it is not a public untrusted-input validator and is called once per fresh
device description. CuBit provider wiring is still outstanding.

Run `nix develop -c python3 tests/mesa-anv/test-runtime-finalize.py
tests/mesa-anv/target/source-cubit tests/mesa-anv/build-host` (one command).
Hosted result `runtime-finalize.tcr71f_2` PASS for all 63 nonempty DSS masks
times 255 EU-pair masks. This compiles the actual patched Mesa implementation,
checks scratch bounds and prefetch, and checks both ADL's 1536-entry geometry
workaround and the subsequent <=32-EU override to 1024. Assertions are enabled.
The test uses host compiler metadata with redirected source/include/dependency
and object paths; it does not rebuild the host library or establish a working
CuBit device provider. The first failed harness attempt wrote a generated host
dependency file; subsequent runs redirect that file into the unique test output.

Sparse scratch-ID and repeated-query regression (`topology-test.ns27Ly`):
all valid DSS/EU-pair combinations now check Mesa's exclusive physical DSS-ID
bound, idempotent translation, and replacement of a prior valid topology
against a fresh translation. For example, DSS mask 0x20 has one enabled DSS
but ID bound six; compute scratch sizing must not use the enabled count.
The pinned `intel_device_info.c:init_max_scratch_ids` uses that bound times
128 IDs for Gfx12 compute, independently of the enabled EU count. Native
provider finalization must invoke the upstream scratch/workaround sequence
after topology discovery, not duplicate it in this mask adapter. Hosted
regression and CuBit adapter compilation passed; no native submission tested.

Topology compatibility now runs against the pinned Mesa host library's real
count, pixel-pipe, L3-bank and EU-query functions. The exhaustive adapter test
verifies their outputs for sparse masks, starts with dirty prior arrays/counts,
and verifies every identity-rejection path leaves input unchanged. Reproduce
inside Nix with `bash tests/mesa-anv/test-topology.sh SOURCE CUBIT_BUILD`.
The script also cross-compiles the adapter for CuBit. This checks translation
compatibility on Linux, not native Mesa device initialization or GPU execution.

`tests/mesa-anv/cubit-topology.c` is now a small Mesa-side adapter for the
driver's decoded ADL-N masks. It writes the real Mesa structure's one-slice,
six-DSS, sixteen-EU mask layout without synthesizing contiguous enabled EUs
from a count. Disabled DSS slots remain zero, and half-enabled EU pairs or
wrong device/generation are rejected before mutation. Host tests cover all
256 DSS bytes by 256 EU-pair masks plus malformed masks; CuBit cross-compilation
passes. This is an internal translation step, not authenticated IPC or a
finished device provider. The adapter itself now runs Mesa's derived-count,
pixel-pipe and L3 finalization; the regression checks the returned structure
without repairing it first (run job7CO PASS). The caller must still finalize
scratch and workarounds before exposing a device. In particular, pinned Mesa's
`intel_device_info_apply_workarounds` limits geometry URB entries to 1024 on
Gfx12.0 devices with at most 32 EUs. Applying offline defaults before replacing
topology is not sufficient: runtime workarounds must see the measured EU count.

EU sampling follow-up: the post-reset ADS snapshot now reads EU_DISABLE
twice alongside slice/DSS/L3/doorbell registers (ten reads total), rejecting
changed samples, all-ones failures, and no enabled EUs. The native read-only
allowlist and getter are wired; boot diagnostics report the decoded mask and
total only for a valid observation. Hosted sampling tests and native driver
link pass. The shipped diagnostic image is unchanged, so this has not yet
produced an NUC EU measurement or supplied a Mesa device-information provider.

Device-information gap: the native steering snapshot has slice/DSS/L3 masks,
but no EU mask. Mesa's offline PCI-ID initialization fills default masks and
is not a substitute for runtime fused topology. Added a CuBit ADL-N decoder
for GEN11_EU_DISABLE (0x9134), following Linux v6.16
`gen12_sseu_info_init`: low eight disable bits expand to sixteen enabled EU
bits, shared by each enabled DSS on the single slice. Exhaustive 64-by-256
DSS/fuse tests and SPARK bounds/termination/validity checks pass. MMIO sampling
and native provider integration remain to be done; no measured EU count is
being advertised yet.

Reference: https://github.com/torvalds/linux/blob/v6.16/drivers/gpu/drm/i915/gt/intel_sseu.c

The ANV patch now also gives CuBit's `drm_fourcc.h` a data-only dependency:
fixed-width `uint32_t`/`uint64_t` typedefs instead of `drm.h`. Other platforms
keep the upstream include path. No ioctl definitions or working Linux ABI
are supplied. `format-header-test.c` compiles as CuBit C11 and C++17 with
warnings as errors; it checks exact XRGB8888 and Intel tiling/compression
identifier values and rejects accidental inclusion of DRM control APIs.
This does not solve the separate i915/xe device-discovery implementation.

Dedicated build follow-up: headless WSI now compiles. The next failures are
`src/intel/dev/i915/intel_device_info.c` and
`src/intel/dev/xe/intel_device_info.c`, at steps602/603 of1069. These are real
Linux kernel-interface implementations, not unused includes. The port must
select platform-specific sources and provide CuBit discovery/device information
instead of satisfying them with dummy Linux ioctl definitions. This is the
next backend boundary; the native build is stopped, not successfully linked.

The headless-WSI header blocker below is fixed by removing its unused
`drm_fourcc.h` include, recorded in `tests/mesa-anv/cubit-anv.patch`.
An exact-flags CuBit compile probe of the changed file passes. This is only
include hygiene, not a replacement for DRM image allocation or presentation.
Apply the patch to a separate writable copy of the prepared CuBit source;
do not mutate the software Mesa content-keyed source cache. The dedicated
copy is `tests/mesa-anv/target/source-cubit`; its fresh build is
`tests/mesa-anv/target/build-cubit`. Configuration via the helper passes and
full compilation is in progress. The original failing build is retained.

First native compile result: failed near step596/1069 in
`src/vulkan/wsi/wsi_common_headless.c`. Its `drm_fourcc.h` include reaches
`drm.h`, which selects `linux/types.h` using the compiler's `__linux__`
macro; that header is intentionally absent from the CuBit sysroot. Utilities,
NIR/SPIR-V compiler sources and much of the Vulkan runtime compiled before
this. Do not add host Linux headers globally or infer working thread/runtime
semantics from these successful object compilations. The headless include
appears unnecessary (no fourcc/modifier constant use in that file); isolate
an ANV source patch before continuing, leaving the softpipe cache untouched.

Reproduce configuration inside `host-shell.nix` with
`bash tests/mesa-anv/configure-cubit.sh PATCHED_SOURCE FRESH_BUILD HOST_BUILD`.
The source must be the prepared CuBit source, and HOST_BUILD the completed
pinned Linux baseline (default `tests/mesa-anv/build-host`). Compile with
`ninja -C FRESH_BUILD -j2 src/intel/vulkan/libvulkan_intel.so` in that same
Nix shell. Do not execute the target artifact on Linux or package it as a
functional native backend merely because compilation succeeds.

Configuration now succeeds with the existing CuBit-patched Mesa source and
`tests/mesa-software/cubit-cross.ini`, using `vulkan-drivers=intel`, no Gallium
or window-system platforms, and target LLVM disabled. Crucially,
`mesa-clc=system` selects the pinned Linux baseline's `mesa_clc` and
`vtn_bindgen2` host executables from `build-host/src/compiler/{clc,spirv}`.
Without this option, configuration rejects disabled LLVM because CLC requires
it. Host code generators and CuBit runtime dependencies must stay separate.

Output is `tests/mesa-anv/build-cubit-probe`. This reuses the software port's
prepared source without editing it. The cross file disallows target pkg-config
fallback to host libraries. Compilation of `src/intel/vulkan/libvulkan_intel.so`
has started with two workers; configuration success is not build success,
a functional CuBit KMD backend, or hardware acceleration. Feature names printed
by Meson are not a truthful runtime CuBit capability report yet.

2026-09-27; source baseline Mesa **26.2.3**, fetched from the upstream release
archive and pinned by unpacked NAR hash in `tests/mesa-anv/source.nix`. This is
an inspected baseline, not a completed dependency audit or production version
commitment. At that initial audit no Mesa build had run on CuBit and Intel was
read-only. Subsequent work now runs native Mesa softpipe in a Desktop window
and has gated Intel reset bring-up; neither is a native ANV backend or hardware
3D acceleration. See `mesa-software-native-boundary.md` and
`intel-gpu-reset-handoff.md` for the newer evidence.

Reproduce the inventory (Nix required):

```sh
nix eval --raw --file tests/mesa-anv/source.nix
# Pass the printed store path to:
nix develop -c python3 tools/audit-mesa-anv.py /nix/store/PRINTED-source
```

The inventory emits hashes, backend callbacks and lexical OS-dependency hits.
It intentionally does not claim that absence of a hit proves portability.

## Concrete integration seams

Paths below are relative to this exact upstream source tree.

| Evidence | CuBit implementation needed |
| --- | --- |
| `src/intel/vulkan/anv_kmd_backend.h`: BO create/userptr/close/map, bind/unbind, synchronous and asynchronous queue execution callbacks | A CuBit backend translating these operations to authorized GPU objects; not arbitrary physical addresses or globally meaningful GEM integers |
| `anv_physical_device.c`: render-node open, DRM discovery, device-info queries | Authorized adapter discovery and immutable, truthful device/engine/memory capability report |
| Same file: DRM syncobj type initialization and assertion | A native `vk_sync` implementation with binary/timeline behavior, waits/signals, device loss and CPU/GPU visibility guarantees |
| `anv_device.c`: pthread/C11 mutexes, condition variables, VA allocation | Audit current CuBit thread/runtime port, cancellation/death semantics and virtual-address reservations; no stubbed successful locks |
| `i915/anv_device.c`: context creation, priority, reset statistics | Per-client GPU VM/context/queue lifecycle and honest loss/reset reporting; policy admits priorities, not the application |
| `anv_wsi.c`: shared WSI plus syncobj FD setup | CuBit surface/swapchain bridge with acquired buffers, damage and explicit render/present/release dependencies |
| `meson.build`: direct libdrm and shared WSI dependencies | Build-selection changes plus common-code audit; adding one backend file is insufficient |

The callback table is useful but **not the whole kernel boundary**. Memory-type
initialization, feature selection and synchronization still branch on Linux KMD
types outside it. Add a real CuBit type rather than claiming to be i915 while
silently stubbing ioctls. `INTEL_KMD_TYPE_STUB` is not hardware acceleration.

The source PCI table (`include/pci_ids/iris_pci_ids.h`) maps our 0x46d2 device to
`adl_gt05` / ADL-N. Reuse that generation-specific compiler/layout knowledge;
do not fabricate topology or usable engines from the PCI identifier alone.

## Memory and submission semantics to retain

Reserve GPU virtual address ranges separately from committed backing storage.
ANV's descriptor/binding-table scheme can require large VA reservations; that
does not mean allocating equally large contiguous physical RAM. Bind operations
must be ordered before dependent submissions. Unbind/free must respect GPU and
external image-reader completion, even when the application's handle is closed.

The CPU-side shader compiler and command generation remain untrusted. CuBit's
service must enforce GPU address-space isolation, allowed command privileges,
resource budgets and recovery. Hardware-generated indirect commands mean a
one-time CPU scan of a mutable command buffer is not a general security proof.

Initial trusted offscreen work should exercise allocation, bind, batch submission,
completion and readback before swapchains. A hardware triangle is evidence for
that path, not Vulkan conformance or safety for arbitrary clients. Then connect
presentation through the existing Desktop/display lifecycle; never implement
"present complete" as merely "batch queued."

## Firmware and API exposure gates

Current upstream [ANV documentation](https://docs.mesa3d.org/drivers/anv.html)
identifies GuC firmware as required for the modern Alder Lake-P/Alchemist stack.
The precise ADL-N firmware selection, version/ABI, loading/authentication and
submission path must be established against Intel/Linux platform tables before
choosing a native engine bring-up sequence. This initial ANV audit did not bundle
firmware; the later Intel bring-up packages its selected blob and notices as
documented in `intel-gpu-reset-handoff.md`.
Preserve its applicable redistribution notices when one is selected.

Do not advertise Linux DRM, external FD, protected-content, sparse, performance
query or other optional Vulkan extensions without their actual CuBit semantics.
Audit mandatory core requirements separately; optional-feature suppression is
not a way to claim an unsupported Vulkan core version.

[Zink](https://docs.mesa3d.org/drivers/zink.html) remains the proposed OpenGL-over-
Vulkan route, avoiding a second hardware-facing Gallium port initially. It still
needs its feature requirements met and a CuBit context/window frontend. Servo's
WebGPU is another consumer, not validation of those requirements by itself.

## Next implementation gates

### Reproducible Linux build baseline

`tests/mesa-anv/host-shell.nix` uses this checkout's pinned nixpkgs. Run:

```sh
nix develop --impure --expr 'import /home/doc/git/cubit/tests/mesa-anv/host-shell.nix {}' \
  -c bash tests/mesa-anv/configure-host.sh SOURCE tests/mesa-anv/build-host
nix develop --impure --expr 'import /home/doc/git/cubit/tests/mesa-anv/host-shell.nix {}' \
  -c bash tests/mesa-anv/build-host.sh tests/mesa-anv/build-host
```

SOURCE is the store path from source.nix. Configuration passed for 26.2.3,
ANV only, no Gallium/EGL/GLX/GBM/window-system platforms. Fallback subproject
downloads are disabled. LLVM cannot be disabled for this build: Mesa's CLC
compiler requires it. LLVM/Clang21.1.8, SPIRV LLVM translator21.1.0, SPIRV
Tools and glslang are supplied by Nix. The shell exposes an unversioned
clang-cpp link name because Nix's split Clang library has a versioned name;
Meson library discovery needs LIBRARY_PATH in addition to linker flags.
The first compile hit the host /tmp per-user quota while emitting the generated
format-table assembly. The resumed build uses a workspace TMPDIR; no temporary
data from other tasks was removed.

This is a Linux build baseline, not a CuBit port or acceleration test. Its DRM
platform and reported ray-tracing/video build support are not CuBit features.

### Native integration

The Linux baseline linked successfully (2026-09-27):
`tests/mesa-anv/build-host/src/intel/vulkan/libvulkan_intel.so`, SHA256
`bcea1323786333e0a1e6382a334e2462bdbc692f370966578364e72825a17565`.
ELF DT_NEEDED lists libdrm, libzstd, libexpat, libstdc++, libm, libgcc_s and
libc; LLVM/Clang are not direct runtime dependencies of this artifact. This
does not establish dependencies of every optional Mesa configuration.

Undefined-symbol inspection confirms DRM device discovery, syncobj timelines,
ioctl, mmap64 and pthread synchronization/cancellation/scheduling imports.
Importantly, `platforms=[]` still leaves DRM display functions such as
drmSetMaster, drmModeAtomicCommit and drmModeAddFB2WithModifiers. The CuBit port
must explicitly separate/replace this WSI path; empty platform selection is
not proof that direct display ownership has disappeared. Do not satisfy these
functions with privileged stubs in application-side Mesa. No Vulkan device or
rendering test was run for this baseline.

1. Finish read-only physical Intel evidence and identify engine/firmware path.
2. Implement GPU allocation/VA/context/submission/sync in the native driver with
   offscreen hardware tests and cross-context negative tests.
3. Build a minimal CuBit ANV backend with discovery and sync integration, first
   without desktop WSI. Resolve actual compile/link dependencies, not fake APIs.
4. Integrate CuBit WSI, then Zink teapot and Servo. Measure transfers, frame
   latency and CPU costs; don't call a software fallback an accelerated result.

Licensing: the inspected ANV files carry MIT-style notices; Mesa as a whole has
per-component licensing. Review selected dependencies and retain notices when
porting. This audit is not approval to import arbitrary Linux driver code.
