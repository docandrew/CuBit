# Mesa ANV: first CuBit OS-boundary audit

2026-09-27; source baseline Mesa **26.2.3**, fetched from the upstream release
archive and pinned by unpacked NAR hash in `tests/mesa-anv/source.nix`. This is
an inspected baseline, not a completed dependency audit or production version
commitment. No Mesa implementation is copied into CuBit and no Mesa build has
run on CuBit. The current Intel service is still read-only inspection.

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
choosing a native engine bring-up sequence. No firmware blob is bundled here.
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
