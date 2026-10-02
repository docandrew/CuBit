# Mesa ANV: first CuBit OS-boundary audit

## Current hardware gate, 2026-10-02

The NUC now reports `MESA-DEVICE create=0` and triangle cycle 1/3 beginning
on that device. Logical-device creation is physically confirmed. The first
image `AllocateMemory` fails; the window and cycle report `-2`
(`VK_ERROR_OUT_OF_DEVICE_MEMORY`) and stop without replay. No Mesa drawing
result has been observed. This error does not itself prove exhausted RAM.
The red-on-black window reported by the user is the earlier driver-only demo;
the Mesa probe expects a red triangle over a blue clear color.

v14 (`kernel/cubit_live_mesa_triangle_repeat_v14.img`, SHA256
`ea035b47912a24bf513a3536434cde9f25c9cc3ed6b3235232d5a78d05579ecf`)
preserves the Mesa/GPU implementation and adds test-only diagnostic pacing and
a final `MESA-LOG bridge dropped(hex)=...` summary. The collector allows a
64-record burst and replenishes one credit per 100 ms. The probe waits 125 ms
between new valid records; it does not replay rejected publications or change
the production budget. A native 80-record regression passes with zero drops,
as do the complete 90-second native gate and exact-image USB/UEFI boot checks.
QEMU does not validate physical Intel rendering. Rate limiting remains a
possible explanation for the old missing messages, not an established cause.
Viewer/service history-loss counters do not count rejected publications.

The allocation audit found a concrete adapter mismatch: common upstream ANV
adds `AUX_TT_ALIGNED` for auxiliary-map GPU VA alignment and `AUX_CCS` for
metadata backing. `anv_allocator.c` expands the allocation size before
`gem_create` and handles GPU VA alignment itself; the i915 backend allocates
ordinary backing. Our native adapter rejected both flags. It now accepts
them only with `has_aux_map` and an initialized auxiliary-map context. Other
unsupported flags remain rejected. Hosted actual-adapter tests verify the
gates and exact byte forwarding without adding metadata twice, alongside
the existing concurrent allocation and retained-lifetime regressions.
The NUC v15 run still fails image allocation with `-2`: size 16448 bytes,
alignment 65536, memory type 0, allowed-type mask 1. The auxiliary flag
correction alone did not resolve this failure; its precise cause remains
unconfirmed. The requested image size is below the adapter's 16 MiB limit,
but this does not establish backend backing availability or GPU VA space.
All 61 configured native static archives and the three-cycle window probe
link passed. The exact v15 image passed four-CPU UEFI USB-flash/no-PS2 boot
and collector/clock log delivery in QEMU, not Intel rendering.
Image: `kernel/cubit_live_mesa_triangle_repeat_v15.img`, SHA256
`8037dd36ca3f294e0da778e09a90148b4f19ff3422ddd2eb655c7d3a1f09149d`.

v16 adds probe-only linker wrappers for the actual common user BO request
and result, failed backing-allocation IPC, and failed GPU VA allocation.
Disassembly verifies the real callsites route through these wrappers. They
call the originals once and preserve results, without retry or policy changes.
Native link and exact-image four-CPU USB/UEFI boot/log-delivery checks pass;
physical Intel allocation remains unverified. Image:
`kernel/cubit_live_mesa_triangle_repeat_v16.img`, SHA256
`290d51c287f41fc8935648cd5e019623975d3b5878cb11d127750d9f999bae8f`.
Collect `MESA-INIT allocation:*` records with the triangle failure. They use
the existing `result=` field for scalar diagnostic values as well as statuses.

The physical v16 result is backing request 20480 bytes, status 3: a validated
GPU-service `Unavailable` reply, not malformed IPC or rejection at the Mesa
adapter flags gate. This status alone does not distinguish exhausted slots,
pending retirement, owner failure, or failed backing acquisition.
v17 retains the exact service rejection branch as an observational enum,
without changing replies, admission, retirement or retry behavior. It logs
`intel-gpu: allocation unavailable reason=...` and, for deferred failure,
`intel-gpu: allocation backing stage=...`. Hosted request/race/reuse tests and
native driver compilation pass. Packaged driver/app bytes match build outputs;
exact-image four-CPU USB/UEFI boot and log delivery pass, not Intel execution.
Image: `kernel/cubit_live_mesa_triangle_repeat_v17.img`, SHA256
`809b90c784c1463f8d9ce7629c3846b5681b9aa8c79084578bfe6a69916742f8`.

Next hardware gates are successful image allocation, Mesa triangle
submission/readback/presentation and three-cycle retirement. Do not infer
Mesa rendering, general WSI, accelerated Desktop, or full driver readiness
from the driver-only triangle or successful hosted tests.

### Confirmed allocation-slot exhaustion and growable registry direction

The subsequent NUC report is `allocation unavailable reason 5`.
`Intel_GPU_Buffer_Requests.Allocation_Outcome` ordinal 5 is `Slots_Exhausted`.
That branch runs after finding no reusable application slot and observing
`Attempted = Intel_GPU_Buffer_Backing.Slot'Last` (16). It rejects the request
before backing acquisition. This establishes allocation-record exhaustion,
not physical-memory or GPU-virtual-address exhaustion. Private bootstrap,
context and page-table allocations share the ticket namespace with app BOs.

The user requests growth as needed, not another small fixed allocation ceiling.
The implementation is not yet growable. The coordinated replacement needs:

1. **Identity independent of committed capacity.** Public handles still use
   `(ID - 1) mod Capacity`. Internal tickets have now been migrated to an
   immutable low32 slot/high32 generation-minus-one layout, including deferred
   retirement admission and supervisor retirement-generation decoding. Public
   handle strides cannot become the current growable size. Choose
   an immutable identity encoding or monotonic identity lookup, retain full
   identity/session validation, and retire an identity namespace on overflow
   rather than wrapping. An index alone is never authority.
2. **Chunked metadata growth.** Allocate and initialize additional record
   chunks on demand without moving existing records. Publish a chunk only
   after initialization succeeds; failed growth must leave existing allocations
   and identities unchanged. Metadata backing must not recursively require
   a free GPU BO record. Keep metadata allocation failure an explicit result,
   not an exception escaping a service or kernel boundary.
3. **Separate budgets.** Track authorized GPU backing bytes, registry metadata
   bytes, and per-session outstanding objects separately. Growing bookkeeping
   does not expand DMA authority, grant more physical RAM, or change the GPU VA
   contract. The existing 32 MiB backing arena is a distinct bootstrap limit,
   not evidence of a growable physical-memory allocator.
4. **End-to-end admission.** Update supervisor backing records, driver request
   records, public handles and dependent mapping/binding registries together.
   The current Ada and C budget decoders assume exactly sixteen slots and
   infer retained-byte bounds from that number; replace that assumption with
   an explicit versioned budget contract. Advertise policy limits separately
   from instantaneous free space. Avoid keeping the obsolete bootstrap ABI
   as a second fallback path.
5. **Retirement and bounded work.** Growth must not mark closed, pending,
   quarantined or GPU-visible storage reusable. Reuse still requires all
   existing GPU/TLB/CPU retirement evidence and the supervisor acknowledgement.
   Replace whole-capacity scans with bounded/incremental work as tables grow;
   neither a larger free list nor reclamation may monopolize the service loop.

Validation must exercise allocations across multiple growth boundaries with
old handles/bindings still live, failure at each metadata-growth stage,
cross-session/stale-handle rejection, generation exhaustion, delayed and lost
retirement acknowledgements, independent byte/object quotas, and truthful Mesa
budget decoding. Follow with the native CuBit boot/IPC gates and the physical
three-cycle Mesa triangle test. Hosted tests and QEMU cannot establish Intel
rendering success. No larger fixed ceiling is presented as completing this
dynamic-allocation work.

The first identity migration passes hosted allocation lifecycle/race tests,
2048 deferred-retirement candidates, and simulated VM-update/private-table
reuse regressions. Slot/generation decoding includes the maximum supported
generation. These are not physical GPU results or a completed growable
allocator; native driver compilation is pending shared build-lock availability.
The registry remains sixteen entries and its backing arena remains32MiB.
Large discrete-GPU VRAM and terabyte-scale host memory must be represented as
separate resource domains with64-bit sizes and checked arithmetic, not inferred
from metadata table capacity or the current bootstrap arena.

## CPU state-table backing, 2026-10-01

Native hardware feedback reaches successful physical-device enumeration and
session attachment, but logical-device creation returns `-3`. Independently,
the actual ANV CPU state-table probe fails on CuBit at Linux `memfd_create`
(syscall 319). This identifies a real porting gap, not proof that it is the
only remaining device-creation failure.

`native-state-table.patch` replaces only the three CPU backing operations on
CuBit. Upstream indexing, free lists and synchronization remain unchanged.
The adapter reserves `BLOCK_POOL_MEMFD_SIZE` bytes of CPU virtual address space
(currently 1 GiB), then commits a page-aligned prefix in chunks no larger than
16 MiB. Saved entry pointers remain valid without copying or remapping old
views. This is CPU metadata, not GPU BO allocation or GPU address-space setup.
Successful partial commits are retained after a later failure; logical capacity
is published only after the requested prefix is backed. Failed retirement
retains the reservation rather than claiming the backing was released.

The common runtime exports reserve/commit-prefix/release operations backed by
current-process-owned kernel reservations. Native reservation regression has
passed eight growth/retirement cycles, including interleaved allocations and
rejected malformed requests. Pure arithmetic policy has eight proved checks;
this does not prove the kernel mapping or concurrency implementation.
The production Mesa helper also passes hosted mocked-syscall failure injection
for reserve failure, partial commit/retry and failed retirement. Full native
Mesa libraries and the actual CPU probe link successfully. The adapted ANV
probe now passes in native CuBit: initialization, growth, stable saved pointers
and lifecycle, followed by the 90-second four-CPU headless gate and final fault
scan (`/tmp/cubit-mesa-state-table-fixed.serial.log`). Hardware logical-device
creation remains a separate gate; this CPU regression does not establish GPU
rendering.

The backing failure fixture is included in the repeatable hosted suite:

```sh
nix develop -c python3 tests/mesa-anv/test-native-memory-policy.py \
  tests/mesa-anv/target/state-table-native.sthIHk/build
```

The 2026-10-01 run passed five production adapter compilations and ten hosted
fixtures. Backing cases include oversized initialization, failed initial commit
plus failed release, partial-growth retry, page rounding, shrink rejection,
same-size no-recommit, retained failed retirement and successful retry. The
fixture never dereferences its mock address and does not emulate a GPU.

The native CPU probe additionally passes four pthread workers released through
a common start gate, with periodic yields and 1,024 allocations per worker.
After all joins, all 4,096 saved pointers still address the current table and
retain their distinct tags. The 90-second four-CPU TCG run and final fault scan
pass (`/tmp/cubit-mesa-concurrent-table.serial.log`). This exercises actual ANV
allocation with CuBit threads and backing; it is regression evidence, not a
proof over all possible interleavings or hardware GPU execution. v12's GPU app
is unchanged by these test-only additions.

The dated sections below record earlier stages and their then-current limits.

Full rebuild of `target/unmap-preparation.KEeT8M/build` subsequently completed:
all 61 configured native archives, 1138 Ninja steps. Optional Linux-service
symbol checks and allocator dispatch regressions pass. The real instance link
still fails at the sole unresolved `anv_physical_device_try_create` reference
from `anv_CreateInstance`; see `target/native-instance-link.5iml41s1/link.log`.
This confirms library compilation, not a usable native Vulkan driver.

## Native factory and mapping integration gates, 2026-09-30

Reinspection of the prepared upstream source confirms `anv_CreateInstance`
still installs `anv_physical_device_try_create` as `try_create_for_drm`.
Mesa already provides `vk_instance.physical_devices.enumerate` for non-DRM
discovery; the CuBit integration should use that callback with authorized
service discovery, not fabricate a DRM device or an always-successful factory.
The callback takes precedence over DRM except for `VK_ERROR_INCOMPATIBLE_DRIVER`,
which falls back. CuBit must leave the DRM callback unset. A genuinely absent
compatible device may produce an empty successful enumeration; missing required
transport implementation is not evidence that device initialization succeeded.
Only publish a physical device after common initialization, backend lifetime,
engine inventory, memory, queue and synchronization requirements are satisfied.

The existing grant API does not directly implement POSIX unmapping:
`CuBit.Memory_Grants.Return_Acquisition` calls `Process.IPC.returnGrant`, which
decrements acquisitions but only unmaps when the final return completes a
previously requested revocation. `revokeGrantLocked` unmaps immediately if no
acquisitions remain, or defers until they return. Thus returning a CPU borrow,
revoking the grant, confirming retirement, and releasing GPU backing are distinct
operations. The future Mesa callback must track these states; a successful
return alone cannot establish that an address is unmapped or safe to repurpose.
Per-mapping grants plus owner-side retirement are one possible implementation,
but no such transport has been wired here. Fixed-address replacement stays
unsupported until its address-reservation semantics can be implemented correctly.

## Backend buffer unmapping, 2026-09-30

`buffer-unmap.patch` adds optional backend `unmap_bo` dispatch after upstream
slab/page-offset adjustment. The backend receives the original BO and adjusted
mapping/size; failures propagate without falling through to POSIX calls.
Linux i915/Xe preserve their existing mmap/munmap fallback. CuBit rejects a
missing callback rather than discarding a shared-memory view through munmap.
This releases a CPU view, not GPU backing; the actual CuBit callback remains
unimplemented. Replacement mappings must fail unchanged when unsupported.

Fresh zero-fuzz preparation: `target/unmap-preparation.KEeT8M/source`.
`test-buffer-unmap.py` passes real-function hosted fixtures for both platform
branches, slab adjustment, replacement, missing backend and error propagation;
removing the native guard or ignoring callback failures is detected. Eleven
real Linux units compile (`target/lifecycle-compile._tavsj4r`), and the real
allocator compiles with native CuBit flags (`target/native-unmap.yyr6w4do`).
These checks do not execute a GPU, validate a live view-release transport, or
establish a complete native Mesa application link. Existing NUC image unchanged.

## Optional Linux services excluded from native target, 2026-09-30

`native-optional-services.patch` retains Linux's fd-based engine-inventory
update but excludes it from CuBit; shared compute-thread policy remains
unchanged. Native engine inventory must come from the authorized backend.
CuBit already leaves both hardware performance-query extensions unadvertised.
Their Linux OA-stream entrypoints are now absent from the native object, rather
than leaving unreachable ioctl/syncobj references or inventing success stubs.
Performance-result interpretation helpers remain available; native stream
cleanup asserts that no stream was opened.

All 61 native archives rebuild. Eleven Linux compilation checks pass, and
`test-native-optional-symbols.py` verifies Linux providers are absent while
shared helpers remain. Fresh patch replay matches the edited source. The
native instance link now fails only at `anv_physical_device_try_create`
(`target/native-instance-link.s07s1ul3/link.log`). This still does not establish
native enumeration, logical-device creation, submission or hardware rendering.

## Backend-owned physical destruction, 2026-09-30

`physical-destroy.patch` moves the real `anv_physical_device_destroy` into
common code. `finish_physical` is mandatory on a backend before allocation;
the i915/Xe tables supply the original DRM budget/perf/descriptor cleanup.
Normal destruction preserves WSI, measurement, common, backend, base ordering.
Failed construction still unwinds in its factory. This is real cleanup, not
a success stub; it does not make device creation or submission available.

Ordering/negative-mutation tests, allocation rejection for a missing callback,
partial cleanup tests, Linux regression compilation, fresh patch replay and
the 61 native archive build pass. Header audit remains 914/914. The instance
link now resolves the destructor, but common code brings additional existing
references into the link: Linux performance-stream/bind-timeline operations
and `intel_common` engine queries, alongside the still-missing device factory.
See `target/native-instance-link.3d2kqb86/link.log`. These must be properly
ported or excluded as unsupported functionality, not silently stubbed.

## Shared physical-device teardown, 2026-09-30

`physical-cleanup.patch` extracts `anv_physical_device_finish_common`: release
and clear the retained engine inventory, compiler and shader cache without
closing Linux descriptors or CuBit endpoints. Linux's normal destruction and
post-common-initialization failure paths now call it; WSI, measurement, perf,
budget registry and transport teardown retain their existing platform order.
A native factory can use the same cleanup before releasing its Vulkan base.

Hosted tests execute the actual routine for all eight resource-presence
combinations, check repeated calls, and reject stale-engine/compiler mutations.
Base-lifetime tests, nine Linux unit compilations and native common-unit
compilation pass. Fresh patch replay matches the edited source. This does not
implement a native factory or prove GPU/context teardown safe; those remain
separate runtime requirements.

## Complete static dependency build, 2026-09-30

For the native configuration, default `ninja` and the ANV archive target are
insufficient: supporting archives such as NIR and Mesa utilities are marked
`build_by_default: false`. A fresh default build left 588 configured target
objects without valid dependency records and produced many additional linker
errors. Do not interpret a successful archive/default build as a complete
application-link dependency build.

After `configure-cubit.sh`, run these inside `tests/mesa-anv/host-shell.nix`:

```sh
python3 tests/mesa-anv/build-cubit.py BUILD_DIRECTORY
python3 tests/mesa-anv/audit-native-deps.py BUILD_DIRECTORY
python3 tests/mesa-anv/test-native-instance-link.py BUILD_DIRECTORY
```

The build helper requests every static-library target from Meson's inventory.
The link probe now rejects missing configured archives instead of silently
linking whichever archives already exist. The header audit requires valid
dependency records for every configured native static-library object and checks
their paths against the source/build trees, CuBit headers, and matching cross
C++/compiler headers. It rejects Linux headers and missing/stale records.
Optional native tool executables such as `spirv2nir` are outside this archive
inventory; the audit does not claim to cover their unbuilt objects.

Verified fresh result: all 61 configured archives built; header audit covers
914/914 native static-library objects and 1,773 unique dependencies. The
instance-link probe against these fresh archives now fails only at
`anv_physical_device_try_create` and `anv_physical_device_destroy`, matching
the older build's known native-factory gap. Log:
`target/native-instance-link.clb0de6x/link.log`. The additional unresolved
utility/compiler symbols from the incomplete default build are gone.

## Native compiler entrypoint, 2026-09-30

The main `configure-cubit.sh` uses `tests/mesa-anv/cubit-cross.ini` and
`native-compiler.sh`, not the private workspace compiler wrappers. C and C++
resolve the raw musl-cross compiler and binutils from the existing libc
toolchain record; no Nix store hashes, GCC version, or checkout path is baked
into this entrypoint. C++ uses the matching toolchain headers; C library headers
and startup objects come from CuBit's sysroot. Ambient include paths are cleared.
This is ANV-specific and does not change the shared libc wrappers.

Fresh Meson cross configuration passes. The C++ header-isolation probe also
passes with deliberately contaminated `CPATH`/`CPLUS_INCLUDE_PATH`. The real
instance-link probe now uses this entrypoint and still fails only at the two
unimplemented physical-device factory/destructor symbols (log
`target/native-instance-link.8pyn9nli/link.log`). That failure remains visible;
neither unresolved-symbol suppression nor a fake device factory is used.
Fresh ANV static archive build completed all 377 steps in
`target/full-preparation.F0lcK1/build`. This is a target-library build, not a
linked/executed native application or hardware rendering result.

## Unified source preparation, 2026-09-30

`tests/mesa-anv/prepare-cubit-source.sh` now applies the complete current ANV
adaptation, including the common physical-device and backend boundaries.
The obsolete private `prepare-device-core.sh` wrapper was removed; historical
references below describe earlier checkpoints. No private wrapper is needed
to reproduce the patched sources from pristine pinned Mesa 26.2.3.

Fresh preparation in `target/full-preparation.F0lcK1/source` matches every
source file in the existing isolated native build (excluding generated
`__pycache__` directories). Hosted physical-base lifetime tests and 288 admission
cases pass, including their negative mutation checks. Nine modified/common
ANV units compile with the pinned Linux build flags; output is
`target/lifecycle-compile.6qvkfqym`. These are source reproducibility and
regression checks, not native GPU execution. The native physical-device factory
and usable CuBit rendering backend remain incomplete; no fake factory or
render-ready advertisement was added to make the instance link pass.

## Backend selected by discovery, 2026-09-30

Common physical-device initialization, heap refresh, extension selection and
logical-device creation now use the backend supplied at physical-device
allocation. The pointer must remain immutable and valid for that lifetime;
NULL is rejected before allocation. The Linux factory still chooses its
original i915/Xe table from the discovered KMD enum, once. A native factory
can provide CuBit operations without misidentifying itself as a Linux KMD.
This does not itself supply those operations, authorize an endpoint, or make
the hardware ready. Existing KMD-dependent feature/workaround policy still
requires auditing before a native device can be exposed.

Job28883 compiles nine Linux units and native physical/common/logical device
objects (`lifecycle-compile.z952yejh`); base-lifetime tests pass including
NULL-backend rejection and a leak mutation. Job49718 passes 288 mock admission
cases, 20 memory-type failure cases, their mutations, and eleven upstream
preservation checks. These are source/compile/hosted checks, not native GPU
execution or Vulkan conformance results.

Full native rebuild5503 completes 169 scheduled steps after these changes
(`/tmp/cubit-mesa-backend-build.omvjge`). Fresh patch reconstruction67099
matches the active source at `target/backend-repro.YQrkT9/source`. A fresh
Linux Meson configuration45676 passes; target introspection confirms Linux
still selects an installed shared `libvulkan_intel.so` and both ICD manifests,
whereas CuBit selects a non-installed static archive and no ICD manifests.
This is Linux configuration evidence, not a rebuilt Linux shared library.
The rebuilt native instance probe still fails to link at the missing native
physical-device factory/destructor (`native-instance-link.okbux_8s/link.log`).

## Shared physical-device base lifetime, 2026-09-30

Physical-device allocation, dispatch-table initialization and Vulkan base
cleanup are now common helpers rather than part of the DRM constructor.
The Linux constructor/destructor use those helpers while retaining Linux
transport/resource cleanup. Allocation or base-initialization failure leaves
the caller's output unchanged and frees any allocated storage. Freeing the
base requires the caller to have released its other resources first.

Nine Linux units and the native common object compile (job16871,
`lifecycle-compile.pwopqqha`). `test-physical-base.py` extracts the actual
helpers and exercises success, allocation failure and Vulkan initialization
failure with a hosted mock; its leak mutation removes the failure-path free.
This is preparatory code for a native factory, not a factory implementation,
device enumeration result, or hardware resource-lifetime proof.

## Backend-owned external handles and tiling, 2026-09-30

Common allocator import/export and typed ISL tiling calls now use explicit
optional backend operations. Missing operations reject the request rather
than dispatching Linux GEM helpers. Both Linux backends retain their original
FD helpers; the original tiling conversion routines moved unchanged into the
Linux adapter. This does not implement native external-memory import/export;
those extensions remain disabled. Optional FD paths still use libc file APIs.

Nine Linux units and the native allocator compile (job42806,
`lifecycle-compile.v3z6q61k`). Eleven moved-routine preservation checks pass,
and the native allocator object no longer references `anv_gem_*` helpers.
Fresh preparation from pinned pristine Mesa with zero-fuzz patches reproduces
the working source tree at `target/allocator-repro.UsKYDg/source` (job68598;
Python caches excluded from comparison).

`test-allocator-dispatch.py` extracts the actual export/get-tiling/set-tiling
functions and tests 32 callback/error combinations under hosted UBSan. It
checks arguments, return values, dispatch counts and unchanged outputs on
rejection; a wrong-exported-FD mutation is detected (job4344). These are mock
backend tests, not native memory mapping or GPU execution. Import behavior is
not covered by this focused harness.

The updated native static build completes all 171 scheduled steps (job71696,
`/tmp/cubit-mesa-allocator-build.kkg80j`). Relinking the real instance probe
still fails at `anv_physical_device_try_create` and
`anv_physical_device_destroy` (`target/native-instance-link.xjrzsaz9/link.log`).
Thus archive compilation is reproducible, but the probe is not a runnable
native application. Authorized CuBit discovery and physical-device lifetime
integration remain the next link boundary; later device/submission paths
are not covered by this instance-only probe.

## Backend-owned placed suballocation mapping, 2026-09-30

The common allocator's fixed-address slab mapping now dispatches an optional
`map_placed_slab` backend operation. Its Linux DMA-BUF export/mmap/close
implementation moved unchanged to the Linux adapter. A missing operation
returns MEMORY_MAP_FAILED without modifying the output mapping, and extension
advertisement requires both mmap-offset support and this operation. No Linux
descriptor is synthesized for CuBit; an eventual native implementation must
map authorized buffer objects through CuBit's memory interface.

Job79314 compiles nine Linux units and native allocator/property objects
(`lifecycle-compile.p45k401z`). The upstream-preservation test now checks nine
moved routines, including this Linux mapping path; both affected patches
reverse-check with zero fuzz. These checks do not exercise real mapping,
and other allocator import/export/tiling boundaries remain unfinished.

## Backend queue-engine lifecycle, 2026-09-30

The common VkQueue initialization path now invokes paired backend
`create_engine`/`destroy_engine` operations instead of switching directly to
Linux i915/Xe symbols. It rejects missing backend, creation or destruction
callbacks before creating an engine. Success retains the existing cleanup
path; backend creation failure must clean up its own partial resources.
The backend table remains immutable for the device lifetime.

Both Linux tables point to their original queue implementations. Job13438
compiles eight Linux units plus the native common queue object
(`lifecycle-compile.5hrhdomv`). A harness extracts the actual common dispatchers
and passes 16 callback/error combinations under UBSan; removing the required
cleanup check is detected (job95341). This validates dispatch/error handling,
not native queue execution or hardware resource reclamation. CuBit still
needs concrete queue and physical-device implementations.

## Explicit native static library and application link probe, 2026-09-30

CuBit now selects a non-installed static `libvulkan_intel.a` target instead
of asking the static-application wrapper to produce a shared ICD. Linux
retains the shared target, link arguments and installation. CuBit does not
generate ICD/development manifests or run the shared-symbol export check.
Build71327 completes successfully. The archive is an intermediate, not an
executable or a working driver; its dependencies must accompany application
integration.

`native-instance-link.c` calls Mesa's actual CreateInstance, resolves physical
enumeration through its instance dispatch, and treats zero devices as an
unsuccessful integration test. `test-native-instance-link.py` compiles/links
it with native archives and the CuBit startup/runtime; it neither runs it on
Linux nor substitutes backend functions. Latest result is a genuine link
failure at `native-instance-link.7kprrufm/link.log`: missing
`anv_physical_device_try_create` and `anv_physical_device_destroy`. This is the
reachable instance path only, not proof that all later device/submission
dependencies are satisfied. The factory and native discovery must be wired
to authorized endpoints before the instance probe can become executable.

## Native compilation reaches final link, 2026-09-30

Full rebuild79103 completed the common and per-generation archives, then
failed at209/210 because the final library still unconditionally selected
Linux `anv_gem.c`. That source is now Linux-only, matching the other DRM
adapters; no fake GEM implementation replaces it on CuBit.

After resolving a generated-file permission issue, retry81919 reaches the
final `libvulkan_intel.so` link. Its first error is missing `-lm`: the CuBit
sysroot contains libm.a, but isolated-c++ lacks the C wrapper's sysroot library
search path. The larger issue is artifact type: this wrapper supplies static
application startup/linker-script options while upstream requests a shared
library. The native static library/application integration must be made
explicit; simply producing an ELF named .so would not prove a usable ICD.
Factory, queue and memory backend operations remain incomplete regardless
of compile success. No native hardware rendering result is established.

Dependency audit56993 passes for 829/915 target objects with valid recorded
dependencies (1561 unique paths). Missing/stale records are excluded, so this
is partial header-isolation evidence, not a whole-driver certification.

## Physical-property code separated from DRM discovery, 2026-09-30

Linux discovery/destruction and its major/minor-keyed budget registry now
live in `anv_physical_device_drm.c`, selected only for non-CuBit builds. The
existing property/feature/extension calculations stay in
`anv_physical_device.c`; a small common initialization entry point invokes
them. This removes Linux headers from the common physical-device unit without
replacing DRM calls with success stubs. CuBit's factory and teardown still
need actual implementations before the driver can link and expose devices.

Job60572 compiles the previously blocked native physical-device unit and
common module, as well as all seven affected Linux units
(`lifecycle-compile.w7k_2n_d`). Fresh full preparation88189 at
`target/drm-split.KdCB6a/source` matches the active source tree (excluding
Python caches) and passes eight upstream-preservation checks. Full isolated
rebuild79103 was started afterward; its results are not yet established by
these targeted checks. Log: `/tmp/cubit-mesa-post-physical-build.log`.

## Mandatory shared device admission, 2026-09-30

Generation admission, force-probe handling, the existing Gfx12.0 workaround
adjustment and the context-isolation requirement now run in common physical
initialization, before backend parameter queries. These requirements are no
longer confined to the Linux discovery path. The native identity/topology
query alone is deliberately insufficient to satisfy them: measured identity
must not be mistaken for a usable isolated Vulkan device.

Linux now allocates/initializes the base object before admission and releases
it via fail_base on rejection. On accepted devices the same checks and
workaround policy apply. Under allocation failure, OOM can therefore precede
an unsupported-device error; successful device policy is unchanged.

Job97615 compiles six Linux units and the native common object
(`lifecycle-compile.jo7tz91c`). The actual admission block passes 288 hosted
mock metadata cases under UBSan, including generation boundaries, force-probe,
isolation and workaround state; disabling the isolation check is detected
(`device-admission.26xsbfws`, job73525 exit0). Both patches reverse-check with
zero fuzz. This verifies admission logic, not actual hardware isolation.

## Callable common physical initialization stage, 2026-09-30

`anv_physical_device_init_common` now assembles the previously separated
parameter/admission, heap, sync-type, addressing, compiler, ISL, UUID, VA-range
and disk-cache setup. The Linux constructor invokes this stage after its base
object, device info and transport are initialized. The stage preserves the
pre-parameter-query device-info snapshot used by the original constructor.
On UUID failure it releases the compiler and clears its pointer; base object
and transport cleanup remain with the caller. Later Linux failures retain
the existing cache/compiler teardown order.

Job12283 passed six Linux compilation checks and native CuBit compilation of
the common module (`lifecycle-compile.k8uc3z4d`). Fresh full preparation75862
at `target/common-constructor.ha7LRa/source` matches the active source tree
(excluding Python caches). Eight upstream source-preservation checks pass,
as do GTT admission and memory-type failure tests including their bug mutations.

This stage is not a complete native physical-device factory: discovery,
transport implementation, engine inventory, budget identity and WSI completion
still require integration. No native Vulkan device or hardware drawing result
is implied by compiling this stage.

## Shared heap initialization and backend memory policy, 2026-09-30

Heap construction and availability refresh now live in the shared physical
module. The Linux-specific system-RAM restriction heuristic lives in the
Linux adapter behind the required `restrict_sys_heap_size` operation. Both
Linux backends use the original implementation; CuBit must provide its own
budget policy rather than implicitly treating authorized memory as all host
RAM. Missing policy or a zero-sized system heap rejects initialization.

Memory-type initialization now returns a backend error before appending a
protected memory type to the possibly incomplete result. The extracted real
branch passes 20 hosted mock cases under UBSan; removing the early error check
is rejected by the test (`memory-type-failure.6nrtp4fm`, job35574 exit0).
This checks failure propagation, not kernel-enforced memory authority.

Job38996 compiles six Linux units and the native common object
(`lifecycle-compile.h3qli2da`). Eight relocated implementations match upstream
(including the renamed Linux budget routine), and all three updated patches
reverse-check with zero fuzz. Full native device construction, authorized
allocation budgets and submission remain incomplete.

## Shared queue-family construction, 2026-09-30

Queue-family construction and its existing debug overrides now live in the
shared physical-device module. The engine inventory callback remains the
discovery boundary; this move does not invent a CuBit queue or enable engines.
In particular, the legacy single-render-queue fallback is preserved upstream
policy, not evidence that CuBit has discovered or initialized such a queue.

`test-common-preservation.py` compares seven moved functions' signatures and
bodies against pinned upstream Mesa, allowing only removal of static linkage.
All seven pass. Job23137 also compiles six affected Linux units and the native
CuBit common object (`lifecycle-compile.cohwtdfy`); both patches reverse-check
with zero fuzz. These checks establish reuse and compilation, not GPU execution.

## Shared UUID and cache initialization, 2026-09-30

The existing build-id/UUID and shader-cache initialization/teardown routines
now reside in `anv_physical_device_common.c`, with internal declarations for
use by platform constructors. Their algorithms and error checks are unchanged;
the Linux constructor and teardown retain their calls. UUIDs still derive
from the driver build and device information, not a CuBit placeholder.

Job99260 passed compilation of all six affected Linux units
(`lifecycle-compile.ri_ox7f9`) and the isolated native CuBit common object.
Both updated patches reverse-apply in dry-run mode with zero fuzz. Native
build-id lookup, shader-cache persistence, full linking and physical-device
construction are not established by these compilation checks. Shared script
promotion remains deferred because the build lock was unavailable.

## Shared compiler initialization and patch reproduction, 2026-09-30

Mesa's existing compiler initialization and logging callbacks now live in
`anv_physical_device_common.c`. The Linux constructor calls the shared helper;
the compiler, callbacks, spilling policy, and failure cleanup retain their
existing behavior. This moves Mesa code; it does not implement a new compiler.
`physical-common.patch` adds the module, declaration and build entry.

The module compiled with the isolated native CuBit toolchain (job28453 exit0).
A fresh application of the entire private preparation sequence to pristine
Mesa 26.2.3 also passed (job59007 exit0):
`tests/mesa-anv/target/physical-repro.JbdHZv/source` matches the active isolated
source tree, excluding generated Python caches. Six affected Linux units
compile from the reproduced source, including both kernel backends and the
new common module (`lifecycle-compile.l46m6bhi`). These are compilation and
reproducibility checks, not native Vulkan device or rendering tests.

The full sequence still resides in the private `prepare-device-core.sh`;
the shared preparation script does not yet include all these patches. A
held-lock attempt to promote it was unavailable, so shared scripts were left
untouched. The physical-device factory still requires DRM separation and a
real CuBit backend before this becomes a usable native hardware Mesa driver.

## Backend-owned relocation and sparse policy, 2026-09-30

The constructor no longer applies Linux relocation/TR-TT/VM-bind policy to
every non-Xe backend. It requires init_addressing, and the Linux backend
contains the existing policy unchanged, including debug options. A future
CuBit backend must explicitly select its implemented addressing mechanisms;
this change does not enable sparse memory or relocations on CuBit.

`test-addressing-policy.py` compares the actual extracted implementation to
the pristine constructor block across 432 generation/backend/debug-option
combinations under UBSan (addressing-policy.be9fa13c, PASS). Five real Linux
units compile (65401 exit0, lifecycle-compile.j9v1pgxo), and both patches
reverse-validate with zero fuzz. The differential test checks Linux policy
preservation only, not GPU memory mapping or native sparse execution.

## Backend-specific feature probes, 2026-09-30

Protected-context, render-timestamp access and VM-fault support probes now
belong to backend callbacks. Missing probes mean unsupported. Timestamp and
fault capabilities also require their corresponding operational callbacks,
so a positive probe alone cannot advertise an unimplemented entry point.
Existing generation/scratch-page gates remain. Linux probes preserve the
original queries, with the Xe-only fault probe in the Xe backend.

Five real Linux units compile (46027 exit0, lifecycle-compile.d545ejxf), and
reverse zero-fuzz patch checks pass. This is compile/source evidence, not
proof of those hardware capabilities on CuBit. The physical factory still
uses DRM discovery; native construction and backend implementations remain.

## Engine discovery backend operation, 2026-09-30

Engine inventory and the adjacent common device-info refresh now run through
init_engine_info. The Linux implementation retains intel_engine_get_info and
intel_common_update_device_info, including its existing legacy queue fallback
when no engine inventory is returned. A native implementation can return an
explicit query failure rather than adopting that Linux fallback.

Common construction rejects missing/failed callbacks through a new cleanup
label that frees engine info and disk cache before the compiler/base teardown.
It intentionally skips the later budget-release step: that object has not
been acquired yet and its release helper dereferences its argument. Published
engine_info retains common free()-based ownership.

Five real Linux units compile (92410 exit0, lifecycle-compile.0l23wxno), and
both patches reverse-validate with zero fuzz. This is compile/source evidence,
not native engine discovery or a runtime resource-lifetime proof. Remaining
DRM constructor queries and CuBit factory/backend implementation are pending.

## Physical-device rejection result correction, 2026-09-30

Audit of the pinned constructor found that the below-4GiB GPU-address-space
branch called vk_errorf without assigning its returned result before jumping
to cleanup. A preceding successful parameter query could therefore leave
VK_SUCCESS as the return value after freeing the candidate device. The port
patch now assigns VK_ERROR_INCOMPATIBLE_DRIVER through the existing logger.

`test-gtt-admission.py` executes that actual extracted branch with six sizes
around the 4GiB boundary under UBSan. It checks failure status and cleanup
selection together. Reintroducing the exact omitted-assignment bug causes the
test to fail, as required. Run55925 exited0 (gtt-admission.zr1asifb); the five
real Linux units also compile (lifecycle-compile.iw3nbb0d). Reverse zero-fuzz
patch validation passes. The fixture mocks logging/cleanup and does not test
complete Vulkan enumeration or native hardware resource disposal.

## Backend-owned synchronization-type discovery, 2026-09-30

Physical construction now invokes init_sync_types rather than constructing a
DRM sync-object type itself. Linux's adapter retains fd/virtio-provider
discovery and publishes the type only after checking its implementation,
timeline support and CPU wait support. These are now release-build checks
instead of assertions. Missing backend support fails initialization; no
native CuBit synchronization type is fabricated or advertised.

Five real Linux units compiled (96534 exit0, lifecycle-compile.n5iligtz).
The actual extracted adapter passed 32 hosted mocked-discovery combinations
under UBSan, including missing features, wrong implementation and both provider
routes; failed admission publishes no supported-type list. The 24 existing
open/cleanup cases also pass (93900 exit0, device-open.yh9l5ko6). Both updated
patches reverse-validate with zero fuzz. These tests do not establish native
GPU synchronization or correctness of an unimplemented CuBit sync type.

## Memory availability refresh boundary, 2026-09-30

Common memory availability refresh now calls a backend operation instead of
passing a Linux fd to intel_device_info_update_memory_info. Linux i915/xe
retain that query in anv_gem.c. Missing or failed refresh leaves the common
heap availability snapshot unchanged, matching the former failed-query path.
Removed unused fd arguments from initial memory-info and heap construction.

Five real Linux units compiled (96380 exit0, lifecycle-compile.bcva4qtl);
both modified patches reverse-validate with zero fuzz. This is not a native
memory-budget implementation: CuBit must provide actual authorized allocation
limits and availability, not substitute global RAM capacity or static PCI
defaults. Physical discovery still has its Linux constructor and cannot yet
create a native ANV device.

## Physical parameter and memory-type backend selection, 2026-09-30

The physical-device audit found direct Linux selection in parameter discovery
and memory-type construction, including a default-to-i915 memory path for
unknown backends. `physical-backend.patch` routes both through backend
callbacks; missing callbacks return initialization failure. Linux i915 and
Xe tables point to the existing functions. This does not invent memory types,
heaps or support flags for CuBit based solely on a PCI identifier.

Five real Linux translation units compiled (68984 exit0,
lifecycle-compile._9y3a0kc), including physical-device code. Both updated
patches reverse-validate with zero fuzz. Native physical construction remains
unfinished: DRM enumeration, synchronization-type discovery, supported-engine
and memory properties must be supplied from actual CuBit interfaces. The
existing typed identity/topology query alone is insufficient.

## Fresh source reproduction, 2026-09-30

Ran the full private preparation script from pristine pinned Mesa26.2.3 into
target/lifecycle-repro.ZUWlvG/source (69953 exit0). All patches applied with
zero fuzz. Whole-tree comparison with the active isolated source found only
two generated Python cache directories, under src/intel/dev and src/util;
source contents otherwise match. This checks that recent lifecycle edits are
captured by the preparation series rather than existing only in a build tree.
It does not make the main shared preparation script complete: its integration
is still separate from the private script. Build28619 remains active.

## Common ANV device unit compiles natively, 2026-09-30

Timestamp reads and VM fault collection now dispatch through the backend.
Linux i915 retains basic render timestamp reads; Xe retains correlated
render/CPU sampling and VM fault collection. Correlated sampling is optional
and selected by callback availability; absent basic reads report device loss,
not a fabricated timestamp. Fault collection requires both the advertised
capability and a callback. Common device code no longer includes DRM ioctl
or Linux i915/xe device headers.

The actual isolated CuBit anv_device.c object compiled successfully (79568
exit0), as did the four Linux regression units (lifecycle-compile.wis440_5).
Reverse zero-fuzz patch validation passed. This clears one significant native
compile boundary, not the full link, backend implementation or hardware
rendering requirements. A full rebuild follows to expose remaining integration
dependencies; no successful native GPU-device creation is claimed.

## Device transport teardown callbacks, 2026-09-30

`context-backend.patch` now routes common creation-failure and normal-destroy
cleanup through abort_device/close_device callbacks. Both must exist before
opening a transport. Linux abort retains virtio-unref then close; Linux normal
destruction retains close only. The closed descriptor is invalidated. Native
backends may share a cleanup callback if their two lifetime paths coincide.
No direct device-fd open/close remains in common anv_device.c.

The actual-function mock fixture now also verifies both cleanup contracts and
descriptor invalidation (24 combinations, UBSan PASS, device-open.h0jcstxa).
Four real Linux units compiled (15601 exit0, lifecycle-compile.9fp1hcd6), and
reverse zero-fuzz patch validation passed. Actual runtime cleanup remains
unverified. Timestamp reads and Xe fault reporting are the remaining direct
OS-specific calls in this common unit; native transport implementation is
still required, not provided by these interface changes.

## Device-open failure-path regression, 2026-09-30

`test-device-open.py` extracts the actual anv_drm_open_device function into
a hosted mock-transport fixture. `device-open-test.c` checks 24 combinations
of open result (-1,0,7,65535), virtio initialization result and provider type.
It verifies descriptor zero remains valid, failed open acquires nothing,
initialization failure unrefs before closing exactly once, failure publishes
no sync provider, and success selects the expected provider without closing.

Nix/UBSan run passed at target/device-open.kl9uyu5m. This verifies adapter
branching with mocked system operations; it does not establish real Linux or
CuBit cleanup, concurrency, or GPU behavior. Real structure/API compatibility
is covered separately by the four-unit Linux regression compilation.

## Device connection and synchronization-provider setup, 2026-09-30

`context-backend.patch` now provides an open_device callback, implemented for
Linux in anv_gem.c. It retains device-path open, virtio initialization and
DRM/virtio synchronization-provider selection. Open failure owns no resource;
virtio-init failure performs the former unref/close cleanup before returning
failure. Common creation checks open/status support before opening anything,
and installs the backend status callback after successful connection.

This does not yet abstract connection teardown: upstream's creation-failure
path does virtio-unref plus close while normal destruction only closes. Those
paths are preserved for a separate lifetime review rather than silently
changing Linux behavior in this extraction. CPU/GPU timestamp and fault-query
dependencies also remain. No CuBit open implementation is selected yet.

Actual Linux compilation of anv_device.c, anv_gem.c and both i915/xe backend
units passed (30437 exit0, lifecycle-compile.dps4mnbd). Reverse patch validation
passed with zero fuzz; private source preparation includes the change.
Compilation is not a runtime resource-lifetime or synchronization proof.

## Active device status versus unused wait wrapper, 2026-09-30

Source-wide reference inspection found no callers of modern ANV's
`anv_device_wait`; HASVK has its own used implementation and is untouched.
Removed the unused modern wrapper and its internal declaration rather than
creating a backend operation for dead code. Lower-level Linux GEM wait
helpers remain unchanged. Declaration removal is `unused-device-wait.patch`;
body removal accompanies `context-backend.patch`.

The active Vulkan device status callback now comes from the backend table,
preserving i915/xe's existing functions. Missing status support rejects device
creation instead of proceeding without device-loss detection. The common
device unit and both Linux backend units compiled successfully under Nix
(52555 exit0, lifecycle-compile.44barmo6). Both patches reverse-validate with
zero fuzz. These are compile/source checks, not runtime fault-injection or
native GPU execution. CuBit transport/sync/timestamp integration is unfinished.

## Context lifecycle backend callbacks, 2026-09-30

`context-backend.patch` extends ANV's backend table with setup/destroy context
operations. Common device code dispatches through those callbacks instead of
selecting Linux i915/xe routines directly. Setup requires both callbacks to
avoid admitting a context without a teardown operation. The Linux backends
retain their original routines: i915 chooses VM versus legacy context teardown
using has_vm_control; xe calls its existing VM setup/destruction functions.

`test-lifecycle-compile.py` compiled the real changed common device unit and
both Linux backend units using the pinned host configuration, with objects in
a unique directory (57697 exit0, lifecycle-compile.pmebcp7h). Reverse zero-fuzz
patch validation passed. This is regression compilation, not Linux execution
or a native CuBit backend. Real CuBit lifecycle, device transport, sync/wait
and timestamp operations remain to be implemented; native full compilation
still encounters the remaining DRM includes in anv_device.c.

## Common batch compilation and device lifecycle boundary, 2026-09-30

Build79780 ended at the unused xf86drm.h include in common anv_batch_chain.c.
`batch-chain-header.patch` removes only that include; actual isolated native
object47285 compiled successfully and reverse zero-fuzz patch validation
passed. Private preparation includes the patch.

Follow-up64640 terminated at anv_device.c's DRM header. Unlike batch-chain,
this file has actual Linux dependencies: device-path open/close, i915/xe
context/VM lifecycle and status callbacks, DRM synchronization setup, GEM BO
waits, and GPU timestamp reads. These need backend lifecycle/synchronization
operations with real CuBit implementations, not header substitutions or
successful no-op callbacks. Existing BO/queue backend hooks alone do not
cover the entire device lifecycle. Full native ANV link remains incomplete.

## Native synchronization regression preparation, 2026-09-30

`futex-native-test.c` links the actual patched upstream Mesa helper with CuBit
libc, without a syscall mock. It tests value mismatch, absolute monotonic
timeout, and a pthread waiter awakened by the kernel's reported wake count.
Both producer and waiter use bounded deadlines rather than treating a fixed
startup sleep as evidence of a queued waiter. Native isolated compile/link
succeeded at `/tmp/cubit-mesa-futex-native.app`; target execution and manifest
integration are still pending. This is not yet a native synchronization pass.

Full Mesa rebuild79780 remains active in isolated-build.PyWBDf/build-linked
(`build-futex.log`). Read-only dependency audit57105 passed for779/915 VALID
target objects,1505 unique dependencies; missing/stale records are not covered.

## Backend routing and native futex adapter, 2026-09-29

anv-backend-build.patch excludes Linux i915/xe source files on CuBit and
prevents native selection of i915/xe/STUB backends. Until the actual CuBit
backend is implemented, selection returns NULL; no successful fake device.
Linux source set and selection remain intact. Rebuild99507 stopped at
ANV allocator's calls to missing Mesa futex helpers.

CuBit libc overlay/src/cubit/syscall.c already translates SYS_futex wait/wake
to CuBit kernel services. Its WAIT_BITSET9 path takes an absolute monotonic
deadline; WAKE1 returns the wake count. futex-platform.patch adds that Mesa
adapter without Linux headers or kernel syscall-number assumptions. The
operation constants refer to the libc adapter contract. ThreadSanitizer's
existing UTIL_FUTEX_SUPPORTED disabling still takes precedence.

Actual isolated native futex.c and anv_allocator.c compiled (13153 exit0).
Hosted UBSan futex-adapter-test.c verifies forwarded absolute/null timeouts,
expected value, match-all bitset, wake count and errno. It mocks transport
only: target scheduling, lost-wakeup and timeout behavior still need native
tests. Private preparation includes both patches; no shared libc edits.

## External-memory capability policy, 2026-09-29

Build61781 ended exit1 at Linux i915/anv_batch_chain.c after compiling the
generation-specific ANV paths. Source audit found unconditional Linux fd,
dma-buf and DRM extension flags in anv_physical_device.c, plus external-buffer
properties accepting fd/userptr handles independently of the extension list.

external-memory-policy.patch makes CuBit's fd fence/memory/semaphore, dma-buf,
host-memory, acquire-unmodified, DRM modifier and physical-device DRM extension
flags false. Generic external-memory/fence/semaphore vocabulary is retained;
supported handle types must come from actual native transports. Image queries
with nonzero external handle types take the existing unsupported/zero-image-
properties path; handleType0 ordinary-image semantics remain unchanged.
Buffer queries use the existing unsupported response preserving the queried
compatibleHandleTypes but advertising no import/export features.

Reverse zero-fuzz patch validation passed. Actual isolated native anv_formats.c
object build87119 exited0. Physical-device compilation and runtime query tests
remain pending; these guards are not a proof of hostile-call confinement or
of a completed external-memory backend. No CuBit GPU device is exposed yet.
Private source preparation includes this patch; shared wrappers unchanged.

## ANV common header and portable image metadata, 2026-09-29

After isolated32765 terminated exit1, anv-header.patch made the pure Intel
address header unconditional and the Linux GEM header non-CuBit-only.
ANV inline address arithmetic retains Mesa's existing helpers. The next
compile error exposed vk_image.drm_format_mod gated to Linux/BSD despite
ANV using it for internal layout decisions.

image-modifier.patch enables the existing field, INVALID initialization,
data-only modifier include and common property getter for DETECT_OS_CUBIT.
It does not set MESA_SYSTEM_HAS_KMS_DRM, grant external buffers, implement
dma-buf import/export or claim WSI capability. Runtime capability advertising
still needs a separate backend audit. Both patches reverse-validate zero-fuzz;
private preparation includes them.

Build61781 is LIVE in isolated-build.PyWBDf/build-linked, log build-anv.log.
It has compiled initial ANV generation-specific command, query, shader and
BLORP objects past the former errors (step25/239 observed). Header changes
also trigger Vulkan runtime recompilation. Not a complete driver link or
hardware execution result; preserve inputs until terminal.

## Isolated build reaches ANV, 2026-09-29

Dependency audit48513 passed608 of924 valid recorded target objects with
1333 unique allowed paths. Full build32765 reached ANV genX_blorp_exec.c;
anv_private.h unconditionally includes common/intel_gem.h, which requires
Linux DRM types. This is the next actual CuBit backend/header boundary.
At handoff the process remains live on polling while another compiler drains;
the logged failed compile is not permission to edit inputs before terminal.
No full-build success or runtime rendering claim. Shared wrappers untouched.

## Backend call-site audit during isolated build, 2026-09-29

The pinned anv_kmd_backend.h and actual anv_allocator.c/anv_batch_chain.c
call sites require a complete CuBit backend, not metadata alone:

- gem_create returns a nonzero local handle plus actual allocation size;
  mmap maps bounded backing, and close must respect outstanding device work.
- vm_bind_bo/unbind_bo operate in the device VM; bind completion participates
  in submission ordering. Sparse vm_bind additionally receives explicit
  operation/address/offset/size and optional bind-timeline signaling.
- queue_exec_locked requires the device mutex already held and must honor
  wait/signal arrays; queue_exec_async has a different locking contract.
- Userptr import, placed mappings, sparse resources and optional counters
  cannot be claimed merely because the function table compiles.

CuBit's current A20 device query deliberately exposes none of these
operations. A local integer BO handle can index retained authorized resources,
but must never become a global authority or physical-address shortcut.
Existing device-address-spaces.md separates CPU VA, GPU VA and DMA/IOVA;
recycling backing requires completed device work and translation retirement,
not just dropping the app's handle. ANV selection currently has i915/xe/stub
only: a real CuBit backend and selection still remain to implement.

## Live dependency audit, 2026-09-29

Private audit-isolated-deps.py derives target objects from compile_commands
using the isolated compiler wrappers, then inspects Ninja's VALID dependency
records. It rejects paths outside the exact Mesa source/build, CuBit libc
headers, and pinned compiler/C++ header roots, as well as Linux headers.
Host generators are excluded explicitly; missing/stale objects are reported
as uncovered, not silently claimed as passing.

Audit6921 exited0 on the ongoing isolated build:149 of924 target objects,
612 unique dependency paths. Example os_time.c uses CuBit time/pthread-era
system headers rather than Linux cross sys-include. Build32765 remains live,
latest step383/1056. Rerun after completion for whole-build evidence; this
snapshot alone does not validate every library or runtime ABI.

## Fresh isolated-header full build started, 2026-09-29

Private isolated-cc/c++ wrappers retain CuBit's static link/startup contract,
but invoke the raw pinned musl cross compiler with explicit CuBit/compiler
headers and C++ standard headers. They unset ambient CPATH/C_INCLUDE_PATH/
CPLUS_INCLUDE_PATH/OBJC_INCLUDE_PATH. No shared wrapper changes. Pinned raw
binutils are selected with -B: first configuration failed without that
Nix-wrapper-provided linker path, then fresh build-linked configuration passed.

All accumulated patches, including latest perf/tracing changes, applied to
fresh source target/isolated-build.PyWBDf/source. Full build32765 is LIVE in
build-linked at handoff, logs configure-linked.log and build-linked.log.
No completion claim until native dependency audit and eventual execution;
library successes from the older header search setup are not substituted
for this rebuild. Private wrappers use exact pinned store paths intentionally
for this experiment; production wrapper integration needs coordination.

## Header isolation audit, 2026-09-29

Actual -E -v probes show cubit-c++ places Linux-target cross-toolchain
sys-include before CuBit's -idirafter headers. This is cross-toolchain Linux
header exposure, not evidence of a glibc host ABI in every compiled object.
cubit-cc also inherits Nix dependency include flags despite -nostdinc.
Existing archive successes therefore do not establish hermetic target builds.
Shared compiler wrappers remain untouched pending coordination/build-lock access.

An explicit raw pinned g++ invocation with -nostdinc/-nostdinc++, only its
C++/compiler headers plus CuBit sysroot, passed header-isolation-test.cpp
(string/vector/atomic/pthread/mmap; rejects reachable linux/types.h).
Private test-isolated-compiler.py replayed the actual Mesa brw_compile_vs.cpp
command under that isolation and audited every generated dependency path:
87346 EXIT0, target/isolated-compiler.ydyjnhsv. Target object not executed.

`tracing-headers.patch` guards the GEM include in intel_driver_ds.cc with
HAVE_PERFETTO, matching its only users. No Perfetto code is removed. Replaying
that tracing unit with the same header allowlist also exits0, output
target/isolated-compiler.dkdpk98q. Full fresh isolated compiler configuration
and rebuild remain necessary; one audited object is not the whole library.

## Counter-result calculations separated, 2026-09-29

`perf-results.patch` moves the unchanged contiguous result-accumulation block
from intel_perf.c to intel_perf_results.c, retaining its license. Linux builds
compile both; CuBit builds the results plus generated metrics, not Linux
counter access/query code. No successful stream-open/configuration stubs.
Native libintel_perf.a job93057 exited0. This archive does not supply the
remaining optional counter-service functions required by ANV callers.

Hosted UBSan test12490 exited0: result clear, 4,096 counter-wrap cases and
65,536 shifted timestamp samples. Test uses real extracted helpers; library
NDEBUG matches release configuration, test assertions remain enabled. Initial
test compile attempts lacked internal include/configuration flags and failed;
corrected HAVE_PTHREAD/HAVE_STRUCT_TIMESPEC and Intel include path. No target
execution. Patch reverse validation passed zero-fuzz; private preparation
includes the patch, fresh-chain retest pending for this latest addition.

Full native build30987 now stops at intel_driver_ds.cc: its GEM include pulls
Linux DRM types that conflict with the CuBit fourcc __u64 alias. Importantly,
the C++ compile sees Nix Linux headers while the C compiler did not. Inspect
both the tracing dependency and target C++ header isolation before treating
this as just a typedef mismatch. No active jobs or changed NUC image.

## Optional OA performance-query policy, 2026-09-29

Source inspection found a non-native shortcut that must not be used:
intel_perf.c's oa_metrics_available treats fd=-1 as supported for offline
metrics generation. CuBit capability handles must never be represented by
that sentinel to satisfy discovery. anv_physical_device.c already gates
KHR_performance_query and INTEL_performance_query on a non-NULL perf config.

`perf-policy.patch` keeps perf NULL and command count zero on CuBit before
any Linux metric initialization; Linux's existing path remains unchanged.
This explicitly means OA counters are unavailable until an authorized native
interface exists. It is NOT a successful counter backend or replacement for
ordinary pipeline-statistics/occlusion queries, timestamps or rendering.

Private test-perf-policy.py compiles the actual init function and extracts
the two actual extension predicates into a minimal hosted fixture. Five fd
values (including -1) and both debug settings leave extensions false without
calling the metric-capability helper. UBSan exit0, target/perf-policy.27g76_gv.
This is policy-branch testing, not a full ANV compilation or device test.
The full build still needs performance-library OS separation; this guard
alone does not resolve the compile failure. Private preparation now includes
the patch; the accumulated chain before this addition was fresh-tested.

## Native shader compiler and reproducible patch chain, 2026-09-29

Full build53972 ended exit1 in Linux `perf/i915/intel_perf.c` at step151/385.
The shader compiler object work had completed; explicitly building
`src/intel/compiler/brw/libintel_compiler.a` then linked successfully (exit0).
This is the actual Mesa Intel compiler cross-built for CuBit, not a substitute
compiler. Target execution, complete ANV linking and rendering remain untested.

Private prepare-device-core.sh now applies every device/ISL/common patch,
including engine, common-build and address-header changes. Fresh preparation
29140 exited0 at target/port-repro.wgst18/source; ten changed key source/build
files compare byte-for-byte with the compiled device-build.jZSWDe tree.
The preceding fresh attempt14467 failed because the ISL parent directory
was read-only; fixed that exact preparation permission and used a NEW tree.
Shared preparation edits remain deferred after another actual flock exit1.

Performance-counter support is not merely a header issue: anv_perf.c invokes
OS metric-stream/configuration operations. The next work must keep optional
performance-query advertising consistent with implemented capabilities while
preserving normal rendering and future counter support; fake successful DRM
calls would not establish a working backend.

## Native Intel common library, 2026-09-29

`address-header.patch` extracts Mesa's unchanged intel_canonical_address and
intel_48b_address helpers into intel_address.h with the original license and
PRM comment. intel_gem.h includes it for existing Linux callers; auxiliary
mapping includes the pure header directly. No mapping allocator or address
authorization behavior is changed. Reverse zero-fuzz patch validation passed.

Native `src/intel/common/libintel_common.a` build84516 exited0 (12 steps),
including auxiliary mapping, L3/URB configuration and engine helpers. Hosted
UBSan address-header-test.c passed 524,288 inputs covering all upper16-bit
patterns and lower/upper canonical-half edges. These functions CONVERT,
not validate, addresses; untrusted bindings still need the separate CuBit
range/canonical validation before conversion.

Full native build53972 started after this success, still running at handoff
through real Mesa shader-compiler objects. Do not edit its inputs until it
terminates. Main preparation script still needs accumulated patch integration.
No new NUC image or claim of full ANV/native rendering completion.

## Engine helper separation, 2026-09-29

`engine-platform.patch` retains Mesa's unmodified engine counting/name helpers
but excludes Linux fd discovery and semaphore support queries under CuBit.
No replacement discovery result or advertised queue is supplied. Hosted UBSan
test `engine-core-test.c` passed 11,520 count cases plus names, including
empty lists and noncontiguous instance/GT IDs. Native object compiled without
i915/xe/drm/ioctl undefined references. Private test-engine-core.py job48504
exited0; output `target/engine-core.squm427z`. Not hardware execution.

`common-build.patch` routes Linux i915/xe/GEM/bind-timeline sources out of the
CuBit common library while preserving the Linux source set. Both patches
passed zero-fuzz reverse validation on device-build.jZSWDe. Not yet integrated
in the shared preparation script. Meson regeneration requires the pinned host
mesa_clc/vtn_bindgen2 PATH used during configuration; a copied read-only
build/bin/drm-shim also required owner-write permission for regeneration.

Latest library build96981 exits1 at intel_aux_map.c including intel_gem.h.
That remaining common-header dependency needs inspection; do not claim the
common archive or full ANV links. Log `/tmp/cubit-mesa-common-build.log`.

## Native Intel surface-layout library, 2026-09-29

Full native build56685 reached step650 and stopped at isl_drm.c's Linux
i915 header dependency. `isl-platform.patch` excludes only that include and
the two legacy i915 tiling conversion functions for CuBit. Mesa's modifier
tables and surface-layout implementation remain unchanged; no ioctl stubs
or invented tiling constants were added. Linux compilation is unchanged.

Applied to isolated `target/device-build.jZSWDe/source`; zero-fuzz reverse
patch validation passed. Nix cross-build of `src/intel/isl/libisl.a` passed
(four steps, exit0). This is compilation, not native rendering verification.
Patch is not yet wired into the shared preparation script.

The next full build exits1 at `src/intel/common/i915/intel_engine.c`:
Linux engine discovery still enters the native target. Next work must
separate that OS query boundary and provide actual CuBit engine discovery,
not satisfy it with fake Linux headers or a successful no-op. Full native
ANV linking and physical Mesa rendering remain incomplete.

## Fresh native device library build, 2026-09-29

`device-build.patch` separates the fd-query portion of intel_hwconfig.c and
selects common device-info/topology/workaround sources for CuBit in Meson.
Linux builds retain their KMD sources. No replacement DRM calls or fake
success paths are supplied. The pure hardware-configuration table processor
remains compiled; consuming a real table still requires validated driver data.

Private `tests/mesa-anv/prepare-device-core.sh` runs the normal preparation,
then device-platform, device-topology and device-build patches, all zero-fuzz,
on a fresh pinned source copy. Job19993 exited zero after fresh configuration
and `ninja -j2 src/intel/dev/libintel_dev.a` in
`target/device-build.jZSWDe/build`. This is a real CuBit cross-built static
archive (11 build steps), not the Linux baseline. Undefined-symbol inspection
found no i915/xe/drm/ioctl query references. Extracted core/topology sources
match the previously differential-tested source tree byte-for-byte.

The initial preparation attempt60915 failed from incorrect patch hunk line
counts; the corrected patch was tested on a NEW source tree, not an already
partially patched one. Main preparation/finalizer script integration still
awaits the shared lock edit window; the private wrapper is the reproducible
working command meanwhile. ANV backend, BO/VM/submission/sync and full native
Vulkan linking are NOT established by this library build.

## Common topology extraction, 2026-09-29

`device-topology.patch` (after device-platform.patch) moves Mesa's mask
construction and topology conversion into `intel_device_info_topology.c`.
i915 now builds an explicit internal view of its DRM reply; no structure cast
or Linux ABI enters common code. The mask builder uses the same algorithm,
with a local descriptor and separately allocated zeroed data instead of a
DRM flexible-array allocation. Original license notices are retained. This
is Mesa metadata code, not a new shader compiler or renderer.

Private `tests/mesa-anv/test-device-core.py` compiles the extracted sources
using the real host and CuBit compilation databases. Job80433 passes 1,400
full-structure differential comparisons against the original pinned Mesa
i915 mask helper (ADL-N and Skylake shapes, sparse slice/subslice masks).
Native combined object `target/device-core-check.tch17y_j/native-core.o`
has no unresolved i915/xe/drm/ioctl symbols. It still imports Mesa utility /
workaround and C runtime functions; this is NOT a linked Vulkan driver or
target execution result. Reverse patch validation passes with zero fuzz.

Build integration remains pending the shared build-script edit window:
add the new common source to Mesa's dev Meson target, conditionally select
Linux KMD sources, and append both adaptation patches to preparation. The
older test-runtime-finalize.py also needs the extracted source added to its
link command; use the private device-core test for the modified tree until
then. intel_hwconfig.c separately imports Linux query helpers and still needs
the same platform split. Do not infer complete native ANV compilation.

## Device-info platform separation probe, 2026-09-29

`device-platform.patch` conditionally excludes DRM includes and fd-based
discovery/memory refresh from the CuBit compilation of Mesa device-info. It
does not replace those functions with stubs. Apply it AFTER
runtime-finalize.patch; it is not yet wired into prepare-cubit-source.sh
(shared build-script lock was occupied). The reviewed source-cubit tree has
it applied, and reverse patch validation passes with zero fuzz.

Native compile job36070 passed with the configured Mesa cross-build flags,
producing `target/device-core.lrr8u1fl/device-info.o`. Undefined-symbol audit
shows no drm/ioctl discovery imports, but DOES retain
`intel_device_info_i915_update_from_masks`. Mesa's common `fill_masks` uses
that helper even for offline PCI defaults. It builds a temporary DRM topology
data structure without performing I/O. Therefore this is NOT a linkable native
device-info library yet: the pure topology construction/conversion needs
extraction into common Mesa code. Keep the original algorithm and validate
against the hosted Mesa path; do not satisfy the dependency with a successful
stub or claim device discovery from this object compile. Native Meson still
unconditionally includes the Linux i915/xe source files and also needs routing.

## C client across native IPC, 2026-09-29

Private ipctest-client now links the production `cubit-device-query.c` and
`cubit-device-native.c` with the real Ada export. `cubit-device-smoke.c` checks
an empty capability fails without modifying decoded output, then executes 32
identity/topology pairs through the authorized test slot. C objects are built
with cubit-cc (freestanding, no SSE/red-zone/PIC/stack protector) and combined
into the client's `build/gpu-query-c.o`; only main.adb links that object.

QEMU4CPU TCG job51782 printed both `GPU-QUERY-C-IPC: PASS C decoder and Ada
bridge` and the existing Ada query / async-ipc PASS markers. The runner exited
zero after its final fault scan. This exercises the actual C/Ada ABI and kernel transport,
not just a mock C callback. The fixture still returns synthetic GPU metadata;
Mesa device-info initialization is separately host-tested and is not linked
here. No Vulkan device, graphics command execution or hardware acceleration
is established by this test. The existing NUC .img remains unchanged.

## Native IPC regression, 2026-09-29

The private graphics workspace's existing ipctest client now calls the real
`Native_GPU_Query.Execute` through its manifest-granted test endpoint. The
server uses the production `Intel_GPU_Device_Query.Respond` with an explicitly
synthetic 46D2/revision17/DSS1/EUFFFF snapshot. The codec lives once in private
`userspace/lib/graphics`, shared by the Intel driver and fixture. There is no
GPU emulation and no new application grant policy.

The test checks an empty capability slot fails with cleared output, then
identity and topology succeed through the authorized slot. QEMU4CPU TCG run
25919 printed `GPU-QUERY-IPC: PASS synthetic topology, native capability IPC`
and `TEST: PASS async-ipc`. This crosses the real CuBit kernel and server reply
path, unlike the earlier hosted capCall fixtures. The C callback and full Mesa
client are not linked into this Ada test yet. The runner's final fault scan
subsequently completed successfully: job25919 exited zero, including the
runner's final fault scan. Log: private
`tmp/cubit-headless-async-ipc-serial.log`.

The disk is a separately created 64MiB ext2 scratch image; neither the user's
disk nor the offscreen-draw .img was changed. The test profile, source changes
and rebuilt kernel/initrd remain in `intel-presence.F8KpDB`.

## Native capability-call bridge, 2026-09-29

`cubit-device-native.c` supplies the client transport callback using the
private `tests/mesa-anv/native/Native_GPU_Query` Ada export. It accepts an
already-authorized capability slot, not a PID, and restricts calls to the
read-only query protocol. Ada builds a native `CuBit.Messages.Message`, calls
`capCall`, and checks both the returned tag and copied reply envelope before
copying four scalar words across FFI. Kernel authority tags are not accepted
from C or exposed as caller-settable data. The slot must remain owned and
unreplaced throughout both queries; no reconnect, retry or implicit grant.

Private job95476 compiled this bridge against the actual CuBit runtime.
Hosted C-wrapper tests and CuBit C compilation pass at
`target/native-query.8lYR9z`, including invalid slots, nulls, transport
failure, and a well-formed UNAVAILABLE response (which is correctly rejected
by the device decoder without changing its output).

Private hosted Ada fixture52315 passes the same bridge implementation with a
substituted capCall: invalid arguments make no call, mismatched return/copy
tags fail with cleared output, and service-level errors remain intact. This
checks bridge branches, not the behavior of the kernel call itself.

The bridge is not yet linked into a native Mesa application, and no app has
been granted the new driver endpoint. Native client/server delivery, death /
revocation handling and endpoint lifetime tests are still outstanding. These
compile and fixture results are not evidence of kernel IPC delivery or GPU
execution. The synchronous query is control-plane startup only, not a design
for submitting frames or waiting for rendering completion.

## Mesa client for device queries, 2026-09-29

`cubit-device-query.{h,c}` validates both identity and topology replies from
an injected transport. It requires exact envelopes, version/status, vendor /
device, reserved bits and full-width range checks before narrowing any field.
Both calls must use one lifetime-pinned authorized endpoint; reconnecting
between them is forbidden. Failures preserve the output. Its C message is an
adapter value, not a castable overlay of CuBit's native IPC structure.

`cubit-device-info.c` connects this decoder to Mesa's actual runtime-defaults
initializer and measured-topology helper. It supplies PCI revision but does
not guess Mesa stepping, run premature finalization or advertise a Vulkan
device. Backend identity stays INVALID. Memory, timestamp frequency, stepping
mapping and public VM/queue/sync services are still required.

Hosted UBSan protocol test `target/device-query.zuO6vR` passes (all EU masks,
reply field-bit mutations, transport failures and unchanged-output checks);
CuBit cross compilation passes too. This transport is still injected in the
test: a native IPC wrapper, endpoint grant and actual client/server delivery
test remain outstanding. No hardware-accelerated Mesa claim follows.

Integration test `target/device-info.toP2gB` passes with real Mesa code: a
sparse DSS mask retains physical ID bound six rather than becoming packed
subslice zero, four measured EUs replace offline defaults, and explicit Mesa
force-probe denial leaves the output unchanged. This adapter also compiles
with the CuBit cross compiler. The fixture uses synthetic replies, not NUC
observations or a native IPC exchange.

## Native read-only device query, 2026-09-29

Private workspace `intel-presence.F8KpDB` now dispatches label `0x0A20`
through `Intel_GPU_Device_Query` from the actual Intel service receive loop.
An already authorized endpoint is required; no discovery/grant policy was
changed. Four-word requests are `[1, selector, 0, 0]`, with zero flags and
reserved tag bits. Responses are `[status, 1, value0, value1]`:

- Selector 0: admitted PCI vendor/device/revision packed into bits 0..39;
  value1 is public rendering features, currently always zero.
- Selector 1: retained, post-reset observed DSS and common EU masks. This is
  a historical topology snapshot, not a current power/ownership assertion.
- Status 0 succeeds; 1 is bad request, 2 unavailable, 3 unsupported label.
  Every failure has zero payload. No addresses or raw MMIO are returned.

Device admission remains 8086:46D2 only. Missing topology, empty/unsupported
DSS masks and incomplete EU pairs are rejected. Queries do not perform MMIO
or block on hardware. Unknown requests receive an explicit response rather
than being silently discarded. This is not yet registered in the public CCL
catalog or consumed by Mesa's native discovery provider.

Private job45398 passed hosted protocol tests (all 65,536 EU masks, all PCI
IDs, all DSS masks, envelope field ranges and request-word bit corruptions)
and the native Intel driver build. This does not prove delivery through the
kernel to a client; that integration test remains required. The existing
offscreen-draw boot image was not repackaged or changed.

Focused SPARK job92572 also passed: `Respond` termination and its version /
zero-payload-on-error postcondition are proved. Endpoint authorization,
hardware snapshot provenance and IPC delivery are outside this proof.

## Runtime defaults boundary, 2026-09-29

The runtime-finalize adaptation now also exposes
`intel_device_info_init_runtime_defaults`: it calls Mesa's existing common
initializer with runtime force-probe policy, preserves the caller's output on
failure, and leaves KMD identity INVALID. It deliberately does not use the
offline PCI helper, which applies workarounds before measured topology and
installs an i915 placeholder. No device table or workaround is duplicated.
The provider must still supply authenticated hardware data, backend identity,
memory information and finalization before exposing any Vulkan device.

Hosted test `target/runtime-finalize.59jni5qq` passes with the real ADL-N PCI
defaults for every nonempty six-bit DSS mask and every nonempty eight-bit EU
pair mask (16,065 combinations), alongside the synthetic scratch-limit tests.
It also verifies unsupported PCI ID, null output and explicit force-probe
denial fail without changing output. The real Mesa finalizer applies the
small-EU GS limit after measured topology. Reproducible patch dry-run against
the pinned pristine source passes with zero fuzz. These are hosted adapter
tests, not native device discovery, Vulkan rendering or hardware evidence.

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

The first binding-boundary adapter is now in
`tests/mesa-anv/cubit-binding.{h,c}`. It accepts ANV's canonical GPU address
and a page-aligned BO slice, verifies sign extension before converting to
raw48, and rejects zero addresses/lengths, misalignment, BO-range overflow
and GPU-range overflow. Rejection clears the output. No BO capability,
physical address, successful bind or submission is fabricated: the adapter
does not issue IPC yet and the future service must revalidate authority.
Sparse NULL binds and UNBIND_ALL are deliberately outside this request form.

Linux-hosted regression `cubit-binding-test.c` exhausts all 65536 upper-bit
patterns in both GPU address halves, all 4095 misalignments and edge ranges.
It passed with undefined-behavior sanitization; the adapter also compiled
with `userspace/libc/cubit-cc -std=c11 -Wall -Wextra -Werror`. Retained outputs:
`tests/mesa-anv/target/binding-test.Raj4xU`. This is a tested port boundary,
not an operational ANV KMD backend. The Mesa-generated rendering path still
needs CuBit BO/VM/queue/synchronization services and discovery integration.

### Offscreen surface oracle (2026-09-29)

`test-offscreen-layout.sh` compiles a Linux-hosted test against the pinned
Mesa ISL libraries. Run in `host-shell.nix`, passing the prepared source tree.
For ADL-N PCI46D2, a64x64 BGRA8_UNORM single-sample, single-level, linear render
target with256-byte pitch requires exactly16384bytes, with4-byte minimum
alignment. The private driver's new four-page offscreen slice therefore has
sufficient capacity/alignment; this does not yet submit any render commands.

The test exercises real `isl_surf_init` and `isl_surf_fill_state`, checks size,
pitch, type, compression/aux disable and address fields, and emits all16DWORDs.
Its MOCS=2 is an explicit **encoding fixture**, not a live-cache-policy decision.
Successful hosted run: `target/offscreen-layout.yO3RaQ`.

Intel TGL PRM Vol2d printed785,791,796-797 was checked alongside Mesa
`gen120.xml`: DW0 bit27 is reserved MBZ in that PRM (Mesa exposes a broader
format field); DW1 bit31 must keep the UNORM path enabled; pitch is encoded
as bytes-minus-one and linear render-target pitch must be element-size aligned.
The actual ISL output meets those checked restrictions. Do not copy arbitrary
Mesa generation-wide field widths into the ADL-N Ada record without resolving
reserved/platform-dependent bits against the manual.

This is a layout/encoding oracle only, not native ISL integration, shader
execution, a complete surface-state audit or a hardware triangle.

The private graphics workspace now prepares a retained state page at GPU VA
`0x206000`, following the four offscreen pages. Binding entry zero points to
the surface record at page offset 64; both satisfy the 64-byte alignment in
TGL Vol2d printed998. The submission image selects installed ADL-N uncached
MOCS index3 (encoded6), rather than the oracle's fixture2. Native startup
must still admit that hardware policy before execution. No draw batch uses
the new state yet, and these changes have not been merged into the main image.
Hosted tests cover all 288 record bits, the full surface fixture, MOCS
rejection, exact page-table entries and zero padding. Targeted SPARK checks
for the enlarged image and encoder pass; this is not a hardware-semantic proof.

The next draw sequence needs compiled shaders and pipeline state. The pinned
Mesa `src/intel/blorp/blorp_brw.c` provides concrete VS/FS examples using
`brw_preprocess_nir`, stage-specific keys/program data, VS VUE mapping, and
`brw_compile`. Preserve compiler-produced program metadata alongside machine
code; shader bytes alone do not specify the dispatch/pipeline configuration.

`test-offscreen-shader.sh` now compiles a fixed red fragment shader with the
pinned hosted Intel compiler. Successful run `target/offscreen-shader.mkNE8e`
emitted112bytes: SIMD8 at offset0 and SIMD16 at offset64, GRF start2 for both,
zero scratch and zero relocations. Machine DWORDs and metadata are retained
in `result.txt`; no GPU device is opened. The harness imports the actual
library's compile definitions because packed NIR header layouts must match,
initializes Mesa's SIMD policy, and supplies compiler logging callbacks.
This is not a native compiler integration or an executed pixel shader. Vertex
code, full pipeline state, command publication and readback remain required.

The oracle now also compiles a position-passthrough vertex shader:176bytes,
one vec4 input at `VERT_ATTRIB_GENERIC0`, VUE2slots with position in slot1,
GRF start2, compiler dispatch mode3, URB read length1 and entry size1 in
Mesa's reported units. It requires no scratch, relocations, vertex-ID or
instance-ID input. These are compiler outputs, not yet programmed GPU state.
Run `target/offscreen-shader.t6g79J` retains explicit little-endian
`vertex.bin` and `fragment.bin` plus metadata in `result.txt`. Vertex fetch
layout must deliver that input, and the pipeline must translate these metadata
fields using the documented hardware units before either shader is submitted.

Private native integration now retains those shader words in a separate page
at GPU `0x207000` (VS offset0, FS offset256), with three clip-space vec4
vertices at `0x206100`. `check-probe-shaders.py` compares all embedded words
against the independently compiled binaries. The private image/backing tests
and native driver build pass; targeted SPARK image-safety checks pass as well.
This does not replace the marker batch with drawing commands. No new GGTT
aliases or application submission authority are introduced by these pages.

### Vertex fetch command oracle

`test-vertex-fetch.sh` checks Mesa's GFX12 generated packers against the
TGL PRM Vol2a printed143-146 and Vol2d printed1141-1147. The fixed vertex
buffer uses GPU `0x206100`, stride16, **size48bytes**, address-modify enabled,
encoded MOCS6 and L3-bypass disabled. One valid R32G32B32A32_FLOAT element
at offset0 stores all four source components. Successful hosted run
`target/vertex-fetch.THBoWq` emits:

```
78080003 02064010 00206100 00000000 00000030
78090001 02000000 11110000
```

These packets alone are not a usable draw pipeline. In particular they do not
reset inherited VF instancing, system-generated values or component packing,
nor configure shader dispatch, URB allocation or rasterization. Do not submit
them as a purported complete triangle batch. Buffer size is not a minus-one
field; linear storage and nonzero size are required for this non-null buffer.

The private `Intel_GPU_ADLN_Vertex_Fetch` now encodes this fixture using Ada
representation records for every header, buffer-control, element and component
field (reserved bits included).128 one-hot layout checks, all encoded MOCS
values0..255 and the full Mesa fixture pass; targeted SPARK safety checks pass.
It is not yet linked into a draw batch.

Important compiler distinction: the current VS key leaves
`vf_component_packing` false. Its four reported packing words are therefore
zero initialization, **not** hardware masks to emit. Mesa
`brw_compile_vs.cpp` only computes those words when that key is enabled, and
`genX_shader.c` conditionally emits the packet. The fixed uncompressed vec4
input needs the documented full-component setup rather than copying zeros.
The metadata rerun `target/offscreen-shader.DnhAXp` preserved both shader hashes.

The private fetch builder now appends `3DSTATE_VF` (`780c0000 00000000`):
component packing, indexed/sequential cut processing and ID-offset controls
are explicitly disabled. TGL Vol2a printed147 and150-152 specify that packing
disabled stores all four components of valid elements, regardless of inherited
packing masks. A fifth Ada record covers every VF header bit, including the
documented bits13/14 omitted from Mesa's packer; both remain clear here.
The expanded10DWORD Mesa fixture (`target/vertex-fetch.5OdjAX`),160 layout-bit
checks and targeted SPARK safety checks pass. Instancing and SGVS reset packets
remain required: this is still not a complete draw sequence.

The subsequent18DWORD version adds `3DSTATE_VF_INSTANCING` for element0,
`3DSTATE_VF_SGVS`, and `3DSTATE_VF_SGVS_2`, all disabled. Intel Vol2d
printed132-138 defines those fields, including instance-stride bit9 not named
by Mesa's GFX12 packer. The typed records preserve that distinction and keep
both instancing modes off. Mesa oracle `target/vertex-fetch.bpMgR8`,288
one-hot layout tests, full packet fixture and targeted SPARK checks pass.
Only element0 is active; other elements are disabled by the single-element
VERTEX_ELEMENTS packet. All system-generated insertion controls are cleared.
Topology, state bases, URB allocation and remaining pipeline stages are still
needed before this sequence can participate in a submitted draw.

Private `Intel_GPU_ADLN_Triangle` now supplies separate topology and draw
packets. Intel TGL Vol2a printed4 explicitly says the topology field within
3DPRIMITIVE is ignored: `3DSTATE_VF_TOPOLOGY` selects TRILIST instead.
The draw uses3sequential vertices/1instance, start and base0, with predication,
indirect parameters, end-offset and extended parameters all disabled. Mesa
fixture `target/vertex-fetch.mFb3p2` matches these additional9DWORDs, and the
combined Ada test covers384 represented bits. SPARK proves the three new
encoders terminate; it does not prove rendering semantics. The packets remain
separate from the submitted marker batch pending complete pipeline setup.

### Current native boundary (2026-09-29 integration audit)

The Intel service's final loop polls service requests but currently ignores
them as unsupported. It does **not** implement the ANV BO, bind, queue or
synchronization callbacks. The successful boot probe must not be advertised as
an application queue: `Intel_GPU_ADLN_Batch_Start.Build` branches only to the
fixed `Intel_GPU_Submission_Image.Batch_VA`, with driver-owned context/page
tables. Neither arbitrary client batch addresses nor mutable client command
buffers may be substituted into that privileged bring-up context.

There is also an address-representation boundary. In the pinned Mesa tree,
`src/intel/common/intel_gem.h` supplies `intel_canonical_address` (sign extends
bit47) and `intel_48b_address` (removes the upper16 bits). ANV's
`anv_private.h` asserts canonical buffer addresses and `anv_allocator.c`
canonicalizes explicit addresses. CuBit's `Intel_GPU_VA_Placement` and
`Intel_GPU_ADLN_PPGTT.Locate` deliberately accept **raw unsigned48-bit** GPU
addresses instead. For example raw `0x800000000000` corresponds to canonical
`0xffff800000000000`, not to CPU VA or DMA address.

The native provider must validate the expected representation before converting
at that boundary. Blindly applying Mesa's truncating helper to untrusted IPC
would alias malformed high bits onto an otherwise valid GPU range. Keep
allocation offsets/ranges raw internally; convert only for interfaces that
require canonical addresses. Validate lengths and exclusive ends before
conversion as well: raw `2^48` is a range limit, not an address to canonicalize.
No provider or conversion API is implemented by this audit, and low-address
boot-probe success does not test this upper-half case.

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

### Private probe state-base encoding (2026-09-29)

The private graphics workspace now has `Intel_GPU_ADLN_State_Base`, with
representation clauses for all bits of the base-address/MOCS, stateless-MOCS,
page-bound, bindless-surface-bound and bindless-sampler-bound records. Its
22-DWORD fixed probe packet matches `tests/mesa-anv/state-base-test.c`, which
uses Mesa's generated Gen12 packer. Instruction state points at private GPU
address 0x207000 and surface/dynamic state at 0x206000. Instruction and dynamic
bounds each admit one 4KiB page. Unused general/indirect bounds are zero.

Source: Intel TGL PRM Vol2a, STATE_BASE_ADDRESS, printed pages1256-1265.
Bindless surface size is a count of64-byte entries minus one: zero does NOT
disable it. The fixed shaders do not use bindless accesses, and base zero is
unmapped in this private VM. The builder checks MOCS encoding, not whether
the owner has installed the corresponding cache policy.

`Intel_GPU_ADLN_State_Setup` now assembles a34-DWORD fragment: stalling HDC
and render-target flush, the22-DWORD base change, then state/constant/texture/
instruction/command-cache invalidation. The before/after PIPE_CONTROL headers
and control DWORDs have complete representation clauses. Intel Vol2a1120-1130
defines these fields; printed1129 requires command-cache invalidation with
state invalidation when SLICE_COMMON_ECO_CHICKEN1 redirects state cache.
Instruction invalidation covers newly populated probe shader backing, not a
claim that ADL-N requires the DG2-specific SBA workaround.

Mesa `genX_cmd_buffer.c` pre-SBA code cites Wa_18039438632 for the render-target
flush. Its Gen12 packer matches all34 assembled words (hosted output
`target/state-base.OkEh4I`). Flush and invalidation are deliberately separate:
invalidation happens at parsing time and must not race the prior flush. PRM
reserved bits remain named reserved in this TGL-layout encoder; differently
named fields in Mesa's shared Gen12 schema do not authorize enabling them.

The fragment now starts with an additional7DWORDs for initial RCS3D selection,
41DWORDs total: stalling HDC/render/depth flush (depth stall accompanies depth
flush per Mesa Wa_1409600907), then PIPELINE_SELECT with mask0x13 and media DOP
clock gating enabled. All32 selection bits have representation clauses. The
complete fixture matches Mesa's packer (`target/state-base.krl7Jl`).

Intel Vol2a1131-1133 specifies mode-switch flushing. Mesa's Gen12 initial-mode
path deliberately omits Generic Media State Clear, citing hangs outside MEDIA
mode; this fragment follows that upstream exception, not a claim of literal
PRM agreement. It is not a general media/compute mode-switch API. Intel p971
marks MI_BATCH_BUFFER_START bit10 reserved; our existing batch-start keeps it
clear (now regression checked), including where older Mesa schemas call it
ResourceStreamerEnable. Do not emit the old MI_RS_CONTROL solely because a
legacy note survives in STATE_BASE_ADDRESS documentation.

This is packet construction, not a submitted drawing batch. The caller must
use RCS and the streamer-disabled batch entry, then reissue state pointers and
configure URB and shader/raster/pixel stages.
The fixed builder is not a general barrier or arbitrary command validation
API. Linux-hosted Mesa packing and Ada representation tests do not establish
hardware rendering.

### Private VS-only URB allocation (2026-09-29)

The hosted shader oracle now calls Mesa's actual `intel_get_urb_config` with
the compiled one-row VS and46d2 PCI defaults. Output
`target/offscreen-shader.YlzG7b` reports one slice, four L3 banks,512KiB URB,
32KiB constant reservation,3576 VS entries, and zero HS/DS/GS entries. All
four stages start at8KiB unit4, with one64-byte row per entry (encoded size0).
Inactive stages have no extent. The compiled shader hashes are unchanged.
These are PCI-default assumptions, not a measurement of configured/fused URB.

Private `Intel_GPU_ADLN_URB` requires usable capacity explicitly, after any
hardware reservations. It accepts40..512KiB for this one-slice/POSH-off probe,
rounds capacity down to8KiB chunks, reserves32KiB, and limits VS allocation
to3576 entries. It emits all four stage packets as required by Intel Vol2a
pp135-142, with the Vol2d pp125-129 field layout represented explicitly.
HS/DS/GS must also be disabled by their separate shader-stage commands.
Push-constant allocation and actual L3 capacity admission remain pending.

Hosted tests sweep capacities0..1024 and Natural'Last, check32 representation
bits, and compare the eight-word fixture with Mesa (`target/state-base.GAZBT3`).
Targeted SPARK proves the valid-result postcondition: entry count64..3576,
multiple of8, and constant reservation plus entries within supplied capacity.
This proves builder arithmetic, not the truth of caller-supplied hardware
capacity, physical behavior, or isolation. No URB packet is submitted yet.

### Private constant-buffer reset (2026-09-29)

`Intel_GPU_ADLN_Constants` emits zero push-constant allocation for VS/HS/DS/GS/PS,
immediately followed by CONSTANT_ALL with shader mask31, no valid pointers,
and update mode0 (clear, not retain). All96 bits of the allocation/clear
records have representation clauses. Intel Vol2a26-28 and81-90 require
reprogramming constants before a committing/preemptible command after changing
allocation; dependent binding-table pointers still need reissue before drawing.
The twelve-word fragment contains no intervening commit or preemption command.

The Mesa packer comparison passes (`target/state-base.3Z79Pa`). The compiler
oracle now asserts all four push sizes are zero and no UBO pulling for both
fixed shaders. Zero allocations are specific to these shaders, not a general
graphics-stage default. Combined private tests check384 representation bits
and the setup/constant fixtures; targeted SPARK checks initialization, ranges,
division and termination. Neither shader-stage disables nor live submission
are implemented by this reset fragment.

### Private vertex-shader dispatch (2026-09-29)

`Intel_GPU_ADLN_Vertex_Shader` builds the fixed nine-word VS packet with
instruction-relative kernel offset, SIMD8 enabled, GRF start2, URB read length1,
no scratch accesses, no VS resources/samplers, and explicit enabled statistics.
It accepts thread limits1..546 from the admitted ADL-N profile and encodes
limit-minus-one; it does not derive the limit from a raw EU count. The kernel,
scratch, shader, payload, dispatch and output records cover256 bits explicitly,
following Intel TGL Vol2d142-149. Scratch's high DWORD stays reserved per PRM,
even though the shared Mesa packer exposes a wider field.

Output attribute offset/length are zero because this position-only probe must
configure SBE with zero varying attributes; they are not valid defaults for
arbitrary shaders. Compile-time guards tie payload assumptions to the retained
shader metadata. The hosted compiler checks SIMD8, GRF/read lengths and absent
clip/cull distances; the packet oracle matches Mesa (`target/state-base.j5kkZz`).
Private tests check all256 record bits, thread limits0..1024/Natural'Last and
the complete packet. Targeted SPARK checks initialization, conversions and
termination, not hardware execution. The packet is not submitted yet.

### Fragment dispatch metadata cross-check (2026-09-29)

The hosted shader compiler oracle now invokes Mesa's actual
`intel_set_ps_dispatch_state` and `brw_fs_prog_data_*` helpers, then packs
the resulting Gen12 PS packet. Run `target/offscreen-shader.SWbmrk` confirms
64 threads per PSD, SIMD8 in kernel slot0, slot1 unused, SIMD16 in slot2,
GRF starts2/0/2 and instruction-relative kernel offsets256/256/320.
The fixed shader has zero varying inputs but **does use vector masking**:
`brw_compile_fs.cpp` deliberately enables it for `verx10 < 125`. The initial
assumption that this simple shader could disable it was rejected by the
oracle; the Ada metadata now retains the true value. This does not change
the shader binary hashes.

`check-probe-shaders.py` checks dispatch metadata as well as both binaries,
including actual helper-derived kernel offsets. This is a Linux-hosted
compiler/state-packing check, not a native render test. The typed PS builder,
remaining raster state and complete drawing batch are still pending.

### Private pixel-shader packet (2026-09-29)

`Intel_GPU_ADLN_Pixel_Shader` builds the fixed probe's twelve-word PS packet.
Three explicit representation records cover shader control, dispatch control
and payload GRF starts (96 bits); kernel encoding reuses the verified VS
kernel record. Intel TGL Vol2d58-67 is the field-definition source, including
scoreboard address size, clear/resolve BTI and scoreboard disable fields
not exposed by Mesa's shared Gen12 packer. These fields remain zero here.
The packet enables VMask and SIMD8/16, selects GRF starts2/0/2, has no scratch
accesses or push constants, and keeps binding prefetch count zero (not a
claim that the shader uses no render-target binding).

The builder admits thread limits1..64 and encodes limit-minus-one. It is
initial-state setup: changing the limit between draws requires the documented
pixel-scoreboard stall. The test sweeps limits0..1024 and Natural'Last,
checks all96 field bits, and matches the complete Mesa helper-derived packet
from `offscreen-shader.SWbmrk`. Targeted SPARK initialization, range checks
and termination passed (private job74805); these do not prove GPU behavior.
PS_EXTRA enable, other stage/raster state and
actual native submission remain separate, unfinished work.

### Private PS_EXTRA state (2026-09-29)

`Intel_GPU_ADLN_Pixel_Extra` explicitly names all32 bits using TGL
Vol2d68-73, including the PRM's reserved bits9..10 (Mesa's shared packer
calls bit9 SimplePSHint). The probe leaves those bits clear. Its two-word
packet is `784F0000 80000000`: valid shader, per-pixel dispatch, no varying
attributes, computed depth/stencil, discard, coverage mask or auxiliary
payload requirements. The constant-red shader writes the render target;
the PRM's No_RT_Write field remains clear. This is a fixed-probe profile,
not a general shader-state constructor.

The hosted compiler oracle builds the packet from actual compiled metadata
and asserts the absence of extra payload/output requirements. Private tests
cover all32 additional record bits and both PS/PS_EXTRA fixtures (128bits
total). Job57468 passed tests and the targeted encoder termination proof;
that proof does not establish hardware execution or arbitrary shader safety.
Remaining stage disables, raster/viewport/blend state, L3 admission and
drawing-batch assembly are still required before a native draw.

### Private offscreen viewport state (2026-09-29)

`Intel_GPU_ADLN_Viewport` defines the complete SF/clip and CC viewport records,
plus aligned dynamic-state pointer fields. Intel TGL Vol2d140-141,236,871-872
defines the layouts. Full-width IEEE754 fields are retained as named raw bits,
so preparing this fixed state does not require runtime floating-point work.
The positive-height64x64 transform has scales32/32/1, translations32/32/0,
normalized guardband[-1,1], screen extents0..63 and depth limits[0,1].
It maps the fixed triangle to (16,16), (48,16), (32,48); clipping/raster
configuration and scissor enforcement are separate, not implied by this data.

SF offset512 and CC offset576 meet64/32-byte alignment. Compile-time checks
exclude overlap with the retained vertex data and the end of the4KiB state
page. These records are now copied into the private submission-image builder
at those offsets; the emitted batch still contains only the marker store.
Mesa `genX_gfx_state.c` viewport construction was cross-checked, and the
hosted packer validates all18 data DWORDs plus4 pointer DWORDs
(`target/state-base.1u48lb`). Private tests exercise640 representation bits
and compare the complete fixtures. Job87185 also passed targeted encoder
termination proofs; this does not prove floating-point transform semantics
or GPU behavior. Raster/SF/CLIP enable state and native
submission remain pending.

The submission-image regression independently checks the new viewport words
at image indices31872..31889, while continuing to check every other word of
the dynamic-state page, shader page, sparse page tables and marker batch.
This catches displaced writes and unintended overlap rather than only
comparing the viewport encoder with itself. The change allocates no new
GPU mappings and leaves scanout, the ring, and the probe command unchanged.
Private job79595 passed the image regression and targeted SPARK checks,
including the zero-on-rejection postcondition. Job83237 passed sparse-VM,
backing-layout and publication regressions and rebuilt the native Intel
driver successfully. This verifies build integration, not a NUC viewport
test: no image was packaged and no drawing command was submitted.

### Private clipping state (2026-09-29)

`Intel_GPU_ADLN_Clip` covers the complete96-bit CLIP body using Intel TGL
Vol2d6-11. In particular, statistics is the PRM's two-bit field, not merely
the bool exposed by Mesa. The fixed probe enables normal clipping, viewport
XY testing and perspective division, with no user clip distances or early
backface cull. Viewport index is clamped to0, render-target layer forced to0,
and point-width limits use the documented U8.3 representation. Depth clip
and final raster culling remain separate RASTER configuration requirements.

The Mesa normal graphics path (`genX_gfx_state.c`) is the cross-check, not
the special simple-shader path that uses pretransformed geometry and disables
perspective division. Mesa packer oracle `state-base.QOdkvO` confirms the
four words `78120002 00000400 90000000 0003FFE0`. Private tests check all96
additional representation bits (736 with viewport) and the complete packet.
Job19628 passed these tests and targeted encoder termination proofs; neither
is a proof of clipping behavior on hardware.
This fragment is not yet submitted; SF/RASTER/SBE/WM, disabled unused stages
and drawing-batch assembly remain pending.

### Private raster state (2026-09-29)

`Intel_GPU_ADLN_Raster` names every control subfield and all three full-width
IEEE754 depth-offset fields from Intel TGL Vol2d77-81. The probe selects
solid front/back fill, cull NONE, CCW front winding, near/far clipping and
zero depth bias. Cull mode must be explicitly1: zero would discard every
triangle. Forced sample count remains0, as required for the intended
single-sample setup rather than render-target-independent rasterization.
Scissor is currently off for the contained fixed triangle; arbitrary app
geometry requires a completed scissor policy and is not admitted here.

Mesa oracle `state-base.rSeSUp` verifies all five words:
`78500003 04210001 00000000 00000000 00000000`. The combined private
viewport/clip/raster test covers864 representation bits and complete packet
fixtures. Job45420 passed those tests and targeted encoder termination
proofs. This is encoded state only; SF/SBE/WM and unused-stage disables,
sampling/blending/depth state and whole-batch submission are still pending.

### Private SF setup state (2026-09-29)

`Intel_GPU_ADLN_SF` implements the96-bit SF body using Intel TGL
Vol2d94-98. It enables viewport transformation and statistics (matching the
CLIP packet), uses8-bit subpixel precision, state point width1, line width1,
true AA line distance and zero provoking-vertex selectors. Widths are
explicit U8.3/U11.7 raw fields rather than runtime float conversions.

The builder consumes the admitted VS URB entry count, accepting64..3576
in multiples of8, and requires VS as the last enabled geometry stage.
Below192entries it selects per-polygon dereference; otherwise block32.
This matches both the PRM restriction and Mesa's `intel_urb_config.c`.
DS/GS require different rules and are not supported by this fixed probe.
Mesa oracle `state-base.kgiOPU` confirms both four-word packets:
`78130002 00080402 [20000000 or 00000000] 00004808`.
The private combined suite checks960 representation bits and entry counts
0..4096 plus Natural'Last, including the192 threshold. Actual L3/URB
capacity still requires admission; PCI default data alone is insufficient.
Job87805 passed tests and targeted initialization/division/termination
checks. No SF command has yet been submitted to hardware.

### Private windower state (2026-09-29)

`Intel_GPU_ADLN_Windower` names all32 body bits from TGL Vol2d150-153.
Force-dispatch and force-kill remain NORMAL as the manual requires; dispatch
must be enabled by the actual PS/RT configuration, not forced around missing
state. Early depth/stencil stays NORMAL for the fixed shader, matching
`genX_shader.c` for a shader without early-fragment tests or side effects.
The hosted compiler now asserts zero barycentric interpolation requirements
and no early-fragment-test requirement. Legacy depth-control bits27..30
stay reserved/zero per PRM rather than using Mesa's legacy field names.

The packet is `78140000 80000000` (statistics only). Line/endcap AA widths
retain their0.5-pixel defaults; line antialiasing is disabled in RASTER and
the fixed topology is triangles, so Mesa's general1.0-pixel line-AA choice
is not needed here. Private combined tests now cover992 representation bits.
The packet does not replace required PS_BLEND writable-RT, sample-mask,
depth/stencil or SBE configuration; these still precede any actual draw.
Job40312 passed the combined tests and targeted encoder termination proof;
job43340 passed both packet and compiler oracles (state-base.z9xe5k and
offscreen-shader.RBtZH6), retaining the existing shader hashes.

### Private SBE attribute setup (2026-09-29)

`Intel_GPU_ADLN_SBE` represents all160 body bits using Intel TGL Vol2d84-90:
control fields, individual point-sprite/constant-interpolation mask bits,
and32 two-bit component selectors. The fixed shader sends zero varying
attributes, consistent with PS_EXTRA.Attributes=0 and the compiler's zero
varying/flat-input assertions. Attribute swizzling and primitive-ID overrides
are disabled. XYZW component selectors follow Mesa's simple-shader setup;
they do not enable attributes when the output count is zero.

The PRM explicitly forbids read length0. Like Mesa, the probe forces
read offset1 and length1 in32-byte units. This accesses[32,64) inside the
allocated64-byte VS entry; compile-time checks guard that allocation bound.
No varying data from that read is delivered to the constant-color shader.
The six-word oracle fixture is
`781F0004 30000820 00000000 00000000 FFFFFFFF FFFFFFFF`.
Combined tests cover1152 representation bits plus the earlier URB thresholds.
This remains an unsubmitted fixed-probe fragment, not general application
attribute validation. Stage disables, output state and final assembly remain.
Job18269 passed the tests and targeted encoder termination proofs;
job18519 passed packet/compiler oracles (state-base.EMiAT8 and
offscreen-shader.bHEb3N), with unchanged shader binaries.

### Single-sample coverage state (2026-09-29)

Private graphics workspace `intel-presence.F8KpDB` now has
`intel_gpu_adln_sampling.ads`: full representation clauses for the
MULTISAMPLE and SAMPLE_MASK bodies, including reserved fields. Primary
source: Intel TGL Vol2d printed50-51 and82. Sample count is log2 and must
match every bound render target. The fixed probe selects one sample,
pixel center, no DX9 offset, and coverage mask1; mask0 would suppress it.
Packets: `780D0000 00000000 78180000 00000001`.

Linux-hosted Mesa Gen12 packer oracle `state-base.jOz01D` agrees. Private
Nix viewport regression covers1216 individual representation bits and
complete packet fixtures, plus an explicit comparison against the built
offscreen surface's sample count. Targeted GNATprove analysis passes for
the two sampling encoders (termination/flow); hardware semantics and
surface consistency are documented/regression-tested, not formally proved.
No native drawing submission or boot image was changed. Sampling remains
a fragment awaiting whole-batch assembly with blend/depth state, unused
stage disables, and admitted L3/URB capacity.

### Pixel-output blend state (2026-09-29)

Private `intel_gpu_adln_pixel_blend.ads` represents every field in PS_BLEND,
the common blend DWORD, both per-target DWORDs, and BLEND_STATE_POINTERS.
Primary source is Intel TGL Vol2d printed3,56-57,199-204; Mesa Gen12 packing
and `genX_gfx_state.c` provide the cross-check. RT0 has four writable channels;
RT1..7 have all channel writes disabled. Blending, logic operations, alpha
testing/coverage, independent alpha and dithering are disabled. Pre/post
clamps both enabled with RT-format range, source-only clamp disabled.

The complete68-byte state is copied at dynamic-page offset640, checked for
64-byte alignment, non-overlap with CC viewport, and page bounds. Commands
are `784D0000 40000000 78240000 00000281`; they are not yet submitted.
The common DWORD is0; RT0 entry is `[0,11]`; RT1..7 each `[15,11]`.
Hosted Mesa oracle `state-base.tDydCM` agrees with those complete fixtures.
Private Nix regression passes1376 representation-bit checks and independently
checks the whole prepared GPU image, including blend bytes and unchanged
marker-only batch. Targeted SPARK image-builder analysis passes its bounds,
initialization, arithmetic, termination and zero-on-rejection checks; this
does not prove GPU hardware semantics. This is preparation for hardware rendering, not a draw
result or general-purpose blend implementation.

### Explicit depth/stencil disable (2026-09-29)

Private `intel_gpu_adln_depth_stencil.ads` describes WM_DEPTH_STENCIL and
DEPTH_BOUNDS including their different header modify-disable fields.
Sources: Intel TGL Vol2a printed41,163-164 and Vol2d19,155-157. Every header
and packed body subfield has an Ada representation clause. Keep-existing
state flags are clear so the commands replace, rather than preserve, prior
context state. Depth/stencil tests and writes, double-sided stencil and depth
bounds testing are off; all stencil masks/references zero; bounds are float0..1.
The packets are `784E0002 0 0 0 78710002 0 0 3F800000`.
Mesa Gen12 oracle `state-base.GS0nyK` agrees. Private Nix regression passes
1568 individual representation-bit checks and complete packet fixtures;
targeted SPARK encoder flow/termination analysis passes. These are not
proofs of hardware semantics. These flags are not substitutes
for explicit null depth/stencil/HiZ buffer bindings, which remain pending,
along with unused shader-stage disables and whole drawing-batch assembly.
No drawing commands or new hardware image were submitted by this change.

### Actual ISL null-attachment oracle (2026-09-29)

`offscreen-layout-test.c` now invokes upstream
`isl_emit_depth_stencil_hiz` with ADL-N device info and no attachments, not
just the individual generated packers. It verifies all24 DWORDs, poisoned
output guards, and all63 even MOCS encodings2..126 used by our policy-field
admission. Hosted Nix run `offscreen-layout.2zc4an` passes.

For encoded MOCS6 the exact emitted sequence is:

```
78050006 E1000000 00000000 00000000 00000000 00000006 00000000 00000000
78060006 E0000000 00000000 00000000 00000000 00000006 00000000 00000000
78070003 0C000000 00000000 00000000 00000000
78040001 00000000 00000000
```

That is null depth (D32_FLOAT), null stencil, disabled HiZ, then invalidated
clear parameters. MOCS remains explicit even without attachments, with a
different bit position in HiZ. Addresses, extents and write/compression
enables remain zero. This oracle is Linux-hosted evidence of Mesa's chosen
sequence, not proof of hardware semantics or CuBit execution. The next step
is primary-PRM verification and typed native packet records for these fields;
none of these packets has yet been added to the submitted native batch.

### Native null depth/stencil/HiZ builder (2026-09-29)

Private `intel_gpu_adln_null_buffers.ads/adb` implements the24DWORD no-attachment
sequence above. Primary PRM review: TGL Vol2a24,42-50,56,125 and
Vol2d5,38-40,101-108. Every packed subfield is represented explicitly; the
common command header reuses the already-tested representation. Whole-field
addresses and IEEE clear value are zero. Both surface types are NULL; depth
format D32_FLOAT; writes/compression/HiZ off; clear-value valid off.

Private Nix raster regression passes1856 individual bit-layout checks plus
complete ISL-derived packet fixtures for every accepted MOCS. It exercises
0..1024 and U32'Last rejection boundaries. MOCS must be an even encoded value
2..126; this is not proof that the corresponding live cache policy is installed.
Invalid input returns an all-zero image. Targeted SPARK analysis passes,
including the validity/zero-on-rejection postcondition and conversion bounds;
this does not prove GPU semantics or stepping-workaround applicability.

Integration caveat: Vol2d101 documents an A-step post-sync PIPE_CONTROL
workaround after stencil surface-state changes. Resolve applicability or
include the required synchronization when assembling the complete batch;
this packet builder does not itself submit/synchronize. Native drawing is
still disabled, pending unused-stage state, L3 admission and whole-batch
assembly/verification. No new boot image was produced.

### Stream-output and tessellator pass-through (2026-09-29)

Private `intel_gpu_adln_passthrough.ads` uses explicit representation clauses
for STREAMOUT control/read/pitch fields and TE control. Primary source:
Intel TGL Vol2d109-116. Streamout function and statistics are off, rendering
disable is clear, force-rendering NORMAL, and all buffer pitches zero
(unbound/no writes). Packet: `781E0003 0 0 0 0`.

TE is off, making its other fields ignored, including float factor values.
Packet: `781C0003 0 0 0 0`, matching Mesa's simple-shader setup. This does
not independently disable tessellation: Vol2d116 requires HS/TE/DS enabled
or disabled together before any draw. HS/DS/GS packet work remains pending.
Neither fragment is submitted yet; the native batch is still the marker
probe. These are fixed probe settings, not general tessellation support.
Private Nix regression passes1984 bit-layout checks and complete fixtures;
targeted encoder flow/termination analysis passes. Mesa hosted oracle
`state-base.YRtJ0r` independently matches both packets. No GPU execution
or hardware semantics are established by those tests/proofs.

### Disabled hull-shader state (2026-09-29)

The private graphics workspace now has explicit Ada representation clauses for
the full TGL HS resource, dispatch, kernel-address, scratch-address and payload
fields. Primary references are Intel TGL Vol2a57 and Vol2d41-47; Mesa's
GFX12_3DSTATE_HS packer and genX_simple_shader.c provide the cross-check.
The fixed disabled packet is nine DWORDs: 0x781B0007 followed by eight zeros.
UAV access is explicitly clear, including while disabled. Payload read fields
are ignored in this mode; this is not an enabled-HS validation API.

The PRM reserves scratch bits63:32 whereas Mesa's generic packer accepts a
64-bit address. The native record follows the primary documentation; the fixed
disabled state uses no scratch address. Do not infer enabled scratch support
from this test. HS, TE and DS must all be disabled together before drawing.

The Nix-hosted native regression passes 2208 one-hot representation/encoder
checks plus fixed fixtures and URB/MOCS boundaries. The independently built
upstream Mesa packer emits the matching HS fixture (state-base.EzCcWA).
Targeted SPARK analysis proves the encoder termination and fixed packet
conversion range checks; it does not prove hardware behavior.
Neither this fragment nor the EU shaders have been submitted in a drawing
batch on physical hardware yet.

### Disabled domain-shader state (2026-09-29)

Private domain_shader.ads adds the four DS control records with complete Ada
bit representation clauses, using Intel TGL Vol2a54 and Vol2d20-27.
The 64-bit kernel and scratch records reuse the identical primary-PRM HS
layouts; the reserved upper scratch DWORD remains zero. UAV access is
explicitly off as required even for a disabled stage. Dispatch mode zero
is ignored for disabled DS and is not a valid enabled-DS configuration.

The eleven-DWORD fixed packet is 0x781D0009 followed by ten zeros, matching
Mesa's GFX12 packer and the simple-shader disable sequence. Nix-hosted
viewport regression passes 2336 bit-layout checks plus fixtures and
URB/MOCS boundaries. It also decodes the actual HS/TE/DS packets to check
all three enable bits are clear together. Independent Mesa oracle:
state-base.Ng1U9z. This is encoding evidence, not native drawing evidence.
Targeted SPARK analysis proves encoder termination and fixed-address
conversion range checks, not device semantics or enabled tessellation.
GS disabling, hardware L3/URB admission and full batch assembly remain.

### Disabled geometry-shader state and remaining integration (2026-09-29)

Private geometry_shader.ads represents every GS resource/payload/dispatch/
thread field and reserved region, using Intel TGL Vol2a55 and Vol2d28-37.
Kernel/scratch layouts reuse HS records; the output record is identical to
DS. The fixed ten-DWORD pass-through packet is 0x78110008 followed by nine
zeros, including UAV access clear. Mesa's simple shader emits this state,
and the actual GFX12 packer independently matches (state-base.ObLOj1).
The disabled packet is not an enabled-GS validator. In particular,
enabled SIMD8 with fewer than16 allocated handles has the documented
post-state-change CS-stall restriction (Vol2d33-34).

Nix-hosted viewport regression passes2464 one-hot layout checks plus full
fixtures, URB/MOCS boundaries, and disabled-stage enable/UAV checks.
This completes the fixed HS/TE/DS/GS packet set, not the submitted draw.
Targeted SPARK checks prove encoder termination and fixed conversion bounds,
not hardware behavior.

Rechecking genX_simple_shader.c identifies primitive replication disable as
still missing, alongside the previously recorded binding/sampler pointer
reissue, drawing rectangle, and applicable coarse-pixel defaults. Unlike
that Mesa helper (VS-disabled RECTLIST), our probe executes a vertex shader
and TRIANGLELIST; its state must retain the prepared VS/viewport transform.
Do not copy the helper's entire packet stream as if those paths were equal.
Hardware L3/URB capacity admission, complete ordered batch assembly, cache
flush/completion/readback, and stepping-workaround review remain before
physical drawing. The current live image still executes the marker probe.

### Primitive replication explicitly disabled (2026-09-29)

Private replication.ads represents the count/mask/reserved control fields
and each four-bit viewport/render-target-array offset. Primary references:
Intel TGL Vol2a65 and Vol2d52-55. The fixed packet is 0x786C0004 followed by
five zero DWORDs, matching Mesa's explicit primitive-replication disable.
The triangle shader exports a single position and targets one viewport/RT;
this packet removes dependence on inherited replication state.

Native Nix-hosted regression passes2528 one-hot layout checks plus all
existing fixtures and boundaries. Nonzero count/mask and ordered nibble
fixtures test layout independently of the all-zero disabled state; those
nonzero patterns are not admitted or submitted rendering configurations.
Actual Mesa GFX12 packer cross-check passes (state-base.VWYlbd), including
both groups of all16 viewport and all16 RTAI offsets. No hardware drawing
or new live image yet.

Targeted SPARK analysis proves both replication encoders terminate; device
semantics remain dependent on primary documentation and future hardware tests.

### Bounded drawing rectangle (2026-09-29)

Private drawing_rectangle.ads/adb encodes Intel TGL Vol2a51-53 with explicit
header (including Core Mode), unsigned coordinate and signed-origin records.
The builder accepts dimensions1..16384 independently, emits inclusive
zero-origin bounds, and returns an invalid all-zero image otherwise.
64x64 yields79000002/00000000/003F003F/00000000. Coordinate upper bits are
never used to silently wrap oversized dimensions. Legacy core mode updates
both cores; this is a non-pipelined command, not the usual pipelined header.

The regression covers each width/height from0..16385 and Natural'Last,
one-hot record layouts and signed-origin boundary encoding. The origin
storage is signed16; the PRM admits signed15 values with a sign extension.
The current builder uses only origin zero. These bounds must agree with
the actual render-target allocation when the full batch is assembled;
the packet builder alone does not authorize GPU memory or make arbitrary
application geometry safe. Native drawing remains unsubmitted.

The first negative-origin regression caught an overflow in direct signed
conversion. Explicit bounded two's-complement conversion fixes it.
Nix rerun passes2624 bit-layout checks, all dimension boundaries and fixtures.
Targeted SPARK proves initialization, range checks, termination, signed
encoding bounds, and validity/zero-on-rejection postconditions. Independent
Mesa oracle state-base.CXM98S matches the fixed64x64 and signed-origin
fixtures. Neither these proofs nor hosted tests establish GPU execution.

### Explicit binding and sampler pointer reissue (2026-09-29)

Private state_pointers.ads encodes all five binding-table and all five sampler
pointer packets (VS/HS/DS/GS/PS). Primary references: Intel TGL
Vol2a13-17/104-108 and Vol2d2/83. Representation clauses include the PS-only
POSH header bit15 omitted by Mesa's GFX12 packer; it is clear, and bit15 is
reserved/clear in other stages. Binding pointers represent the documented
32B software-table interpretation; sampler pointers are dynamic-base offsets.

All offsets are zero. PS selects the already prepared surface-base table at
offset0, entry0 referencing the offscreen surface at offset64. Zero is not a
null-resource guarantee: the binding pool MUST be explicitly disabled in the
full batch; otherwise it changes the table base even with zero offset.
Similarly zero sampler offsets are only harmless because all fixed shaders
have zero sampler counts and no sampler instructions. HS/DS/GS are disabled,
and VS has no resources. These packets must follow SBA/constant updates
before drawing. Pool disabling is still pending; nothing is submitted yet.

Nix-hosted native regression passes2720 bit-layout checks plus existing
boundary/fixture tests and all20 DWORDs of pointer reissue. Actual upstream
Mesa packers independently match all10 packets (state-base.BwCwbR).
Targeted SPARK proves encoder termination, not GPU behavior.

### Binding pool explicitly disabled (2026-09-29)

Private binding_pool.ads/adb implements the fixed disable operation from
Intel TGL Vol2a18-19. The full64-bit address/control and32-bit size use
representation clauses, including unprogrammable MOCS bit0. The builder
accepts encoded even policies2..126, rejects all others with an all-zero
invalid image, and emits79190002/MOCS/0/0. Enable, address and size are zero;
zero size is legal here because the pool is disabled. The caller must have
installed/retained ownership of the chosen MOCS policy separately.

This removes the alternate-pool prerequisite from the prepared packet set,
not from the running image: assemble the disable before binding-pointer
reissue and draw. Resource-streamer disable alone is not sufficient, as
the documented pool-enable behavior still redirects binding fetches.
No enabled pool allocation or arbitrary GPU-address admission is provided.
Physical drawing remains unsubmitted.

Nix-hosted regression passes2816 bit-layout checks plus boundary/fixture
tests. The actual Mesa encoder agrees for all63 even MOCS encodings
(state-base.rA0Gvz). Targeted SPARK proves initialization, range/division
checks, termination and validity/zero-on-rejection postconditions; it
does not establish hardware semantics.

### TGL L3 allocation fields (2026-09-29)

Private `intel_gpu_adln_l3.ads` describes L3ALLOCREG (0xB134) and
L3PARAMINFO (0xB164), from Intel TGL Vol2c printed pages1265-1266/1273.
All bits have representation clauses; parameter decoding is explicit.
Bits8-10 are reserved in this layout. Mesa's generic gfx12 packer names
bit9 FullWayAllocationEnable for other configurations; this is not used.
Mesa `intel_l3_config.c` selects the TGL list for ADL, with URB32/ALL88
and URB16/ALL104. Actual packer cross-checks yield B0000040/D0000020.
Intel Vol7 configuration7 agrees with 128KiB URB +352KiB combined clients
per bank (4KiB/way). Real bank inventory is still required; the 512KiB
device-info value annotated for intel_stub_gpu is not hardware evidence.

Mesa initializes this fixed allocation in `init_common_queue_state`.
`cmd_buffer_config_l3` skips reprogramming on gfx11+, so its older-generation
three-flush transition path must not be mistaken for the ADL startup path.
No L3 write or capacity admission has been integrated yet. Initial queue
programming, topology-derived capacity and first drawing batch remain pending.

Verification: private Nix viewport suite PASS (2880 representation bit checks,
parameter decode and existing fixtures); targeted SPARK proves encoder/decoder
termination and decoder range checks, not hardware semantics. Hosted Mesa
state-base.UliaHX PASS for both allocation constants. No new native image or
physical drawing result is claimed.

### ADL-N probe capacity admission (2026-09-29)

Private L3 package now types FUSE3 (TGL Vol2c part2 pp84-85) including
all eight bank-disable bits, WGBox selection and hash mode. Steering's
four-bit instance mask is not reused as a general capacity count.
Enabled_Banks counts all eight documented bits. The separate fixed-probe
admission accepts only F0 (four low banks), hash0/WGBox0, caller-established
ownership, 120 total ways with 16..32 untagged and 88..104 tagged, and
error-free observed URB32/ALL88/RO0/DC0 allocation. Reserved fields are
ignored in decisions; their encodings remain represented and tested.
The result is either rejection (0) or 512KiB, not a generic GPU sizing API.
Mesa iris_pci_ids maps 46d2 to adl_gt05 (four banks); its topology update
also uses four banks for up to two DSS on gfx12.0. Intel Vol7 documents
4KiB per way and 120 ways per bank. Identity, forcewake, valid coherent
sampling and actual programming remain live-caller obligations, not facts
established by this pure helper. Live queue wiring remains pending.

Private Nix job87102 passed2912 bit-layout checks, all256 bank masks,
all65536 tagged/untagged count pairs and allocation-field perturbations.
Targeted SPARK proved loop invariants, overflow/range safety, termination
and helper postconditions. These are software properties, not a proof of
hardware ownership, cache coherency, or execution. No new image produced.

### Whole offscreen batch candidate (2026-09-29)

Private `intel_gpu_adln_offscreen_batch.ads/adb` combines the validated
state packets into a251-DWORD second-level batch ending in one triangle draw
and MI_BATCH_BUFFER_END. MOCS/URB/thread invalid inputs return zero images.
It reissues pointers after SBA/constants, disables HS/TE/DS/GS and streamout,
and uses the existing private surface, vertex, viewport and shader pages.
The hosted `offscreen_batch` suite walks62 packet headers using command
length fields, checks exact draw arguments and zero padding, and exercises
all encoded MOCS policies and boundary ranges for URB/VS/PS parameters.

This candidate is NOT connected to native Submission_Image. Execution still
requires L3 setup/readback, resource ownership and final startup-state review.
Mesa `init_common_queue_state`/render queue initialization explicitly installs
disabled CPS_STATE via3DSTATE_CPS_POINTERS after SBA; that is not yet present
in the candidate. CC/sample defaults and stencil workaround remain tracked.
The parent ring's post-batch rendering flush and completion marker must be
retained: batch return alone is not completion or CPU-visible pixel evidence.
Mesa WA16014912113 is conditional on a previous nonzero URB configuration;
do not infer that reusing this initial-only batch for arbitrary contexts is safe.

Private Nix job98099 exited0: candidate assembly tests pass; targeted SPARK
proved initialization, range/index/length checks, termination and the
end-marker/rejection postconditions. These proofs do not establish that
the pending hardware-state requirements have been satisfied.

### Explicit disabled coarse-pixel state (2026-09-29)

Intel TGL Vol2a40 and Vol2d18,282-285 specify the CPS pointer and256-bit
state. Important correction to earlier "mandatory CPS" wording: when
per-coarse dispatch is off, the PRM says hardware does not fetch/depend on
the pointer. The fixed shader already uses per-pixel dispatch. Installing
disabled CPS is deterministic initialization following Mesa, not evidence
that its absence caused a hardware fault.

The private CPS package encodes the pointer and each subdivided state DWORD,
with raw S3.7 and signed-focal bit fields (not an enabled-radial validator).
Sixteen32-byte disabled states occupy dynamic-page bytes736..1247,
32-byte aligned after blend state. Submission_Image explicitly copies them;
the candidate batch reissues pointer78220000/000002E0 immediately after
state-base setup. Live submission remains marker-only. Mesa's actual
gfx12 packers independently agree on eight zero state words and the pointer.

Job41353 exited0:3072 representation checks,253-DWORD candidate packet walk
and full backing-image regression pass. Targeted SPARK proves CPS encoder
and array-builder termination; the prior whole-batch proof was251 DWORDs
and is not claimed as a rerun of this changed253-DWORD builder.
Hosted Mesa36312/state-base.NuTIEr independently passed. No hardware draw
or new native boot image is claimed. CPS header has its documented16-bit
length field, rather than borrowing an8-bit-length packet record.

### Color-calculation state integration (2026-09-29)

Private color_calc.ads encodes the subdivided control, UNORM8 alpha-reference
view and valid64-byte-aligned pointer, from Intel Vol2a21/Vol2d4,242-243.
Six state DWORDs at dynamic offset1280 are explicitly copied by
Submission_Image, with compile-time separation from CPS ending at1248.
The candidate emits780E0000/00000501 after SBA and CPS. Alpha testing and
blending remain disabled in their own state; this adds deterministic
zero alpha reference and blend constants, not those features.
Mesa's actual COLOR_CALC_STATE and CC pointer packers agree (host oracle
state-base.z7QYc2). Candidate length is now255 DWORDs. Sample-pattern
initialization and the stencil surface-change A-step post-sync workaround
remain pending, as do live L3 setup/readback and native drawing integration.
The PRM includes a1x sample offset in SAMPLE_PATTERN DWORD8 (bits23:16),
so do not assume the single-sample mode makes that command irrelevant.

### Offscreen sample positions and stencil synchronization

Live-integration boundary checked: Mesa `genX_init_state.c`'s
`init_common_queue_state` calls `emit_l3_config`; `anv_private.h`'s
`anv_batch_write_reg` implements that write as MI_LOAD_REGISTER_IMM.
This is queue command-stream initialization, not a CPU MMIO setter.
CuBit's existing Native_Engine_Settings adapter restricts access to its
separate settings plan and must not be assumed to authorize L3 programming.
The forthcoming L3 path must explicitly handle queue ordering, readback and
capacity admission before the drawing batch can execute.

The private ADL-N candidate now contains the nine-DWORD sample-pattern
packet with standard 1/2/4/8/16-sample locations. Coordinate subfields use
Ada representation clauses and explicit encoders; the packet agrees with
Mesa's GFX12 packer. The one-sample position is the pixel center. Reference:
Intel Tiger Lake PRM Vol. 2a, pages 94–103.

The initial null-stencil packet is followed by a six-DWORD PIPE_CONTROL
post-sync write, conservatively including the A-step workaround described
in Vol. 2d, page 101. Vol. 2a, pages 1124–1130, defines the actual write as
a QWORD despite the stencil note's "store dword" wording. Its destination
is private PPGTT address 0x201008, an aligned eight-byte scratch location
inside the completion page, distinct from the completion marker. The packet
also requests command-streamer stall, render-target flush and pixel-scoreboard
stall. This scratch write is not used as proof of drawing completion.

Private job62619 passed 3296 bit-layout checks and the 270-DWORD batch's
packet-order, bounds and rejection tests. The stencil encoder's termination
and range checks were proved. The Linux-hosted Mesa packing oracle passed
in state-base.OUEtCr. Full-image regression subsequently passed in job52833.
These are CPU-side validation results, not evidence of hardware rendering:
native submission still executes the marker-only batch. Live L3 admission
and programming, whole-path review, and GPU pixel readback remain required.

### Private L3 command-stream initialization candidate

`intel_gpu_adln_l3_commands.ads` adds representation-clause records for
MI_LOAD_REGISTER_IMM, MI_STORE_REGISTER_MEM, register offsets and the
DWORD-aligned memory destination. Primary reference is Intel TGL Vol. 2a,
pages 1002–1005 and 1056–1058. Its fixed seven-word sequence writes
L3ALLOCREG at 0xB134 with URB32/ALL88, then samples it into private PPGTT
address 0x201010. This does not overlap either the completion marker or
the stencil synchronization scratch QWORD. Absolute register addressing,
no remapping, no predication and no global-GTT destination are selected.

The caller must supply the preceding stalling flush, establish command
privilege or register allowlisting, and await completion before admitting
the sampled fields. In particular, an ignored/nonprivileged LRI must not
silently be treated as successful setup. This module is not yet executed
by the native ring. Actual Mesa GFX12 LRI/SRM packing matches all seven
words (state-base.wQjKV7); the private layout suite passes 3456 bit checks.
Job44219 also proved termination of all four encoding functions. Job52833
proved the complete 270-DWORD offscreen builder's initialization, bounds,
termination and postconditions; neither proof establishes hardware semantics.

### L3 dispatch and native sample reader

The private context initializer now has `Build_L3`, a separate 96-word
ring dispatch using the existing publisher. It keeps the stalled flush
before the LRI/SRM pair, trailing flush/invalidation barriers, and the fresh
sequence marker. It removes the private-batch branch and its arbitration
toggle, while retaining the final preemption point. It therefore cannot
accidentally submit the triangle while testing L3 setup.

`Native_Initial_Ring.Read_L3_Result` reads only completion-page offset 16,
with ownership checks before access and after cache maintenance/readback.
Readability alone is not admission; the future caller must check sample
freshness, ordered completion and allocation fields. `main.adb` does not
invoke this dispatch yet.

Job33064 passed context/L3 sequence fixtures and proved initialization,
termination and builder postconditions. Job98028 passed fixed-mapping
readback/address tests, owner rejection before unmapped loads, live-ring
publication (202 callback faults), native ring host fixtures, sequence
completion and initial publication regressions. These are hosted tests,
not GPU execution evidence. No boot image was changed.

### Native L3 phase wiring

Private `main.adb` now appends `Build_L3(3)` only after the repeated marker
at sequence 2 completes and the retained readback slot is observed zero.
It does not clear that slot after GPU publication. The existing bounded
completion wait, notification, scheduling-disable and quarantine paths
apply to this third dispatch. After successful disable, it reads the sample
and checks the decoded allocation fields, ignoring only reserved bits 8–10.
Zero, all-ones, error status and changed allocation fields are rejected.

New diagnostics are `L3 ring fresh=... published=... notify=... completion=...`
and `L3 allocation read=... raw=... fields-match=...`. A field match is
explicitly NOT topology/capacity admission. No drawing is submitted.
Job26420 passed the layout suite plus allocation-bit perturbations and
compiled, linked and staged the native driver in the private workspace.
This is native build evidence, not a hardware run; no new image was packaged.

### Native capacity admission extension

The L3 dispatch now contains eleven words: the allocation write plus
allocation and L3PARAMINFO reads to private completion offsets 16 and 20.
Both slots must be zero before publication. The reader returns both values
only through the existing owned retained-page path; they are consumed after
ordered completion and successful scheduling disable, not as an atomic
snapshot while the GPU is running.

Startup now calls the existing `Probe_URB_KiB` validator with decoded
allocation, parameter and stable fuse fields, requiring device 0x46D2,
valid topology and retained context ownership. Its admitted result is 512
KiB only for the explicitly supported four-bank configuration; otherwise
zero. A new diagnostic reports raw parameters and admitted URB KiB.
No drawing commands are enabled by this change.

Mesa oracle62530 passed all eleven words (state-base.4OuTjX). Private
job33586 passed the layout/decoder, context-sequence and native-reader
host tests. Earlier job2187 proved allocation decoder checks and the L3
capacity helper; the newly added fuse decoder still needs its own proof run.

### Immutable drawing-batch backing

The existing GuC lifecycle intentionally allows no re-enable after its
terminal scheduling disable. The drawing path therefore must not rely on
disabling, rewriting the marker batch, and re-enabling that context.
Instead the private submission image now prepares both immutable batches:
the marker at 0x200000 and the drawing batch at 0x200400 in the same mapped
page. The 270-DWORD draw fits without overlapping the marker or crossing
the page. It uses the admitted 512-KiB URB configuration and minimal VS/PS
thread limits of one for this correctness probe, not a performance target.

`Batch_Start` selects only these two fixed addresses. `Build_Draw` preserves
the context dispatch's barriers, arbitration balance and fresh completion
sequence while replacing its batch branch. Neither function grants live
capacity or permits arbitrary client addresses. The planned caller admits
L3 after sequence 3, appends drawing sequence 4 while still enabled, then
performs the final disable and CPU pixel readback. Startup does not yet
invoke `Build_Draw`.

Private job57978 passed the complete backing-image test, including both
batches and zeroed intervening/destination space. Job46490's context tests
passed the drawing branch against the marker dispatch for valid and invalid
inputs; its native build/proof were still running when this note was written.

### First native drawing submission wired

Private startup now consumes the completed L3 samples while the context
remains enabled and has no pending work. Only matching capacity, the prior
private marker, retained ownership/coherence and initially zero target
samples permit `Build_Draw` at sequence 4. Commands and shaders remain
immutable. It waits for the trailing ring completion, performs terminal
scheduling disable, then reads the private linear BGRA8 target.

The five samples are center (32,32) and the four corners of the 64x64
surface. Expected values are 0xFFFF0000 at center and zero at corners.
This is a bounded smoke test, not verification of every rendered pixel or
a general renderer. Read/marker/disable failure or a pixel mismatch leaves
the context faulted and backing retained. Logs separately report dispatch,
completion, center and corners; marker success alone cannot report a pixel
pass. Firmware scanout is never used as this draw's render target.

Job40007 passed the reader's fixed-mapping/ownership/address fixtures and
202 live-ring callback faults, then compiled, linked and staged the native
driver. Job46490's context/drawing builder proof finished successfully.
No hardware rendering result is available yet. A distinct private image
is being packaged for boot regression before NUC testing.

### Reproducible Linux build baseline

Native draw-test image packaging and regression (2026-09-29): private
`kernel/cubit_live_offscreen_draw.img`, SHA256
`56b8c56ab8c0c831b8b35b6688eb82952e2cbf070691e428edb194c722ea1524`.
The image plan records driver SHA256
`2091f750ae934aa0a52806c5cd5102036774e04e7d1c05e6f1eaa15cb7c24530`.
Job88562 passed image/firmware/Mesa audits. Job61028 passed four-CPU UEFI
USB-flash boot and native software-Mesa pixels/animation/close (194673
geometric pixels). This does not test Intel command execution; NUC pixel
feedback remains required. Existing shared boot images were not replaced.





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

2026-09-30: `Intel_GPU_VM_Image` now constructs bounded offline four-level
PPGTT images spanning raw 48-bit GPU VA, using the existing ADL-N PTE encoder.
The diagnostic `Initial_VM` remains unchanged: its single 2MiB window is not
sufficient for Mesa's separated code/state/buffer allocations. The new builder
preflights directory capacity before each page insertion, rejects duplicate VA
and aliases to any reserved table backing, and seals against subsequent edits.
It retains the existing below-4GiB DMA and read-only-erratum admission policies;
48-bit GPU VA support does not expand physical allocation authority.

`tests/intel-gpu/vm_image.gpr` passed under Nix with assertions/overflow checks:
independent exported-PTE walks; every index at each of four levels (2048
images); 2MiB/1GiB/512GiB boundaries and both halves of raw48 VA; dense leaf
sharing; unused-table aliases; unchanged images after rejected mappings;
capacity exhaustion with a later successful smaller insertion; duplicate
backing rejection; and sealing. This is regression-tested offline construction,
not a SPARK proof, live VM binding, GPU execution or Mesa device exposure.
The owner must authorize backing, materialize/cache-publish sealed tables and
retain them for the context lifetime. Live invalidation/unbind, public buffer
handles and native ANV wiring remain open.

The offline builder now also supports atomic whole-buffer insertion via
`Map_Pages`: consecutive GPU addresses backed by a possibly noncontiguous DMA
page list. A read-only preflight checks all leaves, collisions, reserved-table
aliases and total directory demand; ascending VA prefixes count shared new
directories only once. Commit requires neither allocation callbacks nor a
full page-table copy. The single-page entrypoint delegates to this path.
Nix hosted assertions pass for cross-level ranges, arbitrary array lower
bounds, every bad-page position in a 513-page buffer, last-page collisions,
raw48 overflow, exact-boundary success and insufficient directory capacity.
Failures preserve all exported table words and used capacity. These are
offline transaction tests, not hardware TLB/cache invalidation evidence.

`Intel_GPU_VM_Materialize` now writes a sealed image to trusted retained CPU/DMA
page mappings. It validates every destination before writes, rejects duplicate
CPU pages and mismatched DMA identities, writes children before the root,
flushes each used page and volatile-verifies every word including zero holes.
It returns a root only after all steps and a final ownership check succeed.
Each state permits one attempt; failure retains partial backing with root zero.
The owner must still establish actual CPU-to-DMA mapping authority and exclusive
access, and check the same context lifetime before hardware root publication.
This does not perform root publication, live invalidation or backing retirement.

Nix `vm_materialize.gpr` passed actual host-RAM write/readback and x86 CLFLUSH
tests, guard-page-content checks, all14 ownership-check failure points, every
flush failure, injected readback corruption and invalid final-page mappings.
An initial test caught eager ownership-callback evaluation on unsealed input;
the admission check now short-circuits before invoking that callback. These
tests do not prove device coherence, GPU execution or whole-driver isolation.

The main native `Submission_Buffer` now instantiates this VM builder/writer
for its diagnostic batch/completion mappings. The payload writer deliberately
skips the four VM pages; the sealed VM writer fills them, avoiding duplicate
table writes. Full-image volatile readback still compares against the existing
diagnostic image, and the GGTT preparation address is withheld until flushing
and ownership checks succeed. `Submission_Buffer` is now an Ada generic bound
to the service's real `Publication_Owner_Ready` predicate. An initial nested
function-pointer version was rejected by native `No_Implicit_Dynamic_Code`;
static generic binding removes the trampoline requirement.

Native `make -C kernel intel-gpu` passed, including link/staging. Hosted buffer
tests match both independent buffers byte-for-byte against the prior image and
reject ownership loss at all18 checkpoints. Payload tests verify VM bytes stay
untouched for the separate writer. This is native build integration, not a new
NUC execution result. The private extended offscreen-drawing workspace remains
unchanged: it needs all its extra mappings retained when adopting this path;
copying the main two-page diagnostic mapping there would be incorrect.

The private workspace has now adopted the same preparation path with all
eight offscreen leaf mappings preserved. Its hosted fixture matches the full
extended image byte-for-byte, and the native driver builds. The resulting
`cubit_live_vm_sealed.img` passed UEFI/4-CPU USB-flash QEMU boot and native
software-Mesa launch,194673 geometric pixels,animation and close. Firmware,
Mesa source/license and image-membership audits passed. SHA256:
`0d39c76f6afbc817610e7f8bdc8c8de0a9f14a1dbd226be54abaa21bdc7d469b`.
No physical Intel GPU execution result is available for this image yet.

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
