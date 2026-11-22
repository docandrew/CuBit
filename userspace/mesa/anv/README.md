# Native ANV transport boundary

## Static executable build identity

Native ANV must retain the real linker-generated GNU SHA-1 build ID. Upstream
Mesa uses that identity when constructing its driver/pipeline-cache UUIDs;
the static CuBit libc's `dladdr` does not implement dynamic-object lookup.
`native-build-id.patch` therefore routes only the CuBit utility lookup through
`native_build_id_link.c` and the bounded `native_build_id.c` parser. It rejects
addresses outside the executable's code, missing or malformed notes, duplicate
IDs and placeholder values. It does not manufacture a UUID or bypass Mesa's
initialization check.

`native_build_id.ld` augments the native link before the base layout's note
discard. The native link probe requires the adapted Mesa source and links the
adapter objects. Run `nix develop -c bash tests/mesa-anv/test-native-build-id.sh`
for hosted sanitizer tests and a CuBit-linker-layout check. The separate
`mesa-native-instance` headless fixture exercises the actual Mesa lookup inside
CuBit before testing rejection without a provider; link it with both
`--no-provider --retain-transport`. Linking transport code grants no authority.
Neither test establishes hardware rendering or Vulkan conformance.

Static dispatch tables need separate care: weak entrypoint references do not
extract implementations from static archives. The native link explicitly
retains Mesa's generated `vk_common_GetPhysicalDeviceProperties2`, which WSI
requires during initialization. Without it, the native synthetic-snapshot test
reproduced a NULL dispatch fault inside `wsi_device_init`; with it, enumeration
returned one physical-device description and destruction completed cleanly.
Use `--no-provider --retain-transport --snapshot-discovery` to build this
regression. Its identity/topology/budget replies are synthetic, while common
initialization, the compiler, properties and WSI are real Mesa running inside
CuBit. It has no GPU grants and rejects logical-device opening. This covers
initialization, not the completeness of all Vulkan dispatch tables or GPU use.

## Native physical-device composition

`anv_cubit_physical_device_create` now composes the native transport callbacks
with upstream ANV physical-device allocation, common initialization, measured
runtime discovery, memory types, synchronization types, one RCS queue, WSI,
and generation-specific initialization. Each physical device owns a separate
backend table. Budget accounting is provider-owned and shared across every
physical object/instance for the same process/GPU; construction must not reset
it. Its retained provider reference
is released on every post-retain failure or normal physical destruction.
Factory failure never changes the caller's output pointer.

The trusted provider must implement discovery-endpoint retention and fresh
logical-session admission. Logical/deferred session endpoints must outlive
physical-provider release independently. This is an explicit integration
interface, not an implemented startup broker or an authority lookup by PID.
The authorized-discovery fixture installs this provider for instance
enumeration and may consume its single manifest-bound session to create a
logical device. That is not a production broker for fresh sessions. The live
service's memory-policy query selects coherent owned WB memory only when its
hardware qualification and current ownership/health checks permit it;
explicit-maintenance policy is still rejected by the coherent Vulkan gate.
Physical NUC execution of the Mesa device/draw path and the remaining
advertised-feature/WSI audit are not established by native compilation.

Native link checks reject stale copied transport sources/headers before
compiling probe glue. Successful links write `inputs.json` beside the ELF with
the configured source/build paths and transport, archive, object and executable
SHA-256 hashes. This records exact link inputs, not hardware execution or a
full-system reproducibility claim. Use a freshly prepared build when upstream
adaptation patches change; refreshing copied adapters alone is insufficient.

Native KMD identity is `INTEL_KMD_TYPE_CUBIT`, not I915/Xe/STUB. The factory
does not expose fused engines beyond its one implemented RCS queue, permit
queue-count environment overrides, or advertise priority-selection support.
The CuBit WSI path does not install DRM syncobj-fd support or modifiers.

Run `nix develop -c python3 tests/mesa-anv/test-native-physical.py SOURCE BUILD`
with a newly prepared source tree and configured native build. The real
factory compiles against Mesa's native-configured types; the executable uses
hosted mocks for common initialization, IPC discovery and WSI. It checks
failure cleanup, output preservation, per-device callback lifetime and
admission dispatch, **not** native execution, GPU correctness, or full Vulkan
conformance. The retained native link probe now anchors the factory too.

2026-10-01 verification: fresh patch preparation succeeded with fuzz=0;
all 61 native static libraries built in `/tmp/cubit-physical-factory-build`.
The real CuBit link passed in
`tests/mesa-anv/target/native-instance-link.svpm5orq`, retaining the factory,
transport callbacks and real Ada query/budget/buffer/memory FFI. No unresolved
symbol substitutes were supplied. This executable has not been run or staged
into a live image. Hosted factory failure/two-device lifetime checks and the
nine existing discovery/memory/session fixtures passed separately.

### Read-only runtime budget snapshot

`cubit_mesa_memory_budget` uses immutable heap capacity, atomically sampled
process/GPU usage, and a per-call native allocator observation. It does not
modify `sys.available` or canonical discovery fields. Lost/invalid replies,
capacity changes and exhausted tickets add no availability. Arithmetic is
saturated to the heap size; a zero result is represented by one byte because
Vulkan requires a nonzero budget for each present heap. That is an estimate,
not a grant or assurance that even one allocation will succeed. Absent heaps
are zeroed; the caller's `sType` and `pNext` remain unchanged.
Specification: https://docs.vulkan.org/refpages/latest/refpages/source/VkPhysicalDeviceMemoryBudgetPropertiesEXT.html

Hosted `memory-info-test.c` covers 1,224 usage/availability/failure combinations
and 16,384 concurrent snapshots alongside atomic usage updates. It verifies
the entire physical-device object remains unchanged during runtime queries.
Factory tests verify two physical objects retain one shared usage record and
independent backend/provider lifetimes. `budget-snapshot.patch` routes actual
ANV budget entrypoints through an optional backend snapshot callback before
any mutable refresh. The native factory installs it and leaves runtime refresh
unset. Other backends retain their existing path. The actual ANV entrypoint
is exercised by `budget-dispatch-test.c`; it verifies callback routing,
unsupported-extension handling, and fallback without native IPC or GPU use.

Native memory-budget extension availability depends on the callback, not
whether any bytes happen to be free during construction. Native PCI bus-info
reporting remains disabled because our discovery query provides a device ID
and revision, not authenticated domain/bus/device/function coordinates.

Final snapshot integration validation (2026-10-01): all 61 native archives
built in `/tmp/cubit-budget-snapshot-build`; retained CuBit link passed at
`tests/mesa-anv/target/native-instance-link.oiitx_lt`, with the factory, snapshot
helper and real Ada budget entrypoint present. Final configured factory/ANV
dispatch tests and all nine memory/discovery/session fixtures passed. These
are compilation/link and hosted regression results, not native execution or
physical GPU validation. No live image was changed.

## Offline BO removal

`anv_cubit_unbind_bo` now routes full real-BO removal through the authenticated
offline binding operation before context preparation, and through the existing
generation-checked VM update after registration. Route selection and transport
share the lifetime mutex with preparation. An uncertain offline reply poisons
the session: it is not retried or converted into a live update. Offline removal
changes only the unpublished VM image; it does not free backing or invalidate
GPU translations. The Ada/C bridge rejects sealed images and malformed replies.
`binding-route-test.c` checks unbind/rebind at low and canonical high addresses,
the startup transition and sticky failures with actual ANV types and mocked IPC.
This does not enable the native physical-device factory or public admission.

## Session-queue submission (GPU-001 step 3)

`anv_cubit_queue_exec_locked` and `anv_cubit_queue_exec_async` submit through
the GPU session's queue (`native_gpu_queue.ads`, over `CuBit.GPU_Sessions`;
design in `docs/gpu-async-submission.md`). Each translates its waits, writes
one descriptor and returns: the signals get GPU timeline points
(`anv_cubit_sync.c`) that resolve when the GPU reaches them. No call waits for
the GPU and none holds `lifetime_mutex`; a per-queue lock orders descriptors
and their points. Waits on a point of the job's own context are dropped (ring
order), waits on another context become descriptor waits, and the pre-lock
hook (`anv_cubit_wait_dependencies`) waits only until every dependency is
pending. An empty submission's signals follow the queue's last job, or happen
at once when nothing is outstanding; with a cross-context wait it is a
`Signal` barrier. The first internal batch prepares and registers the context
and opens the queue; device teardown closes it before the session. Live VM
updates (`0x0A28`) stay synchronous under their own lock. Performance-query,
companion-engine and trace submissions remain unsupported.

Hosted coverage (actual ANV types, Mesa's vk_sync, mocked queue ABI and IPC):
`tests/mesa-anv/test-gpu-timeline.py <native Mesa build>`, which also runs the
no-wait source check `test-submit-no-wait.py`. The Ada logic meets the real
step 2 queue service in `tests/mesa-anv/gpu-timeline/` (and its mutation
script). None of this is native or hardware execution.

### Remaining factory requirements

The `cubit-device-*`, `cubit-topology.*`, and `cubit-memory-info.*` adapters
live alongside the runtime here, not under `tests/`. Source preparation copies
them into native ANV and its Meson target compiles them. Test fixtures remain
under `tests/mesa-anv`; there are no legacy copies of the adapters there.
The retained-transport link probe also compiles the real `native_gpu_query`
Ada FFI. A rebuilt archive/link is still required before using the new build
composition; existing archives are not updated by source preparation.

Fresh validation on 2026-10-01 rebuilt all 61 native static libraries in
`/tmp/cubit-native-discovery.1n1NWq/build` and passed the retained native link
in `tests/mesa-anv/target/native-instance-link.db7h8_lz`. The executable symbol
table contains the transport backend, device-default discovery, native query
adapter, coherent memory-type helper, and both real Ada query/budget FFI
entries. The link probe reads the actual source directory from Meson's
metadata and retains those paths explicitly. This was not executed on CuBit;
enumeration still rejects until the factory is connected to a real provider.

Run `nix develop -c python3 tests/mesa-anv/test-native-memory-policy.py BUILD`
against a configured native ANV build to compile the five discovery adapters
with its target command and run nine hosted, mocked-IPC fixtures. This checks
query decoding, canonical memory budget/type construction, session ownership,
native addressing selection, and retained cleanup. It does not execute on
CuBit or establish physical GPU coherence.

The instance platform adaptation separates CuBit enumeration from the excluded
Linux DRM physical-device factory. Until capability-backed discovery is wired,
CuBit enumeration explicitly returns `VK_ERROR_INITIALIZATION_FAILED`; it does
not advertise a fake device or silently report successful hardware discovery.
The Linux path retains its upstream DRM callback. This is a link-boundary fix,
not native provider integration.

`tests/mesa-anv/test-native-instance-link.py BUILD --retain-transport` forces
the backend table and its callbacks into the native static link and compiles
the real Ada buffer/memory FFI into an isolated output directory. Run under
the shared build lock in Nix, after rebuilding the prepared native Mesa tree.
It neither supplies mock symbols nor executes or stages the resulting app.

`anv_cubit_transport_backend` now groups the implemented memory, binding,
null-heap and queue callbacks with the pre-device-mutex dependency hook.
Its native addressing hook selects explicit GPU virtual-address bindings,
never Linux execbuf relocations, TR-TT, or fake sparse support. The actual-type
session fixture checks that every incoming sparse mode is cleared. This
mechanism selection is not an isolation or address-space admission check.
It is a template for factory composition, **not** a publishable full backend:
physical-device and device-open hooks remain unset.

Factory ordering matters: common ANV checks `info.has_context_isolation`
**before** calling the physical-parameter hook, and subsequently requires at
least 4 GiB of GPU virtual address space. Runtime PCI defaults establish
neither property. The native four-level VM builder accepts raw 48-bit VA, but
that alone is not an admission receipt for an active, private render session.
The factory must establish these properties from the native session contract
before entering common initialization; it must not substitute the GGTT
aperture size or physical backing-pool capacity for GPU virtual capacity.

`cubit_mesa_query_runtime_device` now combines measured defaults with device
query selector4: `[version,4,0,0]` returns `[OK,version,48,1]` only when the
native dispatcher resolves an active kernel-stamped render session and its
existing live health check succeeds. The fields describe raw48 private PPGTT,
not physical memory, a reservation, or a lasting ownership lease. The helper
sets `has_context_isolation` and `gtt_size` before common construction, leaves
KMD INVALID, and leaves caller output unchanged on failure. The factory itself
is not yet installed. The native GPU integration and Ada FFI compile; hosted
decoder mutation tests and actual Mesa device-info tests pass. Focused SPARK
proves responder termination and its response-shape postcondition only—not
end-to-end hardware isolation. The extended native IPC probe is updated but
has not yet been rerun for selector4.

Factory memory admission has a mandatory Vulkan constraint, not just an
optimization choice: at least one type must be HOST_VISIBLE|HOST_COHERENT,
and at least one must be DEVICE_LOCAL. See the upstream
[memory-type requirements](https://docs.vulkan.org/spec/latest/chapters/memory.html).
Our cached noncoherent transport alone therefore cannot form a publishable
Vulkan device. Do not infer HOST_COHERENT from UMA, a PAT value, or one readback
probe. The common constructor's `memory-contract.patch` now rejects backends
missing either mandatory type after memory-type construction. This does not
implement or prove coherence; flags must describe real allocation/cache behavior.
`test-memory-contract.py SOURCE` extracts that actual publication code and
checks 768 mock flag sets plus a deliberately bypassed-gate mutation. Fresh
preparation and Linux lifecycle-unit compilation passed on 2026-10-01 using
`/tmp/cubit-memory-contract.DfkmFi/source`; no new native device is exposed.

`anv_cubit_attach_session(device, slot)` supplies the device-open transaction
once trusted bootstrap has delivered a fresh authorized session: attach a
tracker, query live status, and detach into retained cleanup on failure.
Failure to attach never closes another device's session. Uncertain close
remains quarantined without replay. This helper does not acquire authority;
native broker binding and fresh-session delegation remain unwired. A physical
device must not cache a retired render capability for later logical devices.
The status callback uses authenticated active-session query0A2F, not cached
device identity or a memory-budget query. The dispatcher samples current
runtime ownership and rejects retired/quarantined context state; the adapter
turns any nonzero or malformed reply into sticky device loss. This observation
is not an ownership lease: each subsequent operation still validates its own
authority and state. Hosted protocol/status tests and native handler/FFI
semantic compilation pass; native IPC execution remains untested.
The scoped SPARK render-control proof also discharges the status response
postcondition: bounded status, canonical version/zero payload, unauthorized
denial, and success implying authenticated session, valid envelope and trusted
ready input. It does not prove the native hardware observation or IPC boundary.
Single-render-queue logical context setup validates the request without
starting the GPU; preparation remains deferred until buffers are bound.
Context teardown and device abort/close share idempotent retained cleanup.
Engine creation claims exactly one render queue against that context. Queue
release clears its pointer without deregistering the service context or
allowing replacement; both submission callbacks reject unclaimed/released
queues before waits, GPU work or completion signals. Cleanup clears retained
queue pointers before Vulkan wrapper destruction. Common Vulkan external
lifetime synchronization remains required.
Hosted `context-lifecycle-test.c` covers malformed/protected/multiple queue
requests, duplicate setup, pending retirement and creation failure before
setup. This is mock IPC, not native Vulkan device creation.
External-memory/userptr/placed-map operations remain absent. The
mandatory common BO-flags translator is installed: CuBit has no Linux
exec-object flags, so it returns zero, as Xe's corresponding callback does.
This is not allocation-policy admission; unsupported allocation semantics
are still rejected by `gem_create` before common ANV constructs the BO.
The
prepared-source path already copies this adapter into ANV; no backend
selection or public device admission has been enabled by adding the table.


The pinned Mesa source also requires a **null-initialized GPU VA heap**, not
just zero-filled BO backing. `anv_private.h` documents
`ANV_BO_ALLOC_NULL_INITIALIZED_HEAP` as null mappings for unused heap pages plus
a command-stream prefetch guard. `anv_device_init_vma_heaps` reserves that guard;
`anv_device_bind_null_va` submits a BO-less range through `vm_bind` and requests
bind-timeline signaling. Batch-pool flags select this heap. The current native
adapter admits that flag only after the exact null-heap lifecycle setup through
`anv_cubit_vm_bind`; it otherwise rejects it even for noncoherent cached
allocation. The callback accepts one BO-less operation for the physical VA
heap with no queue/waits/signals, before first preparation; matching teardown
closes new work admission and leaves scratch retained until session retirement.
Duplicate setup, replay, late setup, arbitrary ranges and sparse operations
reject. The native factory copies this transport callback. Do not treat the flag as a
physical zero-fill hint or silently accept the BO-less bind. The
`memory-lifecycle-test.c` regression checks rejection separately from the
HOST_COHERENT gate. Source audit: prepared Mesa26.2.3 `anv_device.c`,
`anv_allocator.c`, and `anv_private.h` (2026-10-01).

Backend distinction: Mesa's `i915/anv_kmd_backend.c:i915_vm_bind` returns
success without a bind, whereas `xe/anv_kmd_backend.c` sends
`DRM_XE_VM_BIND_FLAG_NULL` for a BO-less mapping. These are not evidence that
CuBit can ignore the request. Linux v6.16
[`gen8_ppgtt.c`](https://github.com/torvalds/linux/blob/v6.16/drivers/gpu/drm/i915/gt/gen8_ppgtt.c)
provides a scratch hierarchy: `gen8_init_scratch` encodes a backing page and
fills each higher scratch table with the preceding level's entry;
`gen8_alloc_top_pd` initializes roots from that hierarchy. Allocation fills
new tables from scratch entries, and clear restores those entries rather
than zero/invalid PTEs. Sharing scratch between VMs is conditional on
read-only support. CuBit now implements optional private per-VM scratch in
the offline image, initial materializer and retained-scratch update path.
The native application-context allocation reserves four private pages and
passes them through preparation; logical Lookup still distinguishes holes
from explicit BO mappings. Updates validate retained tables without zeroing
the potentially GPU-written data page. Scratch/table/data aliases reject.
Hosted RAM/fault-injection and composed-context tests pass. The native driver
builds and links; physical execution of this application-context path remains
unverified.
The null-heap callback has hosted actual-ANV-type/mock-IPC tests, not native
Mesa execution. A writable scratch page
is not equivalent to immutable zero reads/write discard and must not cross
security domains.

Do not import Xe's bind-timeline machinery as an unconditional ANV requirement:
the pinned `anv_device.bind_timeline` is explicitly Xe-only, and i915's bind
callback does not create/signal a point even when the common null-heap call
passes SIGNAL_BIND_TIMELINE. CuBit's synchronous bind/update path must establish
its own completion-before-submit ordering. A future null-heap callback must
validate the exact heap operation and session policy; accepting arbitrary
sparse binds or claiming an asynchronous Xe timeline would be incorrect.

Application backing lifetime now has explicit Empty/Offline/Preparing/
Published/Retired phases. This fixes an overloaded readiness flag which was
cleared on publication but still required by native submit/update gates.
Published backing remains eligible for those operations, while offline bind
and preparation cannot reopen. Session authority, GPU setup/disable, device
ownership and retirement checks remain independent mandatory gates.

`Intel_GPU_TGL_PTE_Registers` records every bit of the documented client
4KiB PTE (Tiger Lake Vol6, printed pp36–37, HAW39). Its bit9 null field is
documented for tiled resources, not by itself a guarantee about command
streamer prefetch or ADL-N admission. The isolated layout regression checks
all64 single-bit patterns and262144 existing leaf encodings. It establishes
representation only; no native code publishes null-bit entries. Do not
substitute that bit for the scratch hierarchy without engine/platform evidence.

The callback implementations do not yet constitute an exposed Vulkan device.
The current read-only service query supplies identity and measured topology,
not a complete runtime device description. Before selecting a native factory,
integrate measured memory budgets, timestamp support/frequency,
physical-device ownership/cleanup, authorized per-device render-session
acquisition and retirement, queue/context teardown, and feature admission that
matches the implemented transport. Session name retirement alone cannot be
treated as GPU completion or backing reclamation. Public render admission and
WSI/resource sharing remain separate gates. Optional DRM-specific features
must stay unadvertised rather than being satisfied by successful no-op stubs.

The identity adapter now assigns the observed PCI revision to both Mesa PCI
revision and runtime `revision`. This matches Linux v6.16
[`I915_PARAM_REVISION`](https://github.com/torvalds/linux/blob/v6.16/drivers/gpu/drm/i915/i915_getparam.c)
returning `pdev->revision` and the pinned Mesa i915 discovery consuming it.
Do not substitute Linux's internal `enum intel_step` value here. Final Mesa
workaround initialization still belongs after all runtime fields are complete.

`anv_cubit_gpu_timeline_type` is the device's one timeline sync type: a
reached value plus pending points, each a (context, value) on the session
queue, kept by the proved Ada unit `native_gpu_timeline.ads`. `get_value`
resolves points against the queue's status lines (no IPC); waits spin briefly,
then sleep in `OP_GPU_WAKE` with the caller's deadline, or poll the status
line when another thread holds the session's one wake. `WAIT_PENDING` treats
a submitted point as pending, so Mesa's threaded submit works. CPU-only waits
keep the condition variable with 10 ms slices for device-loss checks.
External sharing is rejected. `anv_cubit_init_sync_types` publishes it with
its binary wrapper during physical-device construction.
`gpu-timeline-sync-test.c` tests it with Mesa's dispatcher and host pthreads;
`build-native-sync.py` links the same test for native CuBit
(`tests/headless/run.sh --test mesa-sync` with `MESA_SYNC_IMAGE`), which
exercises CuBit libc threads and timed waits, not GPU submission.

`anv_cubit_binary_sync_type` reuses Mesa's binary-on-timeline wrapper, with a
payload move for threaded queue waits. The move requires the source to be
signalled or pending, transfers that event (with its point) to the
destination, and advances the source to an unsignaled point under the
timeline lock. It rejects an unready
source and overflow; reset also rejects counter exhaustion without wrapping
an unsignaled event to zero. External sync-file hooks are disabled. The
real-dispatch test covers repeated signal/move/reset cycles and exhaustion.
This is not yet installed as
the native device's supported sync types and does not establish WSI support.

### Queue-lock integration requirement

Upstream `anv_queue_submit` takes `device->mutex` before calling
`anv_queue_submit_cmd_buffers_locked`, which reaches the KMD
`queue_exec_locked` callback. The synchronous CPU dependency wait must NOT be
installed there: another queue satisfying the dependency may need the same
device mutex. Keeping only the CuBit tracker mutex unlocked is insufficient.
Resolve CPU dependencies in the runtime-worker path before that lock, then
submit/revalidate under the existing serialization and signal after completion.
`anv_cubit_wait_dependencies` now exposes that separate wait phase; it neither
submits nor signals nor consumes binary waits. It checks the session again
after waiting, but does not confer a lease: submission must revalidate too.
The common queue entry point now has an optional `wait_queue_dependencies`
backend hook before trace flushing and the device mutex. The native factory
is not installed yet, so it does not yet select the CuBit helper.
The Linux i915 path submits kernel-managed dependencies instead of performing
this CPU wait, so copying its callback placement would not preserve progress.
`test-queue-dependencies.py` extracts the actual patched common entry function
and runs two dependent queues with host threads and mocked backend/trace
operations. It checks producer progress, wait-error early return and the NULL
hook path; moving the hook under the mutex deliberately deadlocks and is caught
by the test timeout. This is queue-entry regression evidence, not a full Mesa
device, native threading or GPU execution test.

## Bootstrap hardware drawing evidence and pixel handoff

`Buffer_Views.Share_Completed` accepts a trusted completed-backing callback,
not a client CPU address or a private-context BO handle. It creates only a
read-only forwardable grant to a generation-checked recipient endpoint. It
rechecks the source after creation and retires the grant without exposing the
reference if ownership/backing changed. Existing grant retirement tracking is
reused; failures never free backing. Hosted `view_tests` covers these paths
with mocked kernel grants. Native export dispatch, a bound viewer endpoint,
and the Desktop consumer now compile. The production viewer passed a native
QEMU screenshot/retirement test with synthetic RAM pixels. On 2026-09-30 the
user confirmed a visible triangle on the physical NUC with
`graphics-resume-mogpp532` (image SHA256
`a97fcf4cc073bd07aea043e0b186604ca1c21da734d8e89a106dfad4c63814b6`).
This confirms the Intel-produced diagnostic pixel handoff into a Desktop
window, not Mesa-generated hardware rendering or accelerated composition.

NUC feedback on 2026-09-30 for `graphics-scatter-live-1v7ch8jf` reports completed
hardware drawing, center `FFFF0000`, 512 nonzero pixels and word hash `BEC5DDC5`.
An independent pixel-center calculation for the probe triangle
`(16,16),(48,16),(32,48)` in its 64x64 target reproduces the count/hash.
That earlier run established native Intel offscreen drawing; the later resume
image adds the user-confirmed visible presentation described above. Neither
establishes an accelerated Mesa application.

`Submission_Buffer.Completed_Pixel_View` now selects exactly the 16KiB pixel
slice from the retained bootstrap allocation, with completion/read-owner gates.
It excludes adjacent private tables, context, ring, completion and shader pages.
Native startup retains the view only after completed drawing, readback, barrier,
updated-VM probe and final scheduling disable. The view is not a grant and is
exported only through the read-only probe endpoint with retained backing and
revalidated ownership. Hosted tests cover uninitialized/application contexts, false owner
gates, exact bounds and all32 physical discontinuity positions. Native compile
passes; the new handoff itself still needs hardware testing.

## Generation-checked VM updates (public admission closed)

Service-side `Binding.Handle_Update` composes request preparation with the VM
coordinator and constructs a success reply only after completed publication,
invalidation and resume. Post-callback session revalidation suppresses success
and quarantines on revocation. The hosted `vm_update_pipeline.gpr` test covers
two generations, invalidation failure, and revocation during resume with
simulated GPU events/MMIO. Native 0A28 dispatch now uses event-loop allocation,
retained per-ticket candidates, publication/invalidation, current-image adoption,
and committed replies. Context setup and submissions are excluded while an
update is pending. Submit-on-demand contexts remain acknowledged disabled.
Native compilation passes, but this is not hardware VM-update execution evidence.

`anv_cubit_update_bo_binding` connects this transport to the retained ANV
session after context registration. It serializes updates with submissions,
tracks a VM generation independently of batch completion, rejects invalid BO
slices locally, and poisons the session on any uncertain update reply. It
does not install a KMD callback, allocate VA, release BOs, or enable public
admission. The submission-lifecycle fixture interleaves 20 updates/submissions
and checks sticky failure for malformed generations and service rejection;
these are real ANV types with mocked IPC, not GPU execution. The concurrent
fixture adds four threads, 512 updates and 512 submissions, checks transport
serialization and independent generations, then verifies one uncertain update
prevents all subsequent transport calls across the shared lifetime.

`cubit_intel_update_binding` supplies the C/Ada wire adapter for label
0A28: bind/unbind, BO handle and page offset, GPU range, and expected VM
generation. Success requires the exact next generation; malformed replies
clear the output and fail without replay. The native service dispatches this
operation behind closed application admission. Candidate preparation must never acknowledge
success: publication, invalidation, resumption and generation commitment must
complete first.

The buffer fixture checks C/Ada ABI, bind/unbind packing, generation exhaustion,
stale/skipped/overwide replies, malformed envelopes, clean service errors and
invalid local ranges without IPC. Hosted fixture and native adapter compilation
passed on 2026-09-30. Update replies in this fixture are synthetic, not hardware
VM transactions. Reproduce with:

```sh
nix develop -c gprbuild -p -P tests/mesa-anv/buffer-fixture/buffers.gpr
tests/mesa-anv/target/buffer-requests-host/buffer_bridge_tests
nix develop -c gprbuild -p -c -P tests/mesa-anv/buffer-fixture/native.gpr
```

## Completed-linear presentation lifecycle

The offscreen Vulkan triangle fixture uses `VK_FORMAT_B8G8R8A8_UNORM` and
copies the rendered image into a tightly packed 64x64 buffer (256-byte pitch).
Its full-image oracle checks BGRA storage bytes, not shader component order;
shader outputs and clear colors remain logical RGBA. This matches Desktop's
linear pixel format without a CPU swizzle. It still performs a GPU
image-to-buffer copy and does not export that buffer or implement a swapchain.
Before connecting it to the presenter, retain the Vulkan memory/BO/device,
retire its writable CPU mapping, and keep the backing alive through confirmed
Desktop-grant retirement. A successful fence or attach reply cannot replace
that lifetime protocol. Linux lavapipe validation is not native Intel evidence.

The triangle helper now exposes a synchronous completed-buffer consumer hook
after successful pixel validation and CPU unmap, but before Vulkan buffer,
memory or device teardown. A consumer must return only after all its exported
loans have retired, including on error; uncertain retirement requires retaining
the call/resources. The native probe currently passes no consumer. The Linux
test consumer remaps the same allocation and validates all pixels without a
copy, with Vulkan API/synchronization validation enabled. This exercises the
handoff ordering, not CuBit grants or a Desktop window.

`test-native-instance-link.py --triangle-smoke --present-triangle` (with the
existing authorized-discovery/logical-device/transport/shader options) builds
a separate `mesa-triangle-window.app`. Its manifest explicitly requests Desktop
in addition to render and logging authority. The test-only ANV consumer checks
device health, completed unmap, standalone owned backing and bounds, then uses
the existing presenter for a 64x64 BGRA window. It displays for five seconds,
sends destroy once, and waits for child/root grant retirement before allowing
Vulkan cleanup. Ambiguous retirement retains the callback and all Vulkan
resources indefinitely; it does not retry attachment or claim recovery. This
is a synchronous diagnostic, not a swapchain, WSI implementation or production
presentation API. The native link passes; physical execution remains unverified.
`test-triangle-present.py BUILD` uses actual Mesa types with mocked Desktop
transport to check order, pending/uncertain retention and invalid backing.
The real patched device-info regression also checks that native discovery does
not advertise Linux mmap-offset/partial-mmap-offset capabilities: upstream
ANV uses these flags to enable its slab allocator. Current native defaults
therefore leave slab allocation disabled. This is a tested discovery property,
not permission to remove the consumer's standalone-BO checks. Consumer export
rejections log their reason before creating a Desktop surface where possible.

The ANV synchronous unmap callback allows up to 100 additional retirement
observations, yielding for 1 ms between pending replies. This is an observation
budget, not a guaranteed wall-clock deadline. The transport lifetime mutex
remains held; the callback does not submit GPU work or replay the CPU borrow
return. Only confirmed retirement succeeds. Exhaustion or transport failure
keeps the device lost and the cleanup records retained. The immediate tracker
unmap API retains its original fail-closed behavior on a pending reply.

`native_gpu_presenter.c/.h` composes the native mapping and presentation bridges
for a completed, CPU-visible linear BGRA8888 BO slice. It requests an explicit
read-only forwardable root, acquires it, derives a terminal Desktop child,
returns the temporary parent acquisition, then attaches the child. It keeps
both grant identities for cleanup. Release waits for child retirement before
retiring the driver mapping; attach/present replies are not release fences.

Records are single-use, externally serialized and must outlive surface/BO
wrappers while cleanup is pending. Uncertain map/return/retirement outcomes
retain a failed record for session teardown rather than replaying operations.
Rejected/uncertain attachment initiates tracked retirement; the adapter neither
destroys surfaces nor closes BO handles. GPU completion, cache visibility and
linear layout are caller preconditions, not inferred from a successful map.

The native Intel dispatcher additionally requires completed, disabled GPU work
before granting a presentation mapping. Its mapping table excludes writable
CPU grants to the same BO while a presentation grant exists, and vice versa;
retiring or uncertain grants continue to exclude the conflicting access until
retirement is confirmed. Ordinary read-only mappings can coexist. All new
application GPU submissions for that session are denied while any presentation
grant remains, with an additional check at submission ownership validation.
This is a conservative session-wide interlock, not per-BO GPU write protection:
concurrent rendering of a different back buffer still needs a finer-grained
ownership/synchronization design. It does not establish format or cache
visibility correctness, nor make the presenter an implemented Mesa WSI path.

The sharing fixture tests writer exclusion, pending and failed retirement,
session isolation and owner loss. These are hosted regression tests; a native
driver compile/link is not proof or physical-hardware execution of the policy.

Hosted sanitizer tests: `tests/mesa-anv/presenter-lifetime-test.c` linked with
`native_gpu_presenter.c`; run `nix develop -c bash tests/mesa-anv/test-presenter.sh`.
The `grant-forward-desktop` native fixture exercises
this C implementation and the Ada bridges against a synthetic RAM owner and
real Desktop; it is not an ANV device, GPU test or Mesa WSI implementation.

## Memory and context transport

`Intel_GPU_Application_Submit` now provides a serialized driver-side execution
coordinator, not a public endpoint. After trusted setup completion/disable, it
validates batch extent and session mapping, arms a fresh protected completion
attempt, enables, publishes, notifies, waits, and confirms disable. Completion
sequence advances only after all stages succeed. Any ambiguous execution or
ownership failure quarantines permanently; bad preflight input does not run.
Callbacks must select the same authenticated context, enforce nonprivileged
PPGTT execution and retain resources through bounded recovery. Hosted tests
cover repeated sequencing, every stage failure/ownership loss, raw-field bounds,
and no revival after failure. They do not prove callbacks, GPU security or
execution. Native callbacks and label `0A27` are now connected, and the C/Ada
bridge checks exact completion successors; admission remains closed.

`anv_cubit_prepare_submission` connects that bridge to actual ANV device/BO
types: process-owned lifetime records retain one-shot preparation state;
preparation seals the already-bound VM, executes setup and opens the session
queue. Batches then go through the queue (see "Session-queue submission").
Hosted actual-type coverage of preparation, live VM generations and recycled
lifetimes: `submission-lifecycle-test.c` in `test-gpu-timeline.py`.

`anv_cubit_bind_bo_offline` now connects actual ANV real BOs to the existing
native binding bridge before preparation. It binds the full page-aligned
allocation at a caller-allocated raw48 GPU address; it does not modify the
allocator's `bo->offset`, infer slab offsets or accept a CPU pointer. A failed
bind poisons the retained session rather than retrying an uncertain operation.
Once preparation has begun, it explicitly rejects further offline binds. This
is not a substitute for the live bind/unbind and synchronization contracts
required by the normal ANV allocator; installing it as that callback would be
incorrect. The actual-type fixture now exercises bind -> prepare -> submit,
as well as bad addresses, slab rejection, post-seal rejection and failed binds.

Run the focused coordinator test in Nix from `kernel` using
`alr exec -- gprbuild -p -P ../tests/intel-gpu/application_submit.gpr`, then
`../tests/intel-gpu/build-application-submit/application_submit_tests`.

`native_gpu_memory.ads/.adb/.h` exports `cubit_intel_acquire_view` and
`cubit_intel_return_view`. Acquisition decodes the canonical generation-checked
grant reference and calls `Acquire_Via_Capability`: the kernel derives the
owner from the supplied endpoint capability rather than an application PID.
Invalid slot/reference/access, empty or wrapping ranges, and null output are
rejected before transport. Failed acquisitions clear the caller's output.
Each successful acquisition must be matched by exactly one return, including
when later Mesa initialization fails. The caller must serialize its mapping
records and keep the capability slot stable during acquisition.

Returning a borrow does not revoke the grant or imply that the CPU mapping is
gone; nor does it wait for GPU completion or release GPU backing. These are
low-level transport functions, not a complete Mesa `gem_mmap`/`unmap_bo` pair.
`native_gpu_mapping.c/.h` now supplies a small Mesa-side FFI mapping record,
tracking one grant and borrow, pending retirement, and uncertain failures. It
never returns a borrow twice, treats placed replacement as unsupported without
altering the mapping, and retains failed records for session teardown. ANV still
needs to allocate the device-owned tracker and drive retirement completion
before reporting successful unmap; the shim is not yet selected by its backend.
The record retains its original address as a lookup key separately from its
usable CPU address, plus BO handle/offset/rounded length. Device-owned tracking
must outlive ANV BO records: upstream `anv_bo_unmap_close` and `anv_slab_bo_free`
discard the unmap result, unlike `anv_UnmapMemory2`. A backend must not rely on
those callers to retry a pending retirement, nor let a slab be reused while an
old CPU view is still live. Synchronous retirement or an explicit allocator
quarantine path remains necessary before enabling that backend.
Service-side grant
creation/retirement and dispatch now exist, but admission is deliberately disabled
until trusted startup integration and backend readiness are established. Neither
public rendering nor new device permissions are enabled by this bridge.

The native dispatcher now connects application image preparation/publication
to the same retained GGTT reservation ledger used by bootstrap allocations.
Context preparation uses label `0x0A25`, exactly four words `[1, 0, 0, 0]`,
zero flags/reserved fields and the kernel-authenticated active session. Reply
is `[status, 1, 0, 0]`, with the buffer protocol's status0..3 meanings. Success
means only that the sealed private VM and context/ring have been materialized
and published. It does **not** register a GuC context, schedule it, or grant
submission authority. No client-supplied DMA address or context index is used.
Offline bindings stop after this one-shot transition. Failed preparation or a
lost success reply retires admission; all backing/claims remain retained.
Do not retry uncertain preparation or infer execution readiness from success.
`native_gpu_buffers` exports `cubit_intel_prepare_context(slot)` for this
transition. It rejects invalid slots before transport and validates both reply
tags, version, status range and zero reserved words. Status4 is uncertain/local
failure, never permission to retry. C/Ada ABI and malformed-reply tests exercise
the wrapper with a reply fixture (not native publication).
The native admission controller remains unbound/Ready=False, and there is no
Mesa factory call to this wrapper yet. Native compilation plus composed hosted
publication tests are not an end-to-end native application execution result.

The dispatcher also accepts registration/initialization label `0x0A26` with the same
four-word request/reply shape. It requires successful context preparation and
allows one attempt per authenticated session. The shared context table assigns
the GuC ID and fence interval internally; neither comes from the app. Success
means the setup marker was observed and scheduling disable acknowledged, not
merely that registration and policy commands were queued. Before registration,
the handler arms the completion observer against zero, then publishes a
driver-generated setup-only initial ring into the private context backing.
Unlike the bootstrap initializer, it never branches to the unmapped bootstrap
batch VA. It performs context settings/barriers and an ordered HWSP marker
after the handler enables scheduling and checks the GuC acknowledgement.
It waits for that marker, then disables scheduling and checks that acknowledgement
before replying successfully. These waits are bounded bring-up operations in the
serialized service, not an asynchronous application submission implementation.
Publication requires live session identity, retained backing and no existing
session context. Any failed attempt or undelivered successful reply retires the
session; the drain path does not enable a never-runnable context to retire it.
`cubit_intel_register_context(slot)` exposes this transition with the same
strict reply validation and no-retry rule as preparation. Hosted wire/ABI
tests mock the registration response: they do not exercise GuC. There is not
yet a Mesa factory call for registration, and global admission remains closed.
The composed native handler is compile-checked; component hosted regressions
exercise completion, context routing and setup-ring publication, not the entire
native IPC-to-GPU path. This initialization path still needs hardware validation.

The hosted `native_initial_ring_tests` composes the actual setup publisher,
marker reader and live-ring publisher in one retained RAM mapping. It checks
that marker zero rejects a premature dispatch permanently, marker one permits
the first dispatch, a subsequent completion permits the next, and each append
preserves the setup segment and all backing outside its new words/saved tail.
Batch addresses and the nonprivileged PPGTT selector are checked in the emitted
ring. Marker writes are simulated; no GPU fetch, GuC scheduling, or application
IPC is exercised by this fixture.

The native live-ring writer now takes an explicit retained `Channel` object
instead of keeping one implicit channel per package. Each channel binds its
CPU mapping/extent on first append; switching to another mapping quarantines
that channel before writes. The selected mapping is rechecked during publication.
Tail, sequence and failure state are independent, allowing one writer instance
to serve multiple retained contexts under serialized selection. This is not
authority acquisition: the caller must select the authenticated context and
retain its backing without address reuse. The bootstrap driver uses the same
object interface. Two-context hosted RAM tests cover independent progress and
wrong-backing rejection; application dispatch/scheduling is not wired yet.

Permission-slot address audit (2026-09-30): Intel TGL Vol2c-12.21 printed
pp961-962 (PDF987-988) lists RCS slots0..11 at 0x24D0..0x24FC,
slots12..15 at 0x2010..0x201C, and slots16..19 at 0x21E0..0x21EC.
`Nonpriv_Registers.Documented_RCS_Offset` records this noncontiguous layout;
it is not a hardware presence assertion or MMIO authorization. The render
plan uses it only for the existing twelve i915-managed slots. Linux's
`intel_engine_regs.h` still defines `RING_MAX_NONPRIV_SLOTS` as12.
Do not extend the linear slot formula beyond11 or infer that the extra PRM
entries are implemented/reset-safe on ADL-N. Model applicability, reset state
and the complete hardware register-access boundary remain to be established
before admitting untrusted application commands. No additional register reads
or writes were enabled by this audit.

`Buffer_Requests.Binding.Batch_Mapped` supplies authenticated submission range
preflight. It resolves a live BO handle in the session, requires a sealed VM,
and verifies every page in the requested byte slice against the retained BO
backing, including subpage offsets and page crossings. It returns no physical
address. The caller must use the published VM generation and serialize with
retirement/close. This is not command validation or protection against later
mutation of command bytes, and is not yet a public submission endpoint.

`native_gpu_buffers.ads/.adb/.h` supplies create/close plus map/retire IPC calls.
Map returns an opaque mapping ID and grant reference; acquire that reference
through `native_gpu_memory` at offset zero for the exported subrange. Return
each successful borrow before retiring the grant. Retirement status 4 is pending
and may be polled; it is not permission to reuse backing. Mapping-call local
errors are status 5; existing create/close local errors remain status 4.
Do not blindly retry uncertain create/map calls.

The composed hosted fixture exercises the actual client wrappers and server
request/handle/view implementations together, mocking transport and kernel
grants only:

```sh
nix develop -c gprbuild -p -P tests/mesa-anv/memory-fixture/composed.gpr
nix develop -c tests/mesa-anv/target/buffer-composed-host/composed_tests
```

Mesa's `anv_device_map_bo` can pass a non-page-sized length to `gem_mmap` even
after adjusting slab offsets. The backend must round that length to grant pages,
bound it by the real BO allocation, and retain the rounded range in its mapping
record. `tests/mesa-anv/cubit-binding.c` contains the tested preparation helper;
it is not yet installed as an ANV backend callback. Do not change the service
protocol to accept arbitrary byte ranges merely to emulate Linux `mmap` rounding.

`anv_cubit_memory.c` supplies callbacks using actual ANV types: slab-parent
handles, rounded allocation-bounded ranges, no placed mappings, and Vulkan
device loss on uncertain retirement. Source preparation copies this adapter
and the C mapping tracker into Mesa and adds them to CuBit-only `libanv_common`
sources. The CuBit-only device field is a pointer, not a tracker allocation:
device construction, serialized access, teardown and backend selection still
need integration. Compiling these objects does not enable a Vulkan device.

`anv_cubit_memory_init` attaches a zeroed process-owned tracker, rejects a
second attachment, and records the trusted stable endpoint slot. Finish
transfers CPU cleanup to process-owned storage before clearing the device
pointer. Pending/uncertain cleanup marks device loss but no longer depends on
the Vulkan device or its allocator. `anv_cubit_memory_poll` services detached
lifetimes. A process mutex serializes memory callbacks and pool transitions/
polling, including control-plane IPC. This is a coarse initial lock, not a
graphics performance claim; synchronous submission now holds it through the
completion wait. Callers must still
obey Vulkan object lifetime rules during destruction. The initial pool has
16 lifetime records; only confirmed-complete detached records are reused,
while exhaustion rejects initialization. These
records also reject attachment to a slot still held by active or detached
cleanup. `anv_cubit_memory_slot_retained` reports that CPU-lifetime constraint;
it is not an atomic capability lease or evidence of GPU/session retirement.
The trusted endpoint owner must serialize capability changes with attachment.
The factory's provider must attach logical sessions and drive the polling/endpoint
lifetime contract before enumeration is enabled. CPU-mapping cleanup alone
does not retire GPU work or replace session shutdown.

Within an attached tracker, the 64 CPU-mapping records limit outstanding
bookkeeping, not the total number of successful map/unmap cycles. At capacity,
the tracker discards only confirmed-retired tombstones, preserving outstanding
record order for newest-borrow lookup. Pending/failed records are retained and
device loss remains sticky. Callers must not retain pointers into this array
or unmap stale pointers after a subsequent map. This does not reclaim GPU
backing, service handles, endpoint slots, or the process-owned lifetime pool.
Hosted ASan/UBSan coverage exercises 4,096 cycles, mixed live/retired records,
full live capacity and failure to revive after draining a lost tracker.

The driver's CPU-view table likewise recycles only kernel-confirmed retired
grants. Its 64 storage slots are separate from monotonic 32-bit mapping IDs:
old IDs never identify replacement views, and ID exhaustion rejects requests
rather than wrapping. Pending or uncertain grant retirement cannot recycle a
slot. Hosted sharing tests cover 4,096 cycles, stale retirement and reply-loss
cleanup tickets, foreign sessions, and a full table with pending retirement.
This supports repeated views of retained BOs; BO allocation/backing reclamation
and live GPU binding updates remain separate unfinished requirements.

The lifecycle itself also executes in the hosted
`tests/mesa-anv/test-memory-lifecycle.sh <current-prepared-build>` fixture:
actual ANV types and adapter code, mocked drain completion/device-loss reporting.
It checks invalid slots, pool exhaustion, duplicate attachment, zero initialization,
and deferred cleanup after overwriting the entire Vulkan device wrapper. No
Vulkan allocator callback runs; completed records stay retained without reuse.
Four concurrent allocation callers also exercise 400 callbacks with an atomic
overlap detector in the mock transport; their IPC calls remain serialized.
Run under Nix and the shared build lock. This is not native kernel execution.

### Logical-device teardown integration gate

Inspection of the prepared Mesa 26.2.3 `anv_device.c` shows that both
`fail_fd -> abort_device -> fail_device` and normal
`close_device -> vk_device_finish -> vk_free(device)` unconditionally destroy
the device. The backend close/abort callbacks return `void`; the result of
`anv_device_destroy_context_or_vm` is also ignored by these callers. Therefore
the current memory finish helper MUST NOT simply be installed in those hooks:
its retained pointer would lose its owner on incomplete grant retirement.

Retaining the whole device without an explicit cleanup owner is not a solution.
In particular, deferred use of application-supplied Vulkan allocator callbacks
or their user data after device destruction is not a lifetime contract we can
assume. Before wiring the factory, introduce a native transport lifetime that
can outlive ANV BOs/device state: independently owned cleanup records, a stable
service endpoint, and an explicit drain/quarantine owner. Transfer records
before Vulkan destruction; close admission before transfer, and never resume
allocation/submission through the detached lifetime. GPU context/VM retirement
and CPU grant retirement remain separate conditions. Free neither GPU backing
nor an uncertain borrow merely because the Vulkan wrapper was destroyed.

The process-owned `anv_cubit_memory_init/finish/poll` implementation now supplies
that ownership separation through session retirement: detached records drain
CPU views while ordinary mapping retirement still authenticates, close the
session once, then poll its read-only retirement query. Neither a successful
close nor CPU drain alone releases endpoint retention. Pending queries retain
the record; uncertain close/query results quarantine it without replay. Driver
backing and context IDs remain retained even after confirmed quiescence.
Factory hooks, a polling driver, and stable endpoint retention are still
required; this is not completed device destruction integration. The earlier
allocator-based helper was replaced. Hosted lifecycle tests overwrite the
Vulkan wrapper before deferred cleanup and cover pending, complete and uncertain
retirement; this is not native GPU teardown evidence.

The same adapter supplies create/close callbacks for the service's initial
system-memory allocations (page-rounded, at most 16 MiB). Creation currently
requires HOST_CACHED without HOST_COHERENT and physical `memory.need_flush`
for an explicit-maintenance session. An attached policy2 session also accepts
coherent-only/default flags, using its admitted WB mapping contract.
MAPPED, NO_LOCAL_MEM, INTERNAL and SLAB_PARENT metadata may accompany it.
Flags0 is rejected on explicit-only sessions: common ANV may select WC/PAT1
and skip CPU flushes, whereas this pool maps CPU WB and GPU PAT0. GFX12's explicit
cached/noncoherent selection uses CPU WB/GPU PAT0 and explicit maintenance.
Protected, external, scanout and other unsupported semantic guarantees are not
silently accepted. This cannot yet satisfy all internal Mesa allocations:
the workaround BO, for example, requires HOST_COHERENT. Uncertain
creation marks the tracker/device lost. Close drains known CPU views and retires
the BO name, retaining device-scoped records on incomplete cleanup. It does not
free backing, unbind GPU addresses, or wait for GPU work.

Cache-contract audit (2026-09-30): Intel TGL Memory Views, Vol6-5.23,
printed p17 defines MOCS SCF=0 as coherent and SCF=1 as non-coherent,
and requires skip-caching disabled for coherent surfaces. Our ADLN entries
0/18 have SCF=0 and no skip-caching; entries16/17 explicitly have SCF=1.
The field names/tests now preserve that polarity. Vol7-12.21 pp6-7 separately
describes GPU/IA cache visibility and the non-coherent flush/invalidate flow.
These documents do not prove the complete ADLN platform contract.
Mesa26.2.3 `GFX12_PAT_ENTRIES` uses GPU PAT0 plus CPU WB for its cached-coherent
selection. Our PAT0 is WB and PPGTT Write_Back selects index0, but that alone
does not validate CPU grant mapping attributes, all command-selected MOCS
indices, or completion visibility. The allocation-time CLFLUSH in
`Intel_GPU_Buffer_Memory` covers initial zeroing only, not subsequent app writes.
Before accepting HOST_COHERENT: trace kernel grant PTE attributes, preserve
one cache policy across aliases, check Mesa batch cache/barrier selections,
and test bidirectional CPU/GPU data visibility with real submissions. Keep
Vulkan synchronization obligations separate from memory-type coherence.

Native compile check (2026-10-01): the current memory/null-heap, sync,
sync-type and CPU-mapping adapters compile through the CuBit cross wrapper
and native libc headers against prepared Mesa26.2.3. Evidence is in
`tests/mesa-anv/target/native-adapter.epdz1aas/`; this is neither a link test
nor GPU execution. The factory must remain gated: upstream
`anv_device_alloc_bo` explicitly requires HOST_COHERENT for internal MAPPED
BOs because those internal users do not perform CPU cache flushes. Merely
flushing submitted command buffers cannot satisfy that contract.

Command-selection audit: Mesa26.2.3 `isl.c` selects MOCS2 for internal
Gen12 integrated surfaces, MOCS3 for uncached/blitter accesses, MOCS48 for
HDC L1+L3+LLC, and MOCS61 for external surfaces. The ADLN MOCS regression
now checks coherent-access/no-skip fields for these entries in addition to
diagnostic entries0/18. This validates the programmed table policy, not
physical visibility or that every emitted command chooses the right index.
`intel_device_info.c` uses PAT0 for both cached-coherent and cached-incoherent
Gen12 selections; PAT equality is therefore not a coherence admission test.

CPU mapping trace: `kernel/src/syscall-ipc.adb`'s DMA allocation maps driver
pages with `Virtmem.PG_USERDATA`; `kernel/src/process-ipc.adb:createGrant`
maps recipients using PG_USERDATA/PG_USERDATARO rather than copying source
cache attributes. Both select CPU PAT0, initialized WB by `x86.PATRegister`
and `PerCPUData`. Thus this owned DMA arena and its grants have matching
requested CPU WB attributes; effective hardware memory type still depends on
platform state (including MTRRs). `Intel_GPU_VM_Buffer` now fixes this pool's
GPU policy to Write_Back and rejects other policies even in fresh VM images.
The general offline VM builder continues supporting other policies for other
backing contracts. This does not certify arbitrary grant sources as WB-safe
or establish GPU coherent visibility.

Linux v6.16 coherence cross-check (2026-10-01): the PCI table routes
`INTEL_ADLN_IDS` to `adl_p_info`. Its GEN12 -> GEN11 -> GEN9 -> GEN8 ->
G75 -> GEN7 inheritance retains `has_llc=1`, while GEN11 explicitly sets
`has_coherent_ggtt=false`. Do not confuse coherent CPU mappings of owned RAM
with CPU accesses through the GGTT aperture. The proposed Mesa CPU views are
the former, through the existing WB grants, not aperture mappings.

`TGL_CACHELEVEL` maps both I915_CACHE_LLC and I915_CACHE_L3_LLC to PAT0;
`i915_gem_object_set_cache_coherency` treats non-NONE cache levels as coherent
for both reads and writes. `tgl_setup_private_ppat` programs the same eight
entries as our ADLN PAT implementation. `gen12_pte_encode` selects the PAT
index in the PTE; it does not add a separate coherent bit for this path.
This supports pursuing the existing CPU-WB/GPU-PAT0 owned-memory path rather
than introducing incompatible UC/WC aliases to satisfy Vulkan's requirement.
It is an upstream implementation cross-check, not a CuBit hardware test.

Keep allocation-time zeroing and cache maintenance even when later accepting
coherent memory: Linux's `i915_gem_object_can_bypass_llc` explains how GPU
cache-bypass accesses can otherwise expose stale pre-zero contents. Coherence
does not replace initialization, command-cache invalidation, GPU barriers,
completion ordering, or ownership/retirement. Our pending bidirectional
no-CPU-flush NUC probes must exercise the owned PPGTT backing; GGTT firmware
upload results cannot stand in for those tests. The factory remains disabled
until the service can explicitly admit this contract and the adapter consumes
it instead of unconditionally rejecting HOST_COHERENT.

Pinned upstream references:

- [PCI capabilities and cache-level translation](https://github.com/torvalds/linux/blob/v6.16/drivers/gpu/drm/i915/i915_pci.c)
- [Object coherence and initial-page sanitization](https://github.com/torvalds/linux/blob/v6.16/drivers/gpu/drm/i915/gem/i915_gem_object.c)
- [Gen12 PPGTT encoding](https://github.com/torvalds/linux/blob/v6.16/drivers/gpu/drm/i915/gt/gen8_ppgtt.c)
- [TGL PAT initialization](https://github.com/torvalds/linux/blob/v6.16/drivers/gpu/drm/i915/gt/intel_gtt.c)

Memory discovery now uses device-query selector3 on the same pinned endpoint:
`[version,3,0,0]` returns `[status,version,policy,0]`. Policy1 means owned CPU-WB
RAM with explicit maintenance; policy2 means owned coherent CPU-WB RAM. Unknown
policies, nonzero reserved words and transport failures clear the C consumer's
output to unavailable. The service advertises only policy1, and only when
Runtime_Admitted, Publication_Owner_Ready and no Runtime_Fault hold. It does
not advertise policy2 based on PCI identity, firmware upload or PAT equality.
The physical-device factory stays gated. Session attachment now queries the
policy through Native_GPU_Buffers after live health verification, storing it
in process-owned lifetime state. Unknown/unavailable policy follows normal
retained failure cleanup; explicit-maintenance policy requires need_flush.
Only policy2 allows HOST_COHERENT BO requests for that session. Explicit-only
sessions still require HOST_CACHED; basic memory_init alone
never enables coherent allocation. The production service now conditionally
advertises policy2 only for the documented owned-WB ADL-N path, qualified by
both boot no-CPU-flush checks and current authenticated session/owner health.
See ADMISSION-INTEGRATION.md for the exact scope and unverified NUC outcome;
this is not a claim of completed native Mesa hardware rendering.
Hosted device-query tests and focused SPARK proof pass; C memory-query tests
exercise both policies, all256 single-bit reply mutations, malformed envelopes
and stale-output clearing. Native-transport C fixture passes with selector3;
these are not live IPC/GPU tests of the new selector.

Session-policy validation additionally covers policy0/unknown rejection,
explicit-maintenance without flush support, coherent allocation after policy2,
and isolation from an earlier explicit-only session. Startup callback and
allocation/retained-cleanup regressions pass with actual Mesa types and mocked
IPC. The Ada contract decoder rejects all256 single-bit reply mutations and
malformed envelopes. Native IPC-test apps compile with the new decoder; the
extended decoder probe passes in native CuBit QEMU: selector3 returns policy1
through the production query and buffer FFIs; invalid slot64 is rejected.
Runner completed its baseline IPC checks and final fault scan. Evidence:
`/tmp/cubit-session-policy-ipc.1r9efO/{run,serial}.log`. This remains a synthetic
GPU responder, not a hardware coherence test.

Allocator cross-check: Mesa26.2.3 `anv_CreateDevice` allocates its workaround
BO with HOST_COHERENT|MAPPED|INTERNAL|CAPTURE, without HOST_CACHED. These flags
do not require WC: Linux i915's LLC mmap path chooses WB for nonexternal,
nonscanout BOs when not using its set-PAT API. CuBit likewise uses its own
fixed WB/PAT0 contract, not Linux's PAT ioctl. Consequently policy2 accepts
that exact flag combination and default flags; policy1 still rejects both.
The actual-type session test checks this distinction and session isolation.

The memory-info helper now keeps `info.mem.sram.mappable` and `sys` size/free
in agreement: common ANV's `anv_init_meminfo`/`anv_update_meminfo` copy from
the former, so writing only `sys` was not sufficient for eventual integration.
Failures clear both free values and reject changes to either established size.
`cubit_mesa_init_memory_types` is the future backend callback helper: after
common heap setup it requires one system-RAM DEVICE_LOCAL heap, the canonical
system region, policy2 from the pinned endpoint, and a fresh validated budget.
It publishes one HOST_VISIBLE|HOST_COHERENT|HOST_CACHED|DEVICE_LOCAL type;
common ANV may append its dynamic-visible variant. It rejects protected/VRAM
configurations, explicit-only policy, reinitialization and heap-size mismatch.
Actual Mesa-type hosted tests pass with mocked IPC. This helper does not yet
install a backend or expose a Vulkan physical device, and the live service's
policy1 still correctly prevents its use.

Validation commands (Nix):

```sh
gprbuild -p -P tests/mesa-anv/memory-fixture/memory.gpr
tests/mesa-anv/memory-fixture/build/memory_tests
gprbuild -p -c -u -P tests/mesa-anv/memory-fixture/native.gpr native_gpu_memory.adb
```

Hosted tests cover 256 acquire/return combinations, rejection before transport,
failure output clearing, and actual C-to-Ada calls using the public header.
They mock the grant API; native compilation separately uses the actual CuBit
runtime declarations. Neither check executes kernel grant operations or a GPU.

The separate `tests/mesa-anv/native-integration` probe now exercises this same
bridge with real grants in CuBit/QEMU: read-only denial, shared data, multiple
borrows, deferred revocation and stale-reference rejection. Its synthetic
service uses ordinary RAM, not GPU backing; it does not validate the public
render-session protocol or execute Intel hardware. See that probe's README
for the private integration and serial evidence.

`native_gpu_query.ads/.adb` implements the scalar C entrypoint
`cubit_intel_query` using the native `CuBit.Messages.capCall` API. It does not
cast a C structure into the kernel's message layout, acquire permissions, or
perform Linux device-file operations. It validates both returned envelopes
and clears the four-word output on transport failure. Service status remains
in the reply; transport success is not device readiness.

The caller must hold the authorized capability slot stable during both
identity and topology calls. This bridge does not provide slot ownership or
discovery. It must not be used to expose a Vulkan device until the native
memory/context/submission/synchronization interfaces are implemented.

### GPU binding lifecycle integration gate

#### Upstream-driver hosting alternative (audit, 2026-09-30)

No architecture switch or imported driver is implemented. Before expanding
the native Intel implementation to another hardware family, compare a hosted
upstream DRM driver against the maintenance cost of independent hardware code.
DRM userspace ABI emulation for Mesa and Linux kernel-API compatibility for
the hardware driver are separate projects; the former does not supply the
latter. Keep the public CuBit capability/shared-buffer interfaces independent
of either implementation.

Inspected Linux v6.16 sources:

- [i915_driver.c](https://github.com/torvalds/linux/blob/v6.16/drivers/gpu/drm/i915/i915_driver.c):
  ordered/unordered workqueues, PCI ownership/MSI, runtime power management,
  DRM registration and display/GT initialization ordering.
- [i915_gem_object.c](https://github.com/torvalds/linux/blob/v6.16/drivers/gpu/drm/i915/gem/i915_gem_object.c):
  reservation/fence operations, RCU-delayed destruction, mmap teardown and
  deferred freeing. No-op synchronization substitutes would change lifetimes.
- [i915_gem_shmem.c](https://github.com/torvalds/linux/blob/v6.16/drivers/gpu/drm/i915/gem/i915_gem_shmem.c):
  shmem files/folios, scatter-gather pages, reclaim and writeback dependencies.
  A retained CuBit DMA allocator is not a drop-in implementation of these
  semantics. An initial non-pageable backend would need explicit resource
  limits and correct allocation-failure behavior, not fictional reclaim.

[FreeBSD drm-kmod](https://github.com/freebsd/drm-kmod) demonstrates upstream
driver reuse with LinuxKPI, but is kernel-hosted. More relevant architectural
references are Genode's separate framebuffer/GPU services and Mesa Iris
integration ([24.05](https://genode.org/documentation/release-notes/24.05)),
and its reported Alder Lake support for device 0x46a6
([25.11](https://genode.org/documentation/release-notes/25.11)). Those release
notes do not establish our N95 0x46d2 support, Vulkan support, or a wholesale
Linux i915 port. Inspect the actual Genode hardware/backend split before
using it as evidence for a compatibility-layer implementation estimate.

Proposed feasibility gates, not completed work:

1. Select a pinned upstream configuration and measure its linked host-service
   dependencies, separating disabled features from required paths. Header
   counts and successful compilation alone do not establish feasibility.
2. Exercise real imported buffer/fence/deferred-destruction code against a
   CuBit host adapter: allocation exhaustion, cancellation, device loss and
   completion racing with release. Do not stub success for unsupported paths.
3. Establish device-scoped MMIO/interrupt/DMA authority and IOMMU confinement.
   Userspace CPU isolation alone cannot contain unrestricted device DMA.
4. On hardware, validate probe/reset, actual drawing, repeated allocation and
   release, and recovery through CuBit IPC, with firmware scanout preserved
   until an explicit display handoff. Do not claim a hosted Linux test is a
   native CuBit port or a proof of imported C code.

This is an alternative implementation investigation, not a prerequisite for
running the existing barrier5 NUC image. Do not replace the working native
path until comparative evidence and a project decision justify the switch.

`cubit_intel_bind_buffer` now implements the native `0A24` offline binding
transport. It accepts an opaque BO handle, a page-aligned BO offset and byte
length, and a raw 48-bit GPU address. The request is four words:
`[1 | ((offset / 4096) << 32), handle, GPU address, bytes]`.
The low 32 bits of word zero hold the version, and the high 32 bits hold the
unsigned BO page offset. The FFI bounds the range by the allocation ceiling;
the service independently checks it against the actual authorized BO.
The service authenticates the session and resolves the
backing itself. It rejects duplicate mappings and updates after VM sealing.
This is preparation for a first context, **not a complete ANV VM backend**.
The old offset-zero-only C function signature has been replaced, not aliased;
all callers must provide the offset argument. The composed hosted bridge test
checks a nonzero slice reaches the expected DMA page, as well as offset
overflow, out-of-BO ranges, stale/foreign authority and malformed replies.

The offline VM builder also supports atomic `Unmap_Pages`: the owner supplies
expected DMA backing for every leaf, and a mismatch anywhere preserves the
whole image. Empty directory pages remain allocated and reusable; removing
the last leaf prevents sealing an empty image. Other aliases remain mapped.
This operation rejects sealed images and is not exposed as native live unbind.
It is a building block for an unpublished update candidate, not authorization
to reuse addresses or backing from a running GPU. Hosted tests and a native
driver build cover this implementation; hardware invalidation remains separate.

`VM_Image.Prepare_Update` now creates that mutable candidate from a sealed
source using new, disjoint table backing. Directory pointers are rebased;
data addresses and attributes are retained, and the source stays unchanged.
Both generations and their data must remain retained by the owner. This is
not connected to a live root-switch operation. Tests exercise two generations,
VA boundaries and rejected table/data overlap. Empty directory capacity is
currently retained, not reclaimed, so arbitrary long-lived churn still needs
resource-management work in addition to synchronized publication/retirement.

Activation audit (2026-09-30): do not infer that a new candidate requires
changing the live context's PML4 register. Intel TGL Vol2c-12.21, printed
pp489-490 (PDF519-520), describes PDP0 restoration from context state and
separately specifies pipeline flush/TLB requirements for descriptor programming.
The ring-buffer programming notes are not a GuC scheduling recipe. Linux
v6.16 `gt/intel_lrc.c:init_ppgtt_regs` places the four-level root in PDP0, while
`gem/i915_gem_context.c` accepts a VM on a prototype context but rejects
`I915_CONTEXT_PARAM_VM` on the finalized-context setparam path. Ordinary
`gt/gen8_ppgtt.c` mapping operations update the existing hierarchy instead.

Preferred initial activation design is therefore a stable context root with
submission exclusion and a separately prepared replacement hierarchy. It is
not implemented: all contexts using that VM must be drained/excluded; new
tables need visibility before parent entries; publication failure must keep
submission disabled and retain both generations; stale translations must be
invalidated before resuming or permitting address/backing reuse. Candidate
construction alone proves none of those conditions. Hardware-specific reset
and forcewake serialization remains required around invalidation.

Important ADL-N detail: Linux v6.16 `i915_pci.c` assigns `INTEL_ADLN_IDS` to
`adl_p_info`; `i915_drv.h:IS_ALDERLAKE_P` consequently includes this family.
Thus `gt/intel_tlb.c`'s `Wa_2207587034` OA invalidation branch for ADL-P must
not be omitted merely because its comment does not spell out ADL-N. The
existing CuBit GFX-only register description is not a complete invalidation
implementation. Validate the OA register and full sequence against PRM and
the NUC before using it to authorize reuse.

`Intel_GPU_ADLN_TLB_Invalidate` supplies a bounded RCS/OA
request/poll primitive. It writes only request bit0 (reserved bits zero),
checks both documented completion fields, and rejects reuse of an attempt.
Every callback failure or lost gate ends the operation; a 4ms software deadline
and finite poll count cover slow/stopped clocks. This is not an engine-wide
driver integration: its trusted caller must hold reset/invalidation exclusion,
forcewake, submission exclusion and pipeline-drain evidence, with no other
engine accessing the VM and OA collection inactive/drained. A Boolean callback
does not establish these facts. Mocked-MMIO tests are not proof of hardware
completion.

`Intel_GPU_Native_TLB_IO` supplies the narrow native register adapter using
the already-delegated C000 control page. It accepts only CED8/CEEC and request
value1, with ownership checks and a latched failure. A composed host-memory test uses the real adapter
and sequence with explicitly simulated request-bit clearing; it checks the
CPU access path, not actual GPU completion or bus-fault recovery.

The native driver now instantiates this pair as a post-draw checkpoint for
the sole bootstrap RCS context on 46D2. It requires successful draw/pixel
readback, completed standalone flush marker5, acknowledged scheduling disable,
retained reset/forcewake ownership and exactly one context. A context work hold
blocks tail publication and notification here; no application/OA work is admitted after
the exclusive reset. It logs `native TLB invalidation COMPLETE` only when both
request fields clear within the deadline. Failure faults the context table;
all backing remains retained. The source now reserves four fresh table pages
through the shared allocation-ticket namespace before GuC startup. At this
checkpoint it prepares a candidate preserving existing mappings, adds private
VA `0x208000` as an alias of the retained marker-batch page, publishes the fresh
children through the original root, and only then invalidates translations.
After successful invalidation, the source waits for scheduling enable, releases
the work hold and submits the trusted marker batch through this new alias with
completion sequence6. It then disables again and permanently closes the ring.
Failures fault the context; uncertain disable is not treated as quiescence and
all backing remains retained. `updated-VM batch ... completion=COMPLETE
disable=COMPLETE` is the physical checkpoint still to verify. Native compilation
and hosted ring/hold tests pass; **no physical resume/alias-fetch result yet**.
The published `cubit_live_vm_update.img` is frozen: it contains the earlier
completion-page alias/table-update checkpoint, not this resumed batch fetch.

`Intel_GPU_VM_Update` coordinates the intended serialized drain, publication,
invalidation and resume stages. It blocks `Can_Submit` before the first
callback through acknowledged resume, checks an expected mapping generation,
and permanently quarantines on failure, ownership loss or retirement during
a callback. Repeated successful updates advance the generation; stale and
nested requests do not invoke callbacks. Native application dispatch now
instantiates it, with pending-update exclusion on submission and context setup.
The callbacks must establish the actual GuC/hardware facts
documented in its interface. Mocked stages only test coordinator behavior.
The hosted coordinator regression now also composes the real
`Intel_GPU_GuC_Context_Lifecycle` state machine: acknowledged disable/enable,
backpressure fence reuse, missing/wrong events, late failure, repeated updates,
and delayed acknowledgments after quarantine. Transport and GPU completion
remain simulated; this does not validate the live CT event-dispatch integration.
In particular, retirement during a successful hardware resume still requires
the owner's disable/reset cleanup; software quarantine is not a GPU stop.

`VM_Materialize.Publish_Update` now implements the stable-root publication
step, now connected to native update dispatch. It compares the retained root
with the previous sealed image, materializes/flushes/verifies disjoint candidate
tables, rechecks the old root, then writes/flushes/verifies the replacement root
entries at the original hardware address. Rejected and failed attempts cannot
be retried through the same state. Partial publication retains both generations;
there is no rollback or implicit backing release. The caller must hold actual
GPU drain/scheduling exclusion and must invalidate translations before resume.
Trusted CPU-to-DMA mappings remain an assumption, not something numeric
disjointness or readback can prove. Hosted RAM tests cover child-before-root
visibility, stale/concurrently changed roots, every owner/flush failure point,
CPU alias rejection and untouched guard storage. Native compile-only also passes;
neither result establishes GPU coherence or hardware rendering.
The composed `vm_update_pipeline_tests` regression runs two generations through
the actual image builder, materializer, update coordinator and bounded TLB
invalidator. It adds then removes a mapping while retaining the original root
address and previous table backing. A simulated stuck invalidation after the
second publication leaves the new root in memory but admission quarantined,
without advancing the committed generation or resuming scheduling. GPU drain,
scheduling acknowledgments, cache visibility and register completion remain
fixture assumptions; these tests do not execute the native service dispatcher.
`Application_Image.Updates` now binds that publication step to the exact root
and private context allocation retained during preparation. It rejects table
or data DMA overlap with the context/ring allocation, as well as used table
CPU overlap, and permanently rejects further updates after a failed attempt.
Hosted tests exercise those alias cases and verify the saved context remains
unchanged; native compilation passes. Its exclusive gate must be supplied by
the owning update transaction. Native `main` now supplies that gate, requiring
retained forcewake/reset ownership and all RCS contexts disabled. This child
does not itself drain, invalidate, resume, or grant application admission.

Hosted regression:

```sh
nix develop -c bash -c 'gprbuild -p -P tests/intel-gpu/vm_image.gpr -XVM_IMAGE_OBJECT_DIR=build-tlb-invalidate tlb_invalidate_tests.adb && tests/intel-gpu/build-tlb-invalidate/tlb_invalidate_tests'
```

Audit of the prepared upstream Mesa sources establishes these requirements:

- `src/intel/vulkan/anv_allocator.c`, `anv_bo_init_new`: calls `vm_bind_bo`
  during ordinary allocation. A failed bind frees the selected VMA and closes
  the BO. An uncertain native reply must therefore make the device unusable
  for further submission; returning a recoverable allocation error would let
  a possibly bound address be reused.
- The same file, `anv_bo_finish`: successful `vm_unbind_bo` permits VMA reuse.
  Retaining physical backing alone is insufficient: stale GPU translations
  must no longer reach it before that address is reused.
- `anv_kmd_backend.h`: BO binding participates in the binding timeline waited
  by subsequent submissions. IPC acceptance is not GPU-visible completion.
- `xe/anv_kmd_backend.c`, `xe_vm_bind_bo`: binds `bo->actual_size`, not merely
  the application's logical size, and signals the binding timeline.
- `i915/anv_kmd_backend.c` uses no-op bind callbacks in its execbuffer model;
  its `i915/anv_batch_chain.c` collects BOs into the execbuffer submission.
  Those no-ops cannot be copied into CuBit's explicit GuC/PPGTT path.

Before installing native ANV bind callbacks, implement binding changes after
the initial context has run, with submission exclusion, completion ordering,
GPU translation visibility, and safe unbind/address reuse. A conservative
initial implementation may quiesce the affected context for updates, but it
must support repeated allocation/free/submission cycles, not just a fixed demo
whose resources are all allocated before the first draw. The precise hardware
invalidation sequence still needs the Intel documentation audit and hardware
verification; do not infer it from the current GGTT invalidation path.

`Buffer_Requests.Binding.Prepare_Request` now defines the pending `0x0A28`
bind/unbind request preparation: expected VM generation, authenticated BO
handle, BO page offset, raw48 GPU address and byte length. It rejects malformed
or stale requests before touching the candidate. Generation exhaustion rejects
rather than wraps. A successful result is a sealed offline candidate, NOT a
wire success or completed live update. Main does not dispatch this label yet.
The dispatcher must connect exclusion/drain, stable-root publication, hardware
invalidation, resumption and generation commit before acknowledging it; lost
completion replies must retire the session rather than replaying the operation.
Hosted request tests cover 18 rejection cases plus bind/unbind candidates and
unchanged previous images. Native compilation is not hardware validation.

Failure tests must cover lost bind replies followed by allocation, unbind
failure followed by address reuse attempts, and retirement racing with a
pending update. The existing offline transport tests do not establish these
properties. No bind/unbind callback is installed in a usable Vulkan device yet.

The native bind dispatcher now retires admission and application resources
when a successful offline bind reply cannot be delivered. It uses the
captured process incarnation/session, keeps the committed mapping and backing,
and does not claim GPU completion. A composed hosted regression
(`bind_retirement_tests.adb`) verifies that subsequent allocations and binding
replays are denied while the original page-table leaf remains unchanged.
This covers service-detected delivery failure, not every client-side timeout;
the Mesa-side uncertain-outcome rule and live-update requirements above remain.

`Context_Table.Hold_Work` now closes ordinary work admission per context while
leaving GuC controls/event dispatch available. `Work_Allowed` is checked by
the native live-ring owner before publication, not just at notification.
Release requires an enabled, owned, nonretired context; the update owner must
also establish completed GPU flushing, table visibility and invalidation.
These holds are not a hardware drain or an automatic update transaction.
The eventual coordinator must serialize the last flush-barrier publication
and hold acquisition against application publication, then wait for that
barrier and scheduling disable before changing tables. Hosted tests cover
eight hold/disable/enable/release cycles, another context remaining usable,
nested/premature release rejection, retirement and ownership loss.
The composed `vm_update_pipeline_tests` now uses the production context table
and GuC scheduling-event decoder alongside RAM page-table publication and the
TLB sequencer. It rejects work through all update stages, including after the
enable acknowledgment but before generation commit, and leaves work held on
invalidation failure. CT events, GPU flush completion and MMIO responses are
still simulated; this is stronger integration coverage, not hardware evidence.

That pipeline fixture now enters through authenticated buffer requests rather
than directly editing the candidate image. It creates a registry-backed BO,
prepares a bind with a nonzero BO page offset and then an unbind, and runs both
through the production VM update sequence. Foreign senders and stale generation
requests cannot consume the candidate or issue register writes. Failed second
invalidation leaves the committed generation unchanged, old tables retained,
and submission held. Run the composed hosted test with:

```sh
nix develop -c gprbuild -p -P tests/intel-gpu/vm_update_pipeline.gpr
nix develop -c tests/intel-gpu/build-vm-update-pipeline/vm_update_pipeline_tests
```

The implementation was promoted from the private GPU workspace, where a
synthetic native QEMU service exercised the C/Ada/kernel query path. The
main-tree hosted regression uses a mock `CuBit.Messages` package:

```sh
nix develop -c gprbuild -p -P tests/mesa-anv/native-fixture/bridge.gpr
nix develop -c tests/mesa-anv/native-fixture/build/bridge_tests
```

That test is not a kernel or physical Intel GPU test. This directory is not
yet linked into a Mesa application; it supplies the real IPC implementation
for that integration, not a stub Vulkan factory.

### Supervisor backing budget (integration in progress)

The Intel-only supervisor query0238 uses the existing kernel-stamped driver
identity and exact `[1,0,0,0]` request. The same-label response is
`[1,capacity,retained,unused_slots]`; unavailable backing returns F001 and the
startup wait loop returns F002. Querying never acquires the arena or creates a
buffer. Available bytes are capacity minus retained, but an allocation also
needs a free lifetime slot and remains limited to16MiB per request. The32MiB
arena is shared by private driver and application allocations; closing handles
does not reclaim bytes or tickets. This snapshot reserves nothing and does not
describe system RAM or GPU virtual address capacity. Mesa consumption is
not yet wired; public device admission remains closed. Hosted tests exercise
the codec and allocator, not supervisor IPC authorization or native execution.
The driver-side decoder rejects malformed envelopes and impossible byte/slot
combinations: each consumed slot accounts for1..4096 retained pages. It derives
free bytes and the maximum individual allocation, returning zero for the latter
when tickets are exhausted. Focused SPARK checks prove these bounds and zeroed
unknown results; they do not prove endpoint authentication or snapshot freshness.
Forwarding must use the existing asynchronous supervisor completion path so a
budget request cannot block GPU completion servicing.
The driver now issues a one-shot startup observation using capSubmit15 and a
dedicated token namespace, routes its completion before allocation replies,
and ticks a30-second deadline in the service loop. The query object is limited
(noncopyable); it retains a monotonically issued token serial across cancellation
and timeout. Matching late replies cannot complete a later transaction. Owner
loss, clock rollback, transport failure and malformed replies clear the result.
Boot logging reports this snapshot once; it is not reused as a public Mesa heap
or a reservation. Public on-demand forwarding now uses label0A2E, request
`[1,0,0,0]`, and response `[status,total_bytes,retained_bytes,unused_tickets]`.
Status0 is an observed budget,1 malformed request,2 unavailable,4 busy; all
failure payloads are zero. This endpoint needs an authorized GPU capability but
does not open a render session. It captures the kernel reply capability in
dedicated slot61 (separate from allocation slot62), starts a fresh supervisor
query, and responds after completion or bounded failure. No cached startup
observation is returned. Overlapping queries receive busy, not an unbounded
queue. Failed delivery does not trigger allocation rollback or query replay.
The supervisor and driver adapter compiled natively (build1620); IPC execution
is still unverified and this is not yet in a published image. Hosted tests
cover transaction behavior; focused SPARK analysis is not a proof of the entire
asynchronous transport or token-lifetime assumptions.

`native_gpu_query` also exports `cubit_intel_budget(slot, words)` with the same
scalar FFI and dual-envelope validation as device discovery. The Mesa adapter
sources in `tests/mesa-anv/cubit-device-query.*` decode this fresh observation
with `cubit_gpu_query_budget`; failure clears the output, and success derives
available bytes and maximum allocation while accounting for ticket exhaustion.
It is not yet connected to ANV physical-device heap initialization. The native
driver build does not compile this client bridge; its current evidence is a
hosted Ada/mock-runtime test and a C/mock-FFI test. The decoder separately passes
139281 page/ticket combinations and malformed-envelope/payload tests under
address/undefined-behavior sanitizers. No heap size is inferred from GPU VA or
the machine's total RAM, and public device admission remains closed.

`tests/mesa-anv/cubit-memory-info.*` applies a fresh query to actual ANV
`sys.size`/`sys.available` fields. It rejects discrete/local-memory topology or
a changed established capacity, clears availability on failure, and reports
zero available bytes when all lifetime tickets are consumed. The factory must
serialize these observations and pin the endpoint. It neither installs a KMD
callback nor initializes heap types, region identity or cache guarantees.
Hosted `memory-info-test.c` compiled against the prepared upstream ANV types
passes; this is mock IPC, not physical-device factory execution.

Do not enable `EXT_memory_budget` with the existing common reporting path yet:
the prepared `anv_physical_device.c` scales availability by 90%, rounds down to
MiB and asserts a nonzero budget. A small/exhausted bootstrap pool with zero
application usage can violate that assumption. Also, shared retained bytes are
not per-application `heapUsage`. A native reporting policy and sustainable
allocation/reclamation are still required; truthful observation alone does not
make the 16-ticket bootstrap allocator a complete Vulkan memory implementation.

The native `gem_create` adapter accepts `ANV_BO_ALLOC_FIXED_ADDRESS` as an
allocation policy flag. In the prepared upstream `anv_allocator.c`,
`anv_bo_vma_alloc_or_close` assigns the canonical explicit GPU address after
creation; native binding still validates and publishes that range. This is
not fixed CPU mapping or physical placement. Host-coherent requests remain
rejected. Hosted allocation tests alternate fixed/nonfixed requests and capture
intent across four threads (400 calls); binding/submission lifecycle regressions
also pass. Mesa's internal mapped state pools still require coherent backing;
these flag corrections alone do not make them usable.

`ANV_BO_ALLOC_CAPTURE` is accepted as diagnostic intent, retained by common ANV
in `bo.alloc_flags`. In the inspected upstream `anv_private.h` it requests
error-state capture; the i915 backend translates it to `EXEC_OBJECT_CAPTURE`.
The bundled `include/drm-uapi/i915_drm.h` describes copying object contents into
a GPU-hang error state. It is not Vulkan address capture/replay, a memory
visibility guarantee or a synchronization primitive. Native CuBit currently
exports no raw-buffer hang dumps; accepting this hint does not mint grants or
send buffer contents to logs. This is an explicit native diagnostic policy,
not Linux i915 UAPI compatibility: Linux ANV discovery still requires the
kernel's exec-capture feature. Any future byte-dump export requires its own
authority/redaction policy and bounded retained storage.

### Context deregistration prerequisite for reclamation

The current native drain stops at acknowledged scheduling-disable. It does not
deregister the GuC context, remove every GPU alias or permit arena/ticket reuse.
`Intel_GPU_GuC_Context_Request.Deregister` now encodes the two-word GuC request
(FAST_REQUEST action4503, context ID), rejecting the reserved/out-of-range IDs.
Hosted tests cover all65535 accepted IDs; SPARK proves the exact encoding and
zeroed rejected result. No caller sends this request yet. Completion decoding,
pending-state/credit accounting and quarantine on uncertain outcomes must be
integrated before any reclamation is considered; CPU grant retirement and GPU
mapping/visibility gates remain separate requirements.

Cross-check: Linux `__guc_action_deregister_context` emits action plus ID and
reserves a separate G2H completion in
[intel_guc_submission.c](https://android-kvm.googlesource.com/linux/+/d4d7c03f7ee1d7f16b7b6e885b1e00968f72b93c/drivers/gpu/drm/i915/gt/uc/intel_guc_submission.c).
This is a firmware protocol operation, not permission for a client to free
physical backing or recycle an authenticated identity.

Completion decoding now recognizes exact GuC EVENT4600 with one ID word
(two payload DWORDs including HXG, excluding CT), using Linux v6.16
`guc_actions_abi.h`, `intel_guc_fwif.h` and
`intel_guc_deregister_done_process_msg` as the cross-check. Reserved header
bits, unexpected lengths and reserved/out-of-range IDs are rejected. Routing
uses the context ID, not the CT fence. Sessions accept matching completion only
in the queued deregistration state; an unsolicited or duplicate matching event
quarantines. Events for other IDs remain retained. No completion grants
reclamation permission.
Hosted tests cover every valid ID, every fence for routing, malformed frames,
unchanged session state and retention failure. SPARK proves decoder bounds and
postcondition; the generic router was not analyzed in that focused proof run.

The pure context lifecycle now has `Deregister_Pending` and `Deregistered`.
Only acknowledged `Disabled` with no send in progress can prepare it, consuming
a fresh dynamic fence and accounting for three receive DWORDs. Proven
nonpublication/backpressure may restore the disabled state and fence; uncertain
publication quarantines. The matching completion is accepted only after the
send returns queued; early, duplicate or wrong-phase matching events quarantine,
while another context's ID is ignored. Old request failures remain fatal even
after completion. A context cannot be re-enabled once deregistered. These are
software lifecycle transitions, not a live transport-credit reservation.
The session adapter now sends through its existing serialized Queue callback,
using trusted-local admission-closed and work-drained gates and checking owner
readiness before and after sending. Its existing four-DWORD control allowance
covers the three-DWORD completion. It dispatches the matching event into this
lifecycle and preserves unrelated IDs. Hosted session tests cover gate refusal,
exact packet/fence, no-publication retry, uncertain send, owner loss, late errors
and duplicate completions. The table's bounded retirement loop now invokes
this operation after scheduling disable, closed admission, no VM-update hold,
and trusted dispatcher evidence that setup/submissions completed and no deferred
publisher remains. Disable and deregistration share a deadline and poll budget.
Public retirement queries distinguish Disabled from Deregistered; registered
contexts require the latter before reporting quiescence. No context backing,
GPU mapping, fence range or ID is reclaimed by this acknowledgement.

The ADL-N GuC translation path remains the documented CEE8 MMIO fallback.
Linux v6.16 enables CT invalidation via a platform capability absent on ADL-N;
GuC readiness alone must not select the newer7000 action. The typed MMIO
adapter admits either GuC-only or GFX/OA-only offsets, not their union. A
bounded GuC self-clear waiter is tested through the native adapter on host RAM,
but is not yet invoked live: it requires drained/flushed accesses before the
request (TGL Vol2c p1333). Neither a posted request nor the hosted completion
simulation authorizes physical backing reuse.

### Bootstrap GPU-to-CPU visibility sample

The native triangle diagnostic now samples its five target pixels after draw
completion and acknowledged scheduling-disable, before the ordinary target
CLFLUSH readback. The pre-draw clear check loaded these same CPU cache lines.
`draw no-CPU-flush read=TRUE center=FFFF0000 matches-flushed=TRUE` reports that
this particular observation matched the subsequent maintained readback.
A mismatch is evidence against relying on unmaintained reads on this path.
A match is **not proof of HOST_COHERENT support**: intervening eviction or CPU
migration may hide missing snooping. The probe does not measure CPU-to-GPU
visibility, all Mesa MOCS selections, or simultaneous accesses. Existing cache
maintenance, checked triangle readback and coherent-allocation rejection remain
unchanged. No native Mesa factory is enabled by this diagnostic.

`native_initial_ring_tests.adb` covers offsets, insufficient backing, denied
ownership, and ownership loss at every sample boundary using host RAM. These
tests do not emulate a GPU cache or establish hardware coherency.

The CPU-to-GPU companion probe has a PPGTT-only one-DWORD copy encoder in
`Intel_GPU_Memory_Copy_Command`. Intel TGL Vol2a-12.21 RenderCS pp979-980 is
the source; Mesa's inherited gen80 XML agrees on the five-DWORD packet.
The source/destination are raw48, DWORD-aligned addresses, canonicalized only
when encoded; zero, misaligned and out-of-range inputs return a zero invalid
packet. Hosted tests cover225 address pairs and all32 header bits. Focused
SPARK checks prove the validity/header/failure postcondition and range safety,
not hardware access or cache behavior. The private startup marker batch now
includes the copy, before its END and the ring's stalled completion barrier.
The CPU source is completion-page offset256 and the GPU destination offset128,
separate from marker/L3 data and each other on ADL-N cache lines. Source writing
is one-shot after initial ring publication, before scheduling; an MFENCE orders
the store without CLFLUSH. HWSP polling touches another page. Only after the
first batch completes is the result page flushed/read, before any repeated
submission can mask the initial result. The log is
`CPU-no-flush copy prepared=TRUE read=TRUE value=43504348 match=TRUE`.
Integration still needs native compilation and physical validation. Eviction,
migration, and platform snooping can affect observations; this is not general
Mesa coherent-memory admission. MI_COPY_MEM_MEM is a posted memory producer
(TGL Vol8 pp53-55), not a standalone completion fence. A readback match will
still be diagnostic evidence only, not general coherent-memory admission.

The hosted test also flips each of the 64 bits in each of the two reply
envelopes independently (128 malformed-reply cases). Every mutation must
fail and clear the output, including otherwise valid service-status words.

### Native Mesa static dispatch and device-initialization diagnostics

The standalone native link now retains the aggregate `libvulkan_intel.a` with
`--whole-archive`, before the remaining support archives in the link group.
This follows upstream Mesa's `link_whole` treatment of weak Vulkan entrypoint
overrides (`src/vulkan/runtime/meson.build` and `src/intel/vulkan/meson.build`).
Ordinary archive extraction can leave dispatch entries null even when the
implementation is available in an archive. Individual forced-symbol roots
were replaced by this aggregate policy; final ELF symbol checks remain as
assertions, not as the retention mechanism.

`native-snapshot-discovery.c` assembles the real gfx12/ANV/common dispatch table
and checks 43 entries used by the triangle path and its internal helpers. The
2026-10-01 native CuBit regression passed this check, synthetic physical-device
enumeration, and instance cleanup. It executes no GPU commands and does not
establish successful logical-device creation or hardware rendering.

`perf-portable.patch` moves upstream counter-pass planning and MDAPI result
formatting into portable translation units without changing their algorithms.
Linux-specific performance configuration stays separate; this does not enable
CuBit performance-query extensions or OA access.

`native-device-trace.patch` provides an optional weak diagnostic hook around
31 result-bearing call sites in `anv_CreateDevice`. The authorized test app
prints bounded `MESA-INIT device:... begin/returned result=...` messages.
These preserve the original calls, result handling, and control flow; they do
not retry failed initialization. NUC v10 enumerated one physical device and
attached its session successfully, but logical-device creation returned -3.
The new stage diagnostics are intended to identify the first failing call.

A source-audit finding still requiring implementation is the ANV CPU state
table's anonymous shared-memory backing. The current header specifies a 1 GiB logical
file, maps growing prefixes, and retains older aliases until destruction so
concurrent users see the same underlying entries. CuBit's current libc lacks
the required anonymous-file operations and rejects `MAP_SHARED`; its owned
allocation interface is bounded at 16 MiB without reserve/grow operations.
This is CPU allocator metadata, separate from GPU BO backing. A port must
preserve aliasing and growth semantics, not replace them with independently
copied buffers or eagerly allocate 2 GiB. This finding is not yet confirmation
of the failing stage on the NUC.

`tests/mesa-anv/state-table-mapping-contract.c` exercises the required OS
semantics with six growing prefix aliases: updates through either old or new
views remain visible, newly extended storage is zero-filled, and descriptor
close/old-view retirement preserve the final view. Nix-hosted execution passed;
the `CONTRACT_PRIVATE_MUTANT` variant failed the old-view visibility assertion.
This is a Linux OS-semantics fixture, not execution of ANV's allocator, a
concurrency proof, a native implementation, or GPU evidence.

The optional `--state-table-probe` on `test-native-instance-link.py` goes
further: with `--no-provider --retain-transport`, it links and calls the actual
ANV state-table init/add/finish functions in native CuBit, without a physical
device. Successful execution must also pass growth and retained-alias checks.
The first native run instead reports unimplemented x86-64 syscall 319
(`memfd_create`), then `MESA-CPU-TABLE init=-3 bytes=0` and an explicit test
failure. This reproduces a real CPU memory-port gap independently of the NUC.
It does not establish which operation fails first in the hardware device path.
