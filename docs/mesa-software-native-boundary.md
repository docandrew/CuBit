# Mesa software rendering: native boundary

The pinned Mesa 26.2.3 Linux lavapipe build passes transfer readback, a compute
shader (1024 exact pixel words), and offscreen triangle rasterization (992
interior/exterior samples). See `tests/mesa-software`. None is native CuBit.

## Evidence from the current checkout

The first CuBit-target softpipe cross-build now succeeds (Mesa26.2.3), producing
`tests/mesa-software/target/native-softpipe/src/gallium/drivers/softpipe/libsoftpipe.a`
and static GL dispatch. Meson uses system `cubit`, the CuBit C/C++ wrappers,
`MESA_SYSTEM_HAS_KMS_DRM=0`, and no LLVM/JIT. Optional compression, XML parsing
and shader caching are disabled, and target pkg-config is disabled to avoid
silently importing host dependencies. `_GNU_SOURCE` exposes the musl declarations
needed by Mesa for this otherwise unrecognized OS target. The initial build used
unchanged Mesa sources; the follow-up below adds explicit CuBit detection.

Reproduce configuration inside `tests/mesa-anv/host-shell.nix` with
`bash tests/mesa-software/prepare-cubit-source.sh PINNED_SOURCE NEW_SOURCE`, using
the source pinned by `tests/mesa-anv/source.nix`, then
`bash tests/mesa-software/configure-cubit-softpipe.sh NEW_SOURCE BUILD` and
`bash tests/mesa-software/build-native-softpipe.sh NEW_SOURCE BUILD` under
the shared build lock. On reconfiguration from immutable Nix sources, Meson's
generated `BUILD/bin/drm-shim` copy may need its owner write bit restored.

The explicit dependency build now compiles Gallium/NIR/utilities (493 build
steps), and `build-native-softpipe.sh SOURCE BUILD` links `native-softpipe.app`
with the CuBit libc/C++ runtime and CCL manifest. Readelf confirms a static EXEC
with separate RX/RW loads and non-executable stack, no interpreter or dynamic
section. The small C API boundary test creates a32x32 offscreen resource,
clears red, reads every pixel using the returned stride, and reports through
the explicit diagnostic console rather than unbound stdout. No new syscall
shim was needed for linking. The native four-CPU KVM QEMU test now passes all
1024 readback pixels with512MiB guest RAM. The128MiB run failed context creation
with an observed262368-byte allocation failure; increasing memory resolved it
without changing allocator semantics. Failure-only linker wrappers remain in
the test to report future allocation errors. Serial evidence:
`tests/mesa-software/target/native-softpipe-512m-serial.log`.
The unsupported sysinfo syscall99 diagnostic is still present, but did not
prevent this test passing. This proves context/resource/clear/readback and
cleanup in CuBit. The subsequent native probe draws a green right triangle over the red clear
using vertex/fragment passthrough shaders, vertex fetch, viewport and raster
state. Four-CPU KVM run81238 passes992 non-edge pixel samples plus the1024
clear samples; serial is `tests/mesa-software/target/native-triangle-serial.log`.
The32 diagonal samples are deliberately excluded to avoid an edge-ownership
convention oracle. The no-op draw would fail because inside pixels must be green.
This is Gallium shader rasterization in CuBit, not yet OpenGL API/presentation.
Test-only file wrappers identified the denied filesystem request as
`/sys/devices/system/cpu/cpu0/cpu_capacity` (native filetrace run51367 passed
both render oracles). Meson targets CuBit, but the compiler inherits `__linux__`,
which incorrectly selected Mesa's Linux CPU topology discovery. The explicit
`__cubit__` / `DETECT_OS_CUBIT` adaptation takes precedence and uses Mesa's POSIX
utility substrate without selecting Linux discovery. This is not a claim of
complete POSIX support. The source-preparation script patches a private copy,
never the Nix store. No filesystem authority is added to the probe.
The corrected native build (548 compile/link steps) passes both render oracles
in four-CPU KVM QEMU (run82769). Neither the file-probe diagnostic nor denied
filesystem grant appears; sysinfo99 remains unsupported/nonfatal. Evidence:
`tests/mesa-software/target/cubit-platform-serial.log`. The default native
build/runner directory is now `target/native-softpipe-cubit`.
Run in the repository Nix environment (not only the Mesa build shell):
`flock --exclusive --nonblock coordination/build.lock nix develop -c bash tests/headless/run.sh --test softpipe --accel kvm --timeout 30 --keep-logs`.
The runner defaults to512MiB for this test and changes only a temporary disk.
Set `SOFTPIPE_IMAGE` to an absolute alternative probe path to test a fresh build.
Softpipe avoids needing JIT
permissions for this first native renderer; LLVM/lavapipe and hardware Intel
support still require their own integration. It does not replace those goals.

For the Linux-hosted lavapipe build, `readelf -d` reports LLVM21.1, zstd, DRM, expat, libstdc++, libm, libgcc_s,
and libc dependencies. `tools/audit-mesa-software.py` inventories these and
direct undefined symbols reproducibly. This is not a transitive-link audit.
Mesa's `system_has_kms_drm` is selected from the target OS, independently of
an empty `platforms` option. Thus `platforms=[]` does not remove Linux DRM.

CuBit has native libc threading via THREAD_CREATE and futexes, but the current
libc mapping adapter is not suitable for LLVM JIT. The initial audit found
`sys_mmap` ignoring `prot` and `SYS_mprotect` returning zero without changing
mappings. The follow-up now rejects executable mmap with ENOTSUP and mprotect
with ENOSYS. This removes false success, not the need for an implementation.
Other mmap protections and munmap still have legacy limitations: ordinary
heap-backed mappings do not establish read-only/guard protection, and munmap
does not reclaim them. LLVM must not be admitted without real owned regions.

## Required implementation sequence

### Native OpenGL frontend bring-up

`build-native-opengl.sh` builds Mesa's GL state tracker, GLSL compiler and static
dispatch against CuBit, then links a separate surfaceless frontend probe. The
probe creates a private RGBA8 framebuffer and uses GL dispatch for clear,
compatibility-mode triangle and readback; it does not call Gallium draw methods
directly. Its compile command derives from Mesa's generated state-tracker command
to avoid an internal header ABI mismatch. The headless `opengl` case requires
both pixel oracles and rejects explicit failure markers.

The initial native run41492 faulted at address zero during recursive-mutex
creation. Disassembly identified `mtx_init` calling weak pthread attribute
references; the implementations existed in static libc but weren't pulled into
the link. The CuBit source patch keeps these references strong. Link23652 now
contains all three real implementations. This removes a static-link adaptation
bug, not a reason to disable mutexes or add a no-op threading shim.

The corrected native QEMU run68808 reports Mesa26.2.3 OpenGL3.3 compatibility
and passes 1024 clear pixels plus 992 triangle samples, with success emitted
after context/screen cleanup. Serial evidence is
`tests/mesa-software/target/native-opengl-strong-serial.log`. This is a narrow
GL frontend/rasterization check, not full3.3 conformance, hardware acceleration,
depth/texture coverage or desktop presentation. The optional sysinfo99 query
still reports unsupported.

Native depth follow-up (build56818, QEMU53886 exit0) adds a24-bit depth
attachment and checks4096 color/depth samples: near/far fullscreen geometry
in both orders, with depth testing disabled as a last-writer control and enabled
with GL_LESS. Enabled draws preserve green near geometry and depth0.25;
disabled draws preserve the last color and cleared depth1.0. NaNs fail the depth
range check. Evidence: `tests/mesa-software/target/native-opengl-depth-serial.log`.
This exercises depth storage, comparison, state switching and readback, not
general 3D conformance or Intel hardware.

### Presentation ownership observation

Softpipe's `softpipe_displaytarget_layout` delegates allocation/pitch to
`sw_winsys.displaytarget_create`; map/unmap callbacks can expose a native shared
buffer. The existing null winsys deliberately cannot create display targets.
This is the extension point for native presentation, not evidence of integration.

The Desktop `OP_SURFACE_PRESENT` handler currently validates ownership and
schedules redraw damage; its success reply is not a completed-consumer fence.
Do not overwrite a lent buffer merely because that request replied. Before
continuous rendering, establish the exact attachment-retirement/compositor-read
boundary or introduce explicit completion. CPU and later GPU writers must obey
the same no-reuse-before-retirement contract. The existing Servo frontend copies
into its lent buffer; that implementation is not a zero-copy completion model
to copy uncritically.

Further source tracing narrows the current CPU case: `drawClientBuffer` copies
client rows synchronously, the single event loop serializes that with request
handling, and `releaseSurfaceBuffer` clears the old pointer before returning its
grant acquisition. Successful replacement therefore stops future compositor
reads of the old attachment in this implementation. This is not a guarantee
that the old frame was displayed. It also cannot be carried unchanged into
asynchronous GPU sampling. A future alternating-buffer client must retain the
new attachment unchanged and only reuse the retired old one after unambiguous
success; failed/ambiguous attachment must not imply retirement.

`buffer-winsys.c` is a small Mesa boundary adapter for a single caller-owned,
page-aligned BGRA allocation with explicit capacity and pitch. It neither
allocates a second pixel image nor implements Desktop IPC. It rejects geometry,
alignment and capacity mismatches and duplicate live target creation. Caller
storage remains owned by the caller until resource/screen destruction. No Linux
handle imports or display submission are supported. This is a first adapter
step, not a stable public graphics API or a complete presentation backend.

The isolated native caller-buffer probe (build94217, QEMU71155) checks1152
BGRA pixels in the original allocation after a Gallium clear/flush, plus row
padding and allocation-tail sentinels. It also rejects undersized backing,
zero width, misaligned base and duplicate live target creation. No texture-map
readback or second pixel allocation is used by this adapter. Serial evidence:
`tests/mesa-software/target/native-buffer-isolated-serial.log`.

The initial combined GL-then-second-context test14156 failed a262368-byte
context allocation before exercising this adapter. The standalone process
separates adapter correctness from that unresolved multi-context allocation
issue. Do not claim that repeated context creation or memory reclamation is
validated by the isolated pass.

The three-cycle native diagnostic now reproduces this failure independently
of the buffer adapter. Build85594 succeeded; QEMU72930 correctly exited1
on the second context (512MiB guest). Cycle0 completed all color/depth checks,
grew the heap231628800 bytes, and requested successful `munmap` for231325696
bytes. Cycle1 grew another186896384 bytes before failing allocation262368;
cumulative successful unmap requests were418222080 bytes. Evidence:
`tests/mesa-software/target/lifecycle-3-serial.log` and `lifecycle-3-run.log`.
Musl mallocng individually maps allocations >=131052 bytes; its free path
calls munmap. CuBit libc currently backs mmap with growing SBRK and returns0
for munmap without reclaiming mappings or storage. Thus the released storage
is lost to allocator reuse until process exit. The counter wrapper forwards
the real call unchanged; this is diagnosis, not a fix or proof that all Mesa
allocations are freed. Fix owned-region reclamation/alias and grant lifetime
rules before advertising repeated-context support. Enlarging guest RAM or
shrinking softpipe caches would only postpone exhaustion.

Run94250 used the older diagnostic because build54794 failed a missing-prototype
check; it is not evidence for munmap accounting. The corrected build85594 and
run72930 above supply that evidence. The runner now also requires the final
context-lifecycle marker, so partial cycles cannot count as success.

### Native Desktop window

`native-mesa-window.app` now renders four512x384 BGRA regions via Gallium into
one caller-owned buffer, then attaches that exact storage to Desktop through a
generation-bearing grant. Its manifest requests only Desktop service access;
there is no raw display, device or global input permission. The frame is
immutable after attachment and retained for process lifetime, so this first
presentation check does not infer retirement from a present acknowledgement.
It is a Mesa Gallium window probe, not yet the OpenGL state-tracker drawable.

Build12471 and four-CPU native QEMU1010 passed. The screenshot was inspected,
and `check-window.py` validates all196608 composed pixels at detected origin
(102,112); an intentionally corrupted pixel is rejected. Evidence:
`tests/mesa-software/target/native-window-serial.log` and
`tests/mesa-software/target/native-window.png`. The headless `mesa-window` case
requires the attachment marker and the screenshot oracle. This avoids an extra
renderer-to-client readback copy; Desktop still performs its normal CPU scene
composition. It is not end-to-end zero-copy scanout, a swapchain, or hardware3D.

OpenGL drawable follow-up: the window probe now wraps the same caller-owned
resource in a `pipe_frontend_drawable` with a single BGRA front attachment.
Mesa's GL compatibility context draws four quads through dispatch and finishes
before the explicit Desktop attachment. The frontend flush callback defers
presentation rather than pretending to provide a compositor completion fence.
The old direct Gallium clear path in this window probe has been replaced.
Build19810 and QEMU45858 exit0 passed the same196608-pixel screenshot oracle;
the GL draw marker and no-error check are required too. Screenshot:
`tests/mesa-software/target/native-gl-window.png`, serial:
`tests/mesa-software/target/native-gl-window-serial.log`. This joins the GL
frontend to a visible native CuBit window without renderer readback copies.
Storage remains immutable after its one attachment; animation, replacement,
texture/depth in this window, teardown/restart and Intel acceleration remain.

### Repeated CPU presentation

The adapter now accepts one or two caller-owned buffers, copies only their
metadata, and rejects invalid counts, address-extent overflow and overlapping
virtual backing ranges. The old single-buffer constructor was replaced rather
than retained as an unused compatibility API. Heap allocation/pinning remains
the caller/kernel obligation; this range check is not a physical-alias proof.

The window probe renders nine changing GL frames with one context and two
resources. Before each attachment it checks every pixel in the new target and
confirms the currently attached buffer still contains the previous frame.
Only an unambiguous successful replacement changes the local attached index;
failure exits without further buffer reuse. The final frame remains immutable.
There is no renderer readback copy. This checks repeated render/attachment
replacement, not guaranteed scanout of every intermediate frame or latency.

Build69694 passed. Sequential native QEMU run73922 passed the single-buffer
guard regression and the nine-frame window test, including the196608-pixel
final composed screenshot oracle. Serial logs are
`tests/mesa-software/target/native-swap-buffer-serial.log` and
`tests/mesa-software/target/native-swap-window-serial.log`. No acquisition-return
failure was logged. The separate multi-context allocation failure remains open.
This is regression-tested CPU buffer retirement, not SPARK-proven ownership or
a substitute for GPU submission/completion fences.

### Visible depth-tested OpenGL3D

The `native-mesa-cube.app` variant shares the same drawable, Desktop-only
authority and two-buffer replacement path, adding a Z24/S8 depth attachment,
orthographic projection and rotating six-face cube. Build91967 and QEMU74052
exit0 passed nine rendered frames and the independent final screenshot oracle.
The checker traces orthographic rays into the inverse-transformed box,
selecting the nearest entering face rather than comparing with a blessed
reference screenshot. It validates194680 pixels;1928 pixels near silhouette
or face boundaries are explicitly excluded for float/raster edge differences.
Visible face samples are29784 (-X),15110 (+Y),13774 (-Z); other faces are hidden.
Screenshot `tests/mesa-software/target/native-cube.png` was visually inspected;
serial is `tests/mesa-software/target/native-cube-serial.log`.
A negative control replacing a non-edge visible-face pixel with a hidden face's
color is rejected by the oracle.

Explicit GLSL120 vertex/fragment shaders now replace the implicit fixed-function
shader path, retaining compatibility matrix/color inputs and the same geometry.
Both compile statuses and program link status are checked; failures report the
driver info log and abort rendering. The test-only program cache belongs to the
single context retained for the VM lifetime. Native build24650 and QEMU96837
passed the shader marker, nine-frame retirement checks and the unchanged
194680-pixel geometric oracle. Evidence: `tests/mesa-software/target/glsl-2-serial.log`
and `glsl-2-run.log`. Initial run98563 correctly failed because stdout buffered
the marker; using the demo's existing native debug channel corrected that
diagnostic issue without weakening the test. This validates GLSL compilation
and execution through softpipe, not Intel shader compilation or GPU execution.

The subsequent indexed variant removes `glBegin`/quads from the cube scene.
It uploads 24 interleaved position/color vertices and 36 unsigned-short indices
once, checks buffer sizes and GL errors, then uses explicit shader attributes
and one `glDrawElements(GL_TRIANGLES)` per frame. Native build21599 and
QEMU41613 passed the required buffer-upload marker, nine frames and the same
194680-pixel oracle (identical visible-face counts). Evidence is in
`tests/mesa-software/target/indexed-serial.log` and `indexed-run.log`.
Buffers are retained with the single demo context; multi-context lifecycle
and GPU-backed buffer allocation remain outside this test. No performance
comparison follows from replacing immediate-mode calls alone.

Texture sampling adds explicit UV attributes and a sampler uniform with a4x4
RGBA8 texture (nearest, clamp-to-edge). The pattern has distinct asymmetric
levels per texel. The independent ray oracle computes face UVs from hit
coordinates; all16 texels on every visible face require at least100 samples.
Build43868 and native QEMU43093 passed upload/shader markers, nine frames and
194673 pixels. Only7 additional pixels were excluded near texel boundaries.
The preceding untextured screenshot is rejected (negative test67261).
Evidence: `tests/mesa-software/target/texture-{serial,run}.log` and
`native-textured-cube.png`. This covers opaque nearest texture sampling, not
alpha compositing, linear filtering, mipmaps or Intel GPU texture execution.

The demo is still software-rendered and bounded to nine frames, then retains
the last frame. There is no claim of full GL conformance, texture/lighting
coverage, performance parity with other systems, or Intel execution. The
separate exact-color quad regression remains available.

### Remaining integration

1. Define an owned virtual-memory region operation with real mapping lifetime
   and protection transitions. Reject W+X; validate complete owned ranges;
   serialize against faults, sibling threads and teardown; complete required
   cross-CPU TLB invalidation before reporting success. JIT permission must be
   an explicit policy decision, not a side effect of ordinary heap access.
2. Bind libc/LLVM executable allocation to that operation. Unsupported changes
   must fail honestly. Test write-to-execute, execute-to-write, invalid ranges,
   partial failures, stale ownership and release under native CuBit.
3. Cross-build LLVM/Mesa against the CuBit ABI and static dependencies. Select
   the actual target OS, omit DRM backends, and implement CuBit presentation;
   do not satisfy Linux DRM imports with pretend-success shims.
4. Run the same transfer/compute/triangle checks in CuBit before connecting
   windows. Hosted linking is not an integration result.
5. Present capability-owned shared surfaces with explicit completion and
   retirement. CPU completion of a software draw and GPU completion of a
   hardware draw obey the same consumer-visible ownership rule. IPC carries
   handles/metadata; pixel storage stays in the data plane. Format, pitch,
   adapter compatibility and copy fallback must be explicit.

The existing UI `Surface.View` aliases pixels inside a parent CPU canvas. It
is not a Vulkan image import, fence or swapchain. Preserve that useful local
abstraction without treating it as proof of cross-process/GPU ownership.

This sequence supports the hardware goal too; it does not replace Intel GT
reset, firmware authentication, GPU address-space isolation or submission.

## Mapping-path audit before syscall exposure

Owned mappings reserve the half-open user VA aperture
`0x580000000000..0x590000000000`. MAP_DEVICE, ALLOC_DMA and MAP_INTO reject
destination overlap before mapping/allocation. Bootstrap initrd mapping checks
the same predicate. Heap/images remain below received grants; framebuffer and
stack are above this range. The first trial at0x500000000000 collided with the
initrd seed, caught by native28147; the corrected native54841 passes the
four-CPU storage-grants suite. Boundary test69779 checks221213 cases against
wide integer arithmetic; SPARK proves the predicate's half-open overlap
postcondition. This is destination exclusion, not physical-alias admission.
No owned-region allocation/release syscall is exposed yet.

Native grant creation now holds the source address-space lock across PTE
lookup and `pinOwnedFrame`, nested below `grantLock`. It drops the source lock
before mapping into the receiver, so it never nests two address-space locks
(including self-grants). This closes the lookup-to-pin window against mapping
mutators using that same lock. BuddyAllocator already defers `freeFrame` when
pins remain and reclaims on the final unpin. Neither fact alone implements
munmap or excludes arbitrary privileged aliases.

Native build/boot4599 passed the four-CPU `storage-grants` suite, including
128 grant reuse rounds and the reclamation/reference markers. Evidence:
`tests/mesa-software/target/grant-source-lock-serial.log`. This is an existing
grant regression, not a concurrent unmap stress test or a formal locking proof.

Process teardown frees every remaining node in `proctab.frames`. Region
release must remove frame records as well as PTEs; freeing frames while
leaving their records would cause a later double free. `LinkedLists.detachRange`
now supports checked, nonwrapping contiguous subranges, preserving both
resulting circular lists without allocating or freeing nodes. A future region
allocator can retain first/last node handles for frames added under the
address-space lock. Release must validate this inventory before mutation,
unmap/shoot down, detach it under the same lock, then free its frames and
nodes. User-supplied node handles must never be accepted by a syscall.

Hosted actual-list tests11883 cover816 valid intervals plus outside/null/
mismatched/wrapping ranges and nonempty destinations. Rejected calls preserve
the source; valid detach conserves slab nodes. Native kernel build4096 links
successfully. At that checkpoint no mapping syscall used it; it does not itself
establish page ownership, pin safety or SPARK proof.

The following paths were inspected in the current kernel. They must agree on
region ownership; a new registry must not be consulted only by mprotect.

| Existing path | Integration requirement |
| --- | --- |
| `Syscall.IPC.handleSbrk` | Its heap limit is the minimum of stack bottom and grant-region start. Keep dedicated regions outside its growable interval. |
| `Process.pageFault` and sibling-fault recovery | Both use heap/stack admission under the address-space lock. A temporarily revoked region must not be repopulated as ordinary writable heap. |
| `handleMapDevice`, `handleMapFB`, DMA mapping | Reject destination overlap with owned regions; normal-RAM PTE flags alone are not a physical ownership ledger. |
| `handleMapInto` | Reject destination overlap and prevent a physical frame owned by a protected region from being aliased writable elsewhere. Its existing received-grant overlap test is not enough. |
| `Process.IPC.createGrant` | Reject protected-region source pages, including partial overlap, before retaining pins or publishing a grant. Existing borrowed-region rejection does not cover future JIT regions. |
| Process teardown | Retire registry identities and mappings before freeing frames; block stale handles and concurrent transitions. |

Use a dedicated region API initially, not arbitrary heap mprotect: the kernel
must retain allocation identity and PFNs. A fixed virtual-address reservation
alone does not prevent physical aliasing through privileged mapping APIs.
Device-memory authorities and arbitrary physical mapping privileges are also
part of the trusted computing base; do not promise isolation against a caller
that retains unrestricted physical-memory authority.

`Region_Transition` is currently a hosted-tested callback controller, and
`Virtmem.Regions` is only native compile-checked. Neither is reachable through
a syscall. Keep that boundary until all the above paths and authority admission
are integrated and tested. No permission or alias-exclusion proof exists yet.

## Native owned RW/NX allocations

`Process.Owned_Memory` now implements the distinct non-executable allocation
path, exposed as syscalls 115/116. It uses tracked scattered pages, exact-size
release, acknowledged shootdown and actual frame-list detachment. Legacy
physical mapping admission and process teardown consult the retained registry.
See [the API and limitations](owned-anonymous-memory.md). This does not expose
`Virtmem.Regions`, JIT permission transitions or GPU publication.

Native four-CPU QEMU test `owned-syscall-2-serial.log` records
`OWNED-MEMORY-CHECK: PASS` for 64 rounds of adjacent allocations, complete
zero-fill checks, invalid/interior/wrong-size/double-release rejection, and
first-fit hole reuse without corrupting a neighboring allocation. Existing
grant-reclamation and storage checks also pass. This is native regression
evidence, not a proof or a concurrent release stress test. Outstanding-grant
retention and process-exit cleanup need targeted native tests beyond that run.

The subsequent `owned-grant-serial.log` run adds 64 self-grant rounds covering
source release while acquired, same-VA replacement with distinct contents,
retained original alias data during pending revocation and final-return
retirement (`OWNED-GRANT-RETENTION-CHECK: PASS`). This validates the native pin
retention path, not cross-process concurrent release or exit cleanup.

The subsequent `owned-exit-serial.log` run deliberately leaves two owned
allocations behind and observes the reaper retire both records after tracked
frame reclamation. `Forget_Exited` now enforces empty frame inventory before
forgetting records. This is ordinary exit-path regression coverage, not a
cross-process grant/exit race test or proof of global leak freedom.

The musl shim now calls owned allocate/release for private RW mappings; file
copies also use releasable owned storage. Unsupported protections fail rather
than silently leaving writable pages. The initial native libc run passed 128
map/unmap cycles and large/small malloc checks, then failed at thread creation:
musl requests an inaccessible stack mapping and enables its usable subrange,
which needs actual owned protection transitions. The test ignored creation
failure and faulted in pthread_join (0x40a1f8); it now checks creation errors.
Evidence: `owned-libc-serial.log`. This libc rebuild is not ready for general
applications. Guard-page support must be implemented and tested, not disabled
to pass the thread test. Read-only private file mappings also currently fail;
the copied-file test now separately tests RW copies and explicit RO rejection.

Owned protection syscall 117 now accepts NONE/RO/RW subranges, never executable
permissions. Retained-PFN inspection permits validation and release of guard
leaves. Hosted PTE tests cover 32768 cases; the actual adapter's synthetic-table
tests include RO and non-mutating identity inspection. The subsequent native
`owned-protect-libc-serial.log` records LIBC and CXX PASS, including real musl
thread-stack setup, eight-thread mutex/join behavior, release of mixed guard/data
allocations and restored read-only file-copy mappings. Explicit fault probes
are still needed to test inaccessible reads and read-only writes on hardware.

The repeated Mesa context test now passes natively at the original 512MiB:
`owned-mesa-lifecycle-serial.log` records successful clear/triangle/depth tests
for all three contexts and the final lifecycle marker. First-cycle brk growth
is 65536 bytes; cycles 1 and 2 each grow brk by zero. Successful munmap accounting
is cumulative 231325696, 462446592, then 693567488 bytes. These counts are not
peak-memory measurements or a proof that every allocation is reclaimed. They
do establish that this previously failing repeated-context workload now passes
with real release, without increasing guest RAM. Harness session57573 exited0.

The updated `Region_PTE.Plan` SPARK postcondition also proves (6604) that denied
updates preserve the old value, allowed updates preserve bits outside the
change mask, and no writable/executable result is produced. This does not
prove the native adapter, registry concurrency, or hardware TLB semantics.
No hardware Intel reset or rendering result follows from these tests.

The rebuilt desktop cube also passes (`owned-mesa-cube-run.log`, session99118):
nine frames with retired-buffer reuse and 194673 pixels checked by the
independent geometric/texture oracle. This revalidates native software OpenGL
presentation after replacing the libc allocation lifetime; Desktop still uses
CPU composition and this is not Intel hardware acceleration.

The window probe no longer sleeps forever after its nine deterministic frames.
It polls validated Desktop input while retaining the final immutable buffer;
Escape requests surface destruction and Goodbye, then exits. An unavailable
surface ends the client as well. No acquired buffer is freed speculatively.
The headless window test photographs the final frame before injecting Escape
through QEMU's keyboard path, and requires the explicit Escape exit marker.
`mesa-event-loop` (25949) passes all 194673 oracle pixels plus this close path;
the native reaper subsequently reported 1002 owned regions retired. This is
app-readiness work, not yet an Apps-menu/live-image packaging change.
