# GPU rendering and presentation boundaries

Design decision, 2026-09-27. This is the target architecture, not a declaration
that native Intel rendering, multi-adapter routing or zero-copy is implemented.
It extends [display outputs](display-outputs-and-scaling.md) and
[buffer lifetimes](display-buffer-lifetimes.md), retaining their session,
grant, completion and desktop-layout contracts.

## Two paths, one resource lifecycle

### Hardware cursor milestone (requested 2026-09-30)

Add an adapter-owned hardware cursor path through Desktop -> display -> driver,
with a software-composited fallback. Desktop retains logical pointer position,
shape/hotspot and per-output scaling policy; display resolves the output and
the driver validates cursor format, dimensions, placement and backing. Apps
must not receive cursor-register or scanout authority. Movement should update
position without uploading the shape again or triggering full-scene rendering.
Shape replacement must retain the old backing until hardware no longer reads it.

Acceptance: correct hotspots/clipping at negative and edge coordinates,
mixed-DPI/output transitions, shape replacement, fallback and driver retirement;
responsive movement while scene rendering is busy. Target a supported 240Hz
display mode, but verify monitor EDID, connector/link bandwidth and hardware
timings first. Measure input-to-visible latency separately from IPC completion
and nominal refresh rate. No hardware-cursor or 240Hz support is claimed yet.

Presentation control follows app -> desktop -> display -> adapter driver.
Rendering does not traverse that whole chain: an authorized application uses
its render context at the GPU service directly. Mesa is an application-side API
implementation, not a privileged display owner. Desktop is another render
client, with separate presentation authority.

An app publishes an authorized image reference and render-completion dependency
to its desktop surface. Desktop chooses composition or direct scanout; display
routes presentation to the output's owning adapter. IPC carries descriptors,
references, damage, dependencies and outcomes, not pixels. The shared data plane
has its own allocation, mapping and retention lifecycle. Batch submissions;
do not turn each graphics API call into an RPC.

| Component | Owns | Does not confer |
| --- | --- | --- |
| App / Mesa | Its surfaces, render contexts and authorized allocations | Modesetting, MMIO, arbitrary DMA, other clients' images |
| Desktop | Composition, placement, primary display and workspace policy | Unrestricted device access |
| Display | Output registry, presentation sessions and configuration arbitration | Authority manufactured from output numbers |
| Adapter driver | Hardware programming, GPU address spaces, engines, synchronization and scanout | Implicit permission for clients to share resources |
| Kernel / device provisioning | Enforced device, memory and IPC authority | Proof that command completion means physical presentation |

Intel, virtio and future adapters implement applicable common contracts and
report features explicitly. Firmware framebuffer supports fixed-mode copy
presentation only, not render engines or vblank fences. Hardware-specific pipes,
power domains and command formats stay in drivers and the relevant Mesa backend,
not the desktop surface protocol.

## Identity and ownership

Distinguish render adapter, allocation backing adapter, presentation output and
persistent monitor preference. Rendering and presentation may use different
adapters. EDID identity is descriptive input for preferences, never authority
or a live service address.

A live output binding names an authenticated driver endpoint incarnation and
driver-local output generation. Sessions retain that binding; driver replacement
or local output-number reuse cannot retarget an old session. Registry numbers
are selectors, not capabilities. Retired endpoint generations cannot be rebound
to outstanding completion tokens.

There is one active scanout-control owner per output. Shared pipes, PLLs and
device-wide constraints are arbitrated by that adapter's driver, validating the
complete affected configuration. Desktop chooses the main display independently
of boot-console ownership and preferred rendering GPU.

Firmware takeover has prepare, commit and retirement obligations. Before commit,
retain the working firmware path while validating the new resources/configuration.
Commit requires excluding the old writer, not merely setting a registry flag.
After takeover, never silently fall back to an obsolete firmware address.
Ambiguous takeover preserves uncertain resource ownership and reports failure.
Cross-adapter changes are not atomic merely because each adapter supports local
atomic changes.

## Image sharing without hidden pixel copies

An image descriptor needs checked dimensions, per-plane offsets/strides/spans,
pixel format, storage layout, color interpretation, backing identity and rights.
A virtual address is not a transferable image reference. Imported handles are
bound to authenticated exporters and lifetimes; serialized device IDs or fence
numbers alone are insufficient.

For each source/destination pair negotiate an explicit path:

1. Direct scanout of a compatible authorized image when desktop policy permits.
2. Same-adapter composition into an acquired presentation buffer.
3. Cross-adapter import with compatible layout, memory access and synchronization.
4. Explicit transfer into a destination-compatible allocation when import is
   impossible. Account for transferred bytes/time, including staging/readback.

Import support is pair-specific, not a global zero-copy Boolean. CPU mappability,
coherency, renderability and scanout compatibility are separate properties. Never
expose device addresses to compensate for unsupported imports. Reuse grants for
shareable RAM; finish derived-loan tracking before forwarding borrowed driver
allocations through display.

Render completion, source-reader release, scanout-buffer release and physical
presentation are distinct events. Render completion does not make a displayed
buffer writable. Dependency waits must not block service dispatch. Client death
or timeout stops new admission but does not prove DMA/scanout quiescence. Retain
or quarantine uncertain backing; resetting one adapter cannot release another
adapter's outstanding import.

Public render contexts require GPU virtual-memory isolation, command privilege
restrictions, bounded accounting and verified reset behavior. An IOMMU alone
does not separate contexts inside a GPU. Trusted test submissions are a bring-up
stage, not permission to accept arbitrary app shaders.

### N100 application-command security gate (2026-09-30)

Intel TGL PRM Vol 2a, revision 12.21, `MI_BATCH_BUFFER_START`, printed
pages 971-972 (PDF pages 989-990), defines DW0 bit 8 as the address-space
indicator: PPGTT batches execute non-privileged; chained/nested batches from
PPGTT remain in PPGTT by hardware enforcement. PPGTT must be enabled in the
context. The existing `Intel_GPU_ADLN_Batch_Start.Build_At` emits this selector;
its postcondition now fixes the entire branch header for every accepted VA.
This is an encoder property, not proof of the hardware or the whole driver.

[Linux's i915 documentation](https://docs.kernel.org/gpu/i915.html#batchbuffer-parsing)
separates privileged commands, register access and privileged-memory access.
Its optional software parser can submit validated batches as secure on hardware
that needs that mechanism. This is NOT a reason to make CuBit application
batches privileged. The same document's workarounds section describes hardware
register whitelisting; inherited whitelist state must not silently become app
authority.

Before opening application admission, audit and establish:

- PPGTT-enabled context state and non-privileged batch entry on the supported
  device; no app control of ring, saved context, page tables or GuC messages.
- Explicit register whitelist configuration/readback and device-specific
  command restrictions, including GGTT memory operations and nested batches.
- Completion storage inaccessible to the app, so writing a mapped user buffer
  cannot forge the driver's retirement/fence evidence.
- Retention of all reachable mappings until genuine completion/quiescence;
  hostile loops, faults and client death need bounded recovery, not early reuse.
- If software validation is introduced, validation and execution must observe
  the same immutable command bytes, including indirect batches. Do not validate
  a writable buffer then execute it with greater privilege.

These are pending admission gates, not implemented guarantees. Current native
application admission remains closed; trusted bootstrap results do not satisfy
them. Mesa continues to generate shaders and rendering commands.

`Intel_GPU_Nonpriv_Registers` now models every FORCE_TO_NONPRIV field using
Ada representation clauses, based on TGL Vol2c-12.21 printed pp988-989
(PDF pp1014-1015). The pure evaluator handles read/write selection, all four
address-range sizes (including ignored low address bits), and deny precedence.
Reserved fields/access encodings and unsupported VF mode invalidate the whole
snapshot. `Unspecified` deliberately does not mean allowed: this evaluator
does not model the hardware's built-in non-privileged register table.

Cross-check: Linux `intel_engine_regs.h` FORCE_TO_NONPRIV definitions agree on
the address/access/range/deny bit positions. The
[inspected Linux source](https://codebrowser.dev/linux/linux/drivers/gpu/drm/i915/gt/intel_engine_regs.h.html)
is revision v6.19-rc8-185-g2687c848e578, not the driver's pinned v6.16 reference.
It limits its list to 12 entries, while the PRM enumerates additional slots;
do not blindly expand MMIO access or claim a complete hardware snapshot from
that discrepancy. Model-specific slot availability still needs resolution.

Hosted `nonpriv_register_tests` passes bit roundtrips, range boundaries,
access combinations, deny ordering and invalid encodings. Focused SPARK checks
prove runtime safety/termination, not semantic equivalence to the GPU. No
native whitelist writes or application-admission changes have been made.

## Current code audit and change order

Display currently uses `CAP_SLOT_GPU = 9` for synchronous calls, acquisition and
asynchronous frame submissions. Virtio registers one `DRIVER_GPU`. The display
wire output selector is service-local (0..15), not persistent identity or
authority. Multiple heads behind that endpoint are supported; independent driver
instance routing is not established.

`Map_Backbuffer` remains unavailable. Compositor paint/transfer storage and
firmware/virtio presentation include copies. Existing session/frame validation
and retained grants must survive optimization. Native Intel has a private
read-only bootstrap/mapping experiment, not a presentation or rendering backend.

Implementation order:

1. Continue Intel power-safe register snapshot and ownership inventory, retaining
   firmware scanout. A successful mapping does not register a ready output.
2. Replace singleton routing with per-adapter endpoint bindings. Use the same
   binding for acquisition, submission and completion validation. Distinguish
   driver-local output numbers from broker registry numbers.
3. Implement exclusive native handoff and backend-owned presentation buffers;
   integrate derived loans before exposing direct CPU compositor access.
4. Add trusted offscreen Intel execution, GPU memory and dependencies; connect
   completed images to the existing presentation lifecycle.
5. Complete untrusted-context isolation and Mesa OS/WSI integration. Introduce
   cross-adapter imports with tested compatibility and lifetime contracts.

Acceptance tests must include two drivers with identical local output numbers,
one driver's death/restart while the other presents, stale completions, failed
imports, delayed readers, revocation during rendering and reset while another
adapter holds an image. Measure copies/bytes separately from IPC latency, render
completion and presentation timing. QEMU can exercise routing/lifetime failures,
not establish Intel MMIO or physical scanout correctness.

## Desktop import audit (2026-09-30)

The current implementation cannot safely present an Intel-owned BO merely by
passing its grant reference through the application's existing attachment call:

- `userspace/services/desktop/main.adb`, `OP_SURFACE_ATTACH_BUFFER`, acquires
  the grant with the authenticated application sender as its expected owner.
  Intel BO backing is driver-owned. Forwarding its numeric reference does not
  make the application its owner, and accepting a claimed owner PID would
  weaken this boundary. Preserve the existing check.
- Successful attachment immediately schedules a redraw, and composition reads
  the attached memory directly. An attachment must therefore already be safe
  to read; a later render-completion message is too late for this operation.
- `OP_SURFACE_PRESENT` validates ownership/damage and schedules redraw work.
  Its synchronous success reply does not say that composition has finished
  reading the source. It must not be used as a GPU-buffer reuse fence.
- The existing attachment is fixed linear BGRA8888 geometry. It has no storage
  modifier, render-completion dependency or per-frame reader-release token.
  Importing a tiled/compressed ANV image under that descriptor is invalid even
  if the byte span fits. CPU mappability does not imply linear image layout.

The next presentation implementation must resolve both authority and lifetime:
an authenticated driver-to-Desktop export associated with the application's
surface, or an attenuated derived loan whose original owner stays pinned; then
an explicit ready/read/release lifecycle. The display service's downstream
frame completion is not automatically an application source-reader release.
Existing CPU attachment/present semantics must remain distinct from this new
GPU image path. No import endpoint, owner-PID override or zero-copy guarantee
was added by this audit.

For an initial visible hardware probe, a deliberate copy of completed linear
pixels into an application-owned surface is a valid diagnostic option, provided
it is reported as readback/copy presentation, not the final zero-copy WSI path.
It does not replace the import/lifetime work needed for normal Mesa rendering.

### Explicit presentation mapping (2026-09-30)

Intel application-BO mapping label `0A23` now has operation 3:
`Map_Presentation`. It uses the same authenticated session, BO ownership,
endpoint-incarnation and whole-page bounds checks as ordinary mappings, but
creates an owner-opted-in, read-only forwarding root. Operations 0/1 remain
nonforwardable; operation 2 retires a mapping. Writable presentation requests
are rejected. Internal firmware/context/page-table allocations are not exposed
through this application-BO registry.

The Mesa C/Ada bridge exposes `cubit_intel_map_presentation` separately from
`cubit_intel_map_buffer`; passing 3 as the latter's writable flag is rejected.
An app must acquire the root and derive a read-only terminal child to an
authorized recipient before attaching it. The child is logically owned by the
app, preserving Desktop's sender/owner check, while the root retains backing.
Forwarding is not restricted to Desktop by the kernel: the app must possess a
recipient endpoint capability. No internal driver memory becomes forwardable.

Hosted tests cover opt-in, read-only enforcement, default nonforwardability,
authentication and wire validation. Native driver/bridge compilation is a
separate check. This does not yet connect Mesa WSI to Desktop, advertise a
hardware Vulkan device, authorize application submissions or establish GPU
completion. The ready/read/release and linear-layout requirements above remain.

`Native_GPU_Presentation` now provides a C-callable connector for that CPU-visible
linear path: forward an acquired root to the Desktop endpoint, attach the child,
then revoke/poll its retirement after replacement or destruction. It accepts no
owner PID override, does not copy pixels, and preserves caller ownership of the
child even on a rejected or uncertain attach. A malformed reply is distinguished
from a valid Desktop rejection. Attachment/present success does not retire the
child or allow reuse. The existing compositor still copies into its own output;
this is not end-to-end zero-copy scanout.

This connector is not yet wired into Mesa WSI. The `grant-forward-desktop`
native fixture uses a synthetic RAM owner with real Desktop/display services;
it cannot validate Intel output formats or GPU synchronization.

### Native application submission plumbing (2026-09-30)

The Intel service now dispatches label `0A27` to a synchronous, per-session
submission coordinator. Request words are `[1 | (BO byte offset << 32), BO
handle, raw48 GPU batch address, byte length]`; the four-word reply is
`[status, 1, completed sequence, 0]`. Completion is zero on failure. This offset
is in **bytes**, unlike the page offset in offline binding label `0A24`.

Only an authenticated session can select its retained context. The native
callbacks resolve the entire batch slice against that session's sealed VM and
BO registry, arm a fresh protected marker observation, enable scheduling,
append the driver-constructed nonprivileged PPGTT branch, notify GuC, wait for
the marker, and wait for scheduling-disable acknowledgement. Each session has
its own retained ring channel and monotonically increasing completion state.
Initialization requires the driver's observed setup marker and disable
acknowledgement, not a client assertion. Failure after execution starts closes
the session and retains backing; a lost successful reply also retires the
session rather than replaying work. Ring capacity is finite; wrap/reuse is not
implemented.

**Application admission remains closed (`Ready=False`).** This wiring is not
an enabled Mesa backend, a command-parser/security proof, or hardware validation
of application submission. BO extent validation does not constrain what a GPU
batch can subsequently fetch or do. The command-privilege, protected-memory,
hostile-work recovery, live-VM update and resource-lifetime requirements above
must still be met before enabling application execution. The current NUC image
is unchanged; native compilation and component tests are separate evidence
from actual Intel execution.

The C/Ada buffer bridge exposes `cubit_intel_submit_batch` for this protocol.
The caller supplies its previous completion (initially 1 after setup); only an
exact successor in a canonical successful reply is accepted. The previous
value is a local reply check, not a client-controlled server sequence. Output
clears on every failure, and the call never retries. Status 4 denotes local or
transport/protocol failure; once dispatched it may mean work executed, so the
caller must retire the session rather than replay. Hosted wire/C-ABI tests
cover malformed replies, stale/overflowing sequences, extent rejection and
single-call behavior. Native-runtime compilation is checked separately; no
Mesa queue/backend selection or hardware execution follows from these tests.
