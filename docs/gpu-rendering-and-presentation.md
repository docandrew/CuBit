# GPU rendering and presentation boundaries

Design decision, 2026-09-27. This is the target architecture, not a declaration
that native Intel rendering, multi-adapter routing or zero-copy is implemented.
It extends [display outputs](display-outputs-and-scaling.md) and
[buffer lifetimes](display-buffer-lifetimes.md), retaining their session,
grant, completion and desktop-layout contracts.

## Two paths, one resource lifecycle

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
