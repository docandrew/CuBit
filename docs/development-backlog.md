# CuBit Development Backlog

This is the short operational backlog for user-visible defects and engineering
work that does not belong in the security-hardening ledger. Design requirements
remain in their subsystem documents.

## Authority delegation

- [ ] Audit and simplify devmgr/procmgr bootstrap authority and delegation.
  Separate service use from grant-making powers, constrain steady-state brokers,
  and make each grant chain understandable and inspectable. Track acceptance
  criteria in [SEC-020](security-hardening.md#sec-020--simplify-and-constrain-bootstrap-authority-delegation).

## Code organization

### Qualified CCL names and debugger outcomes

- [ ] Give qualified names (`Type.Alternative`, service operations) room for
  their separately bounded components. The parser currently applies the same
  32-character bound to the whole token as to a single type name. Add boundary
  and BASIC/Lisp round-trip tests; do not silently truncate discovered names.
- [x] Use shared readable VM/parser status labels in Workbench rather than
  enumeration images from the minimal Ada runtime. Hosted diagnostics pass;
  native rebuild/visual verification pending the shared build handoff.
- [ ] Expand aggregate result/locals inspection beyond `<native object>`.

### Separate the CuBit runtime library from GNAT internals

- [ ] Move public CuBit runtime APIs and reusable IPC machinery out of
  `userspace/runtime/gnat` into a dedicated common library (for example,
  `userspace/lib/cubit`; final location to be decided during the inventory).
  Keep GNAT implementation units and Ada runtime support in the GNAT tree.
- [ ] Inventory dependencies first: distinguish portable protocol/type/lifetime
  logic, native syscall adapters, service-specific clients, and compiler runtime
  internals. Build on existing subsystem libraries rather than duplicating them.
  Shared machinery such as `CuBit.Async_Requests` must remain usable outside CCL
  and Config, with no added payload copies or authority semantics.
- [ ] Give the common library explicit project/build boundaries; update native
  linking, source lists, hosted tests and SPARK projects so portable code can be
  tested and proved without pulling in GNAT implementation units.
- [ ] Validate the move with focused proofs/regressions, native application
  builds and QEMU smoke tests. Remove superseded paths rather than retaining
  compatibility copies. This is an organizational refactor, not an IPC redesign.

## Nonblocking GPU follow-up

- **2026-09-22 progress:** bounded pre-paint input dispatch passed an A/B/B/A
  native loaded comparison (repainting p99 <=0.277 ms versus <=1.381 ms; not
  photons). Mixed 1024x768 + 1280x720 output pixels, pointer confinement and
  cleanup are native-tested. Shared display models moved to `userspace/lib/display`.
  Preferred base EDID parsing/storage sizing and pointer containment are SPARK
  checked. Settings exposes a read-only live Displays page.
- Next: generation-bound surface logical/raster size and scale acknowledgement,
  toolkit relayout/rasterization, then native mixed-DPI crossing regressions.
  Rotation/scale controls must wait for an implemented renderer, not merely the
  already-proved geometry math. See the next-boundary section of the display doc.
- Broker startup handoff now permits different native/firmware dimensions and
  never falls back to firmware after GPU failure; native changed-resolution and
  two-output clear-failure regressions cover it. **Kernel console retirement is
  still required for full hardware takeover:** MAPFB suppresses normal mirroring
  but the last-chance handler can re-enable an obsolete framebuffer. Define an
  irreversible, authorized retirement with writer quiescence and panic policy;
  do not merely null a callback concurrently with an executing writer.
  GTK rewrites both display hints and its synthesized EDID; persist desired CCL
  mode policy separately from transient host observations.
- Enumerate EDID/DisplayID modes with bounded parsing, intersect with hardware
  limits, and report advertised versus active versus measured refresh explicitly.

Session presentation now uses asynchronous broker submissions and IRQ-driven,
fenced per-head GPU commands. The native delayed-head test checks independent
progress and busy-output lifetime protection. Before calling this a speed win:

- Measure and reduce async dispatch/wakeup overhead. The first loaded one-vCPU
  repaint run moved p99 from <=1.381 ms to <=1.933 ms; fewer >=1 ms samples do
  not cancel out the worse tail. Preserve fences and source-buffer ownership.
- Add native malformed-fence, missing-completion/timeout and backend-death
  injection tests. Quarantine is implemented; those failure paths are not yet
  covered by the new delayed-head test. Adapter reset/recovery remains separate.
- Migrate remaining synchronous broker configuration and shell presentation
  operations; mixed active modes/DPI remain the next display feature milestone.
- Make serial/debug records atomic or buffered across CPUs. A GTK smoke test
  reached the Desktop but missed its readiness marker because simultaneous
  startup strings interleaved. The redundant per-head announcement was reduced,
  but this is not a fix for the general logging problem.

See [the protocol boundary](display-outputs-and-scaling.md#nonblocking-session-presentation)
and [measurements](../tests/performance/graphics-results.md#nonblocking-brokergpu-session-presentation).

## Userspace allocation

The portable SPARK allocator has a proved bounded, single-owner hosted pilot
and repeatable comparisons against glibc, mimalloc, jemalloc and gperftools
TCMalloc. It is **not** wired into native Rust or GNAT yet. Follow
[the allocator roadmap](userspace-allocator.md): dynamic slab assignment with
empty-only recycling is implemented/proved; next are finer classes and search
costs, backing extent lifecycle, complete runtime allocation semantics, then
explicit remote-free/lifetime handling. Track throughput, tail samples, rounding
waste and retained pages separately; do not promote a microbenchmark win into a
general performance or memory-safety claim. See the
[dated measurements](userspace-allocator-results.md).

## Package metadata

### Live CD local ROM selection omits expected cartridges

Status: reported on the laptop; investigate later.

Additional Game Boy ROMs expected from the local ROM directory were absent from
the ISO. Check the selected image profile, local-input filtering/staging, and
cartridge audit against the actual directory contents. Distinguish files absent
from the ISO from files present but not exposed by SameBoy's current launcher.
Keep ROMs local and explicitly opted in; do not commit or distribute them.

### Harden complete executable identity-section validation

Status: metadata corrected; loader hardening pending

The old Devices C manifest declared a 19-byte identity value for
`com.cubit.devices`, which is 17 bytes. CCL generation now fixes the length;
regression tests assert this exact one-byte correction independently of the
unchanged authority metadata. network-check also gains a complete identity and
ccl-control gains a version. The original C bytes are retained as test fixtures.

`procmgr.parseIdSection` returns as soon as it finds `id`, without validating
the remaining TLVs or rejecting duplicate keys. In the Devices case it could
accept the two following version-header bytes as part of the identity. Harden
this parser to validate the entire section before publishing identity; add
truncated, duplicate, malformed-length, empty-value, and trailing-data loader
tests. Compiler validation does not make arbitrary ELF inputs trustworthy.

All 28 remaining Ada manifests (plus the previously migrated ccl-vm) now use
CCL, with exact authority-section comparisons and reviewed identity updates.
Checked CCL profiles now control normal/fallback initrd contents and USB optical
image composition. Next: source build dependencies and development ext2 payload
plans, keeping binary requests separate from image contents and launch approval.
See [CCL package design](ccl-packages.md).

## Boot and driver discovery

### Scannable physical-boot diagnostic capsule

- [ ] Add an optional, fixed-size QR code to the bootstrap diagnostic panel.
  It must encode a compact, versioned diagnostic capsule—not raw scrolling log
  text—containing the image/build identity, boot stage, first fatal code (if
  any), architecture/firmware facts that are already displayed, and a checksum.
  Keep the payload bounded and privacy-safe: no secrets, full memory map,
  certificate material, network configuration, device serial numbers, or raw
  addresses. Render it without allocation or a general image library, and keep
  the existing text panel readable at 1024x768. The code is a convenience for
  photographing/scanning a headless machine, never the sole diagnostic record;
  serial and the future authority-gated retained boot log remain authoritative.
  Add pure encoding/error-correction tests, hostile payload fixtures, and a
  native screenshot/decode regression before enabling it by default.

### Boot-map admission and userspace ACPI

The Multiboot-v1 decoder now has a bounded pure SPARK core (72 checks discharged)
and 75,085 hosted cases. The kernel snapshots variable-sized records into static
workspace and normalizes once for both allocators. Initial boot-entry numeric
admission/sanitization now adds 11 discharged checks and 169,416 hosted cases,
including the actual assembly gate. Boot module metadata now uses a sealed
catalog; payload pages are validated, disjoint, padding-sanitized and retained
for the lifetime of the boot. Its pure core discharges 62 checks and passes
1,000,460 hosted cases; real-GRUB fixtures reject malformed declarations before
allocator startup. See [module lifetime evidence](../tests/boot-modules/README.md).
Boot framebuffer admission now adds a pure 44-check core and 17,427 hosted cases.
One descriptor feeds reservation, renderer setup, sysinfo and MAPFB. The adapter
normalizes GRUB's aligned RGB union, validates page-rounded exclusions, and maps
framebuffer pages separately from RAM. Hardware backing/cache correctness remains
a trusted/tested boundary, not a proof of firmware truth. See
[framebuffer evidence](../tests/boot-framebuffer/README.md).
Next display slice: generation-bound output objects and explicit native takeover,
then shared mixed-DPI/orientation geometry. Multi-adapter and future Vulkan/3D
boundaries are captured in [display architecture](display-outputs-and-scaling.md).
See [entry evidence](../tests/multiboot-entry/README.md), [proof boundaries](allocator-verification.md)
and [decoder evidence](../tests/multiboot-memory-map/README.md).

Keep future AML interpretation outside the kernel. Move ongoing ACPI discovery,
events and device/power policy to userspace with scoped native device authority;
retain only necessary early bootstrap data and hardware enforcement in-kernel.
Firmware declarations cannot mint access. Implement bounded table parsing before
AML; do not expand the current allocator work into an interpreter project.
See [ACPI userspace plan and remaining decisions](acpi-userspace.md).

### Panic diagnostics must not assume frame-pointer chains

Status: unsafe walk removed; native local-halt regression passes.

The former `Last_Chance_Handler.printCallStack` followed RBP as a linked frame
chain even though optimized kernel builds do not guarantee frame pointers. A
rejected duplicate module logged its admission error, then the diagnostic walk
interpreted non-frame data as an address and could fault again. The handler now disables local
interrupts before output, preserves the original diagnostic, explicitly reports
that no stack trace is available, and loops on HLT. It no longer calls `x86.panic`
(which raises another software interrupt/exception). Optimized builds remain
enabled without runtime assertions. The hardware/output adapter is explicitly
SPARK Off, not a purportedly proved unwinder.

The three real-GRUB rejection fixtures check the expected diagnostic before
allocator admission, a single panic banner, unchanged serial output, and two QMP
observations of CPU 0 halted with IF clear. Remaining work: a supported bounded
unwinder, coordinated SMP panic shutdown, and stronger emergency-output isolation.
This is only a local CPU stop; valid runtime message pointers, a usable stack,
and working diagnostic output remain trusted. It does not contain NMIs or prove
all possible panic causes safe.

### Do not start HDA without a usable controller grant

Status: observed during CCL image regression; native fix pending

A Q35 fallback boot without an HDA PCI device starts hda.drv anyway. The driver
reports a denied device mapping (no CAP_DEVICE_MEM), faults on its MMIO address,
and boot progression stalls. The CCL image migration preserves existing image
membership; this failure is in hardware discovery/startup, not CCL evaluation.
The fallback image test now supplies the HDA device used by run-laptop.

Gate driver startup on a successfully discovered and provisioned device, handle
failed mappings explicitly, and ensure a failed optional audio driver cannot
block the rest of boot. Add absent-controller and driver-failure regressions.
Do not fix this by granting an unprobed driver broader device-memory authority.

## Audio controls

SameBoy now plays through the native mixer and has app-local mute/volume keys.
Next: a separately authorized master-volume interface, desktop volume widget,
multimedia key routing, click-free gain ramps and Config persistence. Ordinary
audio playback authority must not allow changing another application's volume.
See [audio volume-control boundaries and follow-up](audio-volume-control.md).

## Desktop defects

### Desktop close lifecycle and rejected-request visibility

The close-NetSurf/launch-DOOM freeze exposed kernel receive starvation; the
shared request receive paths now rotate fairly between blocked senders and
queued work. See [incident and regression evidence](ipc-receive-fairness.md).

Follow-up: add typed close requests and handle terminal surface errors in
clients, replacing surface deletion followed by a best-effort process kill.
Keep force-termination authority separately scoped; do not grant blanket
process-write authority to the compositor. Count and attribute rejected IPC
alongside accepted traffic, with bounded reporting, and test endpoint budgets
against sustained abusive clients.

### UI-010 — Make input delivery recoverable under loss

Status: in progress

Persistent IRQ doorbells, stable per-surface queues, explicit snapshot recovery,
and deferred-capability input waits are implemented. Typed source reports now
carry capability-stamped identity, device generation, sequence, delivery class,
state snapshot, and explicit resynchronization. Desktop keeps independent
source state and deliberately merges pointer buttons. Event-driven toolkit apps
no longer poll or sleep between input deliveries, and one stalled surface cannot
evict another surface's transitions.

The remaining architectural boundary is driver-to-input-router publication,
which still uses the transitional bounded `sendEvent` lane directly to desktop
rather than a typed `input.svc` handle. Replace it with transport-bound device
class identity and a bounded lossless transition strategy, then enforce latency
admission in the scheduler. Test stuck-button, stuck-modifier,
lost-capture, multiple-device, slow-client, queue-saturation, device-reset, and
mailbox-saturation cases.

### UI-001 — Physical-laptop touchpad movement is severely degraded

Status: in progress

The laptop's internal touchpad responds through the PS/2 service. Removing
synchronous vblank waits from cursor presentation produced a massive physical
improvement and made it mostly usable. A USB mouse is recognized without a
reboot but improved only slightly and remains impractical, isolating a second
problem in the xHCI report-delivery path rather than cursor coordinate scaling.

The first correction gives software-cursor damage an explicit immediate-present
operation so it cannot synchronously wait for legacy VGA vertical blank on every
input packet. The desktop now blocks on its mixed IPC mailbox while idle instead
of polling with a two-millisecond sleep, and reports its kernel event-ring loss
as `event_drop=` in periodic diagnostics. The xHCI driver now keeps eight
distinct interrupt transfers queued, replenishes before publishing reports, and
uses a dedicated MSI or MSI-X vector instead of its old one-transfer-at-a-time
millisecond polling loop. Its bounded polling fallback retains the queued
transfers when neither message-signaled mode is available. QEMU completed more
than a full transfer-ring wrap with MSI-X active and desktop `event_drop=0`.
The physical retest still showed badly jerky USB motion and no obvious clicks,
while the touchpad remained good.

The first Devices UI exposed a separate client-side latency trap: it marked its
entire 900-by-580 surface dirty for every pointer-motion event. The shared
application loop now owns pointer hover, capture, pressed/released state, and
control damage by default. Motion that remains inside one control performs no
client paint or present; hover transitions invalidate only the old and new
controls, and a drag invalidates the captured control's declared damage. This
is now inherited by Devices and future native apps
without application-specific motion handlers. Pointer-position-sensitive
canvases must explicitly request repaint-on-motion. Consuming input must not
imply repainting a surface.

The CCL Workbench had a separate custom-adapter regression: it rendered and
copied its complete canvas on every uncaptured pointer report, then slept for
ten milliseconds before polling again. It now blocks on the toolkit's
deferred-reply input wait, performs no repaint while the pointer stays within
one semantic region, and submits only status/splitter damage on hover
transitions. The deterministic input-stream gate crosses the rich editor with
128 reports and rejects a return to per-motion client-surface presentation.

Software-cursor presentation is now coalesced to a bounded four-millisecond
cadence so a high-rate USB mouse cannot force one synchronous display IPC per
report. xhci.drv also exposes low-rate aggregate report/error/button/raw-byte
diagnostics through devmgr.svc. The authority-scoped Devices app presents the
bounded snapshot alongside the PCI inventory without granting raw MMIO, IRQ, or
DMA access. Repeat the physical test with this image, then use those counters to
distinguish report-layout errors from load before adding end-to-end latency
histograms.

Add bounded diagnostics for controller/device identity, negotiated packet
length, synchronization drops, overflow packets, bytes attributed to the
keyboard versus auxiliary device, and implausible deltas. Use those results to
identify standard PS/2, Synaptics, ALPS, or another extended protocol before
changing acceleration or silently discarding broad classes of input.

Replace the transitional global keyboard/mouse driver registry with an explicit
desktop-session input route. A legacy shell now declines raw input registration
when desktop.svc is present, but the check/register sequence is not a final
authority-safe ownership protocol.

The laptop's stretched 1024x768 framebuffer is a separate display-mode defect.
Native mode negotiation may affect apparent horizontal speed but cannot explain
intermittent motion or event loss.

### UI-002 — Only one desktop application can be opened

Status: open

After one application is launched, attempts to open another application do not
produce a second usable application window. Reproduce through both the launch
menu and process-manager path, then inspect spawn completion, surface creation,
window ownership, focus/z-order, task buttons, and any singleton state in the
desktop service.

## Device management

### DEV-001 — Grow Devices into unified device administration

Status: in progress

The first Devices slice is an inspection-only application backed by devmgr.svc.
It provides a reusable keyboard- and mouse-navigable tree view, bounded PCI
inventory, driver ownership/state, and live aggregate xHCI diagnostics. Extend
the inventory with USB topology and descriptors, interrupt and DMA resources,
driver provenance, failures, and bounded event history.

Administrative operations such as reset, disable, rebind, or policy changes
must not be added to the inspection endpoint. Define a separate typed authority
for each mutation category, mint it only to the approved device-management
role, require an explicit confirmation surface where appropriate, and record
WHAT, WHO, WHEN, WHERE, and WHY through the security event path.

## Kernel / userspace boundary

### Console output is synchronous serial I/O

Every kernel print and user `debugPrint` writes the UART one byte at a time
(an `out` per byte: a VM exit under KVM, about 87 µs per byte at 115200 baud
on hardware). The debug-write syscall does this with interrupts masked, and
since 2026-09-24 a console lock (`TextIO`) serializes CPUs so lines stay
whole, which makes contention visible.

Direction:
- prints copy whole lines into per-CPU, lock-free memory rings;
- a low-priority drainer writes the rings to serial and counts drops instead
  of stalling callers;
- applications log through logstore rather than `debugPrint`;
- serial stays for early boot, panics (direct, unlocked) and test markers.

### Physical allocator functional verification

Status: bitmap, block-head transitions, local split/coalesce geometry and
constant-time link-update primitives proved and integrated.

The actual buddy allocator now uses the SPARK bitmap layout core; inclusive
maximum-frame sizing no longer aliases the next order's bits. Out-of-band
block-head states now validate exact-order releases and list removals, separate
boot admission from runtime free, and preserve pin-aware order-zero retirement.
The new physical-span core proves exact split/coalesce coverage and round trips;
production child/parent address construction uses it. The architecture-neutral
intrusive splice generic now proves exact link writes and count arithmetic
(44 checks across integer/address instantiations); the kernel retains its O(1)
lists with no extra metadata or out-of-line helper calls. The separate free-set
tree remains an experiment, not an allocator speedup. A Ghost sequence/rank
witness now proves single-order membership/count correspondence with the real
ledger/splice primitives (213 combined checks, none unproved), without runtime
bookkeeping. Next establish its physical address/field mapping and boot base
case, arena-wide partition preservation and allocation
non-overlap. Pins, ownership and SMP
integration remain separate end-to-end obligations. See the
[allocator verification plan](allocator-verification.md) and
[bitmap evidence](../tests/buddy-bitmap/README.md) and
[block-state evidence](../tests/buddy-blocks/README.md) and
[physical-span evidence](../tests/buddy-geometry/README.md) and
[intrusive-splice evidence and proof limits](../tests/intrusive-list-splices/README.md).

Boot-boundary review fixed two concrete errors: equality with the inclusive
boot high-water mark no longer admits a boot-owned frame, and the boot bitmap
limit is now its last represented PFN rather than its bit count. Sentinel counts
are explicitly initialized; a block spanning the boot range no longer queries
out-of-range bitmap entries. The shared arithmetic/admission core passes 13
checks, plus exhaustive small-arena and full-width edge tests. See
[boot-admission evidence](../tests/buddy-boot-admission/README.md). Firmware
alignment and conflicting-region handling still need proof. The subsequent
reservation ADT removes the redundant free counter (see below).

Metadata byte/page sizing and descriptor address arithmetic now use a shared
SPARK core: 20 checks prove slot bounds/separation, page coverage and numeric
address nonwrap under a valid span premise. Its descriptor lookup retains the
existing code size/indexed LEA. Physical reservation ownership and exclusion of
metadata/sentinels from payload remain open. The pre-existing division in the
geometry admission path is a future measured optimization candidate. See
[metadata evidence](../tests/buddy-metadata/README.md).

The actual boot bitmap/reservation/high-water state now lives in a private SPARK
ADT. Its focused target passes 27 checks, proving successful span ownership,
unchanged state on failure, disjoint successive reservations and coverage by the
bitmap-checked buddy handoff. Idempotent admission and on-demand diagnostic
counts replace unsafe redundant accounting; the unused boot release API is gone.
Hosted tests pass 206,022 requests, including independent first-fit/count checks.
The packed-map reservation code is a leaf with no runtime proof baggage, and
setup scans only its own arena. Firmware partial pages/conflicting regions and
physical mapping/metadata/sentinel exclusion remain open integration work. See
[boot reservation evidence](../tests/boot-frame-allocator/README.md).

Firmware admission now uses one shared pure policy in both allocators: inward
usable-page rounding, outward reserved-page exclusion, map-order-independent
reserved precedence and unique ownership across duplicate usable entries.
Its 51 checks all prove; byte-oracle tests cover 57,346 ranges, 2,008 maps and
401,354 candidate blocks. Buddy setup tiles edge blocks instead of dropping
aligned/trailing capacity. Empty firmware entries are initialized, numeric
intervals are validated before endpoint arithmetic, and framebuffer extent
arithmetic is bounded. The raw Multiboot buffer/count/variable-entry parser and
direct-map overlap/cache-mode handling still need review, as do the full physical
mapping/free-list refinement and boot-module reservation argument. See
[firmware admission evidence](../tests/firmware-frames/README.md).

### Heap growth must fail locally, not panic the kernel

Status: partial hardening; whole-request admission and fallible runtime page acquisition.

Heap growth now rejects wrapping, oversized and quota-exceeding requests before
allocation, with a proved pure planner and four-vCPU native regression. Runtime
heap/stack page acquisition now reports resource failures; slab exhaustion
releases its lock, and frame-list insertion/removal preserves accounting.
Initial stack/ELF/process construction now returns failures with unpublished
rollback; bounded ELF metadata is snapshotted before validation and use. Native
SPAWN now copies ELF/name sources through a checked page walker and retained
physical frames; pure copy/walk/name helpers have tests and focused proofs.
Native adversarial exhaustion/retirement and authorized bad-pointer tests,
other syscall source-buffer validation, stack-reservation
policy and cross-process/kernel-guard mapping serialization remain open.
The entire allocator is not yet contained.
See [evidence and admission/cleanup work](kernel-heap-admission-issue.md).

### Signed executable admission

Status: local SPARKTLSCrypto API inspection and design; not yet enforced.

Pin and test a minimal verifier, define the signed envelope and trusted-key
policy, then bind approval to immutable bytes actually passed to the loader.
Keep signatures separate from capability grants and cover boot-path bypasses,
tampering, substitution, development exceptions and rollback/key rotation.
See [signed executable admission](signed-executable-admission.md).

### KERN-001 — Move hardware service work out of the kernel

Status: planned — follow-up audit, not an immediate migration

Keep scheduling and low-level memory allocation/virtual-memory enforcement in
the kernel. Review the remaining hardware code by responsibility, rather than
moving entire packages merely to reduce the kernel's line count.

Initial candidates:

- Video: audit `kernel/src/video*`, framebuffer console rendering/scrolling,
  and boot-time display setup. Move ongoing rendering, device-specific display
  work, and mode-setting policy into the existing userspace display/driver
  architecture. Retain only the bootstrap/emergency-output mechanism actually
  needed before those services are available or after they fail.
- ACPI: separate the minimal boot-time topology/interrupt/timer information the
  kernel needs from ongoing discovery, firmware interpretation, and power/device
  management that can live in an explicitly authorized userspace service.
- PCI and bus mastering: separate enumeration, device configuration, driver
  assignment, and DMA lifecycle orchestration from the privileged enforcement
  of device ownership, MMIO/config-space access, interrupts, and DMA mappings.
  Move service policy/mechanism into userspace where safe; do not replace
  capability checks with unrestricted PCI configuration writes.

The existing authority/endpoint/handle model remains the security boundary.
Userspace placement alone does **not** isolate a bus-mastering device's DMA.
Document the trust assumptions on platforms without an IOMMU and define how
bus-master enablement, buffer pinning, device quiescence, driver death/restart,
and eventual IOMMU protection interact. DMA must be stopped or contained before
device-visible memory can be reclaimed or assigned to another process.

Deliver an ownership/dependency map with links to current call sites, an
explicit kernel trusted/proof boundary, and a staged migration order. Preserve
boot/recovery output, existing IPC authority checks, and low-latency input,
display, storage, and audio paths. Gate migrations with QEMU and physical-laptop
boot tests, unauthorized-device-access tests, and driver-failure/lifetime tests.
Do not stall the current CCL REPL/widget milestones on this audit.

## Shared UI toolkit

### CCL source-view proof and remaining surface integration

`CCL.Language.Views` supplies bounded Lisp/BASIC conversion and formatting for
the current expression language. Both surfaces pass the existing analyzer;
tests cover canonical round trips, formatted-source idempotence, node ranges,
and preservation of paused Workbench VM inspection. The new core contains no
`Assume` or SPARK-Off sections. A printer scratch-buffer alias reported by
SPARK was removed by separating the compact-output spans from the read-only
layout spans. GNATprove still aborts internally (`Assert_Failure
sem_util.adb:7554`, during expansion of the bounded append helper); no complete
proof result is available. Minimize/report that tool failure and resume proofs,
without suppressing checks or substituting assumptions. Reproducer:
`tests/ccl-views/README.md`.

Next surface tasks: attach comments to syntax nodes rather than gathering them
above the expression, integrate BASIC with REPL completion/history and other
CCL entry points, and design multi-binding syntax without changing
scope/evaluation semantics. Infix arithmetic/equality and expression-valued
`IF … THEN … ELSE … END` are now implemented and regression-tested for
precedence, unchanged grouping/overflow, short-circuit branches, host admission,
and exactly-once left-to-right invocation. The existing `let` reader still admits
one binding. The Workbench can save/reopen BASIC using its syntax header;
this does not mean other CCL consumers already accept that surface.

### CCL functions: remaining runtime work

The typed-stream prerequisite now has a portable delivery-policy model:
`CuBit.Protocols.Stream_Policies`, with hosted boundary/matrix tests and focused
SPARK compatibility proofs. Still needed: versioned descriptor/wire encoding,
handshake validation before grants, runtime backpressure/close enforcement,
ownership and cancellation integration, and typed CCL stream combinators.
Do not treat schema-only subscription as enforcing the new delivery policy.
See [typed IPC](typed-ipc.md) and `tests/stream-policies`.

Named typed functions now run in the shared interpreter/REPL/Watch and round-trip
between Lisp and BASIC. The initial scope is non-recursive and non-capturing,
with 16 functions, 8 parameters, exact parameter/result types and unchanged
authority admission. Tests cover hostile edits, isolated frames, bounded depth,
fuel, copied text results and function-driven labels. SPARK flow analysis passes
105 initialization/termination checks; this is not a complete runtime-error or
functional proof. No assumptions or SPARK-Off escape hatches were added.

Runtime-owned handler references now support the explicit `() -> Boolean`
profile, owned checked-code snapshots and current-grant identity checks.
The bounded callback queue has generation/lifetime handling, explicit discards
and a focused SPARK proof; the dispatcher is serialized and synchronous.
CCL-visible `(handler name)` values, typed registration, and one real shared
Workbench button are now implemented, with headless lifecycle/authority tests
and rendered Linux-preview tests. See `tests/ccl-callbacks` and the
`button-clock.ccl` sample. The owned source/name transport is checked once at
registration; each click executes retained checked code. Next: connect this UI
owner/client model to native Desktop widget IPC and independent surfaces.
Also outstanding: CCLB call frames and
debug metadata, full function-signature/intellisense support, overloads,
ownership-aware captures, and reclaiming call-local text while preserving
returned values. Current text is invocation-owned and budgeted, not reclaimed
on each return. Definitions do not persist between REPL submissions.

### CCL session diagnostic formatter proof boundary

GNATprove on `ccl-sessions.adb` reports three unproved bounds for diagnostic
string concatenations in `Result_Image`. The message functions return
unconstrained String, so their finite message sizes are not available at this
call boundary. Use a bounded representation or another demonstrably provable
design; do not suppress checks or add assumptions. New admitted REPL submissions
and scoped numeric label hooks have hosted regressions, but this formatter and
the host adapter are not an end-to-end proof of session safety.

### Shared image decoding and a read-only image viewer

Status: deferred; return to CCL work first.

Evaluate an existing Ada image-decoding library for reusable desktop image
support, then build a small native viewer using the shared toolkit and file
picker. Confirm licensing, supported formats, freestanding-runtime dependencies,
memory requirements, and actual SPARK coverage before selecting a library.

Use native filesystem messages and scoped read-only handles for selected files
or an approved pictures folder; no write authority or unrestricted filesystem
access. Keep byte acquisition separate from decoding so Linux-hosted tests can
use the same decoder without introducing host file APIs into CuBit applications.
Image parsing stays in userspace, outside the desktop compositor; assess a
separate restricted decoder process if the chosen implementation warrants it.

Treat images as untrusted: validate dimensions and size arithmetic, bound decoded
memory and work, and test truncated/malformed images and decompression bombs.
Reuse aspect-preserving Fill/Fit/Center rendering and clipped damage handling.
Later this can support a wallpaper file picker and CCL image widgets. Keep the
current embedded wallpaper path as the safe startup fallback.

Package bundled wallpapers as separate read-only files on the Live CD rather
than only embedding rasters in `desktop.svc`. Expose a narrowly authorized
wallpaper asset folder through the shared picker and, where explicitly granted,
Files. Files currently opens only its NVMe/live-memory roots, not the optical
volume; add authorized source selection rather than granting whole-volume access
just to choose a background. Selecting an image must not confer write authority.
Retain a built-in fallback when media is absent or decoding fails, and publish
a new background only after successful loading/validation so the current one
survives errors.

### UI-011 — Complete the Win32-grade shared-widget reliability gate

Status: in progress

Apply the invariants, open findings, interaction matrix, and release gate in
[`ui-toolkit-audit.md`](ui-toolkit-audit.md). Keep interaction state machines,
geometry, input ordering, and minimal damage in the shared toolkit; Files,
Devices, and CCL Workbench are integration clients, not alternate widget
implementations.

### UI-003 — Add density and integer-scale typography

Status: open

The shared toolkit and native desktop now rasterize bundled TrueType fonts in
Rust, with bounded caches; the 13-pixel em/17-pixel line default preserves current
geometry. Scale notifications, adjustable raster sizes and widget metrics still
need integration. Do not stretch the completed framebuffer to implement DPI.

The portable multi-output geometry core now supports rational scales, signed
placement and all four rotations, with SPARK checks and hosted pixel tests.
A native QEMU fixture discovers three differently sized scanouts; it does not
yet render a multi-monitor desktop. Next add generation-bound output/session
ownership, then route composition, damage and input through the shared model.
See [display architecture](display-outputs-and-scaling.md) and
[geometry tests](../tests/display-geometry/README.md).

Connected-edge layout admission is now implemented in `CuBit.Display_Layouts`:
37 proof diagnostics and 6572 hosted arrangements pass. This is not yet wired
into native display configuration. See [layout tests](../tests/display-layouts/README.md).
Pure window placement and the bounded recovery timer are now implemented:
47 proof diagnostics and 231,563 hosted placement/timer cases pass. Successful
proposals preserve full decorated extents inside a ready work area; automatic
fallback does not mutate desired homes. This is not yet live desktop recovery;
see [placement evidence](../tests/window-placement/README.md).
The owner-local output registry and opaque placement tickets now add revision
and incarnation checks with typed presence/power/readiness and fail-closed
counter exhaustion. See [registry evidence](../tests/output-registry/README.md).
The display service now registers the boot-selected output and binds its lease,
attachment and session state to that reference. A compile-time native QEMU
fixture exercises stale-generation rejection and explicit reattachment recovery.
Native adapters still need authority-filtered discovery/topology notifications,
cross-service lifetime identities and window-intent revisions, then serialized
checked placement apply. There is still only one presented output.
Before wiring persistent layout, implement named-display/profile resolution
and revision/generation-safe application of placement plans described in
[desktop layouts and workspaces](desktop-layout-and-workspaces.md). Keep desired
home positions separate from temporary fallback placement during boot/hotplug.
Include all four rotations and workspace membership from the outset; a virtual
desktop is not a physical output or a security boundary. Settings/CCL Config
must edit the same typed model, with explicit confirmation and save status.

### UI-004 — Add bounded responsive layout primitives

Status: open

Add row, column, grid, split-pane, padding, and minimum/maximum-size primitives
that compute checked rectangles without allocation. Workbench panes and native
applications should respond to surface size without duplicating coordinate
arithmetic or risking underflow at small dimensions.

Use the CCL Workbench as the acceptance case: its menu, semantic toolbar groups,
execution/source/bytecode panes, editor scrollbar, and status bar should be
declared as a bounded layout tree rather than a collection of absolute `x`, `y`,
`w`, and `h` literals. The layout result remains ordinary checked rectangles so
rendering, hit testing, damage tracking, and CuBit IPC never depend on hidden
native widget state.

### UI-005 — Define text overflow behavior

Status: open

Text-bearing widgets need explicit clip, ellipsis, horizontal-scroll, or wrap
policies. Each policy must be bounded and safe for proportional fonts. Editable
fields should keep the caret visible without allowing text to escape the field.

### UI-006 — Add chart primitives with units and scales

Status: open

Promote the Workbench's hand-drawn bars into bounded series, axes, units,
legends, and empty/error states. Data bounds and sampling policy must be
explicit so monitoring widgets cannot allocate or render without limit.

### UI-007 — Make CuBit Classic the native application default

Status: open

Apply the CuBit Classic theme by default to every native application using the
shared desktop widget toolkit, including Devices and future CCL-facing
applications. Migrate applications deliberately with screenshot and
interaction regressions so palette changes do not hide focus, authority, error,
or disabled states. Centralize the licensed window-control icon atlas rather
than duplicating hosted-preview masks. Choose and document a stable public name
for the toolkit itself; "CuBit Desktop Toolkit" is the provisional name.

### UI-008 — Add source-editor navigation and diagnostics

Status: open

Add an optional line-number gutter and a legible monospace editor font without
changing the proportional typography used by ordinary desktop controls. Add a
diagnostic marker bar beside the editor that maps the interpreter's bounded,
one-based source position to the affected line and highlights that line after a
parse or type-check failure. The marker must be derived from versioned
diagnostic data, remain aligned while scrolling, and disappear or become stale
when the document changes rather than implying that an old result still applies.

### UI-009 — Complete mouse-free desktop and widget navigation

Status: open

Make every essential desktop operation usable without a pointer. The desktop
now provides the first vertical slice: Super opens the Apps menu, Up/Down move
through launchable entries, Enter launches the selected application, and Escape
closes the menu. Extend this into a shared, consistent toolkit contract rather
than implementing application-specific key handling.

Define focus order and visible focus indicators for every interactive widget;
Tab and Shift+Tab traversal; arrow-key behavior within menus, lists, grids,
trees, tabs, sliders, and scrollbars; Enter/Space activation; Escape/cancel
semantics; window switching and window-control shortcuts; and keyboard access
to context actions. Modal surfaces must trap focus intentionally, disabled and
hidden controls must not receive focus, and client surfaces must not be able to
spoof or consume desktop-owned shortcuts. Add interaction tests that exercise
the entire Apps-to-application path with no mouse events.

## Storage and files

### FS-001 — Replace hardware-specific filesystem backends

Status: in progress

Define one typed, versioned block-device interface for ATA, NVMe, ATAPI, USB
mass storage, and memory devices. It reports logical block size, block count,
read-only state, alignment, transfer limits, and supported operations. Keep
storage discovery and mounting in a control plane, then delegate a restricted
direct endpoint to the filesystem data plane so bulk I/O adds no broker hop.

Add kernel-tracked derived memory loans before using client pages directly with
drivers. The current grant primitive has no parent/child lifetime tracking and
must not implicitly re-grant a borrowed mapping. Derived ranges and permissions
must attenuate their parent, and DMA loans remain pinned through terminal
completion. Add an IOMMU mapping object so drivers receive bounded I/O virtual
addresses rather than unrestricted physical addresses.

Ordinary grants now fail closed on received-grant ranges, and the kernel
implements persistent generation-tagged references plus explicit
acquire/use/return lifetime. The first acquisition pins backing frames;
revocation and owner teardown become pending until the final return; grantee
teardown forcibly returns its inbound acquisitions. `Block.Device.V1` and the
application-facing filesystem operations acquire and return typed references
with direction-specific range and access validation. The live diagnostic checks
direct and capability-directed acquisition, pending revocation, final return,
stale generations, wrong owners, access attenuation, and bounds. Continue
migrating remaining single-word grant protocols.

The new SPARK `CuBit.Filesystems` package is the Ada-side protocol definition
site and constructs generation-bearing requests. Filesystem.svc, config.svc,
procmgr.svc, and storage-check now build against it. Migrate the shell's
remaining hand-built requests and keep the C ABI definitions generated or
cross-checked from the same schema rather than maintaining parallel constants.

Filesystem and config ACL administration no longer trusts the first caller.
Both resolve the currently registered devmgr/procmgr roles on each policy
operation, avoiding stale cached-PID authority, and storage-check requires a
normal filesystem endpoint's self-grant attempt to be denied. Replace this
interim role lookup with distinct policy-operation capabilities so the receiver
does not need to infer authority from process identity.

Acquired single-hop grants are now lifetime pins. Next add typed parent/child
loan derivation, cancellation semantics, and pinned-memory quotas before direct
application-to-device I/O. Cover acquire/revoke/return and owner/grantee death
with concurrent negative tests rather than relying only on synchronous callers.

Remove `@ata:` and `@nvme:` from the application-facing namespace. Mount
logical volumes under policy-selected names such as boot, system, and work;
backend type must never change an authorization result.

### FS-002 — Add the Files explorer and resource chooser

Status: in progress

Build a native `Files` application using the shared tree, grid, editor, and
dialog widgets. It should browse only explicitly supplied roots and support
bounded directory paging, sorting, selection, change streams, and asynchronous
file operations.

The same application provides desktop-owned Open, Save, Export, and Select
Folder interactions. It must display verified requester identity and requested
rights, then return an attenuated typed handle—not merely a pathname—to the
requesting process. Keep volume administration, preview parsers, thumbnails,
and indexing in separately authorized components.

`Directory.Page.V1` now replaces the newline-list prototype with distinct,
generation-tagged directory handles and fixed one-page typed replies. Cursors
are owned by filesystem.svc, each ext2 record is validated before its name is
viewed, and malformed media has a focused negative QEMU test. The first native
read-only Files window consumes the interface, follows pagination, validates
reply layout, and is available from Apps. It currently lists an explicitly
granted NVMe or live-memory root. Child-handle traversal, in-folder refresh,
and Back through retained handles are implemented, with nested-folder QEMU
tests. Sorting, change streams, rich metadata, and chooser delegation remain.

Navigation retains at most 16 handles, displays at most 128 entries, and
publishes a new listing only after validation. The first controls are Open/Enter
and Back/Backspace; double-click activation is still pending. Escape closes Files.

### FS-003 — Add logical per-application storage roots

Status: planned

Construct each process's filesystem view from launch-time handles with friendly
purpose names such as documents, pictures, project, application-data, cache,
and temporary. Required private storage and optional user-gated collection
access must be distinct manifest requests. Recent files and bookmarks retain
object identity and provenance and are revalidated before reuse.

### FS-004 — Complete ext2 allocation and file growth

Status: in progress

The first-block defect is fixed. Block and inode allocation now walk every
group, use group-relative bitmap indices, validate mounted geometry, and reject
duplicate frees that would inflate counters. Fresh partial data blocks are
zero-initialized before their inode pointer becomes visible. Checked block I/O
and explicit read/write outcomes prevent short ATA/NVMe transfers, no-space,
unsupported file extents, and transport failures from masquerading as success.
Allocation metadata updates attempt rollback when a later write fails.

The focused storage test now uses an intentionally sparse file on an image
whose first four block groups are full. It requires successful first-block
allocation and data verification, rejects an unsupported extent with its exact
typed reply, and creates, writes, reads, and closes a second file through the
live filesystem service.

Remaining work is to make every directory, truncate, free, and metadata-read
API return an explicit outcome; add direct-to-single-indirect boundary,
no-space, injected-device-failure, and remount-persistence tests; and define
the recovery story for a failure during rollback. Ext2 has no journal, so
power-loss consistency and online transport failure cannot be claimed merely
from best-effort reversal of completed metadata writes.

The latest audit found a concrete safe-save blocker: `Ext2.renameEntry` removes
the source name before adding the destination, and directory add/remove still
discard metadata-write status. Harden these around a shared validated record
iterator and an explicit commit/failure model before adding Save/replace UX.
See [filesystem maturity](filesystem-maturity.md) for the findings and tests.

### FS-005 — Add crash-consistent filesystem journaling

Status: backburner

Add metadata journaling before claiming crash consistency for writable
persistent filesystems. First define and implement durable completion in
`Block.Device.V1`: ordinary write completion, cache flush, and force-unit-access
must have distinct semantics supported by ATA and NVMe. A journal cannot make
correct ordering guarantees on top of an ambiguous device-completion contract.

Prefer a small, bounded transaction and recovery model that can be analyzed in
SPARK before attempting full ext4/JBD2 compatibility. It must cover descriptor
and commit records, sequence and wraparound handling, checksums, revoke
semantics, ordered data-before-metadata publication, checkpointing, and
idempotent replay. Validate it with deterministic crash injection after every
durability boundary. If the on-disk format is CuBit-specific, describe it as a
transactional ext2-derived filesystem rather than ext4.

### FS-006 — Validate hostile-volume aliases and block ownership

Status: deferred; not a blocker for ordinary Ext2 interoperability or current
Config/Turso file-I/O work.

Treat imported disk metadata as untrusted input. Eventually check directory
references against inode link counts, detect distinct inodes sharing data or
indirect blocks, and reject data mappings into reserved filesystem metadata.
Use bounded validation with explicit resource limits and adversarial fixtures;
keep the on-disk format standard.

Current regular-file admission rejects reported zero/multiple links and does
not follow symlinks. Retain those checks, but do not describe them as proving
the absence of aliases on a malicious image. The existing bounds, feature and
I/O checks remain in force while this broader ownership validation is deferred.
See [Ext2 interoperability](ext2-interoperability.md).

## Native clock/runtime follow-up

### Speed up build-time timezone generation

Status: deferred; current generator is local-only but CPU-heavy.

`tools/generate_time_zones.py` samples every bundled IANA timezone at six-hour
intervals across 2000–2099, then validates explicit transition boundaries.
Replace normal-build sampling with direct TZif transition extraction and
expansion of future recurring rules, preserving the supported date range,
typed zone identities, and alias-table sharing. Keep independent sampling and
boundary comparisons as regression tests rather than routine build work.

Narrow the generated-table prerequisites in `kernel/Makefile`: unrelated
`flake.nix` edits (such as font dependencies) should not trigger regeneration.
Track the actual pinned tzdata and generator/toolchain inputs instead. Add
progress reporting and measure cold-generation time; verify incremental builds
skip unchanged inputs and rule updates still rebuild correctly. No network
service is needed for generation.

### Remaining runtime integration

- [ ] Implement and test standard GNAT `Ada.Calendar`, `Time_Zones` and
  `Ada.Real_Time` adapters over CuBit clock authority, keeping wall time separate
  from monotonic deadlines. Cover full standard ranges and DST ambiguities;
  the current native `CuBit.Clocks` client is not a substitute for these packages.
- [ ] Separate NTP/NTS synchronization service and delegated clock-adjustment
  authority; validate source/freshness/uncertainty and step/slew policy. See
  [clock and time services](clock-and-time-services.md).
- [ ] Extract taskbar audio controls into shared toolkit widgets, add keyboard
  slider navigation, themed icons, accessibility and Config persistence.
