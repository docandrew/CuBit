# Display outputs, aspect ratio, orientation and DPI

Design direction, updated 2026-09-22. The native Desktop now composes across two
rearrangeable mixed-resolution virtio outputs, with a Desktop-chosen primary taskbar and
separate transfer buffers, damage and completion state. Session presentation now
uses nonblocking broker submission and per-head fenced GPU command continuations.
Mixed-DPI/rotated composition, runtime modesetting and GPU rendering remain unfinished. Boot
descriptor admission is implemented separately; see
[boot framebuffer evidence](../tests/boot-framebuffer/README.md).

The first portable geometry core is now implemented in
`CuBit.Display_Geometry`: signed placement, rational scale, all four rotations,
clipped output-local damage and inverse pixel-center hit testing. Its numeric
safety, bounded damage and hit-test containment properties prove with SPARK;
191488 pixel mappings have regression coverage. Native Desktop damage splitting
now uses this core at unit scale/zero rotation; toolkit scaling and rotated
composition remain future work. See [geometry evidence](../tests/display-geometry/README.md).

QEMU 11.1 in the Nix environment supports per-output mode descriptions. The
native three-output discovery fixture now passes at 1024x768, 1920x1080 and
1080x1920 on one virtio GPU. The first two connected heads are activated at the
backend's bounded mode; head two remains detected-only. Validated preferred EDID
geometry is now used when two page-aligned buffers fit the owned bank (otherwise
1024x768 remains the virtual fallback). Advertised and active dimensions remain
distinct. A native mixed-output fixture checks 1024x768 + 1280x720 pixels,
spanning drag, maximize and pointer confinement below the shorter output.
Runtime mode changes and hotplug remain subsequent milestones. See
[EDID scope and evidence](../tests/monitor-edid/README.md).

The native Settings Displays page now provides draggable monitor tiles. It edits
positions: shared edges snap, nearby top/bottom/left/right alignments snap,
and the common layout validator rejects overlaps or disconnected layouts. Apply
updates the running desktop; Revert discards unapplied edits. **Make primary**
sets the selected tile's pending primary role. On Apply, Desktop moves the
taskbar, uses that output for new windows and adjusts maximized work areas;
existing windows retain their monitor. This is currently session-only: no durable monitor
identity or Config layout persistence is implied. See
[arrangement implementation](../tests/display-layouts/README.md#native-arrangement-editor).
The selected output also supports logical scale presets with a minimum usable
work area. Native output modes stay unchanged. Current clients use a pixel-sampled
fallback; native-density text requires a later scale-aware surface/configure
contract. See [scaling scope](../tests/display-layouts/README.md#per-output-logical-scaling).
EDID timing and physical dimensions are parsed in the virtual driver but are not
yet exposed to Desktop, and must not be presented as measured refresh or an
authenticated monitor identity. GTK's synthesized startup EDID can itself be
640x480; that undersized preference keeps the usable fallback. Native mixed-mode
pixel tests run frontend-free. The broker now selects a native backend by boot
adapter identity, not equality with firmware dimensions. A native output's
discriminated backend record contains no firmware rendering address; a GPU
failure stays a GPU failure. Both acquired scanout mappings must match the active
layout before that output is registered. A 1024x768 GRUB boot can therefore start
a 1280x720 native primary alongside a 1024x768 secondary.

Shared geometry, layout, window placement, registry and discovery code now lives
in `userspace/lib/display`, not GNAT runtime implementation sources. This is a
source-library boundary; native services remain statically linked.

The owner-local generation-bound registry and placement-ticket gate now exist
as shared SPARK components, with hosted tests/proofs. They distinguish named
display identity, backend lifetime, output reference and topology revision;
presence, requested power and readiness are separate. Native `display.svc` now
registers its boot-selected backend and the supported second virtio output,
binding leases, source attachments and presentation sessions per output
generation. Typed startup enumeration now
reports boot/virtio outputs, advertised versus active dimensions, and detected
versus backend-ready versus desktop-selected roles over existing authorized IPC.
See [discovery protocol and evidence](../tests/output-discovery/README.md).
Topology events, asynchronous configuration and desktop placement-ticket integration
remain next. See
[registry evidence and integration obligations](../tests/output-registry/README.md).

## Application-facing commitment

### Next implementation boundary: surface scale, not stretched desktops

The current native scene is still unit-scale and unrotated. The next vertical
slice must separate a window's logical content size from its buffer's pixel size
and pitch. Reuse the rational-scale/edge-rounding model in `Display_Geometry`;
do not add independent toolkit and compositor rounding conventions.

1. Introduce a surface configuration generation covering logical extent, raster
   extent, scale and format. The compositor chooses the dominant output with
   hysteresis; discovery order and a momentary boundary crossing must not thrash
   the app between scales. The configuration belongs to an authenticated surface
   lifetime, not to an EDID fingerprint.
2. Have the shared toolkit relayout in logical units, rasterize fonts/icons at
   the requested scale, and attach a buffer acknowledging that exact generation.
   Reject stale acknowledgements without replacing the working buffer. Existing
   buffer grants/fences, not the configuration number alone, determine when old
   storage can be reused. Do not change both logical widget geometry and pixels
   by the same scale factor twice.
3. Composite per-output pixel targets using one logical arrangement. A spanning
   surface initially uses one chosen raster scale, with an explicit resampling
   policy for the other output; later multi-scale representations are optional.
   Keep clipping, cursor rendering and inverse input transforms consistent with
   the same geometry generation. Never encode rotation by falsifying byte pitch.
4. Test native unit/125%/150%/200% crossings, a rejected/stale buffer, a delayed
   render client, and a disappearing output while a buffer is in flight. Check
   pixel placement, cursor hit testing, allocation/copy bounds and input latency
   before enabling the corresponding Settings controls.
5. Only then offer a transactional Settings Apply/Revert flow, with a visible
   confirmation countdown and automatic rollback. Preserve the desired CCL Config
   layout separately from the currently available outputs. A closed lid, KVM
   switch, or slow monitor must not silently overwrite that desired configuration.

The broker's implicit fallback from a GPU failure to a linear boot mapping is
removed. Backend selection is separate from its latched presentation fault;
clear/flip errors do not change the backend, report success, or reuse a firmware
address. Native boot handoff and injected clear failures on both outputs have
QEMU regression coverage, not a whole-service SPARK proof. Authorized MAPFB
permanently retires and drains the kernel boot renderer before mapping pages.
Publishing GPU_IS_PRIMARY retires it before native driver startup; the broker's
native path no longer requests an unused firmware mapping. General live
modesetting and exclusive hardware leases remain separate obligations below.

EDID work next: bounded extension parsing and mode enumeration; preserve an
explicit distinction between monitor-advertised timings, driver/link-admitted
modes, active mode and measured presentation timing. Physical dimensions may be
missing or wrong. Refresh should retain enough precision for fractional rates,
not be flattened to a guessed integer 60 Hz. Intel's connector/link/PLL checks
remain necessary even for a checksum-valid advertised timing.

Named monitors, connected layout admission, delayed discovery, recoverable window
placement, virtual desktops and CCL Config revisions are specified in
[desktop layouts and workspaces](desktop-layout-and-workspaces.md). These are
proposed state/lifetime rules; persistence and workspace switching are not yet
implemented.

CuBit supplies one shared native surface/input/scale contract and a standard
widget toolkit. Applications should not select among window-manager-specific
paths or implement their own monitor math. The desktop owns layout, transforms,
pointer crossing and output selection; the toolkit owns scale-aware metrics,
font rasterization and relayout. Custom renderers use the same surface contract.

Settings will present a draggable display arrangement with explicit resolution,
UI scale and rotation per output. Desired placement is distinct from native
pixel mode and physical monitor size. Configuration authority stays separate
from ordinary app surface access. Fractional scaling must not be implemented by
stretching a completed desktop image or changing input-device gain.

## Laptop improvement now

The USB-live GRUB menu prefers firmware-advertised widescreen 32-bit modes,
with an explicit 1024×768 fallback. QEMU renders DOOM, Workbench and Files at
1920×1080. This avoids stretching 4:3 across a 16:9 panel if firmware offers a
suitable mode. It cannot add firmware modes or control the panel scaler;
native Intel HD 620 modesetting and EDID discovery are separate driver work.
Do not compensate with mouse gain or stretched widget coordinates.

## Output model

Extend the existing typed display protocol, not a parallel GUI subsystem:

- Generation-bound output descriptors: physical-pixel modes, refresh, connection
  state. Observation and configuration are separately granted authority.
  EDID is bounded untrusted metadata, not authority to control a display.
- Signed logical desktop coordinates, including negative origins. Each output
  maps a logical rectangle to physical pixels with rational scale and an
  orientation enum: normal, quarter-turn, half-turn, three-quarter-turn.
- Physical size and effective UI scale are separate. Automatic DPI is a hint;
  Config overrides support bad EDID, accessibility and preference. A display
  fingerprint is a preference key, not authenticated identity.
- Compositor owns layout/transforms; widgets use logical units. Centralize
  rounding so adjacent edges agree. Hit-testing uses the inverse transform.
  Input carries layout/output generation to handle events queued across changes.
- Damage, cursor restoration, clipping and presentation are output-local.
  Rotate damage as well as content. Never stretch the entire desktop across
  unrelated aspect ratios. Specify pixel-edge inclusion exactly.
- Initially choose a spanning window's scale from its dominant output, and
  notify clients on changes. Later support per-output representations. Avoid
  double scaling between client and compositor, as in earlier preview DPI bugs.
- Disconnect atomically invalidates descriptors, relocates inaccessible windows,
  terminates affected presentation obligations and preserves recovery access.
  Input seats and remote views remain explicitly authorized objects.

## Sequence

1. Implemented: model today's framebuffer as one typed output; test/prove the
   geometry, layout, reference and placement foundations. Native discovery
   distinguishes connected outputs from ready presentation destinations.
2. Native broker/driver bring-up now supports separately addressed sessions and
   buffers for two 1024x768 virtio outputs. A guest fixture updates both through
   normal display IPC; QMP checks distinct pixels and continued updates on one
   after the other's lease/grant is released. Session frames now use bounded
   asynchronous backend submission/completion.
3. Native Desktop now spans two equal modes. Damage is split into output-local
   rectangles; window maximize uses the containing monitor, and only the
   Desktop-selected primary reserves taskbar space. Next: mixed resolution,
   orientation and scale. A native compile-time delayed-head fixture checks one
   held frame beside a completing output and responsive broker queries.
4. Settings/CCL Config arrangements and desired-versus-observed recovery:
   delayed detection, output loss/return, recoverable title bars and remembered
   homes. Integrate the existing placement-ticket checks with real window moves.
5. Native Intel modesetting/power/hotplug for the reference machines; QEMU is
   protocol/geometry evidence, not proof of physical connector behavior.

Safe shared backend buffers are a performance track alongside these visible
gates, **not a prerequisite for the first working second display**. Keep one
presentation contract with explicit backend capabilities; do not build a second
multi-monitor protocol just for the copy fallback. Switch to direct composition
only after native derived mappings, revocation, final-reader release and teardown
tests pass. Do not widen display authority as an optimization shortcut.

Retain latency/bandwidth measurements as acceptance criteria. No synchronous
mode queries, EDID parsing or full-desktop redraws on the input hot path.
Preserve existing authority, buffer-loan and completion semantics.

### Nonblocking session presentation

The steady-state path is Desktop async submission -> display broker async GPU
submission -> IRQ-driven GPU command completion -> saved replies back up the
chain. Each of the two outputs admits one outstanding frame. There is no
unbounded frame queue, new authority, or new pixel-copy stage.

The broker captures the output, session/frame, presentation-model identifier,
globally non-reused completion token, target swapchain buffer and damage before
returning to its dispatcher. It saves a distinct reply capability per output.
Completion dispatch never uses the last request's mutable routing selector.
Attachment replacement, session reopening and lease changes are rejected while
that output is busy. Source acquisitions and scanout buffers remain held until
the matching completion; only then are buffer age and the active index advanced.

The GPU has separate command/response storage and descriptor pairs per head.
Transfer, set-scanout and flush are sequential *within* a head, but either head
can advance independently. Every runtime command has a non-reused fence ID;
used descriptor identity, response length/type/flags and returned fence must
match. x86 coherent-DMA publication barriers precede the available index and
notification. The service sleeps on atomic request/IRQ activity waits, with a
500 ms command-failure deadline, not a presentation polling interval.

Timeout or uncertain completion quarantines the affected head and its storage;
it does not synthesize cancellation, recycle DMA descriptors, return a client's
held source, or report successful presentation. Invalid shared ring metadata
quarantines the adapter's heads. Hardware reset/recovery is still future work.

Scope: native session presentation is nonblocking. Boot resource initialization
still waits synchronously. Broker configuration/map/clear and the old shell's
non-session presentation calls still use synchronous GPU IPC; they are not a
general asynchronous modesetting API. CPU copies still occupy the service for
their duration. The shared adapter/device may itself serialize work; this does
not prove hardware fault isolation, scanout visibility, physical hotplug
behavior or a latency bound. The existing SPARK presentation/loan cores are used,
but the native IPC/MMIO adapter and DMA ordering are not wholly SPARK-proved.

### How the lifetime work contributes

The user-facing model is deliberately small: apps provide surfaces; the desktop
places them in one logical layout; independently managed outputs present their
portion. The toolkit receives geometry/scale changes instead of making each app
implement monitor discovery, rotation and persistence.

The grant work supports that model in three specific places:

- Efficient output-local buffers: share a suitable driver-owned back buffer with
  the compositor through the broker, without serializing or staging its pixels.
- Safe transitions: output loss or a mode change closes admission but does not
  declare an old buffer reusable while a reader/device still has it.
- Correct scope: removing one output's mapping must not retire a shared parent
  allocation still used elsewhere. A genuine adapter reset can affect all of its
  outputs; it is a broader failure domain, not an ordinary monitor disconnect.

These lifetimes do not themselves implement multiple scanouts, scheduling,
mixed-DPI relayout, or hardware modesetting. The native broker now has per-output
lease/session/buffer state and nonblocking session presentation. The delayed-head
native test checks dispatcher progress; the hosted lifetime model alone would
not establish that property. Legacy configuration calls remain synchronous.

Hosted composition tests now combine the existing grant, parent-hold and
presentation state machines. Across queued, reading and scanout-held states,
output A closes/stalls while B completes 128 updates. They cover separate and
overlapping read-only page ranges, retained parent holds, delayed release,
retired-slot reuse and broader parent closure. Local frame identifiers can be
equal across outputs: the adapter must authenticate and select output/session
identity before applying a completion, never route on a bare frame number.
See [test evidence and limits](../tests/grant-loans/README.md#multi-output-composition-tests).
This is model-level regression coverage, not native multi-output scheduling or
proof of DMA quiescence.

### Native two-output bring-up

Display/GPU requests use the 16-bit message envelope's `Reserved` field as an
explicit output selector, currently accepting ready local outputs 0 and 1. The
shared codec bounds the protocol namespace to 0..15; unused destinations fail
closed. Catalog requests remain canonical with a zero envelope. This field is
untrusted routing metadata, never an authority tag. The broker validates the
destination, strips the envelope, then runs the existing payload codecs and
checks the authenticated process, that output's lease and its generation-bound
attachment/session. Session numbers are non-reused across the whole broker
incarnation, so moving a frame to another output cannot match its session.
No mutable "select output for subsequent calls" operation is exposed.

Each GPU output owns two 1024x768 resources, a separate active-buffer index and
damage history. devmgr provisions two independent 8 MiB DMA banks; the driver
uses each bank's actual physical base, not an assumption of physical adjacency.
The second bank is optional on allocation failure. The existing boot-provisioning
sysinfo mechanism carries its address for now; this is not a new primary-display
policy or a complete multi-adapter driver-startup protocol. Advertised sizes do
not gate activation: QEMU GTK may replace configured mode hints with its initial
640x480 widget size. Connected supported heads still use the fixed resources;
larger/mixed active modes remain outside this bounded initial backend.

Unused raw-slot GPU attach/copy/flush commands have been removed; presentation
uses the existing checked grant-mapping and buffer-present path. The legacy-copy
measurement counter remains zero for baseline comparability, not as a callable
compatibility implementation.

The regression command and evidence limits are documented in
[headless tests](../tests/headless/README.md#two-native-display-outputs).

## Responsibilities and object ownership

The [GPU rendering/presentation contract](gpu-rendering-and-presentation.md)
details independent render devices, mixed-adapter imports, scanout ownership
and current singleton-driver integration gaps.

Foundational rule: applications own surfaces; the desktop arranges them; display
backends present them on independently managed outputs. Render devices and
presentation destinations are not the same object.

| Object | Owner and meaning |
| --- | --- |
| Adapter | Device-manager-provisioned graphics device and its isolated driver instance |
| Connector | Driver-discovered connection, including panel, socket or downstream dock connection |
| Monitor | Attached sink's untrusted advertised modes, physical size and descriptive identity |
| Output | Generation-bound presentation destination; physical scanout or explicitly authorized virtual destination |
| Surface | Application content with an authorized desktop attachment, independent of monitor placement |
| Layout | Desktop-owned logical arrangement and transforms; Config records desired preferences |
| Render context | Budgeted GPU execution and memory domain, not modesetting or capture authority |

Do not expose pipes, transcoders, PLLs and link-training registers in the public
desktop interface. An adapter can have shared hardware constraints; its driver
validates the complete affected configuration and returns structured reasons.
Connector-to-output mapping is not permanently one-to-one: docks, tiled panels
and hardware cloning need explicit grouping without changing application surfaces.

Device manager provisions MMIO/IRQ/DMA authority. Adapter drivers control devices.
`display.svc` brokers authorized output configuration and presentation sessions.
`desktop.svc` owns composition, window layout, hit-testing and seat routing. The
toolkit lays out widgets in logical units and rasterizes at requested scales.
Settings applications and CCL submit configurations; they do not write registers.
Config stores desired policy, not a second mutable hardware state.

These are responsibility boundaries, not a requirement to add one process for
each connector or to route every frame through the discovery/configuration path.
Provision retained sessions/grants once, then use bounded asynchronous backend
queues. Preserve authenticated endpoint and generation checks on every request.

## Geometry, scaling and input

Use distinct types for logical coordinates, physical pixel coordinates, extents,
byte pitches, rational UI scale, orientation, and refresh timing. Desktop origins
are signed. Use half-open pixel rectangles; quantize shared edges identically.
Map damage outward conservatively. Quantized inverse hit-tests need bounded-error
and containment properties, not the false promise of exact float round trips.

A 3840x2160 output at scale 2 offers 1920x1080 logical units. A portrait
1080x1920 output at scale 1 beside it offers 1080x1920 logical units. Empty space
between irregularly arranged outputs is not a giant backing allocation. Pointer
crossing/gap behavior must be deterministic, with logical motion preserved rather
than compensating by modifying device gain. Absolute input devices are bound to
an authorized target/seat and transformed separately from relative deltas.

Clients receive a geometry/scale generation. A spanning window initially uses a
dominant-output scale with hysteresis; crossing a boundary must not trigger
continuous relayout oscillation. During a scale transition, retain the old buffer
until a correctly tagged replacement arrives. Later support per-output raster
representations of the same logical surface. Fonts, cursors and native widgets
use the same transform policy; do not stretch the whole desktop or scale twice.

## Transactional configuration and recovery

Keep desired, prepared and observed configuration distinct. A request names its
expected topology generation, affected outputs, modes, positions, scales,
orientation and authorized power policy. Validate authority, resource budgets,
shared hardware constraints and topology before hardware modification. Preparation
tokens are scoped to their requester/session and invalidate on topology change.

Validation rejection preserves the active state. Reserve resources before apply.
Within an adapter, expose atomic apply only when supported. Across adapters,
coordinate transitions but do not promise physically simultaneous switching.
Hardware failure after apply begins is a distinct partial/failed outcome, not an
ordinary validation rejection; report the actual surviving configuration. A
previous mode may be impossible after unplugging, so rollback is best effort with
a known-safe surviving-output fallback.

For interactive disruptive changes, use a confirmation deadline. Preserve a
recovery access path, move inaccessible windows on unplug, and surface degraded
state. Handle service death/reset as new generations, never reuse old sessions.
Remote/headless policy may intentionally configure no local output; do not
invent an unconditional requirement for a connected monitor.

ACPI supplies scoped panel/lid/power coordination, not an alternate owner of
display layout. Output blanking, panel brightness, system suspend, and kernel
panic CPU shutdown are different operations. Their authority and sequencing must
be explicit.

Connection detection, requested power and presentation readiness are separate
state. Boot/wake/KVM transitions should preserve a known display's desired place
while bounded driver recovery runs. Debouncing window relocation does not defer
invalid-session rejection or prove DMA quiescence. See the
[readiness-first recovery policy](desktop-layout-and-workspaces.md#readiness-first-boot-wake-and-kvm-recovery).

## Presentation and performance

Each output has independent damage, cursor state, deadlines, queue budget and
completion history. A slow output must not gate faster outputs. Clone groups can
request synchronization but must expose actual hardware limitations. Start with
bounded queue depth/backpressure and explicit stale-frame discard outcomes, not
unbounded latency accumulation. Admission does not imply a frame will be visible.

Separate rendering completion, final buffer-reader release, scanout publication,
and physical presentation timing. Keep uncertain ownership pinned/quarantined.
Only a trusted driver observation may establish device completion; an app-provided
counter or fence value is not evidence. See [buffer lifetimes](display-buffer-lifetimes.md).

Prefer output-local composition and buffers local to the consuming adapter. Share
immutable/loaned surfaces where safe; do not serialize pixels in IPC messages.
Cross-adapter sharing requires supported formats, mapping and synchronization.
Use explicit measured copies when sharing is unavailable. Direct scanout is an
authorized optimization of the same surface lifecycle, not a way around desktop
composition/security policy. Cursor updates must not wait for unrelated repaint
work. Hardware cursor planes are optional; software cursors retain output-local
damage/restoration rules.

### Direct rendering target

The preferred path is for the trusted compositor to render into an available
backend-owned presentation buffer and submit its identity and damage, without
an intermediate paint-to-transfer or transfer-to-backend pixel copy. Use video
memory directly when the adapter exposes a suitable CPU-mappable back buffer;
on unified-memory hardware this may be ordinary shared system RAM suitable for
scanout. Memory placement must also respect cache attributes and CPU access
costs: "video memory" alone is not a performance guarantee.

This is not permission to paint the currently scanned-out front buffer. Each
output needs explicit writable-buffer acquisition, submission, and final-reader
release. Never reuse a buffer merely because a submit/flip request was accepted.
The compositor must repaint regions whose contents are stale in a recycled
buffer, or explicitly account for any damage-repair copies; buffer rotation
must not silently reintroduce a full-frame copy.

Current implementation has not reached this target: the compositor copies
damage into a transfer buffer, then display copies into the framebuffer or GPU
back buffer. Virtio additionally issues a guest-to-host resource transfer.
`Map_Backbuffer` is deliberately unavailable: forwarding a GPU-owned grant via
display requires explicit derived-loan lifetime tracking, not re-granting a
borrowed address as owned memory. Before enabling this path, preserve parent
pins, range/access attenuation, revocation and final-reader release across
driver -> display -> compositor. Do not grant ordinary apps raw display memory
or broaden device authority to work around this requirement.

The first [bounded derived-loan lifetime core](../tests/grant-loans/README.md)
now implements reservation/publication, attenuation, reader draining, mapping
retirement and one-shot parent release. The kernel's parent lifecycle now has a
separate forwarding hold, preserved by receiver teardown, but no native caller
creates a scope yet. Derived mappings, independently pinned child pages and
scope-close cascades are not integrated; `Map_Backbuffer` remains unavailable.

Firmware-only output remains an explicit copy backend until a safe alternative
exists; basic virtio resource transfer is not a claim of zero-copy host display.
Measure and report compositor work, damage repair, intermediate CPU copies and
backend transfers separately. The objective is avoiding unnecessary pixel
movement, not claiming that software composition or every backend is copy-free.

The current 16 MiB desktop attachment limit cannot hold a 4K BGRA buffer (about
31.6 MiB). Before enabling larger outputs, replace the system-wide fixed ceiling
with negotiated, checked per-session/per-adapter budgets and bounded allocations.
Update all codecs, frame stores, quotas and tests together. The boot backend
retains its explicit 16 MiB limit meanwhile; this is not a Vulkan image limit.
Color format, color space, alpha convention, storage layout and orientation must
remain separate. Start with the actual BGRX/BGRA SDR paths; negotiate color-managed
or HDR behavior later rather than silently misinterpreting buffers.

## Boot framebuffer and native handoff

The boot backend represents one fixed-mode output with a validated mapped span,
known storage format and copy presentation. It must not invent EDID, refresh,
vblank timestamps, page-flip capability or native modesetting. Its temporary
identity is scoped to the backend incarnation, not a monitor serial number.

Currently the kernel publishes geometry once and existing sysinfo/MAPFB consumers
use it. The display service puts supported backends into an owner-local registry
and checks per-output bindings; discovery is enumerable, but this is not yet a
transferable exclusive hardware lease or general multi-adapter modesetting.
The existing `GPU_IS_PRIMARY` bootstrap flag means "backs the boot console",
not "hosts the Desktop taskbar". Replace that heuristic with explicit boot-output
identity as native hardware handoff matures; keep it separate from the preferred
app-launch output and from the preferred render adapter.

In particular, the desktop's **primary display** is a Desktop-owned layout role,
not a display-service or driver decision. display.svc supplies authorized output
facts and independent presentation sessions; the Desktop chooses where its
taskbar and recovery controls live. See [primary selection and fallback
rules](desktop-layout-and-workspaces.md#primary-display-belongs-to-the-desktop).

Before native takeover: identify the actual device/output behind the boot
surface, prepare the native backend, stop all firmware-surface writers, transfer
ownership, publish the new generation and retire obsolete mappings only after
their readers/writers are quiescent. Kernel panic output must not blindly revive
an obsolete firmware mapping after takeover. A stopped CPU, stale grant or dead
driver is not proof that GPU DMA has stopped. The broker's startup backend
selection is implemented, but this complete hardware handoff remains future work.
The kernel boot panel now has irreversible retirement and a coordinated
writer-drain boundary. The last-chance handler cannot reopen its framebuffer;
`enableVideo` only affects the separate legacy text adapter. This deliberately
does **not** promise graphical panic output after native takeover. See
[boot diagnostics](boot-diagnostics.md) for implementation and proof boundaries.
Do not confuse writer retirement with unmapping or GPU DMA quiescence.

## Security and visibility

Reuse CuBit authorities, authenticated IPC, handles and grant lifetimes. Output
observation, configuration, presentation, capture, input injection, render-context
creation and power control are separate effects. Discovery is authority-filtered;
ordinary apps need only their surface's geometry/scale, not unrestricted monitor
inventory. Public IDs describe objects; only granted handles authorize access.

An EDID/display fingerprint may select preferences but is not authenticated
identity or a reason to grant capture/configuration rights. Virtual output
creation does not grant capture of an existing desktop. Security-sensitive
overlays remain compositor-controlled. The display/compositor/driver processes
that can inspect pixels remain part of that confidentiality boundary.

Inspector should explain the actual backend, mode, scale source, owning session,
configuration requester/reason, copy/flip path, pending frames and timing quality.
Keep those observations out of the input/presentation critical path.

## GPU rendering and Vulkan

The N95/HD 620 native implementation sequence is tracked in
[Intel GPU bring-up](intel-gpu-bringup.md). Initial hosted probe helpers do not
yet replace the firmware framebuffer or provide hardware acceleration.

Vulkan deliberately separates device rendering from optional window-system
integration (WSI). That maps naturally onto render contexts plus CuBit surfaces;
we do not need to emulate X11, Wayland, file descriptors or a UNIX device node
API to provide a native binding. Vulkan's opaque surface and swapchain model is
the API boundary to adapt. [Khronos WSI specification](https://docs.vulkan.org/spec/latest/chapters/VK_KHR_surface/wsi.html)

Proposed path:

1. An app receives render-context authority independently of its window handle.
   Headless rendering/compute can exist without a desktop or any output grant.
2. An in-process Vulkan implementation records/compiles commands and batches
   submissions to its isolated adapter service. Do not make each Vulkan call an
   IPC. Command buffers and resources use checked shared-memory/object references.
   Vulkan itself exposes queue submission separately from command recording.
   [Khronos devices and queues](https://docs.vulkan.org/spec/latest/chapters/devsandqueues.html)
3. A future CuBit WSI adapter binds an authorized surface to a swapchain. No
   extension name or registry number is assigned here; a supported public binding
   and loader/driver integration are separate porting work.
4. Swapchain images are typed GPU resources with memory domain, format/layout,
   extent, usage rights and lifetime. CPU-mappable linear buffers are one case,
   not the universal representation. Export/import requires explicit attenuating
   authority; GPU virtual addresses are not host pointers or proof of ownership.
5. Render-ready dependencies and final-reader release protect images reused by
   the application. Resizing/output loss/device reset may invalidate a swapchain
   without pretending queued work was cancelled or its memory is reusable.

Prototype the surface/swapchain lifecycle with a software-rendered backend before
hardware 3D. Native Vulkan also needs a loader/driver ABI, shader compilation,
device memory management, synchronization, resource limits, and conformance
testing. A framebuffer and virtio scanout driver alone are not Vulkan support.

### GPU isolation is a prerequisite, not a driver detail

An untrusted in-process Vulkan library or shader compiler cannot be the security
validator. The trusted adapter boundary must constrain commands, resource
references and submission ownership. GPU MMU/context isolation and an appropriate
system DMA isolation strategy are needed before accepting arbitrary untrusted
hardware workloads. IOMMU confinement does not replace per-context GPU memory
isolation; neither automatically proves command-processor privilege checks safe.
Do not permit apps to submit unrestricted DMA rings or device registers.

Per-context accounting, queue budgets, bounded waits, preemption where supported
and watchdog/reset recovery are required. A shader is not a bounded CCL program;
SPARK proofs of host-side code do not establish GPU program termination or protect
against hardware/firmware defects. Adapter resets may disrupt every context on
that device: expose the failure domain, retain memory until DMA is quiescent,
and prefer a separately protected compositor queue/context where supported.
Unsupported hardware stays on a trusted software-rendering path rather than
weakening the security model for acceleration.

## Incremental implementation gates

1. Admit and reserve one boot framebuffer; unify existing consumers. Prove the
   pure numeric core and test native malformed handoffs and RAM-backed scanout.
2. Add a generation-bound output/session registry and explicit firmware/native
   handoff; migrate existing display wire messages without legacy aliases.
3. Prove geometry/rounding and test mixed-scale/orientation layouts hosted;
   integrate scale notifications and output-local rendering in the shared toolkit.
4. Exercise independent outputs, unplug/replug, delayed completions and mixed
   refresh through emulated drivers. Then implement native Intel modesetting.
5. Add typed GPU resource/fence/context semantics and a software WSI prototype;
   only enable hardware 3D once isolation and lifecycle prerequisites are met.

At each gate preserve functional native regressions and report queue/copy/input
latency separately from true presentation timing. Firmware copy completion
cannot substantiate keypress-to-photon guarantees.

Planned physical validation complements QEMU: the existing Intel HD 620 laptop
with its internal panel plus an external monitor, and the owner's incoming
dual-HDMI N95-based NUC as prospective reference hardware. Record the exact board,
PCI IDs, firmware and monitor/connector inventory before choosing driver work;
these are test targets, not claims of native multi-output Intel support today.
