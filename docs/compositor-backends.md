# Opt-in SPARK compositor

## Active goal and acceptance gates

Build a hardware-ready CuBit desktop compositor with instant, animation-free
interaction, GPU acceleration, crisp per-output DPI and tear-free presentation.
Reuse Mesa/Vulkan. Keep new geometry, scheduling, damage and lifetime policy in
proved SPARK and document the narrow trusted foreign interfaces. Preserve a
working software fallback, bound memory/queues, remove avoidable copies and
prioritize fresh input. Target 240 Hz under a defined load; measure the 1 ms
physical keypress-to-photon stretch goal separately from software latency.

The goal remains active until the following integration and evidence gates are
met. A model or hosted proof alone does not complete a native integration gate.

| Gate | Deliverable and acceptance evidence | Current state |
| --- | --- | --- |
| Presentation ownership | Proved bounded acquired-buffer lifecycle, non-reused identities/generations, explicit render/display completion, stale/failure/overflow tests, native held-reader validation | SPARK pool/repaint/retirement policies integrated with three registered CPU targets per output; native held-reader tests pass; GPU fence integration pending |
| Backend boundary | Bounded scene and resource descriptions consumed by software and Mesa/Vulkan, with audited FFI/error handling | Opt-in Gallium software imports tested; hardware resource contract pending |
| Direct output rendering | Render into acquired presentation storage, preserve unchanged pixels using buffer-age damage, no unsafe in-flight writes, bounded queue and memory | Opt-in scene traversal renders each output into its acquired CPU target without a private scene/drag allocation; native allocation ledger and mixed-DPI tests pass; native injected-text-failure/retirement passes; GPU targets/global memory admission pending |
| Native-density UI | Scale/generation configuration, client backing allocations, output-density fonts/decorations, fractional/mixed-DPI movement and clipping tests | Proved geometry/masks and retained fallback connected to per-output decorations/text and all Settings drawing; 125/150% native decoration tests and protected-client backing allocation pass; idle client moves/settings changes render exact native-sized rectangles; rich-client visual coverage remains |
| Hardware integration | Existing Mesa hardware backend executes compositor work and presents via the real driver, with software recovery tests | Depends on graphics-owned i915/ANV work; diagnostic triangle and CPU sync tests do not satisfy this gate |
| Instant modern appearance | No automatic motion/transition animations, bounded static effects, legible typography/focus indicators, no continuous redraw for static content | Animation-free policy and cursor-repair gate pass; broad rich-client typography/effects validation remains |
| Timing and overload | Trace input arrival/dispatch, scene readiness, render completion, submit and presentation completion; validated clock semantics, fault/overload tests, idle behavior | Opt-in stage histograms, bounded source/frame traces, held-reader tests and complete input-queue proof pass; native saturation and time-admission integration pass; causal device-to-frame trace and physical presentation timestamps remain |
| Performance evidence | Record hardware/output mode/resolution, workload, latency distributions, missed deadlines, copy traffic and steady-state memory; physical response measurement | No 240 Hz or 1 ms result established |

Execution order: first extract and prove the actual presentation lifecycle,
then introduce acquired output buffers and the backend resource boundary.
Integrate native-density configuration/allocation/rendering as a coherent change.
Hardware integration proceeds with the graphics agent as capabilities become
available. Instrument each stage during integration; final timing claims require
supported hardware and a defined foreground/background workload, not TCG timing.

The software fallback remains usable at each gate. Surface/queue exhaustion and
unknown ownership must fail explicitly without reusing potentially referenced
storage. Static effects expand damage by their sampling footprint and never
introduce an animation timer. Driver availability is an external dependency;
compositor policy, geometry, protocol and test work can proceed independently.


Status (2026-09-30): both Desktop builds pass and boot. The opt-in Mesa path
requires more memory than the 512 MiB cube fixture provides: a traced texture
cache allocation returns ENOMEM there and safely falls back. At 1 GiB, Mesa
is active and exact cube pixels, nine-frame retirement and the full animation
cycle pass. The animation rerun uses a 45-second correctness deadline. The existing renderer
remains the default; no 240 Hz or physical latency claim is made.

## Implementation and ownership

Desktop still owns windows, input, damage, decorations and application grants.
`Desktop_Composition.Plan` supplies overflow-safe source/target/damage clipping.
`Desktop_Compositor` selects a legacy or Mesa implementation at build time.
The Mesa implementation uses `Compositor_Cache` and `Compositor_Policy` for a
bounded cache and explicit failure transitions. These units are SPARK; existing
Desktop, kernel grants and Mesa are not thereby proved.

The cache holds two destination views (normal scene and drag-base scene),
at most eight client source views, and 128 separate mask views. Imports persist
across draws; geometry/stride
changes replace their descriptors. `releaseSurfaceBuffer` retires the imported
view before returning the application's grant acquisition. Mesa finishes source
reads synchronously. A client must still honor its existing immutable-buffer
obligation while Desktop owns the acquisition.

`Mesa_Binding` marshals calls through `Mesa_FFI`. The C adapter `softpipe.c` uses
the existing Gallium `resource_create_front` and a small winsys whose map
callbacks expose the caller's storage. There is no texture upload or shadow
pixel allocation. The adapter reuses Mesa shaders and rendering, unbinds source
and target references, flushes, and checks outstanding maps before reporting
quiescence. C, Mesa and the correspondence between mapped storage and grants
remain trusted, regression-tested boundaries. This is software rendering.

Desktop applications preserve opaque BGRA semantics. The standalone native
oracle also exercises explicitly requested premultiplied blending, clipping,
nearest scaling and updates to reused source storage. This does not enable
transparency in the existing application protocol.

Initialization failure or a returned quiescent draw failure disables Mesa and
uses the existing CPU blit for the affected rectangle. Unknown reads or failed
view retirement prohibit fallback and grant return: Desktop exits for restart.
A crash or stuck library call is not a recoverable returned failure. The failure
state does not establish that a supervisor will recover from every such fault.

Display submission and completion are unchanged. Output buffers become writable
only after validated completion reports the expected session/frame and Released
disposition. Switching renderers does not release in-flight or quarantined output.

## Build and test

Use the Nix shell and hold `coordination/build.lock` for native builds, shared
build-definition edits and QEMU. Build the ordinary Desktop assets first:

```sh
make -C kernel desktop
python3 tests/compositor/build-desktop.py userspace/mesa/build/native-aee5fe5697d39dd4
python3 tests/compositor/build-native.py userspace/mesa/build/native-aee5fe5697d39dd4
```

The opt-in builder compiles `desktop.gpr` with `CUBIT_COMPOSITOR=mesa`, then
links through the existing CuBit musl wrapper and native Mesa archives. It does
not stage the resulting `tests/compositor/build/desktop-mesa-none.svc`.
The default GPR scenario is `legacy`, with separate objects and no Mesa link.
Optional builder arguments `init` and `draw` create fault-injection binaries.
Draw injection returns a quiescent failure before writing pixels.

`CUBIT_DESKTOP_IMAGE` selects an explicit Desktop ELF in the headless harness's
temporary test image; the normal staged image remains the default. Use
`SOFTPIPE_IMAGE=.../tests/compositor/build/native/compositor-probe.app` with
`--test softpipe` for the native pixel oracle. Require its COMPOSITOR-NATIVE
marker as well as the harness's baseline Mesa markers.

Hosted checks:

```sh
gprbuild -p -P tests/compositor/composition.gpr
tests/compositor/build/composition_tests
tests/compositor/build/cache_tests
tests/compositor/build/damage_tests
gnatprove -P tests/compositor/composition.gpr --level=2 -j1
```

Evidence so far:

- 2,662 independent clipping cases, plus extreme-coordinate cases.
- Mock cache initialization/draw/release failures, capacity limit and 100 reuse
  cycles. Unknown access and failed release retain restart-required state.
- GNATprove: 108 analysis results (including sparse damage), zero unproved, including the instantiated
  Mesa cache. This proves checked SPARK properties under the FFI contracts,
  not raw memory validity, Mesa behavior or whole-system timing.
- Earlier native four-CPU TCG oracle: 192 draws over three contexts passed,
  with the existing Mesa baseline also passing. Log:
  `/tmp/cubit-compositor-import-qemu.log`.
- Updated oracle passes 192 draws/three contexts including release-status checks
  and noninteger scaling. Exact nearest-texel ties were removed from its reference
  cases because floating-point interpolation can select either adjacent texel.
  Noninteger 3:14 and 3:10 sampling now checks unambiguous exact opaque pixels.
  Logs: `/tmp/cubit-compositor-oracle-{run,serial}.log`.
- The CCL owner fixed the TEXT_VALUE omission. With graphics-owner approval,
  devmgr.gpr gained its missing text_operations spec/body pair. Boot now succeeds.
- At 512 MiB the opt-in Desktop safely falls back. Test-only libc wrappers
  identify a 262,368-byte Mesa texture-cache allocation failing with ENOMEM:
  `/tmp/cubit-compositor-alloc-serial.log`.
- At 1 GiB the actual Mesa compositor activates, passes 194,673 cube geometric
  pixels and nine frames with retired-buffer reuse. Its first animation test
  failed the harness's fixed 15-second deadline under TCG. This is explicitly
  not a performance PASS: `/tmp/cubit-compositor-1g-{run,serial}.log`.
- The normal (non-traced) Mesa Desktop full animation rerun passes: exact
  194,673 geometric pixels, nine initial frames, 36 animated frames, pause and
  Escape exit. Mesa-active marker present; no fallback, allocation-failure or
  restart markers. Logs `/tmp/cubit-compositor-final-{run,serial}.log`.
- Initialization-failure and quiescent-draw-failure binaries both pass the cube
  screenshot and retirement fixture through the expected CPU fallback:
  `/tmp/cubit-compositor-fault-{init,draw}-{run,serial}.log`.
- Default Desktop viewer fixture passes exact synthetic RAM pixels and grant
  retirement: `/tmp/cubit-gpu-viewer-native-run.log`. This is not Intel rendering.
- `CUBIT_COMPOSITOR_ALLOC_TRACE=1` enables test-only libc allocation wrappers in
  build-desktop.py and writes a separate `-trace.svc` binary. Normal builds do
  not contain the wrappers. This diagnostic was built and run successfully.
- The normal Desktop build has neither Mesa_FFI nor Mesa_Binding symbols.
  Performance/memory comparisons and physical-display latency remain pending.

For the full animated cube correctness fixture, hold the shared build lock and
use the Nix shell with the explicit Desktop binary:

```sh
QEMU_MEMORY=1G \
CUBIT_DESKTOP_IMAGE="$PWD/tests/compositor/build/desktop-mesa-none.svc" \
MESA_WINDOW_IMAGE="$PWD/userspace/mesa/build/native-aee5fe5697d39dd4/native-mesa-cube.app" \
MESA_WINDOW_SCENE=cube MESA_WINDOW_ANIMATION=1 \
MESA_WINDOW_ANIMATION_WAIT_SECONDS=45 \
bash tests/headless/run.sh --test mesa-window --accel tcg,thread=multi \
  --timeout 90 --serial /tmp/cubit-compositor-final-serial.log --keep-logs
```

The animation wait defaults to 15 seconds and validates a 1..3600-second
integer override. The 45-second override tests correctness only. Besides the
harness PASS, require `desktop: Mesa imported-surface compositor active` and
reject any `CPU compositor fallback`, `COMPOSITOR-ALLOC` failure or restart marker.

## Bounded sparse output damage

Desktop retains up to eight non-overlapping rectangles per output in the
SPARK `Compositor_Damage` package. A contained repeat adds no work. Overlap
or list exhaustion collapses to the conservative bounding box. Contracts prove
that every old/new dirty region remains covered, rectangles do not overlap,
and their envelope is exactly the old bounding box union the new damage.
Thus sparse copies stay within the previous copy envelope and do not duplicate
pixels. The queue has fixed storage and does not allocate.

`pumpOutput` copies or scales those regions only after the output is Available.
It submits the same bounding rectangle through the existing Display protocol
and clears the list only after successful submission. Layout changes queue a
full replacement; uncertain completion still quarantines the transfer. Cursor
restore/draw already queue exact written footprints; redundant bounding-box
flushes were removed from cursor and fast client redraw paths. Existing copy
counters now count the bytes actually copied per region.

Hosted evidence: 3,200 independent pixel-grid coverage/non-overlap/envelope
checks, contained repeats, capacity overflow and Natural'Last edges. The simple
separated-update case copies eight pixels rather than its 1,024-pixel envelope.
The complete SPARK project reports 108 analysis results, zero unproved checks.
Native Mesa cube regression passes exactly 194,673 geometric pixels and nine-frame
retired-buffer reuse with Mesa active. Its first sparse diagnostic records
827,952 bytes copied versus 938,176 bytes in the old envelope: 110,224 fewer bytes
(about 12% for that particular frame, not a workload average or FPS result).
Log: `/tmp/cubit-damage-mesa-{run,serial}.log`.

The final delayed-reader input-stream test passes transfer fingerprints and
input dispatch during the actual read hold:
`/tmp/cubit-damage-delayed-verified-{run,serial}.log`. Its test-only input logging
now records every bounded dispatch: the old once-per-frame marker could precede
the hold and conceal later progress. Production mode compiles that logging out.
Production Desktop/Display staging was restored after the delayed tests.
The final production default-renderer window/drag test also passes:
`/tmp/cubit-damage-default-final-{run,serial}.log`. Its first sparse diagnostic
copies 4,256 versus 5,928 envelope bytes, again a single-frame observation.

This sparse-damage milestone removed unchanged holes from Desktop staging, not
the Display backend's own bounding-box copy. The later acquired-target integration
below adds stable slot identities, validated retirement and per-slot repair, and
removes scene-to-transfer staging for the initial compact single-output layout.

## Copy and latency objective

The target is smooth 240 Hz operation under load and a 1 ms physical
keypress-to-photon stretch goal. A 240 Hz refresh period is 4.17 ms; input phase,
scanout position and panel response prevent treating a submillisecond software
path as a universal 1 ms physical response. Measure physical response with an
input trigger and photodiode or calibrated equivalent, separately from software
input arrival, frame readiness, render completion and presentation notification.
Report distributions, worst observed latency, workload and display mode.

| Stage | Current behavior | Next reduction |
| --- | --- | --- |
| Application to scene | Mesa samples imported storage directly; composition still writes scene pixels | Direct scanout or hardware plane where eligible |
| Scene to output transfer | Initial compact single output renders into acquired storage; compatibility layouts copy/scale per-slot missing damage | Extend direct rendering to output-local scenes at native density |
| Transfer to firmware framebuffer | Copy | Retain compatibility path until real scanout exists |
| Transfer to native GPU target | Copy and previous-damage repair | Render into actual presentation storage with buffer-age damage tracking |

Overlapping surfaces may require composition even when redundant copies are
removed. Never write a scanned or in-flight buffer to bypass retirement. Keep
work bounded, preserve keyboard events, and supersede obsolete visual frames
only once their storage is unreferenced.

Desktop coalesces cursor reports within each bounded event-loop pass and paints
the latest position after input dispatch, without a millisecond pacing timer.
Scene redraws are likewise immediate after the bounded request/input drains;
the unused deferred-redraw path has been removed. These are event-driven
updates, not periodic animation ticks. The interactive scheduling contract now
advertises a 4,167 us period (240 Hz) with the existing 4,000 us budget hint; this
is advisory metadata, not a real-time reservation or measured execution time.
Output availability still controls writes to presentation transfers; a busy
output retains damage until completion. Millisecond GETTIME statistics cannot
verify submillisecond latency, and elapsed TSC ticks are not process CPU time. Validate clock behavior across CPUs before interpreting fine-grained
samples. CPU/frame/memory comparisons on an otherwise quiet host, maximum draw
duration and real-display tests remain required. Removing an upload is not
evidence that softpipe beats the existing row-copy renderer.

## Instant interactions and native-density rendering

Zero animation is the default desktop policy: focus, window geometry, menu and
state changes take effect on the next eligible presentation, with no easing,
transition duration or minimum visible dwell. Optional static shader effects
must redraw only for changed content, geometry, appearance or exposed damage;
enabling a shadow or color treatment must not introduce a continuous render
loop. Animated applications can still submit their own content.

Modern appearance must retain legible typography, consistent spacing and clear
focus/selection contrast at every supported output scale. GPU effects must be
optional and bounded, with damage expanded for any sampling footprint. They
must not delay input dispatch or prevent the simple opaque composition path.

Current output scaling resamples a logical-resolution scene. This provides
scaled layout but does not yet provide native-density text or decorations.
Desktop Get_Information continues to report unit scale for that existing client
pixel contract; changing that field alone would misrepresent the buffer model.
The native-density path needs all of the following together:

- Separate logical window bounds from backing-buffer pixel extent and pitch.
- Configure clients with the chosen output scale and a generation; retire old
  buffers before accepting reuse, including during cross-output moves.
- Rasterize fonts, icons and decorations at target density. Keep input/hit tests
  in logical coordinates and round damage outward into physical pixels.
- Compose each output at its own density, including fractional scale and windows
  spanning outputs, without enlarging a finished low-resolution desktop image.
- Verify mixed-scale transitions, edge clipping and bounded allocations before
  advertising native-density support.

These are compositor/UI responsibilities. The i915/ANV work supplies rendering
and presentation capabilities; driver availability alone does not complete DPI
support or remove the existing scene-to-output copy.

Cursor batching regression evidence (native CuBit under QEMU TCG): default
window/drag test passes; the final held-reader input-stream test passes with
input dispatched between matching hold/stable fingerprints; the final Mesa
cube test passes 194,673 geometric pixels and nine retired-buffer reuse frames
with Mesa active. Logs are `/tmp/cubit-cursor-batch-{default,delayed,mesa}-{run,serial}.log`.
Production Desktop/Display staging was restored and byte-compared to production
build outputs. Existing SPARK components were unchanged in this step; these
checks validate legacy Desktop integration, not a whole-service proof or
physical latency/240 Hz performance.

### Density layout admission

`Compositor_Density` is a pure SPARK preparation step for native-density
surfaces. It accepts separate logical dimensions, a rational scale (components
1..16), a caller-supplied byte budget and BGRA-compatible row alignment. It
rounds physical dimensions upward, aligns the pitch and checks the entire
allocation with wide arithmetic before returning usable dimensions. The result
explicitly distinguishes extent overflow from insufficient budget. It never
reduces the requested density to make an allocation fit.

Accepted results guarantee exact rounded dimensions, enough bytes for every
row, less than one alignment unit of row padding, and total storage within the
supplied budget. For supported physical extents, admission is equivalent to
fitting that budget, so a valid affordable layout cannot be spuriously rejected.
The planner performs no allocation and grants no memory access authority. The
caller must still acquire/map the backing storage and obey buffer retirement.

A 1920x1080 logical surface at 2x needs a 3840x2160 backing image and 33,177,600
bytes at four-byte row alignment. The current 16 MiB attachment limit rejects
that request; changing the attachment budget is a separate protocol/resource
policy decision. The planner does not raise existing limits or advertise DPI
support. It is not yet called by Desktop or applications: surface configure
scale/generation, client allocation, and per-output rendering must be integrated
together before enabling native-density buffers.

Reproduce the isolated hosted checks inside Nix:

```
gprbuild -p -P tests/compositor/density.gpr
tests/compositor/build/density/density_tests
gnatprove -P tests/compositor/density.gpr --level=2 -j1
```

Validation: 785,754 independent hosted admission checks passed across all 256
rational scales, ordinary and boundary dimensions, eight BGRA-compatible row
alignments (including non-powers of two), zero/exact/one-byte-short budgets and
large products beyond 32-bit arithmetic. GNATprove reports 32 analysis results,
zero unproved checks, including minimal upward rounding and exact admission.
This is hosted arithmetic/policy verification, not native DPI rendering evidence.
Log: `/tmp/cubit-density-checks.log`.

### Presentation lifecycle boundary

Desktop now stores `Compositor_Presentation.State` for each output and uses its
SPARK transitions in the actual submit/completion path. New storage starts
Closed. Opening a session authorizes writes only for newly acquired storage;
submission moves to In_Flight before the foreign call, and a failed foreign
submission quarantines it. Submission requires a strictly newer nonzero token.
A second submission while busy is rejected without replacing the tracked token.

Completion releases storage only when all of these match: In_Flight state,
valid/successful kernel completion, decoded payload, kernel token, payload frame,
payload session, published outcome and released disposition. Anything else
quarantines the state. Neither a duplicate completion nor a later plausible
packet can clear quarantine. The proof contracts preserve session/frame identity
across completion and quarantine, and specify both successful and rejected
submission transitions. No heap storage or extra pixel copy is introduced.

This proves the policy over supplied evidence. Kernel completion authenticity,
wire decoding, actual reader quiescence, mappings/grants and FFI behavior remain
trusted integration boundaries. `Open` is not authority to recycle an old buffer:
Desktop currently abandons old pages on teardown and acquires fresh storage.
Unknown completion routing and queue errors still quarantine outputs; the wire
ABI and one-transfer-per-output scheme are unchanged. A future acquired-buffer
pool must preserve these rules for each allocation and generation.

Direct-target integration constraint: `display/main.adb` currently rejects
`OP_DISPLAY_MAP_BACKBUFFER` because it would derive a Desktop loan from the
GPU service's loan to Display. Generic grants reject that operation. Preserve
this boundary: acquire access from the allocation owner (or an explicitly
validated derived-loan mechanism), with resource identity/generation and both
render and display retirement accounted for. Do not alias the private scene to
an in-flight transfer or bypass grants to eliminate a copy.

Lifecycle verification: 864 hosted completion combinations plus duplicate,
old-frame-after-new-submit, cross-output, uncertain-submit, busy-submit and
identifier-boundary traces pass. GNATprove reports 11 results, zero unproved.
Native CuBit held-reader input-stream passes stable fingerprints with input
progress; native Mesa cube passes 194,673 geometric pixels and nine retired
buffer reuse frames with Mesa active. Production staging was restored and
byte-compared with production binaries. Logs:
`/tmp/cubit-presentation-validation.log`,
`/tmp/cubit-presentation-delayed-serial.log`, and
`/tmp/cubit-presentation-mesa-{run,serial}.log`.

### Acquired-output pool policy

`Compositor_Pool` models three fixed backing allocations in a fresh epoch. At
most one is CPU-writable or rendering, one is a completed frame waiting for
presentation, and one is held by Display. Roles always refer to distinct slots;
a writable ticket cannot alias a ready or displayed allocation. Tickets carry
slot, epoch and strictly increasing serial. Frame ordering is part of the valid
state invariant: a writer is newest, a ready frame is newer than a held display
frame, and no serial wraps. Busy acquisition returns no ticket.

Starting a render removes CPU write permission before backend submission.
Confirmed render completion replaces the previous ready frame, making only
that already-quiescent superseded allocation reusable. A confirmed quiescent
render failure keeps the older ready frame. Presentation transfers the ready
role only if no frame is already held by Display; GPU completion alone never
retires Display ownership. A matching release retires it. Unknown outcomes,
wrong epochs/serials, duplicate/stale fences and invalid transitions fault the
pool without dropping tracked ownership. Faulted pools never issue new writes.

The pool does not allocate, map, submit, or present anything. Bind its three
slots to distinct verified allocations and budget their total storage before
opening a fresh epoch. Do not use Open to recycle uncertain old storage. A
partially written or failed frame requires repaint. Desktop now binds this
policy and the repaint history below to the producer-owned Display transport.
Its initial compact single-output canvas renders into the acquired writer;
scaled and multi-output scenes retain a private-canvas compatibility path.

The graphics owner confirmed that the current `Native_GPU_Presentation` path
is for completed linear application pixels forwarded read-only to Desktop.
Owner-opted-in forwarding now exists, but that does not make Display's borrowed
backbuffer a writable compositor target. Proposed integration contract, pending
agreement and implementation:

- The allocation owner identifies storage, generation, verified format/pitch and
  byte extent, and grants Desktop the required write/render access directly.
- Display receives authorized access to the same root allocation and validates
  scanout eligibility. No writable authority is invented by forwarding loans.
- Presentation identifies allocation, output generation and frame ticket;
  completed GPU writes and actual Display retirement are separate evidence.
- Start with explicitly supported linear BGRA storage. Reject unsupported
  layout/modifiers; do not interpret tiled/compressed images as linear pixels.
- Uncertain submission, device loss or output changes retain affected backing
  until both reader/writer lifetimes are proven retired. No blind retries.

No new GPU driver wire ABI has been added. The Display-only transport below
supports caller-owned CPU buffers; GPU-owned allocation sharing and physical
presentation evidence remain outstanding.

Pool verification: 3,000 hosted held-display cycles pass, with four acquisitions
per cycle, newest-ready replacement and independent checks against GPU/display
held slots. Additional traces cover overlapping rendering/presentation,
quiescent failure preserving ready content, unknown completion, wrong epoch,
wrong serial, duplicate transitions and stale fences after slot reuse.
GNATprove reports 22 results, zero unproved, including strict frame ordering and
exclusive writable storage. This is policy verification; it does not establish
native target sharing or driver fence correctness. Reproduce with the Nix shell,
`gprbuild -p -P tests/compositor/pool.gpr`,
`tests/compositor/build/pool/pool_tests`, and
`gnatprove -P tests/compositor/pool.gpr --level=2 -j1`.
Log: `/tmp/cubit-pool-checks.log`.

### Bounded repaint queues and native pooled-target oracle

`Compositor_Repaint` keeps one bounded sparse damage queue for each of the three
pool slots. Opening a new extent marks every target fully dirty. Every scene
change is added to all queues. Taking a target's work snapshots and clears only
that queue *before* rendering; changes arriving afterward remain pending and
completion never clears them. A partial or failed render invalidates the whole
selected target. Layout changes require a fresh fully dirty tracker. This
avoids an unbounded frame-history log while accounting for slots skipped across
multiple frames or superseded while waiting for Display.

SPARK contracts establish bounded valid damage, coverage of new and previously
pending regions, isolation of other slots when taking work, and full repair after
failure. The caller still must provide complete scene invalidation (including
old/new geometry and effect footprints), use a pool-authorized writer, and
report render failures accurately. Dirty metadata may change while a target is
held; its pixels may not. Hosted verification uses 600 independent pixel-grid
checks including post-snapshot invalidations, slot reuse and failed renders.
The repaint project and its damage/pool dependencies report 75 SPARK results,
zero unproved checks.

The native compositor oracle now calls `native_pool_test.adb` after its original
192-draw test. It imports three separate targets into the existing Mesa adapter,
renders a moving colored surface using clipped background/foreground draws,
retains a display-role target across multiple frames, and checks all completed
pixels against an independent scene oracle. Five of 96 frame attempts abort
after a real completed background draw, forcing full repair on reuse. Held
pixels are compared with a test-only reference snapshot. Pool display release
is simulated here; Mesa draws execute inside CuBit, but there is no real Display
scanout or GPU-backed allocation sharing in this oracle.

Native result: PASS96 frame attempts, three imported targets, exact completed
pixels and stable held targets. Repaint work covers 19,055 pixels versus 98,304
for full-buffer redraw of every attempt; this counts background repair area,
not all shader writes, bandwidth, FPS or physical latency. Existing 192-draw
compositor checks and native softpipe pixel/triangle tests also pass. Logs:
`/tmp/cubit-repaint-checks.log`, `/tmp/cubit-repaint-native-run.log`, and
`/tmp/cubit-repaint-native-serial.log`.

Reproduce hosted checks with `tests/compositor/repaint.gpr` and its
`build/repaint/repaint_tests` executable, then `gnatprove --level=2 -j1` for that
project inside Nix. Rebuild the native probe using the existing Mesa archives
with `tests/compositor/build-native.py`, and use its `compositor-probe.app` as
`SOFTPIPE_IMAGE` for `tests/headless/run.sh --test softpipe` under the shared lock.
Require both COMPOSITOR-POOL and COMPOSITOR-NATIVE success markers. Desktop
integration now uses both policies as described below; output-local scenes
at native density remain required for the complete multi-output design.

### Opt-in live Desktop software timing

Build Desktop with `-XCUBIT_COMPOSITOR_TIMING=on` to select the timing policy
and separate `build-timing` output (or `build-mesa-timing` for the Mesa scenario).
The default is off: no timing clock reads or timing log records. The dedicated
Mesa link helper currently targets its normal `build-mesa` directory; extending
that helper to a timing variant remains necessary before combining those modes.
For the tested legacy renderer, build through `alr exec -- gprbuild` from kernel
and pass the resulting binary through the existing `CUBIT_DESKTOP_IMAGE` test
override. Do not overwrite production staging to enable tracing.

Instrumentation uses `CuBit.Monotonic.Read` microseconds, not GETTIME's epoch,
UTC or raw TSC conversion. `Compositor_Elapsed` rejects unavailable and backward
samples while retaining valid zero durations. Its arithmetic contract is proved
(three analysis results, zero unproved). Twelve explicit boundary examples cover
zero duration, a 4,167 us interval, crossing 32-bit timestamps, high 64-bit values
and invalid clocks. The existing bounded `CuBit.Timing_Histograms` implementation
stores at most one million samples per stage/report window. Invalid samples and
saturation drops are counted explicitly, with saturating diagnostic counters.

Stages are input-handler wall duration, request-handler wall duration, drawing
(full/partial/fast-client/cursor paths), the asynchronous submit call, and
successful matching submission-to-completion wall duration. The last stage starts
at submit-call entry and includes queueing and any delayed Desktop dispatch of
the completion. Request duration may include drawing, so stages overlap and must
not be summed. Input-handler duration excludes upstream input-device latency and
does not causally associate an application response with that keypress. GPU render
fences, physical scanout timing and input-to-visible-frame correlation are still
missing. Microsecond units do not certify clock accuracy, suspend continuity or
cross-CPU behavior; instrumentation/syscalls/logging also affect execution.

Reports reuse the existing statistics cadence and do not add redraw timers.
Each stage reports sample count, min/max and p50/p99 bucket upper bounds,
invalid count and saturation drops. The serial console is not a cross-CPU
structured logging channel. `tests/compositor/check-timing.py` rejects malformed,
missing, inconsistent, invalid-clock and saturated evidence, and retains separate
report windows: it never averages quantiles into a fake aggregate percentile.
Its fixture tests cover missing/duplicate fields and corrupted bounds/counters.

Native QEMU TCG window/drag regression passes with the instrumented Desktop.
Validated evidence contains 28 input, 40 request, 30 draw, 20 submit-call and 20
matching completion samples, with zero invalid/dropped records. This validates
instrumentation integration, not the 240 Hz or physical latency objective.
Some input-handler intervals were long in this workload; synchronous menu-config
reads and process-launch calls in the handler are concrete next audit targets,
not a proven attribution of every observed delay.

Logs: `/tmp/cubit-timing-build.log`, `/tmp/cubit-timing-native-run.log`,
`/tmp/cubit-timing-native-serial.log`, `/tmp/cubit-timing-native-report.json`,
`/tmp/cubit-timing-report-tests.log`, `/tmp/cubit-elapsed-final-tests.log`.
Production Desktop/Display staging remained the default off build and was
byte-compared with production outputs. Physical measurements and clock validation
on the reference hardware remain outstanding.

### Asynchronous application launch

Desktop submits process launch asynchronously and continues dispatching input
and presentation completions while Procmgr loads the executable. One persistent
4 KiB filename grant and one pending launch bound the work; additional launch
attempts are declined while busy. Completion updates the single-instance PID
using the captured program name, rather than a menu index that may have changed.

`Compositor_Requests` proves nonreused shared launch/presentation tokens (zero
and the kernel sentinel excluded, no wrap), one outstanding request, and sticky
quarantine on an unconfirmed or mismatched completion. Desktop validates the
kernel completion and the operation-specific reply envelope before confirming
completion. Uncertain submission or completion retains the filename storage;
there is no blind retry or synchronous fallback. Presentation routing accepts
gaps in its frame tokens, and launch completion is routed before the retired
display-token filter.

The proof covers the extracted request policy, not the legacy Desktop service,
kernel completion queue or grant implementation. Procmgr's audited terminal
reply follows its filename read and permits storage reuse under that trusted
service contract; it does not revoke the legacy grant mapping. The permanent
buffer remains allocated, and is never rewritten while pending or quarantined.

Hosted tests cover 3,000 interleaved launch/display allocations plus busy,
replay, mismatched completion, sticky uncertainty and exhaustion cases.
`tests/compositor/requests.gpr` reports ten SPARK analysis results, zero unproved.
The instrumented native CuBit window/drag regression passes; its trace records
launch submission, input dispatch and launch completion in that order with the
same token. `tests/compositor/check-launch.py` checks those markers and rejects
uncertain launch/presentation evidence. This demonstrates input progress while
launch is pending, not physical input latency or a controlled speed comparison.

Logs: `/tmp/cubit-async-launch-build.log`, `/tmp/cubit-async-launch-run.log`,
`/tmp/cubit-async-launch-serial.log`, `/tmp/cubit-async-launch-check.log`, and
`/tmp/cubit-async-launch-timing.json`. Default Desktop/Display production staging
was byte-compared with production outputs. Settings writes and display-layout
calls still require separate input-path work; menu refresh is addressed below.

### Asynchronous launcher configuration refresh

Opening Apps now draws its cached menu immediately and requests a background
refresh. Startup still loads the initial menu synchronously. The refresh reads
the key list and up to fourteen bounded values using a separate generation-
checked Config grant. It submits at most one request per main-loop pass, after
the first input drain, and never has more than one Config request outstanding.
Repeated opens coalesce into one additional refresh. A complete candidate is
published only while Apps is closed; entries do not move beneath a pointer or
keyboard selection. Thus configuration changes appear on a subsequent opening
after refresh completes. Empty/invalid refreshes preserve the last usable menu,
matching the previous loader. Single-instance PIDs follow program names across
reordering, including launches that complete during refresh.
Publication swaps a complete local candidate; Config does not provide a
transactional snapshot across the sequential key/value reads.

`Compositor_Refresh` proves bounded sequential traversal, publication only after
all selected items have been read, refusal to publish while visible, and retention
of a coalesced refresh request across publication. The fourteen-item proof
instance has eighteen analysis results, zero unproved checks. Hosted policy
tests cover 4,500 complete snapshots with repeated requests and visible holds.
`Compositor_Requests` supplies the separate proved transport-token/lifetime
policy already used by application launch. Both routes share Desktop's global
nonreused token allocator with presentation.

The adapter (`Desktop_Launch_Refresh`) is an audited, non-SPARK integration
boundary: message marshalling, imported grant storage and the existing tested
menu parser remain outside the proof. It reserves three pages once at startup
to expose two aligned payload pages, with fixed key/candidate storage. No refresh
allocates or queues another buffer. Success, access-denied, not-found and list-
capacity replies are accepted only with a valid completion/envelope; the audited
Config handler returns its acquisition before those replies. Generic F001 can
mean acquisition return failed, so it quarantines the buffer rather than permitting
reuse. Malformed/stale/failed completions and uncertain submission also retain
storage indefinitely. This is a service-contract assumption about remote access,
not a proof of kernel grant behavior. Hosted transport tests compile the actual
adapter and parser with mocked allocation, grants and completion submission.
They cover normal ordering/publication/coalescing plus allocation failure,
submission failure, generic error, stale token, malformed envelope, oversized
count and invalid kernel completion; faulted storage stays unchanged even after
a later plausible reply.

Native CuBit QEMU TCG window/drag regression passes with the instrumented
Desktop. A nine-entry candidate completes while Apps is open and publishes
after the launch action closes it. The shared completion queue also passes the
input-during-launch regression and all five timing-stage checks. The refresh
trace checker is `tests/compositor/check-refresh.py`; publication ordering and
visible holds are independently covered by the hosted policy/adapter tests.
This is functional integration evidence, not a 240 Hz or physical latency result.
Production Desktop/Display staging is byte-compared with the default off builds.

The scheduling and adapter test projects are `tests/compositor/refresh.gpr` and
`tests/compositor/refresh_transport.gpr`; run them in Nix. Logs:
`/tmp/cubit-menu-refresh-final-build.log`, `/tmp/cubit-menu-refresh-native-run.log`,
`/tmp/cubit-menu-refresh-native-serial.log`, `/tmp/cubit-menu-refresh-check.log`
and `/tmp/cubit-menu-refresh-timing.json`. Physical latency, settings writes and
display-layout calls remain separate work.

### Root-owned three-slot Display transport

`CuBit.Display_Pool_Protocol` adds a bounded producer-owned buffer pool to
Display. This is independent of Intel GPU allocation authority: the submitting
process must own each grant. Borrowed GPU pages cannot be regranted through this
contract. New operations use the existing authenticated Display endpoint:

| Operation | Label | Payload |
|---|---|---|
| Register slot | `0910` | Existing attachment words: grant slot/generation, packed width/height, pitch; flags identify slot 1–3. |
| Open pool session | `0911` | Four zero words, zero flags; success returns status 0, fresh session, slot count 3, version 1. |
| Submit pool frame | `0912` | Existing session/frame/inline damage words; flags identify slot 1–3. |

Output selection remains in the existing reserved routing field and is validated
before payload decoding. Pool completions use `0912`, echo slot in flags, and
return session/frame/outcome/buffer disposition. The producer must match all of
those to a successful kernel completion token. A valid codec result alone never
authorizes a write. Output discovery retains `090D/090E`; a cross-protocol test
checks opcode disjointness.

`CuBit.Display_Pool_Registry` admits exactly three distinct grant identities with
the same valid layout, refuses duplicate slots/references and mutation during a
live session, and retains existing registrations on rejected changes. Its SPARK
contracts prove admission/count growth, preservation of prior slots, full-pool
opening and sticky quarantine. The codec and existing dependencies are also
checked for runtime safety. Distinct grant identities do not prove disjoint
physical storage: the producer must bind the compositor pool's slots to separate
owned backing allocations. Kernel ownership, mapping rights and actual device
retirement remain trusted integration boundaries.

Display retains one read acquisition per registration and acquires another for
each frame, validating live grant authority even after registration. A revoked
registration cannot admit a new frame. The existing presentation state machine
still allows one frame in flight per output; a busy request is rejected without
changing the held slot. Completion returns the frame acquisition, while pool
release returns registration pins. Failed/uncertain backend work quarantines the
output. A failed acquisition return now prevents a successful lease-release
acknowledgement. Legacy attachments and pool registrations cannot be mixed.

The native `display-check` fixture rotates independently owned buffers, validates
exact slot/session/frame/disposition replies, checks replay/out-of-bounds and
revoked-grant rejection, and verifies all pins retire after release. With the
existing delayed GPU fixture it also tests a responsive query, rejected release
and replacement, and a second-slot submission during an outstanding frame; the
unused slot remains independently writable. Native CuBit firmware-framebuffer
and delayed virtual-GPU tests both pass. The latter also retains the existing
two-output scanout pixel checks and verifies another output completes while
the first stalls. Pool frames rotate on output zero in this fixture; native
simultaneous pools on multiple outputs remain an integration test requirement.

Successful runs: `/tmp/cubit-display-pool-native-final-run.log` and
`/tmp/cubit-display-pool-native-final-serial.log` (firmware),
`/tmp/cubit-display-pool-stalled-run.log` and
`/tmp/cubit-display-pool-stalled-serial.log` (delayed GPU). Default Display,
display-check, virtual-GPU and Desktop staging were byte-compared with production
outputs after the delayed test restored production binaries. No hardware Intel
scanout, physical latency or 240 Hz result is claimed.

Reproduce the portable checks with
`nix develop -c bash tests/compositor/check-display-pool.sh`. The project copies
only portable runtime sources into an isolated directory to avoid shadowing the
host GNAT runtime. The final report currently contains 318 analysis results,
zero unproved, including the existing desktop/display/discovery dependencies.

This transport does not by itself eliminate Desktop staging copies. The Desktop
integration below removes that copy for the initial compact single output.
Firmware and current
virtual-GPU presentation may still copy into scanout; direct hardware scanout,
GPU-owned targets, native-density rendering and physical latency remain separate
integration requirements.

### Desktop integration with acquired CPU targets

Desktop registers three distinct root-owned, read-only Display grants per output.
Its writable canvas is never the Display-held slot. A completed synchronous
CPU/Mesa render becomes presentable; the matching slot/session/frame completion
must confirm release before that slot is reusable. Unknown completion or submit
outcomes terminate the compositor without granting further write permission.
The extracted SPARK policies retain their proof boundaries; the Desktop adapter,
allocation identities, grant mappings, Mesa and service contracts remain trusted
and are tested natively, not proved as a whole service.

For the initial single output with compact rows, Desktop draws directly into
the acquired target. Acquisition switches the writable address and invalidates
the previous slot's cursor underlay without painting. After input/request drains,
pending partial work triggers repair of the slot's missing scene regions and
cursor footprint, followed by capture of a clean cursor underlay. Idle targets
are left alone; an upcoming full-scene redraw skips redundant preparation.
Repair draws do not manufacture new presentation damage. The `repair_px` counter
and opt-in Scene_Draw timing include this catch-up work. Zero staging bytes do
not mean zero rendering work or zero copies elsewhere in the stack.

Multi-output, padded-row and rearranged/scaled scenes use the existing private
logical canvas. Each slot copies its own accumulated missing regions, so rotating
targets cannot expose stale unchanged areas. Applying a layout fully invalidates
every target. Scaled output still resamples logical pixels; this is compatibility,
not crisp native-density rendering. A later return to the initial layout currently
stays on this compatibility path.

Storage is bounded to three targets per admitted output, one reserved private
scene and the existing optional drag layer. The reserve permits layout changes
without allocating in Apply. This adds two output targets over the old transport;
the legacy allocator does not reclaim these pages on session replacement. A
lifetime pixel-storage ledger now bounds those allocations as described below.
Whole-compositor memory admission and reclaimable target allocation remain outstanding.

Native Mesa readiness now checks the exact expected final pixels within a bounded
20-second observer timeout, instead of sleeping two seconds after an app present
acknowledgment. Acceptance acknowledges scheduling, not presentation. The old
capture was verified against the previous frame's geometry; the new run matches
all 194,673 final-frame samples without changing the geometric oracle. This
TCG correctness allowance is not a performance target or physical latency result.

`Compositor_Repaint.Preparation_Required` proves that idle work and a guaranteed
full repaint do not request preparation; pending partial work prepares exactly
when the writer has queued stale regions. Desktop supplies those demand/full-area
facts and the authorized writer, so that adapter correspondence remains outside
the proof. The queue is never cleared merely because preparation was deferred.
An independent hosted image model passes 800 exact frames through three poisoned
initial targets and 200 idle gaps with no target writes. The existing 600
dirty-grid checks also pass. SPARK reports 77 results, zero unproved, including
the pool/damage dependencies: `/tmp/cubit-deferred-repair-checks.log`.

The delayed-reader rerun passes with input between matching hold/stable markers
and zero scene-to-transfer bytes (`/tmp/cubit-deferred-repair-held-final-run.log`).
It exposed a fixture assumption: the stress publisher marks retries for
resynchronization as well as injecting a deliberate skipped sequence, so a
literal `source_gap=1` is not a valid overloaded-run invariant. The publisher now
reports its actual successful resynchronization count. The headless gate requires
an exact match across all Desktop telemetry intervals and zero rejected reports;
it also aggregates the existing 20-present/160-input-request limits across the
whole run instead of examining one bucket. `test-input-stream-check.py` covers
split intervals and seven mismatch, rejection, budget or malformed-evidence cases.

The deferred-preparation Mesa run also passes 194,673 exact final-frame samples
and nine-frame immutable-buffer reuse, with zero staging bytes across 14 observed
submissions (`/tmp/cubit-deferred-repair-mesa-run.log`). It records 3,335,172 repair
pixels. This is evidence of the exercised work, not a before/after latency result:
frame coalescing and the observed number of submissions differ between runs.
The production software window/drag regression also passes with zero staging
bytes (`/tmp/cubit-deferred-repair-default-run.log`), covering the retained drag
layer and cursor underlay after deferred acquisition. Production Desktop and
Display staging were compared with their final build outputs.

Native single-output evidence:

- Default software window/drag: `/tmp/cubit-direct-pool-default-run.log`.
- Delayed reader, stable held-buffer fingerprints and input progress:
  `/tmp/cubit-direct-pool-held-run.log` and its `-serial.log` counterpart.
- Mesa final pixels and nine retired-buffer frames:
  `/tmp/cubit-direct-pool-mesa-final-run.log`; the direct-path checker observes
  16 submissions, 6,350,860 catch-up pixels and zero staging bytes. These counters
  cover that run, not a normalized benchmark or a bandwidth comparison.

Run `python3 tests/compositor/check-direct.py SERIAL_LOG` inside Nix to require
the direct-path marker, released frames, repair work and zero staging counters.
Use it alongside native pixel and held-buffer checks; the counters alone do not
establish visual correctness or ownership safety.

For the complete compatibility layout fixture, set `CUBIT_TEST_MIXED_OUTPUTS=1`,
`CUBIT_TEST_ARRANGEMENT=1`, `CUBIT_TEST_PRIMARY=1` and `CUBIT_TEST_SCALING=1`
when running `tests/headless/run.sh --test desktop-dual-output`. Its 1280x720
secondary permits 150% scaling while respecting the 800x480 logical-workspace
floor; 1024x768 does not. The observer rejects incompatible fixture options.
Settings navigation wraps backward from the first menu entry to avoid depending
on the number of installed programs preceding it.

The mixed-output native run passes split dragging, per-monitor maximize,
wallpaper/cursor restoration, Settings navigation, above/left/below/offset
arrangements, primary migration, 125/150% scaling and rejection below the logical
workspace floor. It also checks unchanged physical modes, repaired scaled cursor
pixels, primary reflow and mixed-scale seam crossing. Evidence:
`/tmp/cubit-direct-pool-mixed-final-run.log` and
`/tmp/cubit-direct-pool-mixed-final-serial.log`. This validates two simultaneous
Desktop pools and the resampled compatibility path, not native-density UI.

### Retained CPU pixel-storage admission

Historical milestone: the retained `sbrk` ledger below has been superseded by
the native owned-pixel integration described at the end of this document. Its
old charge totals and alignment allowance do not describe the current adapter.

`Compositor_Storage_Budget` now bounds the combined Desktop-owned output targets,
private scene reserve and drag layer for the process lifetime. Its ceiling is
eight maximum-size protocol buffers, each including page rounding and an extra
alignment page: currently 134,250,496 bytes (128 MiB + 32 KiB). This is a ceiling,
not a preallocation. A compact 1024x768 output uses five allocations totaling
15,749,120 charged bytes. Three targets per output plus the two scene layers give
eight allocations in the two-output case.

Admission reserves the entire request before calling the allocator and admits
only one provisional request at a time. A confirmed successful allocation commits
the charge; later grant/registration failure and retired sessions keep it. Only
confirmed allocator rollback cancels the provisional charge. An unsettled request
retains its charge and blocks further reservations. There is no operation to
refund committed storage, and Desktop never resets the ledger during setup or
release. This prevents repeated failed setup from accumulating unbounded pixel
storage behind the fixed three-slot presentation model.

The native adapter relies on `IPC.handleSbrk` rolling back new physical mappings
before returning failure. That kernel/FFI correspondence remains a trusted boundary;
the ledger proof does not prove the kernel allocator. A future owned-memory adapter
can use the existing allocate/release syscalls, but must first retire Mesa target
views and Display/grant readers and account for any still-pinned storage before
replenishing the budget. No kernel or driver allocator was rewritten here.

The hosted test exercises 131,584 allocation attempts with confirmed rollback,
retained charges after later setup failure, outstanding-request rejection and
integer-limit cases. It checks 49,152 page-boundary payloads against every possible
alignment offset. SPARK reported 25 results, zero unproved. That superseded
ledger and its test project have been removed; current accounting tests use
`storage.gpr` below. Historical evidence:
`/tmp/cubit-storage-budget-final-checks.log`.

Native two-output validation passes dragging, maximize, cursor restoration and
Settings with eight allocations totaling 47,955,968 bytes under the derived
134,250,496-byte ceiling. An isolated `CUBIT_COMPOSITOR_STORAGE=limited` build
sets a 35 MiB fixture ceiling: seven allocations totaling 34,238,464 bytes fit;
the optional drag layer is refused before allocation. The same native visual
tests pass using the uncached drag fallback. The fixture uses its own
`build-limited-storage` output and `CUBIT_DESKTOP_IMAGE`; production remains the
default GPR policy. Logs: `/tmp/cubit-storage-budget-final-run.log` and
`/tmp/cubit-storage-budget-limited-run.log`. These historical logs use the old
alignment charges; the current evidence checker expects owned-memory charges.
This exercises real budget denial and fallback; physical allocator OOM rollback
is established by the kernel contract and hosted settlement tests, not injected
by this native fixture.

This ledger does not account for Mesa heaps, fonts/metadata, borrowed application
buffers or retained storage in other processes. Whole-compositor admission and
reclaimable target storage remain unfinished. The current protocol's 16 MiB
per-buffer limit also excludes a single 3840x2160 BGRA buffer (33,177,600 bytes);
larger native-density outputs require a coordinated change to allocation, grant
and backend limits, not merely a larger compositor ledger.

### Imported target retirement before storage release

`Compositor_Cache.Forget_Targets` retires the normal and drag destination views
without shutting down Mesa or evicting unrelated source imports. On confirmed
retirement both target slots are empty. A failed release stops the walk, retains
the failed and unvisited views, and requires restart; already completed releases
are not repeated. Desktop invokes the backend operation before releasing its
Display leases. The legacy backend has no imported target objects to retire.

This retires Mesa view objects, not Display ownership or physical storage. Other
source views/contexts must not alias the allocation being freed: Desktop's
root-owned targets and client-owned source grants are separate allocations.
That mapping/authority correspondence and the C release/quiescence implementation
are trusted boundaries. The operation alone neither frees sbrk storage nor refunds
committed storage charges. It establishes the missing renderer-side prerequisite
for a subsequent reclaimable allocation adapter.

Hosted checks cover preserved sources/context, duplicate retirement, re-import
and failures on either target release. The native Mesa oracle passes 32 cycles
with two target arrays reused at the same addresses under alternating strides,
changed source pixels, retained source imports and exact destination/padding
checks. The prior 192-draw and three-target repair oracles also pass. Evidence:
`/tmp/cubit-target-retirement-native-run.log` and its serial log, including the
`COMPOSITOR-TARGETS` marker.

Run proof with **`gnatprove -P tests/compositor/composition.gpr -U --level=2 -j1
--report=all`** inside Nix. `-U` is required to cover the Mesa instantiation and
backend adapter outside the hosted mains' dependency set. A default invocation
left old cached results for those units; that report was rejected as evidence
for this change. The explicit all-unit run reports 120 results, zero unproved,
including the current `Mesa_Cache.Forget_Targets` and
`Desktop_Compositor.Forget_Targets` contracts. Log:
`/tmp/cubit-target-retirement-proof-all.log`.

The real Desktop teardown also passes with the Mesa backend in native CuBit
under QEMU TCG. Set `CUBIT_TEST_TARGET_RETIREMENT=1` on the `mesa-window` fixture:
after the exact cube screenshot check, the runner closes the client, waits for
its exit acknowledgment and sends Q to the unfocused Desktop. The fixture
requires `desktop: renderer targets retired`. The run checked 194,673 geometric
pixels and reached that marker without a retirement/release failure. Evidence:
`/tmp/cubit-target-retirement-desktop-retry-run.log` and its serial log.
The first attempt stopped in an unrelated CCL dependency build; the retry built
successfully without CCL edits. Production Desktop/Display staging was restored
and compared with their built binaries. This validates renderer retirement in
the real service path, not physical storage reclamation or hardware latency.

### Display and grant retirement before record reuse

Desktop `closeOutput` now uses `Compositor_Readers`: release the output lease
first, then revoke and confirm retirement of each granted target. The Display
reply must have the expected operation, one-word length, zero flags/reserved
fields and success status. Grant revocation acceptance alone is insufficient:
`Memory_Grants.Retirement_Confirmed` must also confirm the exact generation is
retired. Any failed or uncertain confirmation stops the bounded walk. Desktop
exits without clearing the output record or authorizing reuse. Already confirmed
predecessors are not retried by the coordinator; untouched and failed entries
remain pending. Successful repeated retirement makes no external calls.

This covers partial output setup as well as normal teardown. The hosted suite
checks all eight three-target grant masks, both lease states and all four
possible failure positions plus success (80 cases). The explicit SPARK proof
instance reports 14 results, zero unproved. It proves the coordinator's stated
state contracts and loop invariants for either callback result; callback
truthfulness/return, syscall and IPC behavior, grant identity correspondence,
and the effectful Desktop adapter remain trusted boundaries. The callback count
is bounded, but a synchronous Display RPC is not a proved wall-time deadline.
Reproduce with `tests/compositor/readers.gpr`, `reader_tests`, then
`gnatprove -P tests/compositor/readers.gpr -U --level=2 -j1 --report=all` inside
Nix. Logs: `/tmp/cubit-readers-checks.log` (hosted checks) and
`/tmp/cubit-readers-proof.log` (explicit instance proof). The initial proof
invocation had no selected SPARK instance and failed; it is not proof evidence.

These confirmations do not themselves free pixel storage or refund its charge.
An owned-memory adapter must still release the exact allocation and settle the
budget only after that release succeeds. The current `sbrk` storage remains
retained, and Mesa retirement is still a separate prerequisite.

The owned-memory allocation failure contract needs different accounting from
the current `sbrk` adapter: `Process.Owned_Memory.Allocate` can return zero after
an unsuccessful prefix cleanup leaves quarantined storage. Its result does not
distinguish that case from a fully rolled-back failure. Therefore a future
adapter must retain the failed request's charge (or obtain stronger kernel
evidence), rather than call `Complete_Allocation(False)` solely because the
returned address is zero. No kernel change is required to retain that charge.

Native run `/tmp/cubit-readers-native-seeded-run.log` passes the 194,673-pixel
cube oracle and teardown, with `desktop: renderer targets retired` followed by
`desktop: output readers retired= 0` in its serial log. The latter requires
retirement confirmation for all three real target grants. The run rebuilt the
kernel and used the new Mesa Desktop, but retained the already-staged devmgr
binary using `MAKEFLAGS='-o devmgr'`: the graphics owner's in-progress allocator
dependency was not yet listed in devmgr's GPR. The initial unseeded run stopped
at that build error. This is compositor integration evidence, not validation of
the graphics owner's unfinished changes. Production Desktop/Display staging was
restored and compared against the built binaries.

### Reclaimable allocation identity ledger

`Compositor_Storage` is the SPARK accounting core used by Desktop's owned-memory
adapter. It tracks eight pixel
allocations, charges before allocation and issues a strictly increasing identity
with each successful reservation. Freed table slots can be reused; their old
tickets cannot identify the replacement allocation. Identity exhaustion refuses
further reservations instead of wrapping. `Open` is only for a fresh process
ledger, never a way to reset live accounting or reuse old identities.

Allocation success changes a reserved entry to live. Allocation failure retains
its charge and table slot as quarantined because prefix cleanup can be uncertain.
Release can begin only for a live ticket with explicit reader-retirement evidence.
The bytes remain charged while release is pending. A confirmed release subtracts
exactly that ticket's bytes and invalidates it; a failed release quarantines it
without a refund. Preconditions require adapters to reject stale tickets and
invalid phases before issuing effects. The policy does not establish the truth
of external quiescence or allocator completion evidence, or the correspondence
between an allocation ticket and a native address/size pair.

The explicit `Storage_Model` instance reports 35 SPARK checks, zero unproved,
including capacity/arithmetic safety, exact charge transitions, increasing
identity issuance and preservation of other allocation records. Hosted tests
pass 4,096 release/reallocation cycles beside a live neighbor, retained allocator
and release failures, full descriptor exhaustion, maximum-integer charge/refund
and a reduced identity space that exercises terminal identity exhaustion.
Reproduce with `tests/compositor/storage.gpr`, its `storage_tests` executable,
and `gnatprove -P tests/compositor/storage.gpr -U --level=2 -j1 --report=all`
inside Nix. Final evidence: `/tmp/cubit-storage-identities-verified.log`.
An initial empty report and later three unproved contract-evaluation checks were
rejected; the explicit SPARK instance and short-circuit preconditions are present
in the final verified sources. No native-memory reclamation claim follows from
these hosted tests.

### Native owned pixel storage

Desktop now allocates output targets, the private scene reserve and the optional
drag layer with `SYSCALL_ALLOCATE_OWNED_MEMORY`, using the existing kernel API.
All three categories retain the exact returned base and a nonreused ledger
ticket; the ledger retains the exact rounded size. There is no extra alignment
page. The production ceiling is eight 16 MiB buffers (128 MiB), not an up-front
allocation; the existing native per-allocation/protocol limit remains 16 MiB.
The old `Compositor_Storage_Budget` is no longer used by Desktop.

Normal teardown first removes Mesa destination imports, then releases each
Display lease and confirms retirement of its target grants. Only then does it
release that output's allocations. Private scene and drag storage follow after
all outputs retire. Each native release must return success before the ledger
refunds the matching charge. Stale/mismatched tickets, malformed allocations,
uncertain view/grant retirement or failed native release terminate the compositor
without authorizing reuse. Partial output-setup cleanup follows the same ordering.
A failed allocation remains charged and quarantined, including its bounded
descriptor, even if a smaller fallback allocation is subsequently attempted.

Native Mesa test `/tmp/cubit-owned-pixels-native-run.log` passes 194,673 geometric
cube pixels, client exit and Desktop teardown. Its serial log shows five exact
3,145,728-byte releases after renderer/output-reader retirement and a final zero
pixel charge: 15,728,640 bytes reclaimed. Check it with
`tests/compositor/check-storage-budget.py SERIAL_LOG single --retired`; that
mode checks retirement ordering, all release sizes and each declining charge.
This run uses the matched current devmgr/intel-gpu staging pair, not the older
device-manager seed used by the preceding reader-only test.

The ledger and retirement coordinators have their documented SPARK proofs;
the native address/ticket table, syscall adapter and whole Desktop remain
effectful, regression-tested boundaries. This is a native CuBit QEMU correctness
result, not physical hardware performance or repeated Desktop-session reuse
evidence. Mesa heaps, borrowed client storage and other service memory remain
outside this pixel ledger.

The production software Desktop also passes the native mixed-output fixture
with eight allocations totaling 47,923,200 bytes under the 134,217,728-byte
ceiling. The run checks split dragging, per-output maximize, wallpaper/cursor
restoration, Settings, primary migration, above/left/below/offset arrangements
and 125/150% compatibility scaling. Evidence:
`/tmp/cubit-owned-pixels-mixed-run.log` and its serial log. These scaling checks
do not establish native-density client rendering; that remains a separate gate.
The release evidence checker passes one valid fixture and nine negative controls
in `tests/compositor/test-storage-evidence.py`, rejecting missing retirement,
release-before-retirement, wrong sizes/refunds, duplicate/missing releases and
premature zero-charge markers.

The 35 MiB limited-storage native fixture passes the basic mixed-output Desktop
checks with seven allocations totaling 34,209,792 bytes. Its optional drag layer
is refused before allocation; dragging uses the uncached path and remains
functional. Evidence: `/tmp/cubit-owned-pixels-limited-run.log` and its serial
log, checked with `check-storage-budget.py LOG limited`. This injects admission
denial, not a kernel allocator cleanup failure. Actual failed-release/prefix
quarantine behavior is still covered by policy tests and audited kernel
contracts rather than native fault injection in Desktop.

### Correlated software frame trace

Timing-enabled Desktop now records the output index, Display session, globally
nonreused frame token, submission timestamp and timestamp when Desktop observes
a validated published/released completion. The existing presentation policy
must accept the kernel token, payload session/frame and buffer identity before
the record is collected. This is more precise than unrelated aggregate stage
durations, but it is still software submission-to-completion timing. Display
publication and buffer retirement do not establish physical scanout or photons.

`Compositor_Frame_Trace` stores at most 64 records per reporting window. It
preserves admitted records, counts invalid identities/clocks separately and
counts overflow with saturating counters. It performs no allocation or I/O.
Desktop prints records with the periodic timing report, outside the completion
collector, followed by a count/invalid/dropped summary and reset. Production
timing-off builds make no added clock reads or trace-publication calls. The
bounded text trace is a diagnostic facility: it can overflow during a busy
240 Hz interval, and its opt-in clock reads/reporting perturb execution. It is
not a lossless 240 Hz recorder or a validated hardware benchmark mechanism.

The SPARK report for `tests/compositor/frame_trace.gpr` has 29 checks, zero
unproved, including elapsed-time validation. Hosted checks pass 100 saturation/
reset cycles and invalid clock/identity cases. `check-frame-trace.py` verifies
complete trace batches, identity uniqueness, timestamp order and sequential
submission per output while allowing different outputs to finish out of order.
It rejects invalid/drop counts instead of treating partial traces as complete.
Its evidence tests pass 12 negative controls. Final hosted/proof evidence:
`/tmp/cubit-frame-trace-checks-final.log`; the initial test build had an Ada
Text_IO Count name collision, corrected before verification.

Native `desktop-display` passes with the opt-in timing build: 21 records in six
windows, no invalid/dropped records, and the existing five timing stages pass
their checker. Logs: `/tmp/cubit-frame-trace-native-run.log` and its serial log;
structured records: `/tmp/cubit-frame-trace-native.json`. This QEMU TCG run is
correctness evidence, not a NUC performance result. Production Desktop/Display
staging was restored and compared against their built binaries.

Full causal input-to-presentation instrumentation remains incomplete. Current
input source reports contain source identity/generation/sequence but no device
arrival timestamp. Application presents do not echo the delivered input serial
that caused the update. Completing the chain requires preserving that causal
identity through client rendering/present, adding driver/queue timestamps, and
distinguishing GPU completion from Display latch/scanout timing. Associating
every next frame with the most recent keypress would give false correlations;
this trace deliberately makes no such inference. Physical photon timing still
requires supported hardware and external measurement.

### Mixed-output native-density selection

`Compositor_Density_Selection` chooses the highest rational scale among outputs
that intersect a surface's logical rectangle with positive area. A one-pixel
sliver on a denser display therefore requires backing pixels at that density;
touching an edge does not. The existing geometry model supplies rotated/scaled
output bounds, including negative origins. Empty, inverted or wholly offscreen
rectangles fall back to the caller's primary output. Equal scales retain layout
order. The returned output is a density witness, not a change to window focus,
primary-display preference, input coordinates or maximize/work-area policy.

Scale ordering uses LCM(1..16), 720720, so every supported rational density has
an exact integer rank. The rank identity is proved, not approximated using
floating-point or rounded percentages. `Choose` proves that its result is in
range, intersects whenever any output does, and has density at least as high as
every intersected output; otherwise it returns the supplied primary. Hosted
tests check all 65,536 rational pairs against independent cross multiplication,
8,804 rotation/negative-origin placements and explicit seam/fallback cases.
`tests/compositor/density_selection.gpr` reports 103 SPARK checks, zero unproved,
including the shared geometry dependency. Evidence:
`/tmp/cubit-density-selection-checks.log`. No native integration is claimed for
this selector yet.

The Desktop audit confirms that `Get_Information` still returns a global
`Unit_Scale`, and current multi-output presentation resamples the shared logical
scene. Connecting only a density query would not make this path crisp. The
remaining native-density integration must provide a per-surface configuration
snapshot with logical extent, selected rational density, admitted pixel layout
and a nonreused configuration generation. Client attachments/presents must match
that generation; old contents must stay usable until replacement is ready and
retired safely. A scale change while moving across a seam must not silently
reinterpret an existing buffer's dimensions. Allocation admission continues to
use the existing density planner and bounded storage/reader retirement policy.

Finally, each output must sample the client's native-density buffer directly
into its physical target; downsampling into the shared unit-scale scene before
enlarging it again would discard the extra detail. Desktop text/decorations also
need rasterization at each output's density. The present 16 MiB buffer limit and
GPU/import contracts remain separate constraints. These are outstanding gates,
not properties established by the selector proof or compatibility scaling tests.

### Sampling phase across physical output boundaries

`Compositor_Sampling` supplies an exact integer reference for nearest-neighbor
sampling directly from native-density client storage into an output's physical
pixel grid. It reverses output rotation and retains the pixel-centre fraction
until the final source-index division. For one axis the numerator is
`((2*p + 1)*D + 2*N*(output_origin - surface_origin))*source_pixels`, divided by
`2*N*logical_extent`, where `N/D` is the output scale. A centre outside the
surface's half-open extent yields no sample. Negative surface/output origins
and clipped windows retain the original surface coordinate system.

This matters because the existing Mesa `Draw` interface admits only unsigned,
in-target destination rectangles. Cropping a surface at an output edge and
restarting a whole-source rectangle can change its sampling phase. The reference
checks a surface crossing onto a 150% output: the visible output starts at source
column three, not zero, with the same source rows through every rotation.
Fractional phase must survive clipping when the forthcoming affine draw bridge
converts checked geometry to Mesa coordinates. No Mesa library is rewritten.

The axis contract proves the exact validity interval and final sample formula;
the two-dimensional mapper proves source-index bounds. Rotation correspondence
has regression checks, not a separate abstract inverse-transform proof. Hosted
tests pass 139,264 rational/offset cases against an independent floating reference
over small values (with a small tolerance only at final integer ties), plus
explicit seam, square/non-square rotation, empty/off-target and extreme-range
cases. SPARK reports 113 checks, zero unproved, including shared geometry.
Reproduce with `tests/compositor/sampling.gpr`, its `sampling_tests` executable,
and `gnatprove -P tests/compositor/sampling.gpr -U --level=2 -j1 --report=all`
inside Nix. Evidence: `/tmp/cubit-output-sampling-verified.log`; earlier builds
stopped on syntax errors and are not verification evidence.

This is a geometry/reference primitive, not a proposed per-pixel division loop
for the fast renderer. Batched Mesa/GPU draws or incremental CPU spans must
preserve its mapping. The current Desktop draw path and foreign draw interface
are unchanged; there is no native or performance result for this mapper yet.
Numeric source-index bounds do not establish allocation size, grant authority,
foreign floating-point precision or GPU sampling equivalence. Those require
the checked draw bridge and native exact-pixel tests before integration.

### Affine Mesa bridge: verified foundation, Desktop integration pending

`Compositor_Affine.Plan` builds a 56-byte C descriptor from output geometry and
an original logical surface rectangle. It computes pixel-centre coverage,
rotates the scissor into the physical target, and preserves signed source
phase across output boundaries. Empty or fully clipped surfaces yield no draw.
The descriptor validity contract proves nonempty in-target clipping, bounded
surface extents, offsets, scale and rotation; signed arithmetic expresses the
bounds without modular subtraction. Exact conversions to unsigned ABI fields
are separately contracted. SPARK reports 157 checks, zero unproved, including
the sampling reference and shared geometry. Hosted tests pass 39,936 exact
scissor/reference samples plus ABI-size and extreme-coordinate checks.
Evidence: `/tmp/cubit-affine-final-checks.log`.

`Mesa_Affine_FFI` exposes one draw call. The existing softpipe adapter reuses its
context, imported buffers, blending and synchronous completion path. A full
output quad plus the planned scissor samples the original surface directly;
there is no intermediate cropped source image or pixel upload added by this
bridge. The C adapter rejects malformed descriptors before writes. Existing
rectangle draws share the final binding/drawing routine and retain their
native regression tests. No Mesa library or GPU driver is replaced.

Native CuBit tests cover 128 draws with source updates, 100%, 125% and 150%
scales, signed offsets, four rotations, both blend selections with opaque source
pixels, exact sampling against `Compositor_Sampling`, untouched pitch padding,
and 13 malformed descriptors rejected without changing the target. This is a
functional software-rendering result, not hardware acceleration or a throughput
measurement. Tests use unambiguous sample positions; arbitrary floating-point
texel-boundary ties and extreme-coordinate precision are not established.

Proof boundaries remain explicit: SPARK proves descriptor safety, but scissor
coverage correspondence is regression-tested. `Compositor_Transform` now constructs inverse-rotation coefficients and exact
rational corners in SPARK. The C adapter only converts those fractions to
floating vertex attributes and submits the fixed two-triangle topology.
Mesa rasterization, foreign memory/resource ownership checks and synchronous
flush behavior are trusted and native-tested. A future asynchronous GPU backend
must supply explicit completion/retirement semantics rather than treating the
software flush as a GPU fence. Desktop does not call this bridge yet: native
surface-density negotiation, per-output physical rendering and text rasterizing
at output density remain integration gates.

Reproduce inside Nix using `tests/compositor/affine.gpr` and `affine_tests`, then
`gnatprove -P tests/compositor/affine.gpr -U --level=2 -j1 --report=all` (through
the kernel Alire environment). For native testing, build with
`python3 tests/compositor/build-native.py userspace/mesa/build/native-aee5fe5697d39dd4`
and use the generated `tests/compositor/build/native/compositor-probe.app` as
`SOFTPIPE_IMAGE` for the headless `softpipe` case under the shared build lock.

Final native evidence: `/tmp/cubit-affine-native-final-run.log` and
`/tmp/cubit-affine-native-final-serial.log` (terminal success). Existing pool,
target-retirement and rectangle-draw regressions also pass; production Desktop
and Display staging matches their production build artifacts.


### Exact rational vertex preparation

`Compositor_Transform.Build` specifies all four inverse output rotations as
integer affine coefficients. Its contract gives every coefficient, source
normalization denominator and signed offset exactly. `Vertices` applies those
coefficients at the four output corners with a quantified contract. It does
constant work per draw; there is no CPU loop over surface pixels. Both source
axes retain rational scale and the original unclipped surface origin until the
foreign adapter converts the fraction to a floating vertex attribute.

`Mesa_Affine_FFI.Render` now validates the descriptor and prepares an 88-byte
rational quad before calling Mesa. The C adapter checks the quad dimensions
against the imported destination and uses a fixed corner-index table. The
56-byte logical descriptor remains unchanged. The wrapper and foreign pointer
lifetimes are audited boundaries, not SPARK proofs. Quad data are stack-local
and consumed synchronously; the adapter must not retain their address.
Floating conversion, interpolation precision and rasterization remain trusted
and native-tested. This does not assert exact sampling at every possible tie.

Native evidence `/tmp/cubit-transform-native-run.log` and
`/tmp/cubit-transform-native-serial.log` confirms the 128 affine draws, 13
rejected descriptors, and existing pool/target-retirement/rectangle regressions
with the new corner path. Malformed descriptor tests now exercise wrapper
rejection before the foreign call; they do not independently cover each C
validation branch. Production Desktop and Display staging were compared after
the run and match their production artifacts. Desktop integration is pending.

Final verification: `tests/compositor/transform.gpr` passes 39,936 exact
pixel-centre samples against `Compositor_Sampling`, including the rational
corner checks and ABI size. SPARK reports 242 checks, zero unproved, including
shared affine/sampling/geometry units. Both `Build` and `Vertices` functional
contracts are proved. Evidence: `/tmp/cubit-transform-final.log`. Use the same
Nix/Alire build and `gnatprove -U --level=2 -j1 --report=all` workflow as the
affine project, substituting `transform.gpr` and `transform_tests`. Earlier
coefficient-only results are superseded by this final-source run.

### Surface configuration replacement policy (integration pending)

`Compositor_Surface_State` is a bounded two-slot replacement policy for the
upcoming native-density surface protocol. Configuration generations never wrap.
An accepted configuration change preserves both retained identities and the
old visible buffer, while moving any obsolete candidate into retirement.
The candidate cannot be published or reused until actual readers retire; its
epoch and ticket remain available for an exact retirement receipt. A refused
configuration change leaves the entire state unchanged. Admission requires
the current generation, an empty slot and no other candidate. Each acquisition
receives a separately increasing ticket, including retries at the same density.
A matching present makes the candidate visible and changes the previous visible
buffer to retiring. Failed/stale operations preserve state. Reuse requires the
exact retirement ticket and an affirmative reader-retirement confirmation.
There is no unbounded list of superseded configurations or retained buffers.
`Close` permanently seals this surface identity against configure/stage/present,
changes each retained slot to retiring, and preserves every ticket and epoch.
It is idempotent and does not substitute for actual reader retirement.

The contracts prove exact admission/presentation conditions, nonreused tickets,
record preservation, at most one visible/candidate record, bounded epochs and
retirement only after the supplied confirmation. Both the 4,096-generation
stress model and Desktop's production default-limit instantiation now report
27 aggregate checks, zero unproved or justified. Tests execute 4,096
configuration replacements,
generation and ticket exhaustion, stale configuration presentation, blocked
admission while readers remain, and stale present/discard/retire callbacks after
same-generation slot reuse. Shutdown tests cover every valid phase pair in both
retirement orders, duplicate close, stale/negative retirement confirmation,
and rejection of admission even after all slots are empty. Configuration tests
cover every valid phase pair, stale/negative retirement confirmation, reuse only
after retirement, and both generation/ticket limits at the production integer
boundary. Hosted tests and proof plus the native Desktop main-unit compilation
pass in `/tmp/cubit-configure-retirement-integrated.log`. This is not a native
staged-buffer retirement test: those handlers remain to be connected. Earlier
shutdown evidence is `/tmp/cubit-surface-close.log`; the current summary is
`tests/compositor/build/surface-state/obj/gnatprove/gnatprove.out`.
Reproduce with `tests/compositor/surface_state.gpr`, `surface_state_tests`, and
`gnatprove -U --level=2 -j1 --report=all` through the Nix/Alire environment.

Desktop uses this policy for configuration queries, grant staging and
retirement and publication; toolkit integration remains outstanding. It does not prove
that a mapped buffer contains completed pixels: the service must call Present
only after successful acquisition and producer completion. Reader-retirement
truth, grant authority, renderer import invalidation, byte budgets and the
association between tickets and actual mappings remain integration obligations.
Native surface destruction must call the terminal close transition before
coordinating final retirement; the hosted policy does not perform foreign calls.
The current
Desktop attach/present messages carry no configuration generation; configure
events carry logical dimensions only, and Attach immediately replaces the old
attachment. Native-density support must replace that behavior with a complete
logical extent, rational scale and physical layout configuration, require its
generation on attach/present, and preserve these bounded transitions around the
actual grant/renderer operations. Advertising a density without changing the
shared unit-scale composition path would still lose detail and is insufficient.

### Toolkit publication audit (2026-10-01; integration outstanding)

The current toolkit does not enforce immutable publication. In
`CuBit.UI.App`, `Ensure_Buffer` attaches the same address that `Canvas` exposes
for drawing, and `Present` sends a request without transferring producer write
ownership. Desktop's attach handler immediately installs the mapping and queues
a redraw. Its present handler replies after scheduling damage; the event loop
calls `flushFrame` later. Thus an application may resume and mutate pixels while
the Desktop can still read that attachment. A successful status reply is not a
reader fence. Output-buffer retirement tests do not establish client-source
immutability, and the Mesa cube's retired-grant reuse does not cover toolkit
clients.

Desktop now connects `Compositor_Surface_State` to actual grant mappings and
renderer imports. The client-side integration must adopt that protocol with a
current configuration generation, logical extent, rational density and derived
physical layout. A client must finish its
candidate before publication, retain the visible source unchanged, and regain
write/reuse permission only after the matching reader-retirement receipt.
Replacement and close must preserve these rules under stale messages, failed
imports and overloaded queues. A bounded second buffer also needs damage-age
repair (or explicit complete repaint), and reclaimable allocation instead of
repeated `SBRK` growth. Simply delaying Present's reply until one frame finishes
would not suffice: later exposure or cursor repair may need the visible source
again. The toolkit's drawing coordinates/font rasterization and its configure
handling must be migrated together with the service; publishing density alone
would leave client pixels at unit scale.

### Reclaimable client-frame component (2026-10-01)

`userspace/lib/ui/client_frame_buffer.*` is a limited, serialized owner of one
frame allocation. It uses the existing owned-memory allocation/release syscalls
rather than growing `SBRK`, exposes a write pointer only while eligible, and
makes the owner's mapping read-only before sending stage/publish requests.
Before restoring read/write protection, it authenticates a retirement query
through the Desktop endpoint and checks the exact epoch/ticket and canonical
reply. Malformed/mismatched replies or protection failures quarantine the handle.
A known stage refusal leaves it sealed but unborrowed for explicit reopening.
Release revokes its grant, confirms kernel retirement, and only then releases
backing. Pending readers or failed release retain the handle and prevent a new
allocation from overwriting it. The existing 16MiB per-buffer limit remains.

The pure `Client_Frame_State` policy proves seven functional contracts and
termination (eight checks, zero unproved/justified). Hosted tests execute 4,096
cycles and test stale/negative receipts, protection failure, every release outcome
and quarantine. The adapter's IPC, kernel protection, grant authority and actual
memory reclamation remain audited foreign boundaries; it is not a whole-adapter
SPARK proof. Callers must serialize handle operations, complete rendering before
submission, and avoid external writable aliases. No forbidden-write fault test
or protection-overhead performance measurement is claimed here.

The initial native publication fixture used two of these handles for 13 frames and
checks exact pixels, denial of visible-frame write eligibility, failed release
while a reader remains, quarantine allocation refusal, reclamation after surface
destruction, and reallocation/zero-initialization smoke checks. Original low-level
protocol adversaries remain in the fixture. The full90s CuBit/QEMU run and fault
scan pass, with 51,792 exact RGB pixels including the final13x11 patch. Evidence:
`/tmp/cubit-client-frame-integrated.log`, `-native.serial`, `-pixels.log` and
`/tmp/cubit-client-frame-source.json`. The persistent hosted project is
`tests/compositor/client_frame.gpr`.

`CuBit.UI.App` has not yet adopted this component. Its migration still needs
explicit begin-frame ownership, bounded damage-age repair or full repaint,
DPI-aware canvases/fonts, configure handling and capability negotiation. The
component adds no frame copy and does not by itself make toolkit clients
immutable or establish hardware fences/240Hz latency.

### Client buffer repaint debt (2026-10-01)

`Client_Frame_Damage` tracks two bounded repaint regions and one publication
region, using half-open coordinates up to 65,535. Opening a configuration makes
both buffers fully dirty. Every retained-state change accumulates into both
buffers, including when neither can be written. After obtaining write ownership,
the producer redraws the selected buffer's `Required` region from current
retained state. It publishes only `Publication_Damage`: repairs to stale pixels
are not new changes to the visible scene. Successful publication clears the
selected buffer's debt and publication region, retaining the other buffer's debt.
Failed rendering or publication leaves all debt intact. There are no pixel
copies, allocation, frame counters or unbounded queues in this policy; disjoint
changes conservatively merge into a bounding rectangle.

Sixteen SPARK checks (seven functional contracts, nine termination results) pass
with zero unproved or justified checks. An independent 17x13 pixel model runs
4,096 cycles with multiple input changes, partial render failures and publication
failures; all 3,192 successful publications match current scene pixels. Boundary
cases cover a changed configuration and coordinates at 65,535. Evidence:
`/tmp/cubit-client-damage-proof.log` and the private proof report at
`/tmp/cubit-client-damage-private/obj/gnatprove/gnatprove.out`.

The policy does not prove that a renderer actually painted its claimed rectangle
or that presentation completed. A serialized producer must obtain write
ownership separately, finish painting the reported rectangle with current state,
and acknowledge repair only after an accepted publication. State changes between
painting and acknowledgment are forbidden. Configuration changes reset the policy
only alongside buffer-layout replacement. Ordinary `CuBit.UI.App` integration
and DPI-aware drawing remain outstanding.

Native selective repaint now passes the full 90-second CuBit/QEMU protocol run
and final fault scan. The protected fixture alternates two buffers for 15 frames,
changes a 13x11 patch on the last three frames, and checks that frame 15 repaints
only 143 pixels. The final displayed image matches all 51,792 RGB pixels.
This measures producer paint coverage, not total compositor/GPU work or latency.
The persistent test/proof project is `tests/compositor/client_damage.gpr` (same
16 proved checks); native evidence is `/tmp/cubit-client-damage-integrated.log`,
`-native.serial`, `-pixels.log` and source manifest `-source.json`.

### Toolkit physical-pixel primitives (2026-10-01)

The shared `CuBit.UI.Canvas` integration separates logical canvas size and
clip coordinates from physical pitch/address. Each canvas records a rational
density and its logical origin within the root raster. Nested views retain that
origin so fractional-density boundaries agree with their parent. Both edges use
ceiling: adjacent logical cells partition storage without overlapping or leaving
gaps. Bitmap sampling maps each physical pixel back to its logical cell. Fill,
bitmap, bitmap-font and surface-view code use this mapping. The TrueType mask
path below now uses native-density masks; application buffer/configure integration
is still required before non-unit scaling can be enabled.
The unit-scale path avoids density divisions.

`Client_Canvas_Geometry` proves 22 checks, zero unproved/justified, covering
minimal upward rounding, relative edges, in-range inverse samples and clip-end
arithmetic that cannot overflow on oversized rectangles. Hosted tests exercise
all 256 numerator/denominator combinations with padded rows, adjacent cells,
fractional-origin nested clips, transparent/opaque bitmap colors and bitmap font
pixels. Existing normal-scale TrueType/control tests also pass. These are
geometry proofs plus imperative drawing regressions, not a proof of arbitrary
caller-provided memory mappings. The caller still supplies valid backing and
physical pitch. Native Desktop, desktop-shell and Files builds pass, along with
hosted surface routing/clipping and CCL Workbench compilation. The normal
90-second CuBit/QEMU `desktop-display` regression passes, including a native
Workbench window and the final fault scan. Applications remain at unit scale;
non-unit native screenshots remain unvalidated. TrueType density is covered by
the later hosted integration below.

Persistent evidence: `tests/compositor/client_canvas.gpr` and its report at
`tests/compositor/build/client-canvas/obj/gnatprove/gnatprove.out`, expanded
`tests/ui-fonts/main.adb`, `/tmp/cubit-ui-density-integrated-final.log`,
`/tmp/cubit-ui-density-native-final.log`, `-native.serial` and `-source.json`.
The first hosted build rejected old-style array syntax under warnings-as-errors;
that test syntax was corrected. The broader build also exposed Workbench's
pre-existing missing text/character/list value cases; they now delegate to the
existing `CCL.VM.Value_Image` formatter. No VM semantics changed. No application
DPI or protected-buffer migration is claimed by this primitive stage.

### Toolkit native-density TrueType masks (2026-10-01)

`CuBit.UI` now renders non-unit-density Sans/Monospace text from fresh A8 masks
at the requested physical density. It reuses the existing Rust font rasterizer;
normal-scale text retains its existing path. The mask origin follows the canvas's
root-relative pixel boundary, clipping stays in logical coordinates, and blending
writes directly into the writable canvas. No intermediate text image is copied.
Opaque text paints its background once before glyphs; transparent text preserves
the existing destination under coverage. Logical text advances remain stable.

`Client_Glyphs` reuses `Compositor_Glyph_Cache`, `Storage`, `Memory` and `Arena`.
Its fixed backing arena is 512 KiB per UI process, plus policy metadata; this is
not total process memory and excludes existing glyph caches and font scratch.
Warm glyph reads reuse masks, with at most 128 resident slots and 32 simultaneous
read leases. A bounded eviction pass never waits on held readers. Views cannot
be copied; finishing through another owner is rejected using the lease, mask
identity and backing address. Terminal close retains pinned storage until the
correct owner finishes those views. Four existing cache mutator contracts now
explicitly preserve their byte limit.

The persistent owner project `tests/compositor/client_glyphs.gpr` proves all 148
checks, including the instantiated cache policy, with zero unproved/justified.
Hosted tests compare 570 masks with independent rasterizer calls, exercise warm
reuse, eviction while a mask is pinned, cross-owner rejection, reader exhaustion
and deferred close. The expanded UI pixel oracle matches fresh masks for five
scales and both faces, checks clipping/alpha, and verifies that doubled-size
coverage differs from simply enlarging a normal glyph. The earlier 256-density
primitive oracle and normal-scale TrueType/control tests also pass.

Native compatibility passes: Desktop, desktop-shell, Files, Config Inspector,
Devices and Boot Logs build, the surface tests and all 144 Settings callback
cases pass, and the 90-second CuBit/QEMU `desktop-display` run passes its final
fault scan. These native applications still use unit-density canvases, so this
is not native execution evidence for the fractional-density text branch. New
UI dependencies are declared in its consumer projects; the explicit Settings
source list and its old positional Canvas aggregate were repaired as part of
that compatibility check.

Proof boundaries remain explicit: the new proof covers cache/lease/budget
orchestration, while font parsing, raw memory validity and backing operations
remain foreign/storage boundaries. `CuBit.UI` is not wholly SPARK. Its density
mask loop now calls `Client_Glyph_Blend`, a pure SPARK operation with 75 checks
proved (zero unproved/justified): channel arithmetic, array bounds, termination,
and preservation of every pixel outside the destination rectangle. The proof
does not specify the complete resulting image; hosted independent pixel checks
cover that behavior. The bridge validates pitches, lengths, address overflow
and mask/target overlap before importing arrays. The target ends at the last
accessed pixel, so a nested view's partial final row does not imply a full extra
row of backing. Mapping validity and exclusive writable ownership remain caller
obligations. No intermediate pixel image is allocated by the production path;
assertion-enabled tests may snapshot arrays for contract checking. The integrated blend passed the five-scale/two-face hosted UI pixel oracle,
all 144 Settings rendering cases, six native application builds and the
90-second normal-scale CuBit desktop-display regression/final fault scan.
Evidence: `/tmp/cubit-text-blend-integrated.log`,
`/tmp/cubit-text-blend-native.log`, `/tmp/cubit-text-blend-native.serial`,
`/tmp/cubit-text-blend-source.json`, and
`tests/compositor/build/client-blend/obj/gnatprove/gnatprove.out`.
The production native compiler reports a static 176-byte `Paint` stack frame;
this is not a whole-call-chain stack bound or a performance measurement.
Applications must serialize calls to the process-owned glyph cache.
Native fractional-text offscreen coverage now passes: `Desktop_Density_Text`
in `desktop-check` compares every pixel at five scales for both faces against
fresh native Rust masks and independent integer blending, checks clipping and
padding, and rejects nearest enlargement as a substitute for native outlines.
Explicit failure returns remain effective with native assertions disabled.
The required marker and full 90-second desktop-protocol/fault scan pass in
`/tmp/cubit-native-density.log` and `.serial`; source hashes are recorded in
`/tmp/cubit-native-density-source.json`. This does not exercise client output
configuration negotiation or mixed-output scanout. Protected two-buffer
`UI.App` adoption and configured per-output application scaling remain gates. No 240 Hz or physical-latency result is claimed.

Evidence: `/tmp/cubit-ui-text-integrated.log`, `-integrated-final.log`,
`-native-final2.log`, `-native-complete.log`, `-native.serial`, `-source.json`,
and `tests/compositor/build/client-glyphs/obj/gnatprove/gnatprove.out`. Strict UI
builds exposed redundant type-visibility and contract-only-parameter warnings
in reused glyph modules; those were annotated without changing their algorithms.

### Native configuration query connected (2026-10-01)

Desktop now stores the configuration-generation policy and last accepted layout
in each real surface record. The new owner-checked configuration query reports
logical client size, rational density and derived physical layout; repeated
queries preserve the generation, while a changed configuration advances it.
It reuses the existing density selection, layout admission and nonwrapping
surface-state policies. It does not attach or mutate the visible source.

The rebuilt native `desktop-protocol` regression passes malformed requests,
foreign/missing surface rejection, repeated queries and resize-driven generation
changes, alongside the existing protocol adversary. Evidence:
`/tmp/cubit-configuration-retry{,-run}.log`. This is the legacy unit-scale
backend: mixed-output query transitions still need native validation. Grant
staging and retirement handlers are now implemented with two fixed slots and
ordered renderer/grant release. Publication now switches the drawing reference
using committed logical dimensions; configure-event integration and toolkit
migration remain incomplete. Partial publication damage now uses the proved
clipped source-to-logical mapper; first frames and changed source geometry still
repaint the full client. See the protocol document for proof and native-test
boundaries. Codec round-trip proofs now pass all 128 checks
in the shared-source hosted project, with no unproved or justified checks.
A configuration reply must not be treated as feature negotiation for immutable
publication. See `docs/desktop-protocol.md` for the wire contract and
current boundaries.

### Live client-source retirement confirmation

Desktop's `releaseSurfaceBuffer` now uses the existing proved ordered-reader
coordinator for renderer-import retirement followed by returning the grant
acquisition. It clears the attachment address/layout only after both callbacks
confirm success. Either uncertain result exits the compositor before normal
replacement/reuse; the actual attachment record remains intact until process
termination. Previously the service cleared the record before testing the grant
return result, logged a failure, and could continue accepting a replacement.
The new flow makes the uncertainty path fail-stop, like output retirement.
Kernel process teardown remains responsible for final cleanup after that exit.

The policy's hosted regression suite passes all 80 setup/failure/order cases,
and its explicit proof instantiation reports 14 checks, zero unproved. Evidence:
`/tmp/cubit-surface-retirement-policy.log`. These are coordinator proofs and mock
callback tests, not proof of the syscall or the SPARK-Off Desktop handler.
Native success-path teardown and protocol tests are recorded separately.

The current toolkit revokes its previous grant immediately after an accepted
Attach, explicitly relying on the old acquisition already being returned.
Therefore delayed promotion cannot be silently substituted into that operation.
The native-density protocol must carry generation/ticket identities and define
replacement completion for clients as well as Desktop. The portable grant
reference already has a proved one-word encoding, leaving room in a four-word
attachment message for a configuration generation without dropping grant
identity. ANV's native presentation adapter also uses this protocol and must be
updated or negotiated alongside the toolkit and direct test/app callers.

Final native evidence for the live source-retirement change:
`/tmp/cubit-surface-retirement-native-run.log` and its serial log record a
successful Mesa Desktop cube check (196,608 composed pixels), client close and
full output teardown with zero charged pixel storage. The normal Desktop
`desktop-protocol` fixture also passes, including 140 repeated attachments,
pending-revoke replacement, stale-grant rejection and client destruction;
evidence `/tmp/cubit-surface-retirement-protocol-run.log` and its serial log.
Both runs completed successfully under QEMU TCG. These exercise successful
native retirement and protocol rejection paths, not injected syscall-return
failure; uncertainty retention is covered by the coordinator fault tests.

### Retained-cache physical-output draw API

`Desktop_Compositor.Draw_Output` accepts the original logical surface rectangle,
output geometry and actual imported image layouts/capacities. It plans the
scissor in SPARK and submits exact rational corners through the affine binding.
The Mesa backend reuses the same bounded image cache as ordinary client draws.
`Compositor_Cache.Render_Checked` centralizes missing-view rejection, descriptor
validation and the Ready/Reading/completed-or-uncertain transitions; the existing
rectangle path also uses this operation. A mismatched physical target is rejected
before foreign rendering. Unknown foreign access still forbids retirement and
requires compositor restart; confirmed quiescent failure shuts the cache down
before the caller may use a fallback.

The narrow `Mesa_Binding.Affine` child converts return codes using the same
completion meanings as the rectangle binding. `Mesa_Affine_FFI` prepares rational
corners synchronously; the library remains the existing Mesa softpipe stack.
No additional image copy or pixel allocation is introduced by this API.
The two destination cache slots currently serve as reusable view slots; changing
a pool writer can require retiring/reimporting its view. This is not yet an
optimized six-view cache for two three-buffer output pools.

The native `Native_Output_Test` calls the actual Desktop backend API for 128
draws, alternates its destination slots, changes source layouts, and compares
all target pixels/padding against the integer sampling reference. It also checks
that a target/geometry mismatch leaves every target pixel unchanged, then tests
safe source/target retirement after cache shutdown. Both slots use the same test
allocation serially; this is not a simultaneous dual-output hardware test.

The production main loop does not call `Draw_Output` yet. Per-output scene
traversal, clipping of decorations, native-density text rendering and the
configuration protocol remain required. The legacy implementation reports that
this API did not draw, allowing a future caller to choose its CPU path; the
existing software desktop remains the currently integrated fallback.

The all-unit `composition.gpr` proof reports 354 checks, zero unproved, including
`Draw_Output` and both instantiated checked-render paths. Existing hosted cache
failure/reuse/retirement tests, 2,662 composition cases and 3,200 damage grids also
pass. Evidence: `/tmp/cubit-affine-cache-hosted.log`. The serial oracle in
`/tmp/cubit-affine-cache-verified-serial.log` reports `COMPOSITOR-OUTPUT` success
for the 128 cached affine draws, alongside the existing native pixel tests.
The earlier run sent its new marker to stdout; the final run uses the established
serial debug hook. This is functional native CuBit evidence, not throughput.

Before wiring this API into partial desktop repaint, extend its planned scissor
with the current physical damage clip while retaining the original source
transform. Otherwise a draw outside the damaged region could overwrite an
unchanged occluding window that is not visited by that repaint. The current API
covers a whole visible surface; whole-frame traversal is safe when all layers
are redrawn in order. Per-output decoration/text rasterization and client
configuration generation remain separate outstanding integration requirements.

The combined final native run completed successfully: softpipe oracle and normal
Mesa Desktop cube/close/zero-charge teardown. Both Desktop backends build, and
production Desktop/Display staging matches their build artifacts. Evidence:
`/tmp/cubit-affine-cache-verified-run.log` and
`/tmp/cubit-affine-cache-desktop-serial.log`.

### Physical damage clipping for output draws

`Draw_Output` now requires an explicit physical damage rectangle. The SPARK
`Compositor_Affine.Clip` function intersects it with the planned surface scissor
and proves the exact resulting edges, validity and preservation of every source
transform/blend field. Empty, inverted and off-target intersections produce no
draw. The full-target rational vertices retain their original source phase;
clipping never restarts or rescales a cropped source. This closes the scissor
integration gate described above, without adding a pixel copy or allocation.

The hosted oracle checks 307,200 pixel-membership cases over normal, inverted,
empty and off-target rectangles. The all-unit composition proof reports 373
checks, zero unproved, including exact clip and backend-call contracts. Evidence:
`/tmp/cubit-affine-clip-verified.log`. Native CuBit tests pass 128 cached output
requests with eight damage shapes across the existing scale/rotation/offset
matrix. Thin rows and columns, empty/inverted/off-target damage and partial
rectangles preserve every untouched pixel and padding word. Empty requests are
successful no-ops, so 128 requests does not mean 128 foreign draw submissions.
Evidence: `/tmp/cubit-affine-clip-native-run.log` and its serial log. Existing
native rectangle, pool, source-phase and target-retirement regressions pass.

The main-loop scene traversal and native-density configuration/text integration
remain pending. These results establish safe clipped output draws, not a complete
per-output desktop or hardware performance measurement.

The legacy Desktop also builds successfully with the explicit damage parameter
(`/tmp/cubit-affine-clip-legacy-build.log`) and is staged in the production slot.
Desktop and Display staging comparisons pass.


### Live direct-output hookup and evidence correction

The main Desktop client-draw path now calls `Draw_Output` when its current
canvas is a direct pool writer for one unrotated, unit-scale output at origin
zero. Source logical extent remains the attachment's extent; the legacy plan
supplies the damage/client clip. This preserves existing placement when a window
and its old attachment temporarily differ in size. Shared-canvas and drag-cache
rendering still use the rectangle API. Multi-output/native-density traversal is
not implemented by this initial hookup.

The first live run (`/tmp/cubit-live-output-api-run.log` and serial log) passes
the frame oracle and zero-charge teardown, but its 512 MiB guest logs Mesa pipe
creation failure and CPU fallback. It does **not** verify activation of the new
live path. Audit also found this fallback in the earlier
`cubit-surface-retirement-native-serial.log` and
`cubit-affine-cache-desktop-serial.log` runs. References above to normal Mesa
Desktop checks mean an opt-in binary running the Mesa client; those particular
runs establish CPU fallback, not active Mesa composition. Their 196,608-pixel
oracle used the default quads scene, not the cube scene. The separate native
128-request backend tests did exercise Mesa directly and remain valid evidence.

The runner now supports `CUBIT_TEST_PHYSICAL_CLIENT=1`: it requires the live
physical-output marker and rejects any CPU-fallback marker. Use an explicit
`QEMU_MEMORY=1G`, `MESA_WINDOW_SCENE=cube`, and an explicit
`MESA_WINDOW_IMAGE` pointing at `native-mesa-cube.app` for the live check, along with
that gate and `CUBIT_TEST_TARGET_RETIREMENT=1`. The graphics agent has the next
shared native build window; the gated rerun is pending that window's completion.

### Checked legacy-client to output conversion

`Compositor_Client_Output.Plan` now owns the unit-output bridge used by the
live Desktop handler. It checks output origin/rotation/density, dimensions,
source offsets and both source/target bounds before building the original
surface rectangle and physical damage edges. Its postcondition specifies those
coordinates exactly. Desktop passes the checked result to `Draw_Output`; the
new coordinate additions and narrowing conversions no longer live in the
unchecked handler. Ineligible geometry keeps the existing rectangle path.
This adapter preserves old attachment extent during resize; it is not the
future native-density surface configuration protocol.

Hosted tests pass 171,072 clipped placement/size cases, plus malformed and
extreme input rejection. SPARK reports 156 checks, zero unproved, including
shared legacy composition/output geometry. Evidence:
`/tmp/cubit-client-output-proof.log`. Reproduce with
`tests/compositor/client_output.gpr`, `client_output_tests`, and the all-unit
Nix/Alire `gnatprove -U --level=2 -j1 --report=all` workflow. The service still
owns state/authority and the foreign renderer remains a documented boundary.

### Live physical-output Mesa path verified

The corrected gated native run completes successfully with a 1 GiB guest and
an explicit `tests/mesa-software/target/native-softpipe-cubit/native-mesa-cube.app`.
The cube oracle checks 194,673 geometric pixels; the serial log contains the
physical-output client-drawing marker, texture/vertex upload markers, zero
Desktop staging-copy bytes and final zero charged pixel storage. The gate rejects
CPU fallback, so this run verifies the new live Mesa compositor path rather than
only the client renderer. Evidence: `/tmp/cubit-checked-live-cube-run.log`,
`/tmp/cubit-checked-live-cube-serial.log` and
`/tmp/cubit-checked-live-cube-serial-mesa-pixels.log`. Both Desktop backends build;
production Desktop and Display staging comparisons pass.

The preceding `/tmp/cubit-checked-live-output-run.log` is a rejected fixture run:
it loaded the default quads executable with the cube oracle. It is not accepted
pixel/teardown evidence. Set both `MESA_WINDOW_SCENE=cube` and `MESA_WINDOW_IMAGE`
explicitly; the former selects the oracle, not the executable.

This live result covers the existing single-output, unrotated, unit-scale direct
pool path. It is Mesa software composition under QEMU TCG, not GPU acceleration,
multi-output native-density text, or a 240 Hz performance result. Zero staging
copies applies to Desktop's scene-to-output stage; display/backend copying and
other memory remain separately instrumented/trusted boundaries.

### Bounded density-specific glyph storage

The existing bundled-font cache exposes only normal and double-size cached
rasters. Desktop still requests normal glyphs, so its existing compatibility
output scaling does not provide native-density text. `Compositor_Glyph_Layout`
now defines the SPARK storage policy for a caller-owned coverage-mask
raster request: exact `13*N/D` em density, upward-rounded `17*N/D` line height,
a conservative `32*N/D` width bound, 16-byte row alignment and exact byte charge.
It accepts all 256 numerator/denominator pairs supported by output geometry.
Equivalent rational densities produce identical storage dimensions and charge.

This is an 8-bit coverage-mask layout, not a BGRA image or a new permanent font
cache. At 100%, 125%, 150% and 200%, the conservative mask charges are 544, 1,056,
1,248 and 2,176 bytes. The extreme 16x request is bounded by 139,264 bytes
(136 KiB); no allocation happens in the planner. The existing fixed font cache
and production text rendering are unchanged.

Hosted tests check every density against an independent floating ceiling oracle,
equivalent-ratio dimensions and the maximum bound. SPARK reports 98 checks,
zero unproved, including shared output geometry; it proves exact em selection,
minimal upward-rounded dimensions, pitch alignment and byte accounting.
Evidence: `/tmp/cubit-glyph-layout-checks.log`. Reproduce with
`tests/compositor/glyph_layout.gpr`, `glyph_layout_tests`, and the all-unit Nix/
Alire proof workflow. Font parsing, outline bounds, coverage quality and foreign
writes are not established by these layout proofs.

### Caller-owned glyph raster boundary

`Compositor_Glyph_FFI.Rasterize` now calls `cubit_font_raster_mask` in the existing
Rust font library. A 32-byte request carries face, code, exact em ratio and mask
layout; an 8-byte result carries advance and height. Rust independently checks
the supported ratios, exact dimensions, pitch and capacity before using the
existing bundled fonts and streamed outline rasterizer. The Ada wrapper accepts
only successful results with advance inside the planned width and the expected
height. No caller pointer is retained and no permanent cache is added.

Rasterization writes directly into caller-owned A8 storage. It clears and writes
only the requested row width, preserving row padding and bytes beyond the mask.
Null, misaligned metadata, overlapping address ranges and invalid layouts are
rejected. These numerical checks do not establish that pointers are mapped,
authorized, exclusively owned, or free from physical aliasing: those remain
caller obligations at this SPARK-Off boundary. Bundled-font parsing, outline
rasterization, floating arithmetic, allocator behavior and foreign writes remain
trusted/regression-tested. Temporary coverage scratch still allocates; OOM can
terminate the process. The mask byte bound is not a total heap or timing bound.

Hosted Rust tests compare 48,640 face/density/ASCII cases against `ab_glyph`, with
at most one grayscale level of difference, and exercise malformed requests and
pointer geometry. The Ada/Rust hosted test passes 512 face/density requests,
including padding and short-capacity rejection. Native CuBit/QEMU tests pass 12
masks covering both faces, fractional scales and the 1/16x and 16x extremes;
normal/double-size results exactly match the existing cached raster. Existing
native pool, target-retirement, affine, damage and rectangle tests also pass.

Evidence: `/tmp/cubit-density-raster-final.log`,
`/tmp/cubit-density-raster-abi.log`, and
`/tmp/cubit-glyph-mask-native-{run,serial}.log`. Reproduce hosted tests using
`nix develop -c make -C userspace/rust fonts-test fonts-host`, then the
`tests/compositor/glyph_ffi.gpr` executable. The native probe builder now builds
and links the native font archive; use `tests/compositor/build-native.py` with
the existing Mesa native build and the headless `softpipe` test under the shared
build lock.

Bounded cache admission/retirement and per-output text placement remain open
integration gates. Production Desktop text is unchanged. These results establish
neither GPU glyph rendering nor DPI-correct Desktop text, hardware performance,
or a per-frame timing bound.

### Retained A8 coverage composition

`Mesa_Mask_FFI` adds a separate, read-only mask import and tinted draw entry point.
It reuses the glyph layout validation and the existing SPARK affine validation,
clipping and rational corner construction. Mask storage stays with the caller
until `Mesa_FFI.Release` confirms retirement, using the existing completion
codes. This interface does not itself provide cache admission, eviction or
ownership authority; those policies must surround production use.

The Mesa adapter imports A8 storage as an R8 texture with coverage in every
sample channel. A fixed shader multiplies coverage by premultiplied tint; the
existing source-over blend state composes it into a BGRA target. Public tint
input is straight-alpha AARRGGBB. No temporary BGRA glyph, pixel expansion or
adapter upload copy is introduced. The mask shader is created lazily once per
context. Mesa still owns internal caches and allocations, including its small
constant-buffer wrapper; this is not an allocation-free draw claim.

Mask imports enforce a read-only image, bounded dimensions, aligned pitch and
sufficient byte capacity. Color draw entry points reject mask sources, mask
draws reject color sources, and masks cannot be render targets. Geometry and
damage use the same proved SPARK path as color affine draws. Shader execution,
float conversion/blending, Mesa resource access and synchronous softpipe flush
remain audited/tested foreign behavior, not new SPARK proof claims or GPU fence
guarantees. The actual allocation, non-aliasing and serialized-access obligations
remain with the caller.

The native CuBit probe passes 320 tinted mask draws: two faces, four densities,
four rotations, two damage clips and five tint/opacity choices. It compares
every target pixel against an integer coverage/blend oracle (up to two channel
levels for rounding), requires untouched pixels/padding to match exactly,
checks source storage is unchanged, and mutates already-imported masks between
draws to detect stale sampling. Wrong-format and short-capacity requests are
rejected; a subsequent BGRA draw confirms shader-state restoration. Existing
native pool, retirement, affine/output, glyph and rectangle regressions pass.
Evidence: `/tmp/cubit-mask-composition-{run,serial}.log`; reproduction uses the
same native probe builder/headless `softpipe` command described above.

This is an actual CuBit execution test using Mesa software rendering under
QEMU, not a physical GPU or performance measurement. The Desktop text renderer
does not call this interface yet. Production integration must place density-
specific glyphs at their intended raster extent and provide bounded retained
mask storage before advertising native-density text.

### Bounded mask admission and read leases

`Compositor_Glyph_Cache` is a pure SPARK policy with 128 fixed mask slots, 32
outstanding read leases and an explicit caller-selected byte budget. Keys include
face, printable character and rational density; equivalent fractions match.
Construction and retirement occupy the key as well as ready masks, preventing
duplicate work while an earlier request is pending. Reservation charges the
proved mask layout before any rasterization/import. A full byte budget, full
slot table or exhausted identity counter rejects admission without mutation.

Publishing requires confirmed raster completion. Each read acquisition gets a
new, non-wrapping identity, even when it reuses the same lease slot and glyph.
Only the exact active lease can complete. Withheld or uncertain completion keeps
the mask pinned. Retirement refuses any pinned mask, blocks further reads and
retains its charge until all foreign imports and backing storage are confirmed
retired. Failed construction uses the same retirement path. A bounded round-
robin search proposes an unpinned ready victim; it never silently frees one.

The contracts prove budget preservation, exact successful admission/refund,
monotonic mask/read identities, stale-operation rejection, reader-count changes
and refusal to retire pinned masks. Inductive accounting lemmas cover changes
to the fixed tables. With the widened identities, the hosted project reports
197 SPARK checks, zero unproved
or justified, including shared glyph layout/output geometry. Tests pass 4,096
reuse cycles, stale callbacks while a replacement is live, all 32 concurrent
leases, all 128 occupied slots, all 256 density budgets, one-byte shortages,
withheld completion/retirement and reduced-counter exhaustion. Evidence:
`/tmp/cubit-glyph-identity-checks.log` and the report under
`tests/compositor/build/glyph-cache/obj/gnatprove/gnatprove.out`.

Reproduce inside Nix using the kernel Alire environment:

```sh
alr exec -- gprbuild -p -P ../tests/compositor/glyph_cache.gpr
../tests/compositor/build/glyph-cache/glyph_cache_tests
alr exec -- gnatprove -P ../tests/compositor/glyph_cache.gpr --level=2 -j2 --report=all
```

The budget accounts for coverage-mask payload, not allocator rounding, Mesa
objects, raster scratch or the existing fixed font cache. The policy allocates
no pixels and does not prove caller-supplied completion/retirement facts. Those
must come from authoritative adapters; ambiguous frees must not be blindly
replayed. Tokens belong to one cache owner, whose state must never be reset while
old callbacks can reach it. Production use still needs bounded backing storage,
retained Mesa handles sharing the Desktop context, and native-density placement.

The native CuBit mask probe now brackets its real raster/import/draw/release
sequence with this policy. All 320 tinted draws pass with a matching read lease,
retirement refused while confirmation is withheld, and each retired mask's
charge returning to zero before fixture storage is reused. The existing native
compositor regressions also pass. Evidence:
`/tmp/cubit-mask-cache-native-{run,serial}.log`. This deliberately withholds
confirmation after synchronous softpipe success; it does not inject a real GPU
hang or prove GPU fence behavior. This earlier run used fixed fixture storage;
the backing implementation below replaces it. Desktop text integration remains
outstanding.

### Fixed glyph backing storage

`Compositor_Glyph_Arena` reuses the existing `Heap_Extents` contiguous-run
ownership core unchanged. That core performs metadata operations on abstract
page identifiers; the adapter maps those identifiers to 128-byte cells, without
using its 4 KiB page-size constant or mapping a 16 MiB heap. The resulting arena
has exactly 4,096 cells and 524,288 payload bytes. Requests round upward to cells,
adding at most 127 bytes per mask. Even the largest supported 16x-density mask
fits; three such masks fit concurrently, while a fourth is rejected.

Allocation returns a new non-wrapping identity plus an aligned byte offset and
capacity. The contracts prove that the region stays inside the arena and was
previously free, preserves occupied cells elsewhere, and rounds minimally.
Release requires the current identity and confirmed reader retirement; stale or
withheld confirmation leaves state unchanged. No live mask moves, so retained
Mesa pointers remain stable. Fragmentation can reject an allocation despite
enough aggregate free space; there is no compaction or unbounded retry loop.

`Compositor_Glyph_Memory` supplies the actual fixed buffer and a narrow audited
pointer bridge. Its limited Ada object cannot be copied by assignment and must
remain alive at its address until all imports/readers retire. It initializes
the metadata once, calls the proved arena operations and returns the address of
the selected cell. There is no per-mask heap allocation, growth or pixel copy.
The tested object occupies 589,952 bytes including backing, metadata and
alignment; cache keys/leases, Mesa objects, raster scratch and the existing font
cache remain separate costs. Releasing a mask makes cells reusable; it does not
return the fixed backing to the OS.

The arena project passes 165 SPARK checks, zero unproved or justified, including
the reused extent core and shared layout/geometry. Hosted tests check 4,096
reuse cycles, completely full and fragmented arenas, all 256 densities, 2,176
exact/below-boundary requests, stale retirement and identity exhaustion against
an independent cell-occupancy model. The real pointer bridge passes 512 direct
font rasters with aligned addresses, adjacent live-allocation guards, preserved
pitch/rounding padding, withheld/stale release and bounded exhaustion.
Current evidence: `/tmp/cubit-glyph-identity-checks.log`; the initial arena and
pointer results are also recorded in `/tmp/cubit-glyph-arena-checks.log` and
`/tmp/cubit-glyph-memory-final.log`. Reproduce with the Nix/Alire projects
`tests/compositor/glyph_arena.gpr` (hosted test and all-unit proof) and
`tests/compositor/glyph_memory.gpr` (hosted raster/pointer test with the font
archive built by `make -C userspace/rust fonts-host`).

The allocation policy is proved; pointer validity, object lifetime and actual
foreign quiescence remain adapter/caller obligations. Allocation may scan a
bounded extent table and is not a constant-time or hard-real-time claim.
Production Desktop integration still needs the mask handles to share its Mesa
context and the text path to use native-density placement and fallback policy.

Native CuBit/QEMU now runs all 320 mask draws using `Compositor_Glyph_Memory`
directly: reserve arena cells, raster into the returned address, import that
same storage into Mesa, complete cache read leases, retire the import, release
the arena token and only then refund the cache charge. Withheld arena retirement
keeps the address valid; confirmed retirement invalidates it before reuse. All
prior compositor/native software-rendering tests pass. Evidence:
`/tmp/cubit-mask-arena-native-{run,serial}.log`. The first attempt stopped before
boot on a shared-runtime formatting error; the runtime owner repaired it before
this verified run. No GPU or latency performance measurement is claimed.

### Glyph identity lifetime at high redraw rates

Glyph masks, read leases and arena allocations now use the shared
`Compositor_Identity` type: 64-bit storage with positive values through
`2**63 - 1`; zero is reserved. The previous native `Natural` range was only
`2**31 - 1`. At an illustrative 1,000 glyph reads per frame and 240 frames/s,
that read counter could exhaust in about 2.5 hours. This is a workload arithmetic
example, not a measured rendering rate. The larger range removes that practical
limit while preserving refusal at exhaustion and full-identity comparison for
stale callbacks. It does not reset or recycle live identities.

The common successor function has an exact non-wrapping SPARK contract. Hosted
tests use volatile inputs to exercise both 32-bit boundaries and the final
permitted identity at runtime, and reject glyph/read completions whose identities
differ only above bit 31. The complete cache/arena proofs and the existing fault,
overload, raster and pointer tests pass with the widened representations.
The backing object grows by exactly 16 KiB of identity metadata to the size
reported above; its 512 KiB pixel capacity does not change. All identities remain
local to a cache/backing owner; this is not a public IPC ABI change.

The native CuBit probe also passes all 320 fixed-arena mask draws and preceding
compositor regressions with these wider identities. Evidence:
`/tmp/cubit-glyph-identity-native-{run,serial}.log`. Production Desktop text
activation and sustained hardware refresh/latency measurements remain open.

### Masks in the Desktop compositor context

`Compositor_Cache` now reserves distinct mask slots 10..137 alongside target
slots 0..1 and client slots 2..9. The client admission/search limit stays at eight;
mask capacity cannot silently enlarge it. Checked mask imports reuse the same
library instance and failure state machine as color images. A matching retained
descriptor avoids reimport. A replacement retires the previous view before
importing the new one, and uncertain retirement prevents replacement.

`Mesa_Masks` supplies the SPARK validation and checked render/import
instantiations, with `Mesa_Binding.Masks` as the narrow audited FFI adapter.
Target retirement preserves every mask and client view. Shutdown walks the
entire owned table; a failed release stops the walk and prevents context
destruction. Its strengthened postcondition states that a safely completed
shutdown leaves every view slot empty. Arena storage still must be released
only after its corresponding import and read leases are retired.

Hosted tests populate all 128 mask slots beside color views, check retained
hits, preserve masks across target retirement, reject short capacities and
failed replacements, and inject a shutdown release failure at every mask
position. The mock library refuses destruction while any handle remains live.
Existing client-cache, composition and damage tests pass. All-unit SPARK
analysis (`gnatprove -P tests/compositor/composition.gpr -U --level=2 -j2` inside
the Nix/Alire environment) reports 418 checks, zero unproved or justified,
including the actual `Mesa_Masks` instantiations and complete shutdown loop.
Evidence: `/tmp/cubit-shared-mask-cache-final.log`. The first all-unit run found
a missing loop invariant; the final run proves it without a suppressed check.

The native pixel oracle now runs twice: 320 direct FFI draws and 320 through
`Mesa_Masks`/`Mesa_Cache`. The latter uses one Mesa context for both color and
mask views, checks repeat imports, retires/reimports targets while keeping the
mask, switches back to color drawing and finishes with complete shutdown.
Both paths and prior native regressions pass. Evidence:
`/tmp/cubit-shared-mask-cache-native-{run,serial}.log`. The Mesa Desktop backend
also compiles/links. The final proof-only invariant does not change the native
unchecked execution path.

This establishes the shared-context API and its native integration test, not
live Desktop text activation. The bounded key/arena/lease policy still needs
to be joined to these view slots, with physical text placement and a batching
strategy before drawing dense text through the accelerated path. The current
single-draw softpipe flush is not a 240 Hz text-performance result.


### Bounded mask command batches

`Compositor_Mask_Batch` holds at most 32 ordered mask commands, matching the
existing 32 glyph read leases. Its SPARK append contract preserves prior
commands, admits only valid source-over geometry, and rejects a full packet
without modifying it. A packet contains metadata, not storage authority:
the owner must acquire and retain every mask lease through confirmed completion.
`Mesa_Masks.Render_Batch` checks the entire packet and resolves all retained
mask handles through the shared context before making the foreign call.
A missing or invalid source cannot cause a partially submitted batch. Unknown
foreign completion requires restart and prevents ordinary import retirement.

The narrow FFI marshals fixed 160-byte command records and SPARK-generated
rational corners. The existing softpipe adapter preflights every command,
binds common target/viewport/shader/blend state once, then draws in order.
It keeps bounded vertex and tint arrays alive until the final synchronous
flush, followed by binding cleanup and a second flush. Thus the adapter makes
two explicit pipe flush calls per nonempty batch rather than two per glyph;
Mesa may still flush internally for state changes. No glyph pixels are copied.
This is neither a single GPU draw nor a claim of allocation-free Mesa operation.
A future asynchronous GPU implementation needs completion-owned command and
constant storage; the current stack storage relies on softpipe quiescence.

Softpipe tiles retain floating-point color between commands. This changes
rounding compared with separately flushing each glyph into an 8-bit target:
the first native comparison correctly failed on a one-level color difference.
The batch regression therefore uses independent fixed-point source-over
arithmetic with 16 fractional bits, rounding only at the end, and allows one
8-bit level for Mesa conversion. It separately bounds accumulated rounding
against serial draws. The oracle shares the already-proved rational corners;
it is not an independent proof of Mesa interpolation, font coverage, or C code.
Padding, source storage, empty batches and rejection-before-write checks remain
exact. This preserves blend order while permitting normal precision differences.

Hosted packet/cache tests cover lengths 0..32, overflow, invalid geometry,
all four completion outcomes, ordered foreign handles and missing/invalid
sources at each of the 32 positions. All-unit proof of `composition.gpr`
reports 436 checks, zero unproved or justified, including actual checked
batch instantiations. Evidence: `/tmp/cubit-mask-batch-hosted-final.log`.
Native CuBit/QEMU now passes 132 batches (all lengths 0..32 at each of four
rotations), including 32 simultaneous leases, withheld-completion retirement
refusal, three retained font masks at distinct raster densities, overlapping
tints, source/padding guards, final invalid/missing-command rejection before
writes, and zero final glyph charge. The initial exact comparison failure is
recorded in `/tmp/cubit-mask-batch-native-{run,serial}.log`. A second oracle run
exposed nearest-texel boundary ties in floating interpolation; the final blend
fixture uses 4:1 and 4:3 geometry to avoid exact ties for its source dimensions,
as the existing rectangle oracle does. This does not claim a defined sampling
choice on exact floating boundaries. Final native evidence:
`/tmp/cubit-mask-batch-final-{run,serial}.log`. Existing 640 mask oracle draws,
rectangle, affine, output, pool and target-retirement tests also pass. The
runner terminates successfully and the Mesa Desktop backend links. Production
Desktop/Display staging and the original GRUB configuration are restored.

This adds the batching API, not live Desktop text activation. The next required
integration is a single owner joining glyph keys, arena allocations, leases,
Mesa mask slots and per-output physical placement. Hardware frame-time and
keypress-to-photon evidence remains outstanding.


### Physical placement of DPI-sized glyph masks

`Compositor_Glyph_Placement` converts a logical glyph origin relative to its
output into the nearest physical pixel (half-pixel ties toward positive
infinity). It uses the mask dimensions from `Compositor_Glyph_Layout.Plan`
for that output's DPI, then emits unit-scale source-over geometry with the
output rotation. It clips the destination without changing the original
source phase. Far off-screen logical positions are rejected before narrowing
to the affine ABI's coordinate bounds.

This avoids scaling a ceil-rounded mask back into a logical 32-by-17 box.
For example, a 5:4 raster has 40-by-22 storage; stretching its 22 rows into
21.25 physical rows would resample an already rasterized glyph. The placement
path instead uses those 22 rows directly. A caller must use a mask rasterized
at the same output density, and retain its lease through completion. Logical
text origins should be accumulated before snapping each glyph independently;
adding separately rounded physical advances would introduce spacing drift.

Hosted tests pass 8,192 placement cases across all 256 supported DPI ratios,
four rotations, partial clipping and signed/extreme logical origins. Every
output pixel is checked for exact rectangle membership, and every covered
pixel maps to its corresponding source texel centre, including after rotation.
A separate rounding oracle checks 601 signed positions for each DPI ratio.
All-unit SPARK analysis of `glyph_placement.gpr` proves 292 checks, zero
unproved or justified. This includes the exact nearest-pixel rounding interval,
valid clipped affine geometry, unchanged snapped source origin, unit scale,
matching raster dimensions and rotation. Coverage equivalence and integer
texel-centre correspondence are exhaustive checks over the hosted fixture,
not additional quantified proof claims. Evidence:
`/tmp/cubit-glyph-placement-final.log`. The earlier postcondition timeout was
resolved with explicit bounded conversion contracts, without suppression.

Native CuBit/QEMU passes 192 cases using a sharp binary mask pattern, six
raster/output DPI ratios (including 1:16, 16:1, 5:4 and 3:2), all four rotations,
four origins, full and partial damage. Every output pixel, target padding and
source storage byte is checked exactly. The retained Mesa context and mask
imports are retired before fixture storage goes away. Existing batch and
compositor regressions also pass. Evidence:
`/tmp/cubit-glyph-placement-native-{run,serial}.log`. The harness exits
successfully after its final fault scan; production Desktop/Display staging
checks match and GRUB has no changes.
The later live-text integration below uses this policy; no animation is added.


### Single owner for glyph resources and queued reads

`Compositor_Glyph_Renderer` joins the existing key cache, fixed backing arena,
Mesa mask slots, DPI placement and 32-command batch under one limited owner.
It uses the caller's existing `Mesa_Cache.State`; it does not create a second
Mesa context. That context and its mask slots must remain exclusively associated
with this owner. Other compositor code can still use the separate client and
target slots. The owner itself must stay at a stable address while imports or
queued work exist.

On a miss it charges the cache, reserves backing, rasterizes directly into that
backing, imports the mask in the same context, then publishes the key. A bounded
pass can evict unpinned entries to relieve cache capacity or arena fragmentation;
it never waits for a reader. Allocation refusal with all candidates pinned
leaves existing queued commands intact. Build failure retires any partial import
before releasing backing and refunding its charge. There are still 128 keys,
32 read leases, a 512 KiB payload limit and the existing 512 KiB physical arena;
allocation alignment and fragmentation can cause refusal below the payload limit.

Queueing requires a matching output density and uses physical glyph placement.
A full batch or a change in target/output dimensions flushes the prior batch
before appending. The caller must explicitly flush before any intervening
non-text draw, target replacement/retirement or presentation. Shutdown cancels unsent work, retires all mask
imports before their backing allocations, and leaves client/target imports to
the outer context owner. A quiescent rendering failure clears reads and disables
the text owner; the affected target may have partial output and must be repainted
through fallback. Unknown draw or release completion prevents ordinary cleanup,
keeps storage charged and requires recovery rather than reuse.

The SPARK owner invariant associates every queued command with an active lease
for that exact mask slot. Cache reader-frame contracts preserve existing reads
through reserve, publish and unrelated retirement. Explicit proved assertions
at backing release exclude both pinned slots and references from the pending
packet. All-unit `glyph_renderer.gpr` proof reports 694 checks, zero unproved or
justified, including the checked owner and its instantiated cache policies.
Evidence: `/tmp/cubit-glyph-renderer-final.log`. Hosted fault tests pass cache
hits, automatic 32-command rollover, 250 distinct keys and eviction, three large
pinned masks exhausting payload admission, rounded-arena exhaustion below
that payload limit, recovery after completion, target rollover, density
mismatch rejection, raster/import
failures, all draw outcomes and failed retirement quarantine.

`Compositor_Glyph_Storage` is a narrow trusted mapping from cache slots to the
existing fixed arena tokens, pointers and capacities. Its implementation is
SPARK-off because it owns the pointer-bearing limited backing object and calls
the raster FFI. It makes no eviction, batching or retirement decisions; the
SPARK renderer decides when those operations are permitted. Its frame contracts,
correct pointer/capacity correspondence, font parsing/writes, Mesa completion
truthfulness and the caller's exclusive context association are proof boundaries.
The arena allocator and reader/key policies retain their separate proofs.
The foreign rasterizer can still allocate transient font scratch memory.

The hosted rounded-arena fixture pins seven differently sized masks. Their
payload totals 523,936 bytes, but 128-byte rounding occupies all 524,288 backing
bytes. A further 64-byte glyph passes payload admission but cannot allocate;
its temporary charge is refunded without disturbing the seven queued reads.
After completion, one eviction lets it proceed. This is distinct from the
three-large-mask case that refuses admission at the payload budget itself.
Evidence: `/tmp/cubit-glyph-renderer-pressure.log`.

The first native run passed 260 real-raster pixel oracle cases with eviction,
32 pending reads canceled without drawing, payload pressure and recovery, and
zero final charge, plus previous regressions and the runner's final scan.
Evidence: `/tmp/cubit-glyph-owner-native-{run,serial}.log`. The final native run
also passes rounded-arena refusal/recovery with the seven masks occupying every
cell below the payload limit. Its complete owner marker and previous regression
markers pass in `/tmp/cubit-glyph-owner-final-{run,serial}.log`. The harness
exits successfully after the final fault scan. Production Desktop/Display
staging matches and GRUB has no diff.
The live-text integration below now uses this owner in Desktop's immediate
drawing loop. Per-output scene traversal remains outstanding.


### Live Desktop retained text and bounded scene recovery

The opt-in Mesa Desktop now routes `drawUIText` through the retained glyph
owner in its existing Mesa context. Each call prepares at most 32 glyphs per
batch and completes that batch before returning to other drawing. Opaque
backgrounds are filled first; each glyph is clipped to its existing logical
advance and line-height cell intersected with current damage. Existing 24-bit
theme colors receive an opaque alpha byte for mask tinting. A warm glyph is
reused without another rasterization or mask upload. No extra scene snapshot
is allocated for this path. The default legacy Desktop still uses CPU glyphs.

Preparation failure disables the Mesa context only after safe retirement and
fills that chunk through the existing CPU renderer. A known-quiescent draw
failure can leave changed pixels, so the caller instead reconstructs the entire
current damaged scene before presentation. `Compositor_Text.Finish` permits
only one replay; a second damaged attempt requires restart. Cached window
moves use the same scene reconstruction before publishing. Unknown foreign
completion forbids ordinary reuse/retirement and exits the compositor for
process recovery. Mesa client failure also retires glyph backing before
destroying their shared context. Target retirement alone preserves cached
masks for reuse.

`Compositor_Text` proves clipped physical bounds and the two-attempt recovery
decision. The glyph owner's full-view type invariant preserves the association
between queued commands and read leases across its public API. All-unit
`glyph_renderer.gpr` proof now reports **752 checks, zero unproved or justified**.
Hosted Desktop modes 0–6 pass warm reuse, retained masks across target renewal,
raster/import rejection, both quiescent draw failures, unknown completion and
failed target retirement. Evidence: `/tmp/cubit-desktop-text-final-proof.log`.
The backing bridge's empty default initialization is part of its trusted
contract, alongside the pointer/FFI boundaries described above. The existing
Desktop event/drawing loop itself is not claimed to be proved SPARK.

The normal live CuBit fixture passes with both retained-mask text and imported
client-compositing markers, 194,673 checked cube pixels, client retired-buffer
reuse, renderer target retirement, and final Desktop pixel charge zero. The
captured desktop has been visually inspected for title and taskbar text.
Evidence: `/tmp/cubit-desktop-text-live-{run,serial}.log` and
`/tmp/cubit-desktop-text-live.png`. The runner exits successfully after its
final scan; default Desktop/Display staging matches and GRUB is unchanged.

This is still the existing unit-density scene canvas. It does not yet provide
native-density per-output decorations or remove the scaled-output canvas
resampling path. QEMU TCG results establish functional integration, not GPU
acceleration, 240 Hz operation, or physical input-to-photon latency.

Reproduce the hosted text checks from the repository root:

```sh
nix develop -c gprbuild -P tests/compositor/glyph_renderer.gpr -j0
nix develop -c tests/compositor/build/glyph-renderer/glyph_renderer_tests
nix develop -c bash -c 'for mode in 0 1 2 3 4 5 6; do tests/compositor/build/glyph-renderer/desktop_text_tests "$mode" || exit; done'
nix develop -c gnatprove -P tests/compositor/glyph_renderer.gpr -U --level=2 --report=all -j0
```

The native partial-draw fixture is built by passing `text` to
`tests/compositor/build-desktop.py` with the existing native Mesa build
directory. It defines `CUBIT_MESA_FAIL_TEXT_PARTIAL` only in that test image:
Mesa writes and finishes the first glyph, then reports a quiescent failure.
Normal builds do not include this fault. Run `mesa-window` with that image as
`CUBIT_DESKTOP_IMAGE`, the existing cube app as `MESA_WINDOW_IMAGE`,
`MESA_WINDOW_SCENE=cube`, `QEMU_MEMORY=1G` and
`CUBIT_TEST_TARGET_RETIREMENT=1` under the shared build lock.
The physical-client requirement must be omitted because the test deliberately
disables the shared Mesa context. In addition to the runner's normal checks,
require the text scene-repaint and CPU-fallback markers and inspect the captured
desktop; the cube pixel oracle alone does not validate shell text.

The partial-draw capture shows successful software recovery. Four stable text
regions match the normal Mesa capture **byte for byte**: window title
`[108,88,200,108)`, Apps label `[38,739,90,761)`, task label
`[112,739,250,761)`, and volume `[892,739,927,761)` at 1024×768.
All 20,370 RGB components match; the changing clock is excluded. This checks
this scene's restored text, not every possible overlap or font. Evidence:
`/tmp/cubit-desktop-text-fault.png` and the normal/fault `-serial-mesa.ppm`
captures.

The fault fixture also passes the final native runner scan and 194,673-pixel
cube oracle, with both scene-repaint and CPU-text-fallback markers, completed
target retirement and zero final Desktop pixel charge. No uncertain-completion
restart or exception was reported. Evidence:
`/tmp/cubit-desktop-text-fault-{run,serial}.log`. Default Desktop/Display staging
is restored exactly and GRUB remains unchanged.


### Physical-density software glyph painter

`Compositor_Glyph_Software` paints an existing A8 glyph mask directly into a
caller-owned pixel array. It uses the same raster layout and independently
snapped origin as the Mesa path, reverses the output rotation, and samples
exact mask pixels without scaling the ceil-rounded raster a second time.
The caller supplies a mask produced for the selected output density. Straight
ARGB tint and coverage are composited over premultiplied ARGB with one final
integer rounding per channel, including destination alpha.

The painter allocates no glyph cache, target buffer or scene snapshot. Target
pitch is measured in pixels. Its admission contract checks mask capacity,
target capacity and pitch; SPARK proves the source and target accesses remain
in bounds, and that every pixel outside the clipped glyph region remains
unchanged. The region is explicitly constrained to the physical output and
requested damage. These are typed-array guarantees: any eventual raw-address
bridge must establish their capacities, ownership and non-aliasing premises.
The painter itself has no foreign calls or SPARK-off implementation. Font
rasterization and Mesa remain the existing foreign boundaries.

The all-unit software-painter project passes **383 proof checks, zero unproved
or justified**. Hosted tests check 12,288 output frames over all 256 supported
scale ratios, four rotations, four origins (including the coordinate extreme),
full/partial/inverted damage, and padded/guarded targets. A separate arithmetic
oracle checks 262,144 blend combinations. Evidence:
`/tmp/cubit-glyph-software-boundaries.log` and
`/tmp/cubit-glyph-software-build/obj/gnatprove/gnatprove.out`.

```sh
nix develop -c gprbuild -P tests/compositor/glyph_software.gpr -p -j2
nix develop -c tests/compositor/build/glyph-software/glyph_software_tests
nix develop -c gnatprove -P tests/compositor/glyph_software.gpr -U --level=2 --report=all -j2
```

This provides the checked physical-pixel primitive required for a crisp software
fallback. The retained integration below connects it to Desktop's glyph backing.
Desktop still needs to traverse each output at its own density; these glyph
changes do not remove the scaled-output canvas copies by themselves.

The native placement fixture compares this painter, Mesa and an independent
pixel oracle over **384 cases**: six DPI ratios, four rotations, four origins,
two damage clips and two coverage/tint variants. The software result must
match integer arithmetic exactly. Mesa placement and opaque binary masks must
match exactly; translucent color/alpha channels permit at most one level of
foreign floating-point/UNORM rounding. Source bytes and target padding remain
guarded. The compiled native painter has no allocator or pixel-copy imports.
Evidence: `/tmp/cubit-glyph-software-native-{run,serial}.log`.

The complete native harness exits successfully after its final fault scan;
all preceding compositor, mask-batch, glyph-owner pressure/retirement and
softpipe baseline checks pass. Default Desktop/Display staging compares equal
and GRUB has no diff. These are CuBit functional tests under QEMU TCG, not
hardware throughput or input-to-photon measurements.


### Retained glyph fallback after Mesa shutdown

The glyph owner now has a one-way software transition. `Use_Software` first
requires known foreign quiescence, cancels unsent queued reads, and retires
every imported mask view. It preserves cache identities, raster backing and
the exact charged-byte count. Its postcondition explicitly proves that every
mask view is empty on success. Only after that succeeds may Desktop shut down
the shared Mesa context. A failed or uncertain release refuses ordinary
painting/reuse and retains backing for process recovery. A repeated software
transition does not re-enable a painter disabled by a later resource failure.

In software mode, a warm draw reuses the same A8 allocation; a miss rasterizes
directly into the existing arena. No second cache or Mesa context is created.
Each synchronous paint acquires and completes a normal cache read lease; the
owner invariant prohibits queued GPU commands while in software mode. Eviction
still follows the bounded unpinned-victim policy. Fully clipped draws return
before allocating or rasterizing. Equivalent densities such as 5/4 and 10/8
share masks: `Same_Raster` checks the rational em size and storage dimensions
without requiring identical numerator/denominator encodings.

Desktop uses this painter after a known-quiescent Mesa failure, retaining the
existing bounded scene replay before presentation. If software preparation
fails after earlier glyphs have changed the target, it disables that painter
before requesting replay; the existing legacy renderer can complete the replay.
The diagnostic `retained software text active` distinguishes this path from
Mesa and from the older uncached fallback. Normal Desktop coordinates are still
the unit-density scene canvas; this integration accepts physical-output
geometry but does not yet supply it from per-output scene traversal.

Two narrow, trusted memory bridges remain explicit. `Compositor_Glyph_Storage`
validates the current arena token, recorded raster layout and capacity, rejects
virtual source/target overlap (including partial overlap), and exposes a read-only
A8 array to the proved painter. `Compositor_Glyph_Target` validates the existing
image descriptor, capacity, writable flag and exact output dimensions, then
exposes the caller-owned target as a typed array. Their pointer overlays are
SPARK-off; actual mapping authority, stable backing and absence of physical
aliases remain caller/runtime obligations. Geometry, clipping, arithmetic,
cache/lease policy and pixel-array writes stay in SPARK. Font parsing and Mesa
completion truthfulness retain their existing trust boundaries.

The all-unit owner/backend proof passes **893 checks, zero unproved or
justified**. Hosted tests cover 32-command cancellation without rendering (including 32 distinct masks),
warm reuse after context destruction, equivalent DPI reuse without rerasterizing,
software eviction, cold software startup, failed rasterization without writes,
invisible-draw elision, unknown draw/release refusal, target stride/tail guards,
six descriptor rejections, partial virtual overlap and stale backing rejection.
All seven Desktop text fault modes pass with the new transition. Evidence:
`/tmp/cubit-retained-fallback-retirement.log` and
`tests/compositor/build/glyph-renderer/obj/gnatprove/gnatprove.out`.

Fresh native builds and the complete native harness pass with this integration.
The owner fixture now checks 260 Mesa and 260 retained-software real-font
frames against an independent pixel oracle. It cancels 32 distinct queued
masks without drawing, retires their imports, destroys the Mesa context, and
then exercises retained software reuse and eviction with final charge zero.
The existing 384 software/Mesa placement and blend cases and preceding
compositor/pressure/retirement checks also pass. Evidence:
`/tmp/cubit-retained-fallback-native-{run,serial}.log`.

The live Desktop partial-draw fault fixture passes its 194,673-pixel cube
oracle and reports `retained software text active`, with no legacy text
fallback or uncertain-completion restart. The title, Apps label, task label
and volume regions match the prior normal Mesa capture byte for byte: all
20,370 checked RGB components agree. The complete capture was visually
inspected. Renderer targets and output readers retire, and tracked Desktop
pixel charge returns to zero. The final headless scan passes; default
Desktop/Display staging is restored exactly and GRUB remains unchanged.
Evidence: `/tmp/cubit-retained-fallback-live-serial.log`,
`/tmp/cubit-retained-fallback-live.png` and the combined native run log above.
These are functional CuBit/TCG results; GPU hardware and physical latency
validation remain outstanding.


### Direct scene traversal into each output writer

The opt-in Mesa Desktop now selects an output-local physical drawing context
without changing logical window/input coordinates. It consumes each pool
writer's existing `Compositor_Repaint` repair list, traverses current scene
state, and publishes only after synchronous rendering completes. The legacy
backend retains its previous canvas path. A held Display slot is never chosen
as the writer. Native-path redraw and drag functions enqueue invalidation;
they do not paint a logical desktop image first.

Rectangle cells use the existing proved affine pixel-centre coverage, avoiding
fractional-scale overlaps from outward-rounded *damage* geometry. Text supplies
the actual output density and physical damage to the retained Mesa/software
mask renderer. Clients are sampled straight into the writer with their original
logical buffer extent, so an old attachment does not stretch merely because a
window was resized. Quiescent Mesa failure uses the existing proved inverse
sampler directly against the same writer. Cursor repair redraws the old scene
footprint and blends the current cursor against each physical destination;
it does not reuse an underlay from another pool slot. Wallpaper uses the
physical output extent and damage directly. Current native admission remains
unrotated; the geometry library's rotation proofs do not establish rotated
wallpaper/toolkit integration.

Settings remains an explicit compatibility exception: its existing toolkit
paints into the retained logical staging reserve and the visible client area
is sampled into the output in software. This temporary source is never imported
into Mesa, avoiding a new retained-source lifetime. Settings content and existing
application buffers are still unit density. The scene reserve remains allocated
for Settings; the opt-in path no longer allocates the full-size drag snapshot.
Neither native-density client configuration nor zero-copy GPU scanout is claimed.

Proof boundary: clipping, inverse sampling, glyph cache/painting and pool/repair
policy reuse their SPARK units. The existing Desktop service, wallpaper rasterizer,
raw address overlays and orchestration are not whole-service proofs. Their
mapping authority, pitch/capacity consistency and synchronous Mesa completion
remain trusted integration obligations, checked by native regressions. This is
an intermediate integration gate; extracting the remaining pixel-memory bridges
and render-scope invariants into checked interfaces is still required.

The first unit-scale native cube run passes its 194,673-pixel oracle and nine
retired-buffer reuse frames. The Desktop staging counter remains zero; target
and reader teardown succeeds and pixel charge returns to zero. Its title, Apps,
task label and volume pixels match the previous retained-software capture in
all 20,370 RGB components, and its complete capture was inspected. Evidence:
`/tmp/cubit-native-output-normal{,-run}.log` and
`/tmp/cubit-native-output-normal.png`. This initial result precedes final
Settings/drag cleanup; the complete mixed-output run below uses the final code.
The fault and default-backend runs below also use the final code.

The first mixed-output attempt exposed insufficient functional-test settling:
the initial capture still contained the pre-client placeholder and the drag
capture preceded input dispatch. A later capture showed the correct split
position. Inspection also found and removed a duplicate legacy drag repaint.
No pixel assertion was weakened, and this TCG run supplies no refresh-rate or
hardware latency evidence.

The final native mixed-output fixture passes with 1024x768 and 1280x720 outputs:
window dragging across the seam, double-click maximize, client/cursor restoration,
Settings, above/left/below/offset arrangements, primary changes and maximized work
areas, 125/150% scaling, minimum-workspace rejection, unchanged physical modes,
primary reflow and mixed-scale cursor movement. All existing pixel assertions
remain intact. `CUBIT_TEST_SETTLE_SECONDS=3` explicitly extends functional TCG
settling (the legacy default remains 0.8 seconds); this is not a frame deadline.
Evidence: `/tmp/cubit-native-output-dual-idle{,-run}.log` and its `settings-*.png`
captures. The final runner reports `PASS desktop-dual-output`; the VM was stopped
only after the observer completed all assertions. The 125% primary capture was
visually inspected: native caption/taskbar text is distinct from Settings' still
resampled client content. Desktop scene-transfer staging remains zero.

The interaction fix removes duplicate legacy-canvas drag rendering, avoids
repainting an already-focused top window, and skips native presentation for a
title press/release with no visible change. Otherwise even a cursor-sized
invalidation could force a large stale-writer repair between click edges. Actual
movement, focus/z-order changes, resizing and maximizing still invalidate. The
500 ms double-click policy is unchanged. This does not replace the outstanding
source-arrival timestamp work for general overload causality. Physical writer
repair pixels and draw time now feed the existing counters at the render scope.

The final partial-text-failure native run passes the same 194,673-pixel cube
oracle, nine retired-buffer reuse frames, target/reader retirement and zero
tracked pixel charge at teardown. It reports retained software text, with no
legacy text fallback or uncertain-completion restart. All 20,370 RGB components
in the four stable text regions match the normal capture exactly. The fault
capture was visually inspected. Evidence:
`/tmp/cubit-native-output-fault{,-run}.log`,
`/tmp/cubit-native-output-parity.log` and `/tmp/cubit-native-output-fault.png`.
The single-output opt-in path holds four tracked full-sized pixel allocations
(three presentation slots plus the temporary Settings reserve), rather than
also allocating the old drag snapshot. This does not count Mesa, client or font
storage and is not a whole-process memory measurement.

The final default legacy backend also passes the native dual-output fixture at
its unchanged 0.8-second functional settling allowance: split drag, per-monitor
maximize, wallpaper/cursor restoration and Settings. Its historical logical
staging counter remains active, as expected; the zero-staging claim applies to
the opt-in scene path. Evidence: `/tmp/cubit-native-output-legacy{,-run}.log`.
After all runs, staged Desktop and Display binaries match their default builds,
GRUB has no diff, and the shared lock and all own VMs/jobs are released. This
slice reuses existing SPARK policies; it adds no new formal-proof result and
makes no physical presentation, 240 Hz or keypress-to-photon measurement claim.

Next integration work is to replace Settings' compatibility staging with native
output-aware toolkit primitives, extract/check the remaining render-scope and
pixel-memory glue, and wire native-density client configuration/allocation.
The GPU-resource/fence and causal source-arrival trace gates remain open.

### Settings renderer separation (2026-10-01)

`Desktop_Settings.Render` now requires seven synchronous drawing callbacks;
its layout does not dereference the Canvas pixel address. The legacy `Draw`
entry point explicitly supplies canvas primitives and the wallpaper painter.
`CuBit.UI.Control_Renderer` decomposes buttons, tabs and bevels into required
fill, stroke and text callbacks. Existing canvas control entry points use the
same generic implementation, so native bindings can preserve the toolkit's
appearance without duplicating its control styling or installing global hooks.

The hosted `tests/settings-renderer` regression passes 144 page/preference/clip
cases with a null pixel address, through both direct operation recording and
the actual toolkit control decomposition. Additional calls cover all button
styles, tab states/orientations and empty/tiny controls. This is runtime
regression evidence, not a new SPARK proof.

The native Mesa-selected `desktop-dual-output` regression passes dragging,
per-monitor maximize, cursor/wallpaper restoration and Settings navigation.
The Appearance, Displays and returned-Appearance captures match the preceding
renderer-separation run exactly on both outputs over rows 0 through 699:
12,902,400 RGB components, zero differences. The changing taskbar clock is
outside that comparison. The Displays capture was also visually inspected.
Evidence: `/tmp/cubit-settings-control-renderer.log`,
`/tmp/cubit-settings-control-native-run.log` and
`/tmp/cubit-settings-control-parity.log`.

Settings still stages its pixels in Desktop: the physical-output callbacks,
including clipped gradient and wallpaper preview sampling, are not wired yet.
This separation does not establish native-density Settings, removal of that
allocation, hardware acceleration or display latency measurements.

### Native Settings controls (2026-10-01)

The subsequent binding connects Settings fills, strokes, gradients, text,
buttons and tabs directly to the currently acquired output writer. Logical
layout and hit coordinates stay unchanged. Text uses the output's density and
physical damage, including the retained software glyph fallback. The generic
Settings canvas has a null pixel address in this path, so it cannot silently
write the former logical window image. The legacy backend still supplies its
ordinary canvas.

The full Settings client rectangle is no longer painted then copied/scaled.
Only its 236 by 150 wallpaper preview remains rasterized into private storage
and sampled into output damage; it bypasses Mesa's persistent source imports.
Preview work is skipped when its physical clip is empty. The full private scene
reserve is still allocated and still participates in layout admission: removing
the preview staging and separating layout limits from that reserve are pending.
The zero whole-desktop staging counter does not count this preview sampling.

`Compositor_Gradient` proves bounded row weights and rounded channel blends,
including the original-rectangle origin needed for clipped gradients. Its
focused GNATprove run passes 14 checks with zero unproved and no assumptions.
The hosted oracle covers all 16,777,216 channel combinations, all rows for
heights through 4096, the maximum integer extent and RGB endpoints. Run in Nix:

```sh
cd kernel
alr exec -- gprbuild -p -P ../tests/compositor/gradient.gpr
../tests/compositor/build/gradient/gradient_tests
alr exec -- gnatprove -P ../tests/compositor/gradient.gpr -u compositor_gradient.adb --level=2 -j1 --report=all
```

Proof evidence: `/tmp/cubit-settings-gradient-proof.log` and
`tests/compositor/build/gradient/obj/gnatprove/gnatprove.out`. This does not prove
the main procedure's mutable clip scope, raw preview memory access or the entire
toolkit layout; those remain explicit trusted integration boundaries.

The initial native binding passes the full mixed-output, arrangement,
primary-switching and 125/150% scaling fixture. Unit-scale Appearance/Displays/
returned-Appearance captures match the preceding staged mixed-output run over
both outputs' rows 0 through 699, and the 125% primary Settings capture was
visually inspected. Evidence: `/tmp/cubit-settings-native-controls-run.log` and
`/tmp/cubit-settings-native-controls.settings-scale-primary-head-0.png`.
After integrating the proved gradient helper and preview damage culling, both
normal and partial-text-fault Desktop binaries compile. The final-source fault
binary passes the full mixed-output, arrangement, primary and scaling fixture.
Serial output confirms the partial batch failure, scene repaint and retained
software text path. Twelve captures (six phases on two outputs), including the
125% primary Settings view, match the normal-path captures over rows 0 through
699: 29,030,400 RGB components, zero differences. The fault capture was visually
inspected. Evidence: `/tmp/cubit-settings-native-fault-run.log`,
`/tmp/cubit-settings-native-fault.log` and
`/tmp/cubit-settings-native-fault-parity.log`. The final independent gradient
oracle also passes (`/tmp/cubit-settings-gradient-oracle.log`).

Both test runners finished successfully after their exact VMs were stopped via
QMP following all observer assertions. Shared locks are released, staged Desktop
and Display match their default build outputs, and GRUB is unchanged. No
hardware or timing claim follows from these TCG functional tests.

### Direct Settings wallpaper preview (2026-10-01)

`Desktop_Wallpaper.Paint_Output` now samples the immutable wallpaper asset
directly into the acquired output buffer. Settings no longer paints a logical
preview then copies/scales it. Its controls, text and preview all use output-local
damage. The obsolete client-blit option used to bypass Mesa for a transient
preview source was removed; previews no longer masquerade as client surfaces.

`Compositor_Image_Sampling` proves Fill/Fit extent bounds, centered placement,
half-pixel edge clamping and bilinear source-index bounds (45 checks, zero
unproved). `Compositor_Sampling.Fine_Map` preserves fractional source coordinates
through the existing rational output transform. The pixel API delegates to the
same implementation and retains its contracts. Its project proof reports 130
checks with zero unproved, including the existing display geometry dependencies;
that count is not 130 newly added checks. Both reports contain no assumptions.

Hosted validation passes 395,307 image-placement/sample cases, centre-clamp
coverage through size 255, and 139,264 rational/offset sampling cases plus
rotations and extremes. The real pointer writer is linked to synthetic exported
atlases and passes 384 style/scale/rotation cases with negative origins, padded
rows, guard pixels and tiled-damage equivalence. At unit scale it matches the
legacy painter exactly. The 144-case Settings callback test also still passes.
Commands and boundaries are in `tests/compositor/wallpaper-output.md`.

The native normal build passes the full mixed-DPI, arrangement, primary,
scaling, drag/maximize and cursor/wallpaper regression. Unit-scale Settings
captures match the preceding staged-preview build over 14,515,200 RGB components
on two differently sized outputs. After the fixture, an additional native UI
interaction selects primary 125% scaling and the Appearance page; serial confirms
the scale and the preview capture was visually inspected:
`/tmp/cubit-settings-direct-preview.appearance-125-head-0.png`.

The final-source partial-text-fault build passes the basic two-output native
regression, including Settings navigation. It confirms scene repaint and retained
software text recovery; 12,902,400 captured RGB components match the earlier
normal staged Settings at unit scale. Evidence is in
`/tmp/cubit-settings-direct-preview{,-fault}{,-run}.log`,
`/tmp/cubit-settings-direct-preview{,-fault}-parity.log`,
`/tmp/cubit-wallpaper-output-hosted.log`, `/tmp/cubit-image-sampling-proof.log`
and `/tmp/cubit-subpixel-sampling-{proof,tests}.log`.

Both native runners and all hosted jobs are terminal. The exact native VMs were
stopped via QMP after successful observers/captures; shared locks are released.
Staged Desktop/Display match their default builds and GRUB is unchanged.

The private scene allocation is still reserved, although Settings no longer
uses it. Remove it after separating logical layout-capacity admission and the
remaining legacy-pointer readiness checks from actual native pixel storage.
The pointer writer's allocation authority, atlas correspondence and pixel
encoding remain trusted integration boundaries; the new pure sampling units do
not establish a proof of the whole Desktop service. Native rotation admission,
GPU resources/fences, client density and hardware latency evidence remain open.

### Native scene allocation removed (2026-10-01)

The native backend no longer reserves a private scene image or drag image.
`privateSceneAddr`, the legacy `backBufferAddr` and its capacity remain null/zero
in that mode. Text and client rendering recognize the active output writer
instead of requiring a legacy pointer. Pixel helpers avoid evaluating legacy
logical offsets before their native branch or bounds checks. The default legacy
backend retains its allocation and capacity behavior.

`Compositor_Workspace` separates logical admission from pixel storage. It bounds
each coordinate extent to 65,535 and requires dense logical stride arithmetic to
fit `Natural`; it does not allocate that logical footprint. Physical output
buffers still have their existing validation and size limits. Native layout
changes use this policy, while the legacy path still checks its scene capacity.
The workspace proof passes 8 checks with zero unproved; its hosted fixture checks
every supported height's admission boundary and verifies a logical workspace
larger than the old 16MiB scene cap. Evidence: `/tmp/cubit-workspace-proof.log` and
`tests/compositor/build/workspace/obj/gnatprove/gnatprove.out`.

The native mixed-output regression passes primary switching, monitor arrangement,
125/150% scale, cursor repair, dragging/maximize and Settings navigation. Twelve
captures match the preceding direct-preview build over 29,030,400 RGB components.
The actual allocation ledger contains exactly six registered targets for the
1024x768 and 1280x720 virtual outputs: 20,496,384 bytes, down from 34,209,792 bytes
with the earlier scene allocation. The saved 13,713,408 bytes belong to the
compositor pixel ledger; Mesa, fonts, clients and other process memory are outside
this figure. Evidence: `/tmp/cubit-no-scene{,-run}.log` and
`/tmp/cubit-no-scene-parity.log`. Reproduce the ledger check with
`tests/compositor/check-native-storage.py`; commands are in
`tests/compositor/workspace.md`.

The native runner finished successfully after its exact VM was stopped following
all observer assertions. Staged Desktop/Display match their default builds, GRUB
is unchanged, and the shared build window is released for the graphics agent's
requested launcher changes. The subsequent dedicated fault/retirement run also
passes on this allocation-free scene path: injected text-batch failure activates
retained software text, the Mesa cube completes nine frames with retired-buffer
reuse, and all three 1024x768 targets retire. The pixel ledger falls from
9,437,184 bytes to zero with no remaining output readers. Evidence:
`/tmp/cubit-no-scene-fault{,-run}.log`; the native runner and storage checker both
pass. The 90-second fixture finished through its configured QEMU timeout.
This is not a complete Desktop proof or a hardware latency/refresh measurement.


### Bounded toolkit frame owner (2026-10-01)

`Client_Frame_Pair` now provides the two-buffer owner needed for `UI.App`
migration. It reuses the existing proved frame lifetime, repaint debt, canvas
geometry and publication codec. Its narrow serialized adapter handles actual
IPC, owned allocations and protection through `Client_Frame_Buffer`; that
adapter remains SPARK-off and must not be represented as a whole-owner proof.

A canonical configuration resets both slots' repair debt. Only a retired,
writable candidate may be resized/reallocated; the visible frame remains held.
Painting exposes that candidate temporarily, successful publication withdraws
access and alternates slots, and rejected/cancelled work retains debt. Publishing
maps logical changed cells to physical damage separately from repair work.
No previous-frame pixel copy is performed. At most two allocations exist,
each capped at the protocol's existing 16 MiB limit; failed retirement cannot
be escaped by allocating a third buffer. This limit still excludes 4K BGRA
buffers and is not a claim of complete high-resolution support.

The native fixture verifies 12 frames, full retained images after partial
repair, resize, cancellation, insufficient repair rejection, allocation bounds,
and visible-loan retention followed by destruction and final reclamation.
All protocol/fault gates pass in `/tmp/cubit-frame-pair-staged.log` and `.serial`.
This used explicitly recorded staged boot services because unrelated CCL enum
changes prevented a fresh process-manager build; the new fixture itself was
freshly linked. `/tmp/cubit-frame-pair/staged-inputs.json` records exact inputs.
Ordinary application adoption, fractional-configured frame-owner tests, retry
scheduling and raw-renderer scaling remain integration work; no latency or
240 Hz claim follows from this correctness fixture.


### First ordinary application on protected frames (2026-10-01)

Files now opts into protected frames through `CuBit.UI.App.Open`. `Window` is
limited/noncopyable. The harness obtains the compositor's canonical logical
size, physical layout and density, acquires a writable candidate before drawing,
and renders its full repair debt from application state. Publication reports
changed content separately from repair and withdraws writable canvas access.
No previous-frame copy is added. Full follow-up rendering cancels the current
paint interval and reacquires it with full damage before publishing.

The harness retains deferred damage if configuration querying fails. Pending
publication gets a timed retry without replacing an earlier application timer;
input wakes the existing deferred wait immediately. `Client_Frame_Wakeup` proves
the minimum-deadline contract and termination (2 checks, none unproved or
justified). The four-millisecond pending-work retry is not a regular frame clock
or a measured latency bound. GPU completion-driven waking remains future work.
Reopening cannot discard a pending input tracker or reuse its token history;
frame-owner reset requires no retained allocations. Uncertain closure retains
references for a later close attempt.

Fresh native Files, desktop-shell, Config Inspector, Devices and Boot Logs
builds pass. The 90-second `files` regression passes column resizing, scrollbar
click/drag, wheel scrolling, refresh, directory entry/back navigation, retained
window movement and the final fault scan. The accepted protected publication
marker is at line655 of `/tmp/cubit-app-frames.serial` and is now required by the
persistent Files runner. Logs: `/tmp/cubit-app-frames-native.log`; proof:
`tests/compositor/build/client-wakeup/obj/gnatprove/gnatprove.out`; source hashes:
`/tmp/cubit-app-frames-source.json`. A late screenshot attempt found the VM had
already exited, so no visual inspection or exact displayed-pixel claim is made.

Migration is intentionally still in progress: other applications retain their
existing attachment path until opted in and tested, and the CCL and NetSurf raw
renderers require density-aware integration. The temporary legacy branch must
be removed after those migrations. Actual per-output configured application
scaling, stalled-publication fault/overload testing, complete input tracing,
hardware scanout and hardware performance measurements remain goal requirements.
`UI.App` and the allocation/IPC adapter are not whole-program SPARK proofs;
proved geometry/debt/lifetime/codec policy remains distinct from those boundaries.


### Protected toolkit rollout (2026-10-01)

Devices, Config Inspector and Boot Logs now opt into the same protected-frame
harness as Files. Their rendering uses the existing toolkit, so this changes
buffer ownership without adding a source-image copy. The separate `managed-ui`
headless profile starts all three with current staged application/service
binaries. It requires every application-ready marker and exactly three first
successful protected publications; each application owns one window and emits
that marker only once after an accepted publication.

Fresh builds and the full 90-second native run/final fault scan pass in
`/tmp/cubit-managed-ui.log` and `/tmp/cubit-managed-ui.serial` (ready703–705,
publications707–709). A capture taken after all three publications was visually
inspected at `/tmp/cubit-managed-ui.png`: Config Inspector is populated in front
of Devices and Boot Logs. This is a visual inspection, not an exact pixel oracle
for all three obscured windows. Source, binary and capture hashes are in
`/tmp/cubit-managed-ui-source.json`. The test uses the unit-density software
backend and is not a mixed-DPI, overload, hardware acceleration or latency result.

The remaining existing `UI.App` clients on the mutable path are the raw CCL
Workbench and NetSurf renderers. Their source raster geometry and manual frame
submission need adaptation before enabling managed frames and deleting the
legacy attachment branch. No new SPARK policy was added by this rollout; it
uses the already documented frame/debt/geometry/codec proof boundaries.


### Direct native Workbench painting (2026-10-01)

The CCL Workbench now acquires the native window's protected candidate before
any rendering. A common paint dispatcher chooses the existing specialized
renderer when its requested rectangle covers the exact repair rectangle;
otherwise it redraws current application state clipped to the larger repair
area. Native submission withdraws the canvas pointer. Deferred paint remains
in the owner and participates in the existing bounded retry-deadline policy.
Logical dimensions and physical density come from the managed canvas.

The fixed 1280×720×4-byte (3,686,400-byte) common pixel array and native row-memcpy
presentation bridge are removed. The native renderer draws directly into its
candidate. This is removal of an intermediate image, not a measured reduction
in total process footprint. SDL uses an isolated host-owned image sized to the
current canvas; SDL texture upload remains a hosted-preview boundary.

Fresh native linking and the 120-second `ccl-workspace` regression pass: file
save/open, live-label start/sample/stop, integer/string REPL evaluation and the
final fault scan. The protected publication is logged before the first-frame
marker. Native capture `/tmp/cubit-ccl-direct.png` was visually inspected after
these operations. The clean bounded hosted preview also passes and its BMP
matches the earlier inspected hosted capture exactly. Evidence:
`/tmp/cubit-ccl-direct.log`, `-verified.log`, `-host-final.log`, `.serial`, and
`/tmp/cubit-ccl-direct-source.json`. The three CCL native runner cases now
require the protected-publication marker present in this passing run.

Hosted checks exposed two integration issues, both fixed: adding portable glyph
sources had inadvertently selected native `softpipe.c` in the mixed-language
SDL project, and the one-frame preview limit could sleep forever after its
last frame. The project excludes that native bridge; the final requested frame
now queues an SDL quit event to wake its event wait. The first preview needed
an externally delivered quit and is not counted as a clean hosted pass.

The renderer still relies on its existing drawing callbacks to reconstruct
current content; this change adds no whole-renderer SPARK proof. It reuses the
proved frame/debt/geometry/codec policy with the documented pointer/IPC adapter
boundaries. Native testing here is unit-density software/virtio presentation,
not configured mixed-DPI or GPU performance. NetSurf is the remaining `UI.App`
legacy caller; CCL's independent input drain also still needs bounded-load
validation before the overall latency/overload goal is met.


### NetSurf foreign frame lease preparation (2026-10-01)

NetSurf's embedding redraw adapter now treats the application's pixel pointer
as a synchronous borrow. It saves libnsfb's own RAM surface and clip, binds the
application view for redraw, then restores the original pointer, dimensions,
pitch and clip. It does not retain an application pointer that could become
read-only after publication, allocate an intermediate frame, or copy pixels.

Integer extent checks precede clip arithmetic and foreign plotting. Damage is
clamped to the supplied view; caret coordinates use widened intermediates and
are intersected with damage before narrowing. Caret clipping is explicitly
restored after browser redraw, independent of the plotter's last clip.
Mapping validity and exclusive write ownership remain caller obligations.

The production function passes 140 ASan/UBSan cases with foreign-library mocks,
including null/overflow inputs, padding, caret bounds and restoration after a
renderer failure. A production-flags syntax check against the actual native
headers passes. Evidence: `/tmp/cubit-netsurf-frame-final.log`,
`/tmp/cubit-netsurf-frame-syntax.log`; reproduction and boundaries are in
`tests/netsurf-frame/README.md`. This is audited C FFI code, not a new SPARK
geometry proof. Production-flags native object compilation and the complete
`netsurf-https-test` build pass; that target also rebuilds/restores the normal
homepage application. The 120-second four-CPU TCG `netsurf-https` regression
passes native shell startup, actual TLS 1.3 page fetch and its guest fault scan.
Evidence: `/tmp/cubit-netsurf-frame-native.log`, `.serial`, and
`/tmp/cubit-netsurf-frame-native-inputs.json`. This is real-engine integration,
not a rendered-page pixel oracle or a hardware performance measurement.

NetSurf remains on the legacy path until physical-scale page rendering and
managed-frame adoption are validated. Its current framebuffer font path
(`font_internal.c` / `framebuffer.c`, explicitly selected by the freestanding
build) quantizes glyphs to one-times or two-times bitmap sizes. Merely changing
the page buffer dimensions or browser zoom would not establish crisp fractional
DPI. Page layout metrics, text rasterization, pointer/scroll coordinates and
fractional subview origins need a consistent scale contract before managed
adoption. Renderer failure handling itself is unchanged; the fault test
establishes pointer restoration only.


### Signed browser damage boundary (2026-10-01)

NetSurf invalidation now calls `Client_Signed_Clip.Edge` for each rectangle
edge. Previously the C frontend subtracted signed content and scroll coordinates
before clipping, allowing overflow on extreme inputs. Translation now occurs
in widened arithmetic in a pure SPARK function with an exact clamp contract;
all signed inputs, including nonpositive limits, have a defined bounded result.
The C frontend marshals coordinates and suppresses empty/inverted rectangles.
No frame allocation or copy is introduced.

The SPARK report contains four successful analysis results and zero unproved or
justified checks. 583,164 hosted policy cases and 6,562 actual C/Ada ABI cases
pass (ASan/UBSan on the C side, Ada assertions/overflow checks enabled).
The private verified sources were compared byte-for-byte with the integrated
policy, C binding and Ada dependency. The full `netsurf-https-test` build and
120-second four-CPU TCG regression pass with real TLS page fetch and final
fault scan; the normal homepage application was restored by the build target.
Evidence: `/tmp/cubit-browser-clip-policy.log`,
`/tmp/cubit-browser-invalidation.log`, `/tmp/cubit-browser-clip-native.log`,
`/tmp/cubit-browser-clip-native.serial`, `/tmp/cubit-browser-clip-inputs.json`.
Reproduction is documented in `tests/netsurf-frame/README.md`.

Foreign pointer validity, NetSurf's own layout and rendering, and the C ABI
correspondence remain audited/tested assumptions. This step removes unsafe
translation from C; it does not claim that the browser supports fractional DPI,
protected publication, a native page pixel oracle, or measured hardware latency.


### Bounded Workbench input batches (2026-10-01)

CCL Workbench no longer drains a continuously ready queue indefinitely before
rendering. `Client_Input_Budget` admits at most 32 polls and stops admission
once one millisecond has elapsed after the batch began. One initial poll is
always allowed; a frozen clock cannot defeat the count bound, and backward
clock movement stops further admission. Pointer press/release/drag and REPL
barriers still end batches early. The loop services existing local work and
renders before another batch. If the queue was not observed empty, it yields
to ready peers (native syscall 118, hosted `sched_yield`) without a timed sleep.
Normal empty-queue waits and the existing VM-only wait behavior are unchanged.

The pure SPARK policy has eight successful analysis results, zero unproved or
justified checks. Policy tests cover 1,188 admission cases and 10,000 sustained
batches. A hosted interposer drives the actual Workbench editor/event/render
loop with a never-empty text queue: frozen-clock mode passes 128 events across
four frames (32 per frame), advancing-clock mode passes four events across four
frames, both with four yields and no timed sleeps. The full native Workbench
build and 120-second four-CPU TCG `ccl-workspace` regression pass protected
publication, live label, save/open and REPL checks plus the final fault scan.
Reproduce via `tests/ccl-input-budget/README.md`; evidence is recorded in
`/tmp/cubit-input-budget-policy.log`, `/tmp/cubit-input-budget-verified.log`,
`/tmp/cubit-input-budget-native.serial`, `/tmp/cubit-input-budget-inputs.json`.

This proves bounded admission, not a per-handler execution-time bound or a
whole-loop SPARK proof. Actual event handlers, rendering and scheduler fairness
remain separate assumptions/measurement targets. The sustained-input frame
oracle is hosted; native saturated-input latency, physical presentation timing
and 240 Hz performance remain unverified. No input is dropped by this policy.


### Input provenance policy preparation (2026-10-01)

`Client_Input_Provenance` provides the pure client-side state machine for the
missing input-to-frame link. It is present and independently tested/proved;
The integration described below now calls it from UI.App and carries its
watermark through Frame_Pair and the publication protocol; native validation
is recorded below.
Existing native traces therefore remain submission-to-completion measurements.
The state owns four serial values and one flag; there is no allocation, queue,
clock read or foreign call.

The caller begins an event before handling it and finishes only that exact
serial after completing its state changes. Duplicate, zero, stale, nested and
out-of-order event transitions are rejected without changing state. Serial gaps
are allowed because Desktop can coalesce events; wraparound is not accepted.
Beginning a paint freezes the already-handled watermark. A paint inside an
unfinished handler conservatively excludes that handler. Finishing that handler
later cannot advance the frozen value. Publication emits the frozen watermark
only for a paint that was accepted; cancellation/rejection/unknown publication
emits zero and closes the capture. Retrying requires a fresh paint. Last
accepted publication watermarks never regress. Zero means unknown/no completed
input, not input serial zero.

The watermark means "drawn from application state after handling through this
serial". It does not prove that the last event changed pixels or that those
pixels became visible. Callers must honor handler/paint serialization and fully
repair the chosen candidate from current state. The publication protocol binds this
watermark to the exact surface, epoch and publication ticket;
Desktop must then retain the matching visible-source identity when composing an
output. Occluded sources and unrelated redraws must not be reported as visible
responses. Known visual-change fixtures are needed for input latency evidence.

Remaining integration work:

1. Add appropriate completion hooks to custom asynchronous adapters such as
   Servo. Do not substitute delivery or queue dispatch for completed page work.
2. Extend the native provenance fixtures to stale, occluded, cancelled, deferred
   and mixed-output cases; the initial toolkit/Workbench integration passes below.
3. Trace driver arrival, Desktop enqueue/delivery, client handler/paint,
   accepted source publication, actual per-output composition, GPU completion
   and Display presentation using correlated identities and explicit clock
   validity. Retain bounded storage with visible loss counters; partial traces
   cannot establish a complete latency distribution.
4. Add stale/occluded/cancelled/deferred/multi-output negative controls and a
   native known-visual-change fixture, then measure on supported hardware.
   Software completion must remain distinct from latch/scanout and photons.

Reproduce the policy checks:

```sh
nix develop -c gprbuild -p -P tests/compositor/input_provenance.gpr
nix develop -c tests/compositor/build/input-provenance/input_provenance_tests
nix develop -c gnatprove -P tests/compositor/input_provenance.gpr \
  -u client_input_provenance.adb --level=2 -j1
```

The hosted oracle exercises 10,000 interleaved event/paint/retry cycles,
unfinished handlers, nested paint rejection, failed publication, stale serials
and exhaustion. No native integration or physical latency result is claimed for
this policy-preparation step.

The permanent hosted target passes with 18 successful SPARK analysis results,
zero unproved or justified checks. Evidence:
`/tmp/cubit-input-provenance-integrated-final.log`,
`tests/compositor/build/input-provenance/obj/gnatprove/gnatprove.out`,
`/tmp/cubit-input-provenance-inputs.json`. That policy-only checkpoint did not include runtime publication.

### Input provenance runtime integration (2026-10-01)

UI.App now brackets event handlers explicitly and freezes completed-input
metadata when it acquires a paint. Publication and cancellation both close that
capture; bad handler sequencing disables attribution until the window reopens.
CCL Workbench uses matching hooks around its custom translated-event loop.
Adapters that do not opt in emit zero. Delivery alone does not advance the
handled watermark.

Frame_Pair and Frame_Buffer pass the optional watermark through the publish
request. Epoch and ticket share word 1; word 2 carries the full watermark.
The updated codec has 129 successful analysis results with zero unproved or
justified checks, including its encoder round-trip contracts. The permanent
hosted tests pass; evidence is `/tmp/cubit-provenance-publication-permanent.log`.
All clients and Desktop require a coordinated native rebuild.

With timing enabled, Desktop emits at most 64 `COMPOSITOR-SOURCE` records over
its lifetime for accepted publications: surface, epoch, ticket, input watermark
and acceptance time. Timing-disabled builds do not read the clock or print
these records. This is a bounded diagnostic sample, not a complete trace: it
lacks an explicit loss counter and actual output-source composition correlation.
Driver arrival, enqueue/delivery, GPU completion and display latch still need
correlation. No input-to-presentation latency claim follows from acceptance.

The coherent default Desktop and managed-client rebuild passes, as do native
`desktop-protocol` (90 seconds) and `ccl-workspace` (120 seconds), each with its
final fault scan. The hosted Workbench continuous-input tests still pass both
frozen and advancing clocks. Evidence: `/tmp/cubit-provenance-native-complete.log`,
`/tmp/cubit-provenance-{protocol,workspace}.serial`, and
`/tmp/cubit-provenance-hosted-ccl.log`.

The first requested timing run actually used the default service: the CCL
fixture overwrote `CUBIT_DESKTOP_IMAGE` after the common image-selection block.
The corrected CCL selection honors that override. A separate 120-second native
run using the actual timing Desktop passes the workspace checks and final fault
scan (`/tmp/cubit-provenance-timing-native.log` and
`/tmp/cubit-provenance-timing-workspace.serial`). Its 64-record bounded sample
has unique surface/epoch/ticket identities, valid acceptance clock values and
nondecreasing handled-input watermarks, including both advances and repeats.
The final sampled watermark is 181. This exhausts the diagnostic budget and
cannot establish complete trace coverage or a latency distribution. Record
checks are saved in `/tmp/cubit-provenance-source-records.json`.

The optional Mesa Desktop also relinks successfully against this ABI; that
relink is not a fresh Mesa rendering or hardware test. Source and binary hashes
are in `/tmp/cubit-provenance-inputs.json`. Servo must rebuild its standalone
bridge against the same protocol before native testing.


### Idle managed-window DPI refresh (source integration, 2026-10-01)

Desktop previously refreshed a source's publication configuration only when
that source queried, staged or published. Moving an idle window between outputs,
or changing output scale without resizing the window, could therefore leave it
using an old-density buffer indefinitely. Move completion only notified resizes;
arrangement changes also notified only changed logical dimensions.

Before pending scene paint, Desktop now refreshes configurations for its bounded
managed surface table. The existing proved density planner/selection and
`Surface_State.Configure` transition compute the new physical layout and epoch.
Changing an established configuration queues the ordinary configure event, even
when logical dimensions are unchanged. Unchanged configurations cause no epoch
change or notification. The previous visible publication stays owned; obsolete
candidates enter retirement through the existing policy. This adds no idle timer,
allocation or pixel copy. The event loop, notification delivery and imperative
service integration are outside the SPARK proof boundary.

Run the hosted integration regression with:

```sh
nix develop -c python3 tests/compositor/test-configuration-refresh.py
```

It compiles the actual two service functions extracted from `main.adb` against
the real portable geometry, density, publication and surface-lifetime units,
with a mocked input enqueue. It checks a stationary client's move to a 150%
output, a same-size change to 200%, 1,000 unchanged straddling-window refreshes,
return to 100%, retained visible/candidate identities, and exclusions for
unused, unmanaged and internal surfaces. Additional cases require an explicit
resource-exhaustion result for an intersecting 16x output, unchanged buffer
state and no wake on rejection, recovery to a supported density, and refusal
after lifetime shutdown. The initial rejection fixture accidentally missed the
shrunken output; an explicit status assertion caught that, and both coordinates
were corrected. A missing-notification mutation must fail the assertions.
The final hosted run passes (`/tmp/cubit-configuration-refresh-admission-verified.log`).

The native Desktop and desktop-check rebuild passes. The 90-second, four-CPU
TCG `desktop-protocol` suite also passes its final fault scan, visible publication,
resize, frame-pair and retirement checks (`/tmp/cubit-dpi-refresh-native.log` and
`/tmp/cubit-dpi-refresh-protocol.serial`). This is the unit-scale legacy backend;
it checks integration compatibility, not an actual cross-output density wake.
Source and rebuilt binary hashes are in `/tmp/cubit-configuration-refresh-inputs.json`.
The default staged Desktop includes this source change. The Mesa variant was
subsequently rebuilt and passed the native idle DPI gate below. Input-overflow
recovery and performance remain unverified; the timing variant still requires
rebuilding for this change.


### Native idle DPI fixture (passed, 2026-10-01)

`native_dpi_client.adb` uses protected UI.App frames and the normal input-only
loop, with no application timer or animation. It paints a unique solid color
for each density. The opt-in `CUBIT_TEST_IDLE_DPI=1` dual-output fixture starts
this client, moves it to the secondary output, applies 125% and 150% through
real Settings input, checks configure-triggered paints and exact displayed
rectangles, checks an idle interval without further paints, then moves it back
to unit scale and closes it. The fixture image is dumped back from the copied
guest disk and compared byte-for-byte after installation.

Run under the shared lock in Nix after building Desktop/runtime/fonts and the
desktop-check manifest:

```sh
(cd kernel && alr exec -- gprbuild -p -P ../tests/compositor/native_dpi_client.gpr)
python3 tests/compositor/build-desktop.py userspace/mesa/build/native-aee5fe5697d39dd4
CUBIT_TEST_IDLE_DPI=1 CUBIT_TEST_MIXED_OUTPUTS=1 \
CUBIT_DESKTOP_IMAGE="$PWD/tests/compositor/build/desktop-mesa-none.svc" \
bash tests/headless/run.sh --test desktop-dual-output --accel tcg,thread=multi \
  --cpus 4 --timeout 180 --serial /tmp/cubit-idle-dpi.serial --keep-logs
```

The native client and updated Mesa Desktop compile/link successfully. The first
attempt rejected one cursor-shadow pixel over the client; the observer now
parks the cursor before comparing pixels. The next attempt delivered the 125%
configure event to the idle client and produced its new green frame, but exposed
an incorrect fixture assumption about displayed extent. Allocation rounds an
extent upward; output coverage instead samples pixel centers and depends on the
window origin. At logical Y=112, height234 and scale5/4, the correct visible span
is physical rows140..431 (292 rows), although backing storage has293 rows.
The observed green400x292 rectangle at(127,140) matches that definition.

`dpi_pixels.py` now derives coverage from rational pixel-center inequalities.
Its independent enumeration passes20,480 scale/origin/extent combinations, plus
exact image dimensions and stale-color, damaged, oversized and truncated image
rejection controls (`/tmp/cubit-dpi-pixel-oracle-center.log`). Neither earlier
attempt is a passing native mixed-DPI gate.

The corrected native rerun (session2633) completed the full180-second,4-vCPU TCG
run with exit0 and the runner's final fault scan passing. The actual Settings
changes woke the idle protected client at125% and150%, retaining logical320x234.
Captured output rectangles were400x292 at(127,140) and480x351 at(153,168),
respectively. Both unit-scale captures and the returned primary capture were
320x234 at(102,112). Each scaled phase had a0.7-second observation interval
with no additional application paints; Escape closed the client cleanly.
This establishes a functional idle wake and solid-pixel coverage gate, not a
long-duration idle/overload guarantee.

Evidence: `/tmp/cubit-idle-dpi-native-v3.log`,
`/tmp/cubit-idle-dpi-native-v3.serial`, and its phase-named PPM captures.
`/tmp/cubit-idle-dpi-fixture-inputs.json` records exact source/binary hashes;
all nine inputs matched the manifest established before this run.
No text-raster quality, GPU execution, physical presentation timing or240Hz
performance claim follows from this fixture.

### Cursor damage across direct target rotation (2026-10-01)

The direct software path had two coupled cursor-history omissions. After
presentation, the saved cursor underlay correctly becomes invalid because it
belongs to the submitted target. Its footprint still matters: Display must be
asked to replace the previous cursor pixels, and reusable targets must retain
repair debt for that footprint. `repairDirectWriter` now adds that rectangle
to output damage and invalidates it in all target histories before taking the
current writer's repair regions. It preserves the existing bounded sparse
queues, performs no allocation or new scene-wide copy, and does no idle repair.

`test-cursor-repair.py` compiles the actual Desktop repair, flush and cursor
restore procedures against real pool/damage/repaint units, with mocked pixel IO
and cursor art. It models target contents and Display's distinct scanout image.
The corrected code passes1,000 exact target/scanout comparisons, interleaved
sparse scene damage and idle gaps. Removing display damage fails at frame1;
removing history invalidation fails target correctness at frame2. The pre-fix
code reproduced the scanout failure before the source change. Evidence:
`/tmp/cubit-cursor-repair-before.log` and
`/tmp/cubit-cursor-repair-two-boundaries.log`.

The native `check-cursor-repair.py` observer waits for the ordinary
`desktop-display` drag/maximize/restore input sequence, then checks visible
cursor movement and exact restoration of a wallpaper strip after parking the
cursor elsewhere. The pre-fix native run completed the standard regression
but failed this new oracle:1,342 changed pixels remained in the first restored
strip, with repeated cursor shapes visibly present in its screenshot.
This reproduces a compositor defect on QEMU independently of Intel hardware.
The corrected Desktop compiles, links and passes the same native comparison
(session1628,exit0): all four cursor movements become visible, and all four
returned wallpaper regions match their baseline exactly. The full100-second
standard desktop regression, drag/maximize/restore checks and final fault scan
also pass. Logs are `/tmp/cubit-cursor-after.log` and
`/tmp/cubit-cursor-after-native.log`; source/binary hashes are recorded in
`/tmp/cubit-cursor-{before,after}-inputs.json`. The default staged Desktop now
includes this fix. Intel NUC confirmation remains pending; this test covers
the direct software path and does not establish tear-free hardware scanout.

Run after building the desired Desktop, under the shared lock in Nix:

```sh
python3 tests/compositor/run-cursor-repair.py UNIQUE_TAG /absolute/path/to/desktop.svc
```

Both the screenshot observer and the standard runner/final fault scan must
pass. Logs and phase screenshots use `/tmp/cubit-cursor-UNIQUE_TAG*`.
The Desktop orchestration is still SPARK-off, with native/pixel regression
evidence rather than an end-to-end proof. The bounded damage and repaint
operations retain their existing SPARK contracts; no new proof of FFI or
physical scanout is implied.

### Bounded publication trace batches (2026-10-01)

Timing-enabled Desktop now appends accepted publication identities, untrusted
client input watermarks and acceptance timestamps to `Compositor_Source_Trace`.
It replaces the silent64-record process-lifetime limit. The normal diagnostic
drain emits records followed by `COMPOSITOR-SOURCE-STATS` with count, invalid
and dropped counters, then resets the batch. There are at most64 retained
records; overflow increments a saturating counter without overwriting records.
Unavailable clocks are counted as invalid. A zero input watermark remains an
explicit unknown, not an invalid publication or an inferred input event.

No serial formatting occurs on the source-acceptance path. Storage is fixed
(64records of five64-bit fields plus bounded bookkeeping); no per-frame
allocation is introduced. Timing-disabled acceptance does not sample the
clock or append records. Diagnostic serial draining can still perturb the
measured process, and64records per drain can overflow during heavy activity;
such a batch is explicitly unsuitable as complete measurement evidence.

The policy passes26SPARK results (13runtime checks,6functional contracts,
1initialization and6termination results), with0unproved or justified checks.
The hosted test covers1,000 saturation/reset batches, exact record retention,
unknown/full-width watermarks, invalid identity/clock cases and saturated
counter increments. `test-source-trace-integration.py` compiles the actual
Desktop append/drain snippets against this policy:200records across five
batches pass, disabled timing does not read the clock, and overflow/unavailable
clock batches are emitted with the expected nonzero counters.

`check-source-trace.py` rejects duplicate identities, malformed fields,
unavailable/reversed clocks, over-capacity batches, mismatched counts,
nonzero loss/invalid counters and unterminated emitted batches. Its evidence
test passes256successive batches and20negative controls. A valid report covers
only emitted, closed batches: it does not prove that an unflushed process tail
or an entire run was captured. It does not establish that an asserted input
watermark caused the pixels, or join a source to an output frame, display latch
or physical photon. Those remain separate instrumentation requirements.

Hosted evidence: `/tmp/cubit-source-trace-{policy,integration,evidence}.log`.
The timing Desktop compiles and passes a120-second native4-vCPU TCG
`ccl-workspace` run with final fault scan (session37517,exit0). The emitted
trace checker accepts84records across20closed batches, with zero reported
invalid/dropped records and zero unknown watermarks. This exceeds the old
64-record lifetime cap without changing the storage bound. Evidence:
`/tmp/cubit-source-trace-native.log`, `.serial`, `-native-report.json`, and
exact source/binary hashes in `/tmp/cubit-source-trace-inputs.json`.
These are functional trace tests, not hardware latency measurements.

### Metrics destination and transport boundary

The intended destination is a subscribable system metrics/trace stream. Serial
text is a temporary timing-test export, not the production consumption API.
Keep compositor collection as compact, bounded typed records; a transport
adapter should export them asynchronously outside rendering and input handling.
No subscriber, absent collector, full queue or lost acknowledgement may make
the compositor wait or accumulate unbounded work.

There is existing related machinery: `CuBit.Logging.Publisher` supports one
asynchronous publication at a time with explicit drops, and `logstore` has
bounded subscriptions with per-subscriber loss accounting. Reuse its authority,
retirement and bounded-delivery patterns where suitable. Its current record
schema is bounded text with millisecond timestamp variants; it is not yet a
high-rate typed metrics protocol. Low-rate human summaries can use logstore,
but detailed latency records should not require subscribers to parse debug
strings or infer microseconds from the logging header.

The eventual metrics envelope needs a schema version, producer incarnation,
monotonic microsecond clock domain, producer sequence and explicit loss at each
boundary (collection, export and subscriber delivery). Frame events also need
source publication and output/session/frame identities. Collection timestamps
must be preserved through asynchronous export; collector arrival time is a
different measurement. Subscribers can filter and process records away from
the compositor. Keep detailed tracing opt-in and aggregate counters/histograms
cheap; do not enable a permanent high-volume serial stream. A new metrics
transport, subscription wake mechanism and complete-run capture delimiters are
not implemented by the current source-trace work.

### Proved bounded input queue and recovery (2026-10-01)

Desktop uses `Compositor_Input_Queue` for insertion, acknowledged dequeue,
forced recovery and close-time serial reservation. Fixed 32-event storage and
the wire protocol are unchanged. Replacement is allowed only for pointer motion
when the highest-serial pending event is also motion; key, text, button, wheel
and configure events remain ordering barriers. A full queue becomes one explicit
resynchronization event containing the state after the overflowing report.
Other surfaces retain independent storage. Non-motion insertion does not scan
for a motion candidate.

`Pop` selects the oldest serial newer than the acknowledgment, clears stale
entries and the selected entry, and preserves all other entries. `Recover`
replaces the queue with one snapshot event. `Reserve` allocates close-time reply
serials. All allocation paths refuse zero or exhausted serials without wrapping;
Desktop exits rather than emitting an ambiguous identity. No inline next-serial
increment remains in the service. Snapshot generations and overflow counters
saturate. Selection and clearing use bounded scans of 32 entries.

The policy has 59 SPARK results: 8 initialization, 29 runtime, 8 assertion,
7 functional-contract and 7 termination results; 44 prover checks and 15 flow
results, zero unproved/justified. Contracts cover exact unaffected-entry
preservation, serial order, coalescing identity, recovery and exhaustion refusal.
Hosted tests cover 1,000 cycles with 100 coalesced moves each, transition barriers,
overflow, fragmented slot/serial order, acknowledgment sweeps and exhaustion.
Actual Desktop enqueue/dequeue/has-pending snippets pass ordering, isolation,
stale-ack and overflow-snapshot tests; a wrong coalescing-kind mutation fails.
Actual forced-resync/close snippets pass 1,000 two-channel cycles, snapshot
payloads, transient interaction reset, and waiter-clear-before-reply ordering.

Proof boundaries: channel lookup/ownership, snapshot construction, waiter reply
capabilities and the service loop remain SPARK-off glue. Hosted glue tests mock
geometry and kernel replies; they do not prove kernel capability semantics.
This does not establish browser throughput under overload or end-to-end latency.
Queue overflow explicitly loses continuity; it is not lossless buffering.

Evidence: `/tmp/cubit-input-queue-complete-policy.log`,
`/tmp/cubit-input-queue-complete-integration.log` and
`/tmp/cubit-input-recovery-integration-fixed.log`. Native session 25223 exited 0:
Desktop compiled/linked, then both 90-second, four-vCPU TCG `input-stream` and
`desktop-protocol` regressions passed including final fault scans. The stream
fixture observed one expected source gap, zero rejected source reports, and its
IPC budget checks passed (34 input requests, zero presentation requests).
Native evidence is `/tmp/cubit-input-complete-native.log`,
`/tmp/cubit-input-complete-stream.serial` and
`/tmp/cubit-input-complete-protocol.serial`. All eight source/binary hashes in
`/tmp/cubit-input-complete-inputs.json` matched after completion.

These native fixtures exercise source discontinuity, normal delivery and deferred
waiter protocol behavior. Per-surface saturation now also has a native gate: session 74508 exits 0
after a 90-second four-vCPU TCG desktop-protocol run. It verifies all 32 admitted
configure events, four overflows to a sole resync, exact recovery serials and
client-local snapshot, fresh delivery, no stale replay, and second-surface
isolation. Its final fault scan and nine source/staged/kernel hashes pass.
Evidence: `/tmp/cubit-input-overload-coordinate-native.log`, `.serial` and
`/tmp/cubit-input-overload-inputs.json`. This uses admitted configure transitions,
not a physical keyboard flood or source-mailbox overload. Default staged Desktop includes
the complete queue policy and source-trace integration with timing disabled.

The current native `Compositor_Input_Queue.Pop` object was also inspected for
proof-artifact overhead: its ghost `Before` queue is absent from generated code;
there are no calls or bulk-copy instructions in `Pop`, just queue scans, validity
updates and copying the selected 48-byte event to the result. Compiler stack
usage is 8 bytes, including the return address. This validates the absence of a
runtime proof-snapshot copy for this build, not a CPU-time bound. Disassembly and
stack report: `/tmp/cubit-input-queue-native-assembly.txt` and
`/tmp/cubit-input-queue-native-stack.txt`.

### Elapsed-time dispatch admission (native integration verified)

Desktop previously bounded each turn only by 64 input events and 32 requests
when a frame was pending, otherwise 96 requests. Its service-loop source now uses
`Compositor_Dispatch_Budget` to add elapsed-time admission:
500 microseconds per input drain and 1,000 microseconds for request admission,
while preserving those count caps. Its input state permits exactly two phase
openings per turn, before and after request dispatch; the second opening retains
the turn's event count. This preserves an opportunity to handle fresh arrivals
just before painting after the first phase exhausts its time allowance.

Each opened phase permits a first item if its count cap allows it. Subsequent
admissions require a valid, non-regressing timestamp strictly inside the phase
allowance. An unavailable or backward clock therefore cannot admit an unbounded
batch; a frozen clock remains bounded by the original count caps. Once a request
makes a frame pending, the lower request cap applies immediately. Admission at
the exact time boundary is refused. Time arithmetic does not add a duration to
a possibly near-maximum timestamp.

Hosted session 66076 exited 0: 10,000 flood, two-phase, exact-boundary, unavailable,
backward/near-overflow-clock and newly-pending-frame cycles pass. SPARK reports
21 results (3 runtime checks, 7 functional contracts, 11 termination results),
zero unproved or justified. Evidence: `/tmp/cubit-dispatch-budget-two-phase.log`,
`tests/compositor/build/dispatch-budget/obj/gnatprove/gnatprove.out` and exact
hashes in `/tmp/cubit-dispatch-budget-inputs.json`. The initial single-phase-count
version also passed, but the final source additionally prevents a third opening.

Desktop now reads the independent microsecond `CuBit.Monotonic.Read` clock,
even when profiling is disabled. Both drains share one input state and empty
polls do not consume event credit. Request admission observes current
`framePending`. Handler/transport glue remains SPARK-off. The actual clock
adapter, drain procedures and their source call order pass 8,000 hosted scenarios
with mocked clock, transport and handlers (session 58022). Tests check frozen and
unavailable clocks, elapsed limits, pending-frame transitions, fresh arrivals
after requests, shutdown and the existing wait predicate. Mutations resetting
the event count, ignoring a pending frame or omitting the fresh drain all fail.
Existing enqueue/dequeue and recovery/close integration tests also pass (85101,
18193). Evidence: `/tmp/cubit-dispatch-integration.log` and
`tests/compositor/test-dispatch-integration.py`. This is not a proof of the whole
service loop, kernel timing or handler execution.

Native session 38145 exits 0 after building/staging Desktop and desktop-check,
then running both 90-second four-vCPU TCG input-stream and desktop-protocol
regressions. Both final fault scans pass. Input-stream reports one expected
source gap, zero rejected reports and event drops, 35 input requests and zero
presentation requests. The protocol fixture verifies capacity, ordering,
per-surface isolation, all four recovery records and deferred waiter behavior.
All 14 source/staged-binary hashes match `/tmp/cubit-dispatch-native-inputs.json`;
kernel hashes are captured separately for each profile in
`/tmp/cubit-dispatch-{input-stream,protocol}-kernel.sha256`. Native logs:
`/tmp/cubit-dispatch-native.log`, `/tmp/cubit-dispatch-input-stream.serial` and
`/tmp/cubit-dispatch-protocol.serial`. This supersedes the earlier 74508 saturation
result for the admission-policy integration without discarding its evidence.

The numeric allowances are admission limits to validate and tune on supported
hardware, not measured optimal settings or a 2 ms execution guarantee. A handler
already admitted can overrun its allowance; collection, drawing, presentation
and kernel scheduling have separate costs. No 240 Hz, handler WCET or physical
keypress-to-photon claim follows. The default staged Desktop now includes this policy. Native functional tests
do not substitute for hardware throughput, tail-latency or clock-overhead measurements.

### Resize release retains the old surface footprint

The resize-release path previously substituted the last presented outline for
its old surface bounds. After shrinking, that outline could already equal the
new, smaller window, leaving exposed old client/chrome pixels undamaged.
`Compositor_Transition.Cover` now includes old surface, new surface and previous
outline, with visual-margin expansion in Desktop. The pure SPARK containment
contract and termination prove with zero unproved checks; the actual release
adapter passes 6,868 hosted cases. No image allocation or pixel copy was added.

A native pixel oracle reproduced 36,978 stale pixels before the fix, despite
the ordinary desktop-display gate passing. Afterward it passed three vertical
enlarge/shrink cycles, restoring all 48,000 checked wallpaper pixels each time.
The 100-second four-CPU TCG desktop regression and final fault scan also passed
(session 42907, unchanged source/staged hashes). This is software direct-output
functional evidence, not hardware/Mesa rendering or latency evidence. See
[`tests/compositor/resize-repair.md`](../tests/compositor/resize-repair.md) for
commands, proof boundaries and captures. The default staged Desktop has the fix.

### Input dequeue to client-declared publication watermark

The timing-on Desktop now retains up to 64 input dequeue records per reporting
batch. The pure recorder has 26 SPARK checks with zero unproved results; the
actual queue hook and append/drain paths have hosted regression coverage,
including overflow, invalid clocks, and no additional clock reads when disabled
or when no input is found. The hook formats no text, allocates no storage and
performs no IPC. Formatting remains in the opt-in diagnostic drain.

Native session 34623 passed the 120-second four-CPU TCG CCL workspace regression
and final fault scan. Exported evidence contains 247 dequeues and 84 accepted
publications; all 84 publication watermarks match observed surface/serial pairs,
with no unknown/unmatched references or reported invalid/lost records. Seven
source/binary hashes matched at exit. Commands, logs and limitations are in
[`input-publication-trace.md`](../tests/compositor/input-publication-trace.md).

This correlates client metadata between two software stages. It does not prove
that published pixels respond to that input. Neither output-frame consumption
nor scanout/physical photons is joined yet. The bounded serial diagnostic export
is temporary; first-class metrics/trace transport and supported-hardware
measurements remain required. Timing remains disabled in the default image.

### Source draw work to output submission and software completion

The timing-on Desktop now records successful client draws using the complete
output writer identity (output, slot, epoch, serial) and the acquired visible
publication identity. Successful asynchronous submission records bind that
writer to the frame/session later validated by the existing completion path.
The bounded recorder has 33 SPARK checks, zero unproved; Desktop's surrounding
SPARK-off glue has actual-helper regression tests. No pixel buffer was added.

Native session 54528 passed the 120-second four-CPU TCG CCL workspace gate and
final fault scan, with all nine source/binary hashes matching. Its 168 draw
records join 84 accepted publications to 84 completed output frames; the full
capture contains 87 completed submissions. All draw associations have matching
input watermarks, with zero missing links, invalid/lost records or unsupported
paths in this direct software-output test. See
[`render-pipeline-trace.md`](../tests/compositor/render-pipeline-trace.md).

This extends the earlier input/publication correlation to software completion
for draw work. It is not pixel visibility or response-time evidence: later
redraws reuse old publication/input metadata, and draws can be overwritten or
occluded. Startup/idle captures may have no input matches and report that
explicitly. Logical-canvas copy bridging and unprotected source publications
are explicitly unsupported. Hardware presentation timestamps, transport and
supported-hardware performance measurements remain required.

### Metrics service publication from Desktop

The separate `CUBIT_COMPOSITOR_METRICS=on` build now publishes typed per-output
software submission-to-release spans to metrics.svc. Main routes telemetry
completions separately, submits at most one batch after presentation work, and
adds a bounded flush wake to its idle wait. Each batch repeats both metric
declarations so collector source eviction cannot permanently remove its schema.
At the initial metrics integration checkpoint, default startup stayed unchanged.
Normal desktop-session builds now enable metrics as described below; bare
`make desktop` still supplies a metrics-off build for collectorless profiles.

The conversion has 18 proved checks, the batching/wake policy 21, and the
completion release predicate two; each has zero unproved/justified checks.
The surrounding adapter is an audited SPARK-off IPC boundary, covered by 17
hosted fault cases. The current SDK's invalid-completion and disabled-Put bugs
remain pending for other callers; Desktop rejects those paths before SDK entry.

Two native four-CPU TCG 120-second CCL workspace gates passed. In 58905, an
authorized observer saw the real Desktop series grow with zero reported loss
or rejection. In 67661, an isolated malformed-reply collector caused only
telemetry quarantine; subsequent workspace interaction and the final fault scan
passed. All source/binary hashes matched at each gate's exit. The publisher
object reserves 16 KiB in the enabled native ELF (including two 4 KiB payload
pages and alignment); no corresponding object exists in the disabled build.

Commands, evidence and assumptions are in
[`desktop-metric-publisher.md`](../tests/compositor/desktop-metric-publisher.md).
This is aggregated software-stage measurement, not raw tracing, scanout timing
or physical input-response latency. Default startup rollout, stalled-reader
testing, collector scheduling under load, user-facing collection/export and
supported-hardware performance validation remain outstanding.


The metrics-enabled native Desktop also passed a finite stalled-acquisition
fixture (session 80435, 120 seconds/four-CPU TCG). The test collector retained its first
read grant for 600 sleeps of 50 ms, comparing every published word after each
wake. During that hold, Workbench produced two live-label samples and four
workspace saves. After returning the grant, batch 3 resumed collection and
reported 83 dropped samples; no telemetry quarantine or final fault was seen.
Ten source/binary hashes remained unchanged. See
`tests/compositor/desktop-metric-publisher.md` for reproduction and the required
capture-order validator. This is functional stall/recovery evidence, not a
hardware latency or CPU saturation measurement; production sources were
unchanged for this test.


A real-collector scheduling fixture also passed (68284, 120 seconds/four-CPU
TCG): metrics.svc ran at priority 2, below Desktop at 4 and Workbench plus four
CPU-bound workers at 3. Workbench saved three files during worker overlap.
After all four workers finished, explicit Desktop redraws grew the observed
series from 82 to 83 completed frames across 75 batches, with no reported
producer drops, rejects or batch gaps. Eight recorded hashes remained
unchanged and the final fault scan passed. The first attempt lacked post-query
redraws and failed its growth oracle; it is not included as a passing run.
See `tests/compositor/desktop-metric-publisher.md` for reproduction and limits.
The fixture shows functional integration under CPU work, not calibrated CPU
saturation or a measured 240 Hz/input-to-photon result.


Normal desktop-session build wiring now uses `make desktop-metrics`, stages
metrics.svc in the desktop disk overlay, and starts the collector at priority 2
before Desktop at 4. The helper `tools/build_desktop_metrics.sh` is shared with
test builds; bare `make desktop` remains available for collectorless profiles.
The native build and a real temporary-ext2 packaging check passed (67016 and
14712), including byte-for-byte extraction of both services and the startup
profile, service ordering, filesystem integrity and unchanged base image.
Full normal-profile boot remains a separate pending gate; earlier native
metrics tests used dedicated CCL workspace fixtures. Hardware-specific startup
profiles and the generic world startup are unchanged.


The normal-profile boot gate then passed (94329): exact normal startup profile,
private ISO/disposable disk, seven service launches, three real keyboard Apps
menu cycles, and exact restoration of a 252,000-pixel wallpaper region. No
faults or telemetry quarantine were observed; twelve recorded build inputs
and the temporary base stayed unchanged. Evidence:
`/tmp/cubit-normal-session-evidence/` and
`/tmp/cubit-normal-session-native.log`. Kernel/initrd and non-Desktop services
were recorded staged inputs. This establishes the normal startup integration
in four-CPU TCG, not a hardware frame-rate or latency result.

## Standard build selection

The normal Desktop targets now use the existing musl/Mesa linker when Mesa is
selected. Run inside Nix with the shared build lock held:

```sh
make -C kernel desktop-metrics CUBIT_COMPOSITOR=mesa \
  CUBIT_MESA_BUILD=/absolute/path/to/cubit/userspace/mesa/build/native-aee5fe5697d39dd4
make -C kernel desktop-metrics CUBIT_COMPOSITOR=legacy
```

`CUBIT_MESA_BUILD` names an existing native Mesa build; these commands do not
rebuild Mesa. Prefer an absolute path (Make recipes execute from `kernel`).
`desktop` omits metrics; `desktop-metrics` includes the authorized publisher
manifest. Both preserve timing, storage and display-test scenarios and stage the
artifact from the matching GPR directory. The Mesa linker uses the matching
manifest and replaces its output only after successful linking. Fault-injection
and allocation-trace builds cannot replace a normal scenario artifact.

The shared implementation is `tools/build_mesa_desktop.py`; the existing
`tests/compositor/build-desktop.py` entry point still produces isolated fault
and native-test artifacts. This integration selects the existing Gallium
softpipe compositor: selecting Mesa does not itself enable Intel acceleration
or establish tear-free hardware presentation.

Validation: native Nix build session 5588 passed Mesa metrics-off and metrics-on
links, then the legacy metrics build. Both metrics variants matched the staged
Desktop byte-for-byte after their respective builds. The final staged artifact
is legacy with metrics. Evidence: `/tmp/cubit-mesa-variant-repair.log`. This was
a build/staging check, not a new guest boot or hardware performance test.


### Normal-profile Mesa runtime gate

Native CuBit under four-CPU QEMU/TCG, session 65804: the normal desktop profile
booted all seven services with the Mesa metrics-enabled binary. The test
required the actual Mesa retained-mask text marker and rejected silent software
fallback. Three real keyboard Apps-menu open/close cycles each restored a
252,000-pixel region exactly. No guest fault or metrics quarantine was detected;
all twelve recorded build inputs and the private base disk remained unchanged.
The previous legacy-with-metrics staged Desktop was restored and byte-checked.
Evidence: `/tmp/cubit-mesa-normal-evidence/result.json`, `serial.log`,
`inputs.json`, and screenshots; run log `/tmp/cubit-mesa-normal-retry.log`.

The first run (46547) failed because its screenshot was taken one second after
input, before the TCG software redraw completed. The fixture now waits for the
actual open/close pixel transition, with a 30-second deadline for each. This is
a functional gate, not a latency threshold. It neither establishes 240 Hz nor
physical input-to-photon performance, and it does not query the metrics data
path (covered by the separate collector/observer fixtures).


### Partial-write recovery with metrics enabled

Native session 19317 passed the normal-profile boot gate with the existing
`CUBIT_MESA_FAIL_TEXT_PARTIAL` injection. The shim completes one glyph write,
finishes its outstanding work, then returns a quiescent error. Desktop logged
exactly one scene replay and selected retained software text without restarting.
Three keyboard menu cycles restored the checked 252,000-pixel region exactly;
seven startup services were present, all twelve recorded inputs and the private
base disk were unchanged, and no guest fault or metrics quarantine was detected.

The opt-in builder accepts `text --metrics on` and produces
`tests/compositor/build/desktop-mesa-text-metrics.svc`, distinct from normal
scenario artifacts and metrics-off fault binaries. The normal boot fixture's
`--backend mesa --mesa-fault text` checks the exact staged fault binary, requires
the failure/recovery markers, and retains its pixel, input and fault checks.
The native wrapper restored and byte-checked the prior legacy-with-metrics
Desktop after the test. Evidence: `/tmp/cubit-mesa-partial-evidence` and
`/tmp/cubit-mesa-partial-native.log`.

This exercises synchronous softpipe failure after a partial write. It does not
establish recovery from a hung GPU, a device reset, or an uncertain foreign
completion; those cases require their own safe ownership and retirement gates.


### Live stage metrics before further compositor tuning

Desktop now publishes input-handler, request-handler, scene-draw and submit-call
execution durations through its existing bounded metrics publisher. The native
CCL observer and six-row Observatory view pass with this build; metrics-disabled
Desktop also compiles. The new conversion and updated six-declaration batch
policy have 43 SPARK analysis results, zero unproved/justified. Native and hosted
evidence, exact measurement scopes and limitations are documented in
[`desktop-metric-publisher.md`](../tests/compositor/desktop-metric-publisher.md).
No extra telemetry pages, queued measurements, animation or SDK changes were
introduced. Physical input arrival, display latch and photons remain unmeasured.

The user has prioritized returning to compositor work once this visibility is
available. The next tuning investigation is redundant scene/target repair and
presentation copying. Current `repairDirectWriter` repairs stale target areas
before `flushFrame` redraws pending damage; overlapping regions can be painted
twice. Separately, Display's `prepareGpuRect` copies previous damage from the
active buffer before overwriting the new damage, even when new damage covers
all of that prior region. These are source findings, not measured hardware
speedups. Any elision must preserve cursor underlays, pixels outside new damage,
and writer/reader retirement, with SPARK policy and native pixel checks.


### Remove overlap between retained repair and imminent redraw

`Compositor_Repaint.Before_Draw` now subtracts the upcoming ordinary frame damage
from each stale writer region, preserving the cursor intersection for a clean
saved underlay. It emits at most five disjoint rectangles per existing damage
region, with no pixel allocation or additional queued frame. SPARK proves exact
point coverage, subset bounds and disjointness; the complete repaint-policy proof
has 85 analysis results with zero unproved/justified.

Desktop uses this only when an ordinary pending redraw will complete before
submission. Drag/split and cursor-only work retain the conservative path. Fast
client redraw admission now requires a valid source covering the client, so a
small or missing source cannot leave skipped background stale. The surrounding
legacy event loop and acquired mapping validity remain integration boundaries.

Native cursor, resize and repeated-viewer interaction tests pass. In comparable
fixed-client fast-redraw intervals, repair area per frame fell from 420,730.87 to
532 pixels (99.87% less). This is a work-count reduction, not a hardware timing or
frame-rate result. See [`repair-overlap.md`](../tests/compositor/repair-overlap.md)
for proof, independent pixel models, actual-helper fault tests, native inputs
and comparison limits. Display's previous-damage copy remains a separate next
step; no Display or i915 implementation was changed here.


### Avoid immediately overwritten Display repair copies

Display now reuses the proved repair-subtraction policy when preparing its
inactive GPU backing buffer. It copies previous damage only outside the new
source damage, then copies the new source pixels. The same single union upload
request and existing confirmed-flip ownership transitions remain in place.
This adds no pixel storage or queued presentation and makes no i915/Mesa changes.

Actual-helper tests cover 2,000 two-output frames, exact pixels, minimal repair
bytes, active/other-output isolation, failed flips and negative controls. Native
cursor and resize pixel checks, the full repeated-viewer workload, and a
four-worker overload run pass with the combined compositor/Display changes.
In comparable native workloads, repair-copy traffic fell from 159,756,320 to
1,999,520 bytes (98.75%); tracked source-plus-repair traffic fell about 48.5%.
Source copies and GPU uploads remain. These are work-volume observations rather
than hardware throughput/latency measurements. See
[`display-repair-overlap.md`](../tests/compositor/display-repair-overlap.md) for
proof boundaries, native manifests, exact counters and reproduction details.

### Proven backend target lifetime

Display's two GPU backing targets now use `CuBit.Backend_Targets` for clear,
prepare, seal and completion transitions. The inactive target is writable only
during preparation; submission closes that window before foreign IPC. Failed or
unrecognized completion retains ownership in a terminal failed state. Both
synchronous and deferred presents allocate identities from one bounded,
non-reusing sequence. No additional pixel buffers or queued frames are added.

The pure policy is proved; memory mappings, capability IPC and the backend's
successful-present retirement guarantee remain audited adapter assumptions.
This is groundwork for future writable target leases, not a direct-target API
or a removed source copy. See
[`backend-targets.md`](../tests/compositor/backend-targets.md) for exact proof
boundaries and actual-function regression coverage.

### Completion admission before input

Desktop's completion drain now uses the proved dispatch policy: at most 64
completions, with a 500 µs elapsed admission check between handlers. A queue
refilled during consumption can no longer keep this phase running indefinitely.
Malformed polling returns after quarantine. Single-handler execution time and
scheduler delays are outside the bound. See
[`completion-budget.md`](../tests/compositor/completion-budget.md) for proof,
actual-admission mutation tests and native four-worker integration evidence.

The next direct-target boundary is documented in
[`compositor-shared-targets.md`](compositor-shared-targets.md): front ownership
and pending presentation must be separate, and presentation success cannot
release the currently visible allocation. The proposal requires GPU-owner
agreement and implementation; existing copied-source replies keep their meaning.

### Retained front and pending presentation

The three-slot compositor pool now tracks a visible front independently of its
pending Display ticket. All existing operations preserve front ownership;
authoritative replacement evidence moves the pending ticket to front and retires
only the exact prior front. Final-front teardown has its own quiescent retirement
operation. A full front+pending+ready set returns no writer without expanding
storage. The copied-source Desktop path continues to use its existing release
transition and keeps the front role empty.

The pure policy's 27 analysis results have no unproved checks. Hosted pixel
ownership tests and the native Mesa target oracle exercise retained fronts,
stale identities and allocation backpressure. Direct Display/GPU transport is
still pending; the native oracle supplies simulated latch/retirement evidence.
See [front-retention.md](../tests/compositor/front-retention.md).
