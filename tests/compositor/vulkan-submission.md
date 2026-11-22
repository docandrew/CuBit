# Bounded Vulkan submission and writer retention

`Vulkan_Submission` now controls the actual Vulkan command buffer and fence used
by the hosted affine renderer oracle. It has not been selected in Desktop. The
new code adds command and render-pass lifetime policy in SPARK and a narrow C Vulkan dispatch
adapter, without implementing a driver or allocating pixel storage.

## Policy

A fresh, exclusively owned context moves through `Idle`, `Recording`, `Sealed`
and `Pending`. Only `Idle` permits source-table edits or beginning another frame.
The wrapper resets the command buffer and fence only before recording, seals
once, submits once and polls the existing fence once per call. It contains no
wait loop or resubmission path. A pending observation leaves the state unchanged;
a successful fence observation returns to idle; any uncertain foreign result
quarantines the context and keeps resources held.

There is a hard cap of 4,096 draw attempts per frame. Admission occurs before
calling the renderer, including empty/rejected attempts. `Draw_Output` verifies
that the borrowed drawing context refers to this command buffer and device,
and that the output dimensions match the active pass.
A rejected draw, mismatched context or exhausted budget marks the entire frame
incomplete. An incomplete frame cannot be sealed/submitted; it must be cancelled.
Cancellation resets only never-submitted command buffers. Failed cancellation
also quarantines the context.

Each frame has exactly one render pass. `Begin_Scene` opens it before drawing;
`End_Scene` closes it before sealing. A second pass cannot be opened in the same
frame. Successful cancellation clears the pass state, including cancellation
inside an active pass. Unknown begin/end results quarantine the context. These
transitions now invoke the production foreign adapter in the actual Mesa oracle.

`Quiescent` means no recorded or pending GPU work; registered sources are still
retained. `Can_Destroy` additionally requires an empty source table. Neither is
a display signal. Successful cancellation permits source removal but does not
produce a rendered frame. Only `Poll`'s
`Finished` outcome establishes the wrapper's trusted GPU-completion evidence.
Neither outcome establishes latch, scanout retirement or physical photon timing.
Never clone/reopen a state around live or quarantined objects to bypass ownership.

The draw cap bounds attempts admitted through this wrapper, not all commands a
caller could insert directly. Render-pass/framebuffer/pipeline compatibility,
attachment ownership and layouts, final resource barriers, authorized image
imports, descriptor retention and a unique owner of the native context remain
integration obligations. The C boundary rejects malformed pass descriptions,
nonzero render-area origins, dimension mismatches and unsupported clear counts;
it cannot establish the authority or compatibility of opaque Vulkan handles. Vulkan call execution time
is not proved bounded; in particular, library submission can have internal cost.
The production event loop must interleave input and later fence polls rather
than busy-polling.

## Retained sources

The submission owns a fixed table of 136 borrowed draw contexts: capacity for
8 client images and 128 masks, matching the current Desktop cache. It does not
allocate pixel storage. Managed entries use the Vulkan provider below; borrowed
entries remain available for already-owned foreign draw contexts. `Install_Source` accepts an empty slot, a nonnull context,
and an idle controller. Duplicate context addresses and occupied slots are
rejected without replacement. A monotonically increasing 64-bit generation tags
each installation; exhaustion refuses installation rather than wrapping. As with
native context ownership, `Open` is only for a fresh lifetime, never a reset of
live or quarantined state.

`Draw_Output` takes a source ticket instead of an arbitrary pointer. Stale or
absent tickets invalidate the candidate frame before entering the renderer.
`Remove_Source` returns the retained context only while idle and for the exact
current ticket. It rejects recording, sealed, pending and quarantined states.
Every command transition preserves the source table, including errors. Retained
sources may be reused between successful frames without reinstallation.

The table establishes a retirement gate, not cross-process image authority.
For borrowed entries, the external provider keeps the immutable context and its
engine, descriptor, view and pixel allocation alive until successful removal.
Different contexts sharing underlying resources require provider-level shared
ownership. Opaque pointer validity, descriptor contents, import authority and
cross-context aliases remain foreign assumptions. Target/framebuffer lifetime
remains a separate obligation. The actual Desktop/native importer is not yet
connected.

## Fixed Vulkan descriptor provider

`vulkan_sources.c` creates a fixed pool and 136 combined-image-sampler descriptors
once. `Import_Source` calls this narrow foreign adapter only while idle, with an
empty slot and an available generation. The adapter accepts a previously
authorized same-device sampled `VkImage`, creates its BGRA8 or R8 view, updates
one descriptor, and returns a stable draw context. Output dimensions belong to
the draw viewport; source dimensions/layout/format and nonaliasing remain the
image-lease adapter's obligations. The provider neither allocates pixels nor
copies, submits, waits, or performs layout transitions.

`Release_Source` invokes view destruction only after the SPARK gate establishes
idle state and an exact managed ticket. Success retires just that registration;
unknown status preserves it and quarantines the controller without replay.
`Remove_Source` cannot bypass this by detaching a managed entry. Import rejection
with a known-clean status leaves the state unchanged; unknown results hold the
controller and require retaining the attempted image lease even if no ticket
was returned. Native descriptor-pool destruction also rejects occupied entries.

Descriptors are reused within the fixed pool. Reusing an entry requires a new
view import and descriptor update after retirement; retained sources can instead
be drawn across many frames without allocation. The fixture deliberately imports
and releases on each test frame to exercise lifecycle boundaries. Mesa's internal
allocation sizes are not proved. In the tested 64-bit build, provider metadata is
7,672 bytes and the controller's Ada value size is 3,300 bytes; neither includes
pixel storage or Mesa's descriptor/view allocations.

This adapter does not turn a CPU address or another application's image handle
into an authorized local Vulkan image. The native image lease/import transport
still has to provide that authority and keep backing storage alive until release.

## Output frame ownership

`Vulkan_Frame` is now the production adapter between `Vulkan_Submission` and
`Compositor_Pool`. It acquires a writer before starting Vulkan work and selects
the scene context from the acquired ticket's buffer index. Begin/end/submit
failures quarantine the submission and fault the pool while retaining that
writer. A pending fence leaves both states unchanged. Only confirmed completion
makes the target ready; successful cancellation discards unsent work without
publishing a rendered frame. A simultaneous Display protocol fault prevents pool
reuse even if GPU completion or cancellation later succeeds.

The adapter preserves pending-display and visible-front ownership on every
transition. Ordinary full-pool admission defers unchanged; explicit fresh-work
admission may replace the ready target. The pool's existing contract now states
this unchanged-on-deferral property explicitly, and it is proved. Presentation,
combined latch/prior-front retirement, and final-front retirement remain the
existing pool operations with authoritative backend evidence supplied by the
caller. The controller/pool/immutable target table must belong to the same output
and allocation epoch. Native image identities, nonaliasing, framebuffer authority
and physical retirement evidence remain platform obligations.

The real Mesa oracle now uses three distinct image allocations and framebuffers.
It delays simulated presentation while rendering newer candidates, checks more
than 100 ready replacements and 50 simulated latches, and reads all initialized
nonwriter targets to verify their pixels are unchanged. These are real GPU
commands executed by hosted Mesa; the display events are explicitly simulated.
The target views/framebuffers now come from the production provider below; the
underlying image allocations remain fixture-owned, not native Display/GPU leases. Test-only readbacks are not production copies.

## Managed target metadata and shutdown

`vulkan_targets.c` constructs exactly three BGRA8 image views and framebuffers
around already-authorized local Vulkan images. It creates no pixel buffers and
performs no copies, submissions or waits. A failure at any of the six construction
steps destroys all metadata created by earlier steps before reporting known-clean
rejection. An invalid success result retains partial state and reports uncertainty.
The three scene descriptions are published only after the entire set succeeds.
Private request storage and its image leases remain immutable through release.
Different image handles are checked, but backing-memory nonaliasing remains an
obligation of the native lease adapter. The C set occupies 304 bytes in this build,
excluding Vulkan's internal metadata and the underlying images.

`Vulkan_Target_Owner` gates initialization and close in SPARK. It permits one
initialization attempt per fresh output incarnation. Close requires the matching
pool epoch, no source registrations or GPU work, a valid nonfaulted pool, and no
writer, ready candidate, pending presentation or visible front. Premature close
returns unchanged without invoking Vulkan destruction. Unknown construction or
release results quarantine the owner; close cannot replay the failed operation.
The submission, pool and owner must be the uniquely owned set for the same output.

`Compositor_Pool.Discard_Ready` supports disabling an output without presenting
its last unpublished candidate. It accepts only the exact completed ready ticket,
preserves writer/pending/front ownership, and faults on a stale or incorrect
identity. It supplies no authority to retire a pending or visible frame. The
three-image oracle now discards its final candidate and retires simulated display
roles before invoking the production target close path.

The target-owner tests exercise 100 lifecycles through recording, GPU pending,
ready, pending display, visible front and final retirement, plus epoch/source
retention gates, five construction-result faults and three close-result faults.
Real Vulkan tests cover all six partial-construction rollback points, retained
partial state after a fabricated invalid success, four missing dispatch entries,
and fifteen malformed requests. Cleanup after the fabricated result is explicitly
a fixture-only recovery where the callback created nothing; production quarantines.

## Proof and tests

The current combined pool/frame/submission/target-owner summary has **135 SPARK analysis
results: 78 flow and 57 prover, none unproved or justified**. It covers all four units, including
the target teardown gate and the new ready-discard operation. This includes draw-count preservation,
whole-frame rejection, pending-state preservation, cancellation restrictions,
quarantine outcomes, render-pass ordering, immutable source registrations during
work, idle-only removal, strictly increasing installation generations, managed
import/release gating, and preservation of other registrations during release.
It assumes truthful foreign results and exclusive
ownership of the borrowed native objects. C pointer/handle validity, Vulkan,
Mesa, shader execution and hardware synchronization remain foreign boundaries.
The separate affine/transform proof has also completed: 155 results (15 flow,
140 prover), none unproved or justified. Neither result proves physical buffer
lifetime correctness without the native ownership and synchronization bindings.

The frame adapter additionally passes 1,000 frames with 998 saturation deferrals,
three-target selection, seven command faults, and two overlapping Display faults.
The existing pool tests pass 8,000 ready replacements, 15,001 front latches and
3,000 copied-display/mailbox cycles. The mock-status test passes 1,000 lifecycles, 10,000 pending observations, the
4,096-attempt cap, 21 foreign-status cases, rejected command contexts/draws and
empty draws. Seven forbidden orders are rejected by enabled Ada preconditions:
draw before begin, seal before begin, reopen after end, end before begin, submit
before seal, cancel while pending, and seal while the pass is active. Callback
counts verify that these invalid operations do not reach the foreign boundary. Pending polls do not call reset/seal/submit/cancel,
and that rejected partial frames never reach submission. These are policy tests
with scripted foreign results, not GPU faults injected into hardware. Source
coverage includes all 136 slots, duplicate/null/occupied-slot rejection, stale
handle rejection after reuse, and attempts to install/remove while recording,
sealed, pending or quarantined. The tested Ada type reports 3,300 bytes of value
storage (about 3.2 KiB); that excludes alignment, Mesa metadata and pixel storage.
Draw lookup and removal are constant-time. Installation checks at most 136
addresses, only while idle. There is no per-draw allocation. An additional 100
managed lifecycles cover
blocked foreign calls during recording/pending, four import-result faults, three
release-result faults, and absence of replay after quarantine.

```sh
nix develop -c bash -c 'cd kernel && alr exec -- gprbuild -q -p -P ../tests/compositor/vulkan_submission_mock.gpr && ../tests/compositor/build/vulkan-submission-mock/vulkan_submission_mock_tests && alr exec -- gnatprove -P ../tests/compositor/vulkan_submission_mock.gpr -u compositor_pool.adb vulkan_frame.adb vulkan_submission.adb vulkan_target_owner.adb --mode=all --level=2 --report=all --checks-as-errors=on -j1'
```

The real hosted integration uses pinned Mesa lavapipe, the production Ada
submission/draw wrappers and the production C command/fence adapter:

```sh
nix-shell tests/compositor/vulkan-affine-shell.nix --run 'bash tests/compositor/test-vulkan-submission.sh'
```

It passes 232 actual queue submissions and 178,176 pixel comparisons. Each frame
has at least three deliberately delayed `NOT_READY` observations before the
fixture permits the real Vulkan fence status through. The compositor pool's
writer remains rendering/nonwritable throughout pending observations. Each actual
frame registers its source context before recording. Removal attempts while
recording, sealed and pending return no context; after fence completion (or
successful cancellation in the rejected-frame cases), removal returns exactly
the registered context. Image memory remains fixture-owned. Descriptors and per-import views now come
from the production provider. Attempts to destroy its pool while occupied fail
without releasing it. After successful source retirement, the view is destroyed
and its descriptor slot can be reused. The pool
only marks the target ready after the real fence signals. The fixture retains front/pending targets through delayed, simulated display
events and retires them explicitly before teardown. It verifies 354,048 unchanged
nonwriter pixels in addition to the 178,176 rendered-pixel comparisons. This is
not physical Display/i915 retirement.

The final retained-source run recorded 31,446 pending observations (timing-dependent),
exercised actual command cancellation after budget exhaustion and three
command/device/dimension mismatch rejections, and reported zero Vulkan validation
errors. The mismatch cases cancel actual command buffers inside active passes. The foreign adapter also rejects eight missing dispatch entries, 22 malformed
pass descriptions and three invalid end contexts; callback observers show that
none escape into Vulkan. Three valid controls confirm the observers are reached
when expected. Provider-specific coverage fills all 136 real descriptor slots,
rejects occupied slots/double releases, checks six missing dispatch entries,
injects four allocation or invalid-success failures, and rejects eleven malformed
requests. A failed descriptor-set allocation destroys its already-created pool.
The synthetic successful-null-view callback is explicitly a test lie with no
created object; the SPARK layer quarantines this result. Existing affine setup and recording fault checks still pass. The fixture polls tightly to exercise
the state machine; its loop count is not a latency, throughput or scheduling
measurement. Test-only uploads/readbacks inspect pixels and are outside the
production controller.

Both the Ada wrapper and native C adapter compile against CuBit's runtime and
existing musl/Mesa configuration. This is native compilation, not native CuBit
GPU execution. The C control object has no undefined external references because
Vulkan calls use supplied dispatch pointers.

Evidence logs, proof summary, shader hashes and source hashes are retained at
`tests/compositor/build/vulkan-target-evidence/` (current target ownership),
`tests/compositor/build/vulkan-frame-evidence/` (frame integration),
`tests/compositor/build/vulkan-provider-evidence/` (provider),
`tests/compositor/build/vulkan-source-evidence/` (source-retention extension), `/tmp/cubit-vulkan-pass-evidence/` (render-pass extension) and
`/tmp/cubit-vulkan-submission-evidence/` (original controller).

## Remaining integration

Production still needs the native writable-image lease/import adapter feeding
these providers, a connection
from its actual pool/targets to this wrapper, and authoritative
target/import/latch/retirement interfaces from the
GPU owner. Hardware/native CuBit fault and overload execution remain required.
The software fallback is unchanged. Later validation and documentation updates
remain unstaged after the user-requested safekeeping checkpoint.

## Accumulated target damage (2026-10-02)

`Compositor_Target_Damage` keeps at most eight rectangles for each of three
output targets, plus an immutable active paint plan and damage arriving during
that paint. Every scene change dirties all targets. First use covers the whole
output; overlap or capacity exhaustion conservatively merges damage. Completion
clears only the captured work, retaining later changes. Cancellation retains
all damage; unknown completion faults the history and keeps the active plan.
The repaint-aware `Vulkan_Frame` overloads couple this state to writer admission,
cancellation and fence observation. Scene/submit errors retain the active plan
and quarantine the submission. Output recreation must establish new history
only after the old allocation leases have been safely retired.

The hosted Mesa test uses attachment LOAD and explicit image transitions to
preserve three target images. After the 232 affine cases, 96 opaque scenes
change individual source pixels, with additional damage introduced after some
submissions. The entire completed target is compared with the current scene,
and every nonwriter target is checked against its prior contents. The fixture
paints 15,240 pixels versus 73,728 for full repaint (79.3% fewer); it performs
328 actual queue submissions and checks 501,504 unchanged nonwriter pixels.
This is a work-count result, not a throughput or input-latency measurement.
Source uploads and output readbacks exist for the test oracle.

The independent CPU pixel-history test runs 600 cycles with three target
snapshots, more than eight changes per cycle, changes during painting,
cancellation and unknown retention. It checks that every stale target pixel
remains covered by pending damage. Both standalone shader variants remain
covered by their original 232-case matrix.

General composition must rebuild the background and every intersecting layer
inside each damage region; replaying only a translucent changed layer is
incorrect. The layered extension below exercises this requirement. Attachment
contents, layouts, authorized source snapshots and matching output/pool/history
ownership remain integration obligations. This renderer is not yet selected in
Desktop, and simulated display latch/retirement is not physical scanout evidence.

Final repaint proof run checks `compositor_damage`, `compositor_target_damage`
and `vulkan_frame`; the combined project report, including unchanged prior
pool/submission/target-owner results, has 213 analysis results (102 flow,
111 prover), zero unproved or justified checks. The final native Ada/musl compile
passes; this is not native GPU execution. Logs, proof summary and source hashes
are saved under `tests/compositor/build/vulkan-repaint-evidence/`.

## Layered repaint (2026-10-02)

`Vulkan_Frame.Replay_Layer` replays one scene layer over all captured damage
regions, with at most eight draw attempts. The existing 4,096-attempt frame cap
still applies. Wrong output dimensions, invalid source tickets, foreign draw
failure or budget exhaustion reject the entire frame; subsequent replay cannot
issue more foreign draws. Empty plans and fully clipped layers are accepted.
The policy leaves damage and source ownership intact and proves a bound of eight
additional attempts per call. The caller restores an opaque background before
replaying all layers back-to-front from a retained scene snapshot. Layer order,
complete scene enumeration and reporting both old and new geometry remain caller
obligations; this helper is not an autonomous scene graph.

The actual Mesa oracle adds 96 scenes containing a translucent full-output layer
and overlapping translucent windows. It moves, hides and reorders windows,
reports their old and new bounds, clears only captured damage to an opaque
background, then invokes the production SPARK replay routine for every layer.
A separate CPU reference recomposes every output pixel back-to-front, allowing
one UNORM rounding unit per blend. Every nonwriter target must remain bit-exact.
These scenes restore 26,631 background pixels versus 73,728 for full clears;
this does not count layered fragment work or establish a latency improvement.
The complete fixture now runs 424 actual submissions, compares 325,632 output
pixels and checks 648,960 retained nonwriter pixels, with zero Vulkan validation
errors. Both standalone affine shader variants pass their original matrices.

Focused mock tests cover eight-region replay, wrong output/source, exhaustion
partway through replay, foreign draw failure, empty clips/plans and refusal to
replay an already rejected frame. Cancellation preserves damage for retry.
The combined SPARK report has 234 results (105 flow, 129 prover), zero unproved
or justified checks; this run rechecks `vulkan_frame` and retains unchanged prior
results for the other policy units. Evidence is saved in
`tests/compositor/build/vulkan-layer-evidence/`. Physical presentation and native
Desktop activation remain unverified.

Final native CuBit Ada/musl compilation also passes. This confirms compile
compatibility only; the layered rendering execution above uses hosted lavapipe.


## Immutable scene commands (2026-10-02)

`Vulkan_Scene` captures output geometry and up to 512 ordered layer descriptors:
generation-tagged source tickets, logical rectangles, blend/mask flags and tint.
It copies metadata only, allocates no pixels and performs no foreign calls while
collecting. Sealing freezes the list. Overflow, missing source identity, late
append or repeated sealing rejects the list without dropping its captured
prefix. A rejected or unsealed list cannot render a partial desktop.

Replay first validates every source ticket before issuing any layer draw, then
uses the bounded frame replay routine in captured order. Source registrations
are immutable during recording and retained by `Vulkan_Submission` through GPU
completion or quarantine. The scene list itself holds no image authority or
independent leases: all tickets belong to one corresponding submission context.
Never transplant tickets between contexts, retire imported allocations merely
because scene metadata was reset, or treat a fresh scene as recovery from an
uncertain native submission. Opaque-background restoration, complete enumeration,
correct z-order, source-content immutability and old/new geometry damage remain
integration obligations. More than 512 layers requires a complete software
fallback or a separately designed bounded batching path; silent truncation is
forbidden. The 4,096-attempt frame cap remains in force.

The state occupies 20,512 bytes on the tested 64-bit target, with no pixel data.
The actual Mesa layered fixture now captures each scene before recording and
replays the sealed snapshot; its 424-submission full-pixel oracle still passes
with zero Vulkan validation errors. Regression tests exercise full capacity,
collecting overflow, late edits, missing/stale source generations, invalid and
unsealed lists, empty scenes, output mismatch and foreign failure. A stale
second source is detected before drawing a valid first layer.

The combined SPARK report has 269 results (114 flow, 155 prover), zero unproved
or justified checks. This run proves `vulkan_scene`, retaining unchanged prior
policy results in the project report. Logs and source hashes are saved under
`tests/compositor/build/vulkan-scene-evidence/`.

Desktop's current `drawCurrentScene` traverses live window records and its
backend contract completes each drawing call synchronously. Connecting this
snapshot therefore still needs complete Desktop command capture (wallpaper,
chrome, text, clients and cursor), authorized native GPU sources/targets and
asynchronous completion/retirement integration. No Vulkan backend has been
selected in Desktop by this change.

Native CuBit Ada/musl compilation of the snapshot and bridge passes. The actual
rendering evidence above remains Linux-hosted Mesa, not native GPU execution.


## Mixed image and mask snapshots (2026-10-02)

The layered fixture now imports separate BGRA8 and R8 images into two source
slots and captures both tickets in one scene. Its moving/reordered mask uses
coverage and tint, exercising the same sampling/blending path needed for text
and icons alongside client images. Both source views survive recording and
pending fence observations, with independent release only after GPU completion.
The CPU oracle samples the corresponding color or coverage image for each
layer. All 424 submissions, 325,632 output pixels and 648,960 unchanged
nonwriter pixels pass, with zero Vulkan validation errors.

`Vulkan_Scene.Sources_Ready` now exposes whole-list ticket validation. Its proved
replay contract guarantees no draw-attempt increase for an invalid source
anywhere in the list or an unsealed/rejected scene. Existing stale-second-source
regressions pass. The combined report has 271 results (115 flow, 156 prover),
zero unproved or justified checks; unchanged earlier policy results are included.
Evidence is in `tests/compositor/build/vulkan-mixed-scene-evidence/`.
This test uses synthetic R8 coverage, not the native font atlas, and does not
establish Desktop command capture or native GPU import authority.

The native Ada/musl compile also passes for this version. Runtime rendering above
is hosted lavapipe; native glyph/scene capture and GPU execution remain pending.


## Production opaque fills and scene background (2026-10-02)

`Vulkan_Submission.Fill_Output` records an opaque RGB rectangle into color
attachment zero via a narrow `vkCmdClearAttachments` adapter. It requires an
active render pass, checks target bounds and charges each attempt against the
same 4,096-operation frame budget as image draws. Empty/inverted rectangles
consume an attempt without a foreign call. Out-of-bounds rectangles, exhausted
budget or foreign failure reject the frame; subsequent draws cannot resume it.
No source image, descriptor, allocation, upload or pixel copy is needed.

`Vulkan_Scene.Open` now captures an opaque background RGB value (default black).
After whole-scene source preflight, replay fills only the target's captured
damage, then replays every layer in order. The real layered fixture's manual
background clear has been removed. Invalid source lists still record no fills
or image draws. Fill failure or exhaustion stops before any layer replay;
completion/cancellation retains the existing damage and source lifetime rules.
An empty sealed scene now paints its damaged background. The background adds
up to eight operations, so a 512-layer list is not a promise that every possible
eight-region scene fits the shared budget; failure must never publish a prefix.

The full 424-submission BGRA/R8 pixel oracle passes with zero Vulkan validation
errors. Thirteen malformed native fill calls are rejected before dispatch, and
missing-dispatch checks now cover nine submission functions. Hosted tests cover
empty/inverted geometry, target bounds, failed fill, exhausted budget and stopping
all layer work after scene-background failure. The combined SPARK report has
292 results (118 flow, 174 prover), zero unproved or justified checks; submission
and scene units were rechecked, with unchanged prior policy results included.
Native Ada/musl compilation passes; no native GPU execution is claimed. Evidence:
`tests/compositor/build/vulkan-fill-evidence/`.

This supplies a production opaque-fill primitive, not complete Desktop command
capture. Gradient lowering, actual font/icon/wallpaper imports and all Desktop
primitive hooks still need integration with authorized native output leases.


## Ordered solid layers (2026-10-02)

Captured scenes now support `Solid` alongside `Textured` layers. A solid contains
logical bounds and opaque RGB only: source tickets, mask and source-over flags
are rejected. Solids remain in captured z-order, so images/masks can cover them
and later solids can cover earlier content. They require no source descriptor
or texture upload. The bounded scene state is now 24,608 bytes on the tested
64-bit target; capacity remains 512 entries.

`Vulkan_Frame.Replay_Fill` maps logical bounds through the existing SPARK output
geometry transform, rounds fill edges outward, and intersects them with each
captured target-damage rectangle. Empty intersections emit no fill. The proved
bound is at most eight additional attempts per layer; all emitted fills share
the global frame budget. Opaque fill coverage deliberately differs from the
pixel-center texture sampling rule at fractional edges.

The real Mesa fixture now has 448 submissions. Its 120 mixed scenes interleave
solid rectangles with BGRA content and R8 masks. Twenty-four cases cover six
scale ratios, all four rotations and nonzero output origins, with an independent
integer edge-mapping oracle for solids and the existing sample oracle for images.
Full output and nonwriter retention comparisons pass with zero validation errors.
The comparison totals are 344,064 output pixels and 685,824 nonwriter pixels.

Seven focused solid cases cover valid source-free replay, invalid blend/mask/
source combinations, invisible geometry, foreign failure and shared-budget
exhaustion. Existing source/scene/fill/history tests also pass. The combined proof
report has 317 results (124 flow, 193 prover), zero unproved or justified checks;
frame and scene were rechecked, including unchanged prior policy results in the
report. Native Ada/musl compilation passes. Evidence is saved under
`tests/compositor/build/vulkan-solid-evidence/`.

This is production renderer support exercised in a hosted fixture. Desktop still
needs full command capture and the native GPU-authorized lease adapter; its
existing software/Mesa path remains selected. No physical latency claim follows
from these correctness tests.


## Ordered clip replay

`Vulkan_Scene` uses source-free `Set_Clip`/`Reset_Clip` metadata commands without
increasing the 24,608-byte scene record. The real-Mesa submission oracle now runs
616 scenes, including 144 clip cases at six scales and four rotations. It
preserves original texture geometry while independently testing clipped fills,
mask/textures, gradient origins, empty/inverted/offscreen viewports and ordered
reset/replacement. All 473,088 output pixels pass, including 84,512 exact
clipped-background and 1,370 exact solid pixels. Source/target retention and
unchanged nonwriter checks remain active. Zero Vulkan validation errors.

The focused scene/frame proof has 342 results, zero unproved/justified checks;
native runtime compilation passes. Final logs and source hashes are in
`tests/compositor/build/scene-clip-evidence/`. This is hosted llvmpipe execution,
not native CuBit GPU or physical scanout validation.


Glyph scene integration adds 96 real hosted scenes (712 submissions total) using
six density-matched R8 images and all four rotations. Independent pixel-space
sampling checks snapped placement at unit scale, cell/viewport clipping, empty
clips and negative origins: 19,336 covered glyph pixels with <=1 channel-value
blend tolerance and exact background elsewhere. Native-runtime compile passes;
240 mock glyph scenarios and the focused 351-result SPARK proof pass with no
unproved checks. Scene storage remains 24,608 bytes. `Glyph_Mask` retains the same
source tickets and budgets as other textured commands. Font rasterization,
native authorized glyph uploads, cache-key/density association and Desktop GPU
activation are not established by this hosted synthetic-mask test. Evidence:
`build/glyph-scene-evidence/`.

The glyph cases now exercise `Vulkan_Glyph_Sources` and typed `Append_Glyph`,
including generation replacement after each source retirement. The association
uses 128 entries/4,096 bytes, reuses existing font/code/rational-density keys,
and prevents live reassociation, duplicate keys/tickets and pending/unknown
metadata release. Mock capacity and invalid/stale/layout/phase cases, native
compilation, the combined 479-result SPARK report (zero unproved), and the full
712-submission hosted pixel suite pass. Image metadata truth, ready dependencies
and native import authority remain foreign-owner obligations. Latest evidence:
`build/glyph-source-evidence/`.
