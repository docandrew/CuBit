# Shared compositor targets: integration contract

Status: proposed handoff for the GPU/Display owners; not a supported wire ABI.
The current software fallback and copied-source protocol remain operational.
No driver changes or operation numbers are reserved by this document.

## Why forwarding alone is insufficient

The current path uses three Desktop source allocations (`Compositor_Pool`) and
two Display GPU backing buffers (`CuBit.Backend_Targets`). Display copies source
pixels into a backing buffer. Its `finishFrame` may then return the source
acquisition and reply `Published / Released`. Desktop's `collectPresentations`
requires that release before clearing its one outstanding display role.

A directly scanned-out source is still being read after presentation succeeds.
Reusing that allocation on the existing `Published / Released` reply could write
visible pixels. Returning `Published / Still_Held` through the existing protocol
would instead quarantine Desktop. Waiting to reply until replacement would also
stall: `Compositor_Pool.Present` currently refuses a replacement while its display
role is occupied. Thus this needs separate front and pending roles and separate
presentation/retirement evidence, not a flag on the current frame reply.

The virtio `MAP_FRAMEBUFFER` handler currently grants only indices 0/1 using
nonforwardable `Create_For_Process`. Kernel owner-opted-in forwarding exists,
but grant permission is not a GPU retirement fence or a presentation contract.
The i915 owner must explicitly provide equivalent allocation and retirement
semantics; the compositor must not infer them from the application-image path.

## Proposed initial contract

Prefer three distinct GPU-owned linear BGRA targets per enabled output, replacing
the three Desktop sources and two copy destinations. Keep the copied fallback
when allocation, layout, synchronization or presentation support is absent.
Do not pretend that two grants satisfy the current three-allocation invariant.
Two-target mode requires its own explicitly tested admission/repaint policy.

Negotiate once per output incarnation, before importing any Mesa target:

- Backend identity and output generation, target count, immutable width/height,
  pitch, byte extent, BGRA format, linear layout, and cache/coherency requirements.
- For each target: generation-checked allocation identity, owner-authorized
  renderer import and scanout capabilities, and an optional CPU mapping
  capability with explicit access/coherency rules. Device-local GPU targets
  need not be CPU accessible. A writable CPU mapping alone does not establish
  Vulkan importability; a GPU import does not authorize CPU access.
- Negotiate CPU access only for a renderer that requires it (the software
  fallback or CPU Mesa path). If the direct targets cannot be CPU mapped,
  preserve the existing software presentation path rather than requiring a
  writable CPU grant on every GPU allocation.
- Explicit backend guarantees for rendering completion, presentation acceptance,
  latch evidence, retirement and quiescent teardown. Unsupported guarantees must
  reject direct mode instead of being synthesized by Display.

Every submission binds output incarnation, session, monotonic frame identity,
target identity/generation, damage, and rendering-completion evidence. Identity
fields must survive through every asynchronous continuation. The IPC encoding
and operation namespace need owner agreement before implementation.

Presentation response: identifies the submitted target and frame, reports the
new front and the prior front retired by this transition (if any). Completion of
the new frame never implies retirement of that same new front. If hardware
cannot combine latch and old-front retirement, use independently tracked events;
never reinterpret command acceptance or a timeout as either event.

An idle front remains held indefinitely. Output disable/close needs an explicit
quiescent retirement operation so the final front can be released without a
replacement. Hotplug, device loss and uncertain completion retain affected
allocations until authoritative retirement or device teardown is established.

## Roles and bounded work

| Role | CPU/GPU writable? | Evidence needed to leave role |
| --- | --- | --- |
| Writer/rendering | Only its authorized writer; CPU excludes active GPU writes | Render quiescence |
| Ready | No; newest completed frame may replace older ready work | Submission or quiescent supersession |
| Pending presentation | No | Matching presentation result; uncertainty quarantines |
| Front | No | Matching replacement retirement or quiescent disable |

Roles must be disjoint by allocation identity, not merely by ticket. With three
allocations there may be front + pending + writer, or front + pending + ready;
there is no fourth allocation. Lack of a free writer defers rendering while
coalescing bounded scene damage. Keep accepting fresh input. One pending
presentation per output and at most one ready frame; no FIFO of stale frames.
Preserve per-target damage history, cursor underlay, format/layout checks,
per-output scaling and Mesa import retirement before returning storage.

## Required implementation and acceptance order

1. Agree the GPU-owned target/export, coherency and retirement interface; test
   owner death, stale generation, readonly/wrong-recipient grants and partial setup.
2. Extend and prove the compositor pool with distinct front and pending roles,
   bounded allocation exhaustion, newest-ready replacement and sticky uncertainty.
3. Implement Display's negotiated direct transport with target identity checks;
   keep current source-copy transport as fallback. Never change old reply meaning.
4. Bind Desktop CPU and Mesa destinations to the negotiated targets. Preserve
   read/write exclusion and permit input handling when no writer is available.
5. Native exact-pixel/cursor/resize/mixed-DPI tests, deliberate delayed latch and
   retirement, out-of-order/duplicate completions, disconnect and four-worker load.
6. Measure source-copy bytes reaching zero in direct mode, bounded target count,
   stage timings and release delays. Measure i915 latch/scanout behavior on hardware
   before claiming tear-free operation, 240 Hz or physical input-to-photon latency.

For equal, tightly packed layouts, replacing five allocations with three saves
six MiB per 1024×768 output and about 63.3 MiB per 3840×2160 output. This is an
allocation estimate, excluding alignment, GPU metadata and renderer caches;
it is not measured memory reduction in the current implementation.

## Pool implementation status

The retained-front portion of step 2 is now implemented in the existing
`Compositor_Pool`: `Front`, `Latch_Display`, `Retire_Front`, and unchanged-state
backpressure when all three allocations are occupied. It preserves the
copied-source release API. The policy is proved and exercised by the hosted
pixel-ownership model and native Mesa oracle described in
[front-retention.md](../tests/compositor/front-retention.md).
This does not complete steps 1, 3 or 4: production Desktop still uses the copied
transport, and a direct adapter must bind authoritative latch/retirement events
before using these transitions.

The producer policy additionally supports explicit ready replacement for newer
scene work. When all slots are held, opt-in acquisition can reclaim the completed
ready allocation with a fresh serial while preserving front and pending. Default
acquisition still defers. A direct adapter must invoke replacement only after
new damage exists, preserve per-allocation repair history, and never treat an
overwritten ready frame as a fallback after render failure. This avoids needing
a fourth buffer to keep the candidate frame fresh during a delayed presentation.


## Desktop renderer presentation leases (2026-10-03)

Desktop_Vulkan_Startup now connects its actual retained target pool to
Take_Presentation, Confirm_Presentation and Retire_Presentation. Take moves
only a completed frame into the single pending slot; it returns no ticket while
another presentation is pending. The visible front and pending target remain
unwritable while newer completed frames replace the third target. Tickets carry
buffer, epoch and frame serial; no image pointer or scanout authority is exported.

Confirm requires evidence for both the new latch and retirement of the exact
previous front. Final output disable requires explicit front retirement.
The trusted adapter must authenticate output identity and completion evidence;
command acceptance or timeout is not sufficient. Wrong tickets, stale epochs,
wrong previous fronts and unconfirmed events quarantine the pool. Stop cannot
refund or destroy held target storage. Confirm/retire remain usable during
orderly shutdown so known display readers can retire before a second Stop.

A clean selected proof of the updated Desktop owner passes 241 checks (150 flow,
91 prover), none unproved or justified. Mock integration tests cover one pending
frame, 64 newest-frame replacements, exact handoff/final retirement and four
invalid/uncertain evidence cases. The real hosted Mesa wallpaper oracle now holds
a front and pending target across 382 further renderings: neither framebuffer is
selected for rendering, all 2359296 compared pixels match, cleanup succeeds and
Vulkan validation reports zero errors. Display evidence is simulated by this
hosted harness: this is not physical scanout, tear-free presentation, or a native
GPU/240 Hz result. Actual negotiated image export and the authenticated Display
transport described above remain unimplemented.


### Known-quiescent presentation cancellation

Cancel_Presentation retires only the matching pending ticket after the adapter
confirms that this rejected/cancelled request never became visible and has no
remaining display readers. The visible front and newest completed candidate
are preserved. This permits a later Take_Presentation to select fresh work
instead of replaying the rejected frame. The operation is also available during
orderly shutdown, so a confirmed pending rejection can be retired before final
front retirement and Stop. False evidence, missing or stale tickets quarantine
the pool without releasing its readers. A timeout is not quiescence evidence.

The ten-scenario GPU-owner regression includes cancellation, replacement by the
newest ready frame, shutdown cancellation, stale cancellation identities and
uncertain cancellation. Real hosted Mesa also cancels a pending frame after
382 newer renderings, latches the newest ready frame against the original front,
then retires it and completes accounted cleanup. These tests simulate display
responses; actual authenticated Display transport and physical scanout remain
separate integration work. The implementation delegates the lease transition
to the existing proven Compositor_Pool.Retire_Display operation.

The clean selected proof of Desktop_Vulkan_Startup including cancellation
passes 245 checks (152 flow, 93 prover), with none unproved or justified.
The integrated checkout also compiles and passes all ten presentation scenarios.
Evidence: tests/compositor/build/gpu-presentation-cancel-proof.out,
gpu-presentation-cancel-published-r1.log and
 gpu-presentation-cancel-real-r2.log in the same directory. This extends the
owner's proof above; counts are not additive across revisions.


### Captured image readers

Desktop_GPU_Scene now pins each distinct non-glyph source while capturing
textured, straight-alpha and wallpaper layers. The renderer owns a fixed table
of 136 reader tokens. Tokens have a monotonic 64-bit serial, never wrap, and
are independent of source-generation tickets. Repeated draws in one scene
reuse one reader; another scene must acquire its own reader. Exhaustion rejects
the complete capture without submitting a prefix. This is metadata only: no
pixel copy, heap allocation, additional image or new foreign interface.

Release_Source refuses a pinned registration even before GPU submission.
Stop enters stopping state but retains resources while CPU image readers exist.
The scene first drops its command snapshot, then retires readers only after
positive healthy renderer quiescence. Unknown completion retains the pins;
false CPU-retirement evidence and stale reader tokens cannot remove a live pin.
The normal glyph cache retains its separate glyph readers. Providers must still
keep source backing immutable until Release_Source returns its release key;
these reader tokens do not import client grants or establish external authority.

Private hosted regressions passed four image-reader scenarios, both existing
glyph-scene scenarios and 288 wallpaper capture cases. Coverage includes 200
repeated draws with one image pin, 100 pending observations, captured shutdown,
136-token exhaustion, slot reuse with stale-token rejection, missing CPU
retirement, stale source generations and unknown GPU completion. Real hosted
Mesa passed 384 frames/2359296 exact pixels, held presentation targets, accounted
cleanup and zero validation errors with image pins enabled. This real-provider
fixture also attempts source release during CPU capture and verifies refusal
before submission, then checks scene readers are retired after completion.
The clean first owner/scene proof passes 325 checks (194 flow, 131 prover).
After strengthening Pin_Image to preserve phase and layer count, the selected
scene/backdrop proof passes 75 checks (35 flow, 40 prover). The owner source is
unchanged between runs; these overlapping counts are not additive. Neither
report has unproved or justified checks. Reports are retained as
 tests/compositor/build/scene-source-pins-proof.out and
 tests/compositor/build/scene-source-pins-scene-proof-r2.out.
These checks do not establish client-buffer import, native GPU execution,
physical presentation or hardware performance.


## Current-source integration gap (2026-10-03)

The source audit after the scene completion-gate work identifies three distinct
interfaces that must not be conflated:

| Existing interface | What it supplies | What it does not supply |
| --- | --- | --- |
| `userspace/mesa/service-device.h` | Borrowed Vulkan device/queue under a retained render session | Presentation-target negotiation or transferable image identity |
| `userspace/lib/compositor/vulkan_device_storage.c` target preparation | Three private owned images and a render-target bundle | An authorized Display import or scanout export |
| `userspace/mesa/anv/native_gpu_presenter.c` completed-linear attachment | Read-only CPU-grant forwarding from a BO to a Desktop surface | Vulkan cross-session import, scanout compatibility, or latch/old-front retirement |

The presenter explicitly maps for presentation, acquires a CPU view, derives a
read-only child grant and attaches that child to Desktop. It is useful for the
existing application-image path. It is not a bridge from the compositor's
private Vulkan targets to Display's scanout pool. Likewise a local presentation
ticket in `Desktop_Vulkan_Startup` preserves target roles but carries no foreign
image authority. The hosted framebuffer oracle performs readback for validation;
that does not establish a zero-copy native presentation path.

Before activating this GPU renderer in Main, the GPU/Display provider must agree
on either driver-owned presentation images imported by Mesa, or renderer-owned
images exported with equivalent authenticated ownership and retirement. The
required output-incarnation, image-generation, layout, rights, completion and
retirement fields are specified above. CPU mapping remains optional. No new wire
operation or import authority is introduced by this audit. The compositor's
coordination note records the provider-interface request; acknowledgment and a
concrete supported API remain outstanding.

Complete primitive capture and whole-frame software fallback can proceed
independently. Native GPU activation and physical presentation measurements
cannot be claimed from the current device-borrow, CPU-grant or archive-build
interfaces alone.
