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
- For each target: generation-checked allocation identity and owner-authorized
  writable CPU grant, plus the renderer import identity where GPU rendering is
  available. A writable CPU mapping alone does not establish Vulkan importability.
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
