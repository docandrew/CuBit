# Display buffer lifetimes

Status: live asynchronous compositor-to-display-to-GPU session submissions,
updated 2026-09-21. The desktop uses a private paint buffer and a read-only shared
transfer buffer per output, with terminal completion validation before reuse.
The GPU uses per-head IRQ-driven command continuations and checked fences.
These are device-command completions, not physical-vblank or photon timestamps.
See [nonblocking presentation](display-outputs-and-scaling.md#nonblocking-session-presentation).

## Boundary and authority

`CuBit.Display_Protocol` defines the scanout operation enum and checked attachment
and display-lease requests. It reuses the desktop protocol's bounded BGRA8888
layout and wire envelope, but does not introduce a desktop surface/session into
the scanout interface. The message adapter never treats a serialized sender
identity or authority tag as authentication.

Attachment (`0x0901`) contains four words: grant slot, grant generation,
width in the low 32 bits / height in the high 32 bits, and byte pitch. Exact
length and zero header flags/reserved are mandatory. Both extents are positive
and at most 65,535; pitch covers a row and the complete buffer fits 16 MiB.
Display acquire/release also require four zero words and a canonical header.
All in-tree clients are migrated; no numeric-grant attachment compatibility
decoder remains. Other display operation payloads have not all been migrated.

Decoding is not authorization. The service first checks its display lease, then
acquires the reference from the kernel against the authenticated sender, read
access and the complete byte span. The kernel checks grant generation, owner,
recipient, rights, span and revocation. The service uses only the returned
mapping, never caller-derived address arithmetic for this attachment.

## Acquisition lifecycle

- Compositor and older shell grant only read access to their pixel buffers.
- A successful attachment retains one acquisition across presentations.
- Replacement acquires the new reference before retiring the old one, including
  replacement by the same reference. Rejected replacements preserve old state.
- While a session frame is outstanding, attachment/lease/session mutation is
  rejected for that output. After retirement, detach discards queued damage
  and clears local pointers before returning the old acquisition. No extra
  copying is added to the steady-state frame path.
- Revocation denies new acquisitions. An existing acquisition pins pages until
  return, so revocation cannot pull storage out from under a reader.
- The compositor and shell revoke their owned grant on release or failed attach.
  A lost/failed release reply is not proof that the receiver has released its
  pin; revocation must retain that protection.
- The compositor's unavailable direct-backbuffer fallback has been removed.
  Received GPU grants cannot be casually re-granted: derived loans need an
  explicit parent-lifetime and rights-attenuation primitive.

This works for both firmware linear framebuffers (including the laptop path)
and virtio-GPU copy/flip. It does not alter their presentation algorithms.

The current-shell GPU regression also exposed an older invalid bridge: it tried
to grant its `MAPFB` device mapping as owned RAM. Kernel frame-ownership pinning
correctly rejects that. GPU shell mode now renders directly into an owned RAM
buffer and grants it read-only; linear framebuffer mode remains direct. The
test runner stages the current shell instead of silently exercising a stale
copy from the base disk. No device-memory grant exception was added.

## Proof and tests

The portable attachment decoder is SPARK: its successful result establishes a
valid bounded layout before the service narrows integers or asks for a mapping.
Hosted tests check canonical round trips, malformed headers, slots, generations,
geometry and pitch limits, plus bit mutations across every payload word.

`display-check.app` has only a display endpoint, no direct framebuffer authority.
The dedicated `display-grants` and `display-grants-virtio-vga` headless profiles
test lease admission, 140 repeated same-reference replacements, short grants,
malformed attachments/releases, pending revocation, presentation while pinned,
slot-generation reuse, stale references and final-pin retirement. The runner
installs current binaries into a temporary disk; it does not edit the base disk.

```sh
nix develop -c make -C kernel test-desktop-protocol prove-desktop-protocol
nix develop -c make -C kernel display desktop shell display-check
nix develop -c tests/headless/run.sh --test display-grants --accel kvm --cpus 4
nix develop -c tests/headless/run.sh --test display-grants-virtio-vga --accel kvm --cpus 4
```

These are codec proofs and native regression tests, not a proof of the whole
display service, kernel concurrency or device DMA behavior.

Validation recorded on 2026-09-10 (all builds/tests/proofs in Nix):

- Hosted desktop/display codecs pass; focused GNATprove reports 202 checks,
  none justified or unproved. No `pragma Assume` or SPARK-Off escape was added.
- Display grant adversary passes on 4-CPU KVM with both firmware and virtio-GPU
  backends, and on 2-CPU TCG with the focused security disk used by CI.
- Native desktop-protocol and input-stream regressions pass on 4-CPU KVM.
  Input stress retains 6 client presents and 138 input requests; this is a
  repaint-count regression check, not a p99 latency measurement.
- `desktop-display` passes the emulated PS/2 keyboard/pointer/button regression
  on 4-CPU KVM with the current compositor, display service and shell.
- The repaired current-shell GPU bridge passes `virtio-vga-primary` on 1-CPU
  KVM. Its old device-memory grant attempt failed before the RAM-buffer fix.
  The obsolete PCI discovery marker in that test was also updated to the
  inventory format emitted by the current device manager.

CI now includes the portable checks and the TCG display-grants test; the local
equivalent was run, not the remote workflow itself.

## Next slice and remaining boundaries

### Bounded submission core (2026-09-10)

`CuBit.Presentation_State` is now a pure-SPARK, allocation-free ADT for one
in-flight submission. It is not yet wired into the compositor/display IPC path.
Explicit phases distinguish queued source ownership, active reading, a copied
source already released, and presented/discarded outcomes with or without a
remaining source obligation. Thus presentation can precede source release for
direct scanout, or follow release for a copy backend.

Admission returns accepted, busy, exhausted or closed. IDs increase without
wrapping during the state instance's lifetime; zero never names a submission.
A completed result must be retired before the slot accepts another submission.
Stale or invalid transitions leave state unchanged. Buffer return is emitted
once, only by a valid release transition. Closing stops admission and discards
queued work but never pretends an in-flight reader has finished.

The backend adapter, not an untrusted IPC peer, must drive these events. It may
admit a submission only after authenticating the target and acquiring the real
buffer; rejected admission must return any provisional acquisition. It may
release only after the final source reader has stopped and must quiesce an
active reader before reporting discard. It must discharge the returned kernel
acquisition obligation and serialize access to this ADT. The pure record is not
an atomic queue, a lock, or evidence that hardware has actually finished.
In particular, closing after a source copy does not manufacture cancellation:
scanout might already be queued and uncancellable. The state remains pending
until actual presentation or backend-confirmed discard, even though that source
buffer no longer has a reader.
Transport integration must bind submission IDs to authenticated object/session
generations so recreating a state instance cannot make old messages current.

Hosted tests exercise an independent transition matrix, rejected/stale events,
close in every phase, exactly-once release and exhausted admission. A small
generic identifier ceiling makes exhaustion testable without a public reset API.
The proof target analyzes actual full-range and small-range generic instances;
GNATprove does not prove an uninstantiated generic template.

```sh
nix develop -c make -C kernel test-desktop-state prove-desktop-state
```

The combined click/presentation state proof currently discharges 35 checks,
including functional transition, nonmutation, ownership-release and close
contracts. No assumed facts or SPARK-Off regions were added. Native integration,
timing measurements and a guarantee that every admitted submission eventually
finishes are still outstanding.

### Integration progression

#### Event-driven dispatch and staged frame wire format (2026-09-10)

The compositor now uses `Wait_For_Activity_Until` for idle/frame-deadline waits.
Unlike receive, this syscall does not consume messages, completions, or current
reply authority. Under the mailbox lock it checks requests, events/IRQ doorbells
and completions before registering the waiter. Available work wins over an
expired deadline; `Unsigned_64'Last` means no deadline. Ordinary IPC publication,
completion publication, target death and deadline expiry detach waiters before
making them runnable. A sibling may drain the work, so readiness is a hint, not
a reservation. This reuses the existing mailbox/deadline machinery, not a
periodic completion-polling timer. It confers no new endpoint authority.

The native async-IPC fixture covers empty deadline expiry, one-way and synchronous
request wakeups, a deliberately delayed completion, preservation of reply
authority, repeated readiness without consuming the completion, and target death.
The delayed/death tests also check elapsed time: merely waking at the timeout
and finding an already-published completion must not pass as a correct wakeup.
These tests do not prove scheduler concurrency or establish a latency percentile.
Thread-specific async submission/completion ownership remains an existing separate
audit item; these native tests exercise process dispatchers, not threaded clients.

Staged `Submit_Frame` messages use all four inline words: nonzero session ID,
nonzero frame ID, packed 16-bit x/y/width/height, and a zero reserved word.
Result words contain session ID, frame ID, a typed outcome
(`Published`, `Rejected`, `Failed`), and a distinct source disposition
(`Not_Acquired`, `Released`, `Still_Held`). Exact headers and bounded enum values
are checked by the pure codec. Geometry relative to the attached layout, peer
authority, session freshness, replay protection and completion-token matching
belong to the live adapter, not the decoder. `Open_Presentation_Session` is also
reserved for that adapter; neither new operation is enabled in display yet.

Hosted tests cover coordinate/extent boundaries, header corruption, zero IDs,
all outcome/disposition combinations, and per-bit hostile payload mutations.
The focused desktop/display codec proof now discharges 222 checks; the separate
click/presentation-state proof still discharges 35. Neither proves the kernel
wait implementation or the eventual live adapter.

Native validation in Nix: async-IPC passed on 4-CPU KVM and 2-CPU TCG;
the final no-waiter fast path was retested on 4-CPU KVM. Desktop-protocol,
input-stream, and multi-app desktop-doom passed on 4-CPU KVM. Input stress
retained six client presents and 138 input requests, including gap recovery.
The DOOM regression opened/closed Workbench and NetSurf first, then verified
game pixels and a responsive Apps menu. This is not an audio-quality or
end-to-end latency measurement. Serial logs from this run are
`/tmp/cubit-activity-{fastpath,desktop,input,doom}.log` and
`/tmp/cubit-activity-final-tcg.log`.

The next live slice keeps compositor-private paint memory and one read-only
shared transfer buffer. Accumulate bounded pending damage while a frame is held;
copy damage to the transfer buffer only after a validated source-release result.
Successful `capSubmit` means kernel enqueue, not service acceptance or release.
A failed/malformed completion must quarantine uncertain ownership, not permit
reuse. Replace both ordinary rectangle presents and the drag-region special path.
The delayed-consumer rendering/input regression is still to be implemented with
that adapter; the delayed IPC test above is not a rendering regression.

#### Live adapter (2026-09-11)

The earlier staged operations are now implemented. `Open_Presentation_Session`
requires a canonical zero-word request, the display lease, and a fresh checked
acquisition of the attached grant. It returns four words: status, nonzero session
ID on success, and two zero reserved words. Session IDs never wrap/reuse within
a service instance. Kernel endpoint generations separate service incarnations.
An attachment replacement or display release invalidates the session before
dropping the attachment pin. Reopening creates a new session; old frame requests
cannot read the new attachment. Other synchronous/packed presentation operations
are rejected while an asynchronous session is active.

Each admitted frame validates owner, session, increasing frame ID, nonempty
in-layout damage, and freshly reacquires the read-only grant. Thus revocation
denies new frames, even though the older attachment acquisition remains pinned.
The live adapter drives `CuBit.Presentation_State` through admission, reading,
publication/discard, confirmed acquisition return, and retirement. Release uses
a trial state: the state change is committed only when the real kernel return
succeeds. No failed return is reported as `Released`. Backend/state failures
quarantine further admission; backend failure does not silently become a
successful framebuffer fallback.

Desktop paint/cursor operations only modify private memory. Pending damage is
one bounded union rectangle. With no frame in flight, the compositor copies
that damage into the transfer buffer and submits it through `capSubmit`. Input,
window requests, and private drawing continue while display holds the buffer.
There is deliberately one additional damage copy in this first implementation;
it is not zero-copy or a multi-buffer swapchain. Disjoint damage can make the
union larger than its individual rectangles. Both ordinary presents and the
drag-region path now use this same submission queue.

A successful enqueue is **not** a source-release fence. Reuse requires a valid
kernel completion with the outstanding non-reused token, matching session/frame,
`Published` outcome and `Released` disposition. Failed/malformed completions or
failed enqueue quarantine the transfer; there is no retry spin or timeout-based
reuse. Retired sessions' late completions cannot release a newer buffer. Session
replacement allocates fresh storage; old uncertain storage is not recycled.
SBRK-backed session allocations are not yet reusable storage-pool management.

The virtio-GPU map reply now uses a generation-checked attachment-layout payload:
four words `[slot, generation, width | height << 32, pitch]`; one-word replies are
errors. Display validates geometry and obtains mappings with
`Acquire_Via_Capability(CAP_SLOT_GPU, ..., Write_Access)`, not numeric slot-address
arithmetic. These two acquisitions last for the display instance and are retired
with its kernel acquisition ledger. This protects CPU access to that memory;
it is not a proof of DMA quiescence or IOMMU isolation.

`Published` means a completed framebuffer copy or acknowledged backend publication,
not that a physical monitor has scanned the pixels. The terminal message couples
that outcome with source release for each output's single in-flight frame. Early release and
later presentation events would require extending the transport, not pretending
that a kernel enqueue acknowledgement is presentation. Desktop statistics now
call the aggregate in-flight duration `completion_ms`, not the old `submit_ms`:
it is wall time overlapping other work, not time blocked inside submission.

#### Live tests and proof boundary

The native display adversary exercises 140 consecutive frame returns, identity
matching, replay/stale-session rejection, empty/out-of-layout damage, malformed
frames, replacement invalidation, and revocation between frames. It retains the
older lease, stale grant-generation and final-pin-retirement tests. This passed
on both framebuffer and virtio-GPU backends with four KVM CPUs.
The framebuffer adversary also passed with two TCG CPUs; the full virtio-GPU
desktop profile passed with four KVM CPUs after the final build.

The delayed-reader fixture builds the **same** display/compositor adapters with
a test-only policy. It fingerprints the shared buffer, sleeps for 75 ms while
holding the acquisition, then fingerprints it again. The runner requires at
least two stable frames and input dispatch strictly between the hold/stable
markers for the **same** frame. This passed, including the ordinary input-stream
gap-recovery and repaint-count checks. It is a regression test, not a proof of
immutability against a malicious writer or a p99 latency measurement.

```sh
nix develop -c make -C kernel display desktop virtio-gpu display-check
nix develop -c tests/headless/run.sh --test display-grants --accel kvm --cpus 4
nix develop -c tests/headless/run.sh --test display-grants-virtio-vga --accel kvm --cpus 4
nix develop -c bash tests/headless/run-delayed-display.sh --accel kvm --cpus 4 --timeout 30
```

The wrapper stages the delayed binaries **before** preparing the VM disk and
restores production display/desktop binaries on exit, including failure. The
normal builds disable the delay and per-frame diagnostic tracing at compile time.
The first attempt correctly failed because a production binary had been staged;
the wrapper fixes that fixture ordering rather than weakening the test oracle.

The normal desktop-input/window test and the Workbench/NetSurf → DOOM sequence
also pass on four-CPU KVM. The latter verifies game pixels and a responsive Apps
menu, not audio quality. The focused codec proof now discharges 224 checks and
the click/presentation-state proof discharges 35, with no assumptions or added
SPARK-Off regions. The effectful live adapter, kernel concurrency, and hardware
completion semantics are not thereby formally proved.

#### Remaining integration

1. Measure frame-copy CPU cost and input latency under redraw load. Consider a
   coherence-aware swapchain or bounded multi-rectangle frames after measurement.
2. Extend app/toolkit surface lifetime handling before adding command batches.
   Writable sender memory cannot hold trusted command descriptions: snapshot
   bounded commands before validation, or establish enforceable immutability.
3. Measure input responsiveness under redraw load; do not infer p99 latency
   from these correctness tests. Menus/combos/radio groups remain shared-widget
   work, tracked in [the toolkit audit](ui-toolkit-audit.md).

Still open: full validation of other scanout
operations, generation-bound display-owner identity and dead-owner lease
recovery, retained-pin quotas, and recovery when display/GPU services fail.
Idle owner death does not yet automatically retire a display lease. Pins protect
memory lifetime but are not a resource-reclamation policy or a frame-release
fence. Receiver read-only access also does not stop the sender mutating pixels.
