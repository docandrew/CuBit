# GPU DMA retirement boundary

Status (2026-10-01): the native Intel path now publishes buffers and submits
GPU work; physical NUC testing has demonstrated a hand-built drawing batch.
Native Mesa window probes link and are packaged, but their actual Intel
execution still awaits hardware validation. The tested/SPARK transition model
in `Intel_GPU_DMA_Lifetime` is NOT an integrated general reclamation mechanism.

## Sustained-rendering limitation

The native pool still has bounded retention limitations:

- `Intel_GPU_Buffer_Backing` has 16 allocation slots and a 32 MiB CPU arena.
- `Intel_GPU_Buffer_Requests` spends those slots for both application buffers
  and private context/VM allocations. Failed attempts also spend tickets.
- `Intel_GPU_Buffer_Handles.Close` only closes an authenticated name; it keeps
  reserving the backing range. Mesa's `gem_close_locked` explicitly relies on
  this retention and does not establish GPU retirement.
- CPU-view bookkeeping can recycle after confirmed grant retirement. That
  does **not** recycle a buffer allocation, GPU mapping, or allocation ticket.

Consequently, repeated create/draw/destroy probes can exhaust the pool even
when every frame succeeds and every Desktop grant retires. They test lifecycle
ordering, not unlimited rendering. Raising the slot count is not reclamation.

The underlying `Intel_GPU_Extent_Allocator` already supports exact-generation
`Retire_Buffer` and next-generation `Acquire_Buffer`. It releases a slice of
the retained physical arena, not the kernel's DMA blocks. Its retirement input
is a **trusted assertion** that all references are gone, not evidence it can
derive itself. Devmgr now exposes private operation `0239` to the pinned Intel
driver incarnation, accepting `[slot,generation,arena,1]` only with its kernel
authority tag and an already granted arena. It calls `Retire_Buffer` for that
exact generation and acknowledges the allocation key. Foreign callers, malformed
requests, stale generations and duplicate retirement cannot release a slice.
This trusts the driver to certify all references retired; it is not hardware
isolation from a malicious bus-mastering driver. The live close handler now
issues this operation only after current admission, acknowledged scheduling
stop for all retained contexts (Disabled or Deregistered),
work-drain, committed-VM disjointness and CPU-grant checks. It saves the close
reply authority and blocks allocation/VM update/submission while retirement is
pending. Lost or ambiguous supervisor acknowledgements require quarantine,
not replay. This is distinct from losing the final close-status reply to an
application: once retirement is confirmed, a departed caller does not undo it
or quarantine other sessions. That reply transfers no new handle or grant.
The kernel consumes its one-use reply capability even on failed delivery.
The allocation transport's `Buffer_Memory.Retire` can now submit that operation
for an exact local generation. Only a matching success acknowledgement clears
the local retained slice and advances its generation; failures quarantine the
pool. The next allocation reuses the full zero/flush/readback path. Hosted
tests exercise 128 cycles with mocked supervisor replies and actual CPU buffer
initialization, plus uncertain-outcome rejection. The request layer supports explicitly acknowledged
application-ticket reuse across authenticated sessions, pairing fresh tickets with fresh handle
generations. Original context/root/scratch tickets and uncertain allocations
remain one-shot. Replacement VM-table tickets can be reused across sessions
after exact retirement acknowledgement and offline-image reset. Reservation
advances the full ticket and replaces its owner before allocation starts;
stale-owner completion/acknowledgement cannot affect the new generation.
The close-time coordinator is compiled into the native driver, but
its actual retirement execution has not yet been tested on the NUC. It is not
a complete session reclaimer: closed buffers are revisited by a bounded
background queue, but remain retained while references exist. Requests stay in
the kernel mailbox during a retirement handshake while completion polling
continues. Superseded replacement tables have a separate bounded retirement
pass with physical-alias checks. Already acknowledged application and private
replacement-table slots may be reassigned to another authenticated session.
Closing a session preserves these acknowledged slots but does not promote
unacknowledged or pending private allocations. Whole-session context/root/scratch cleanup remains
unfinished. Native
compilation and component regressions are not a proof of the whole hardware
lifecycle. Close alone never establishes retirement.

Acknowledgement also releases the closed handle's range reservation while
retaining its identity tombstone. This matters when another session allocates
that range before the original session requests a replacement: the replacement
must respect the new occupant, and the old handle must never resolve its memory.
Hosted cross-session tests cover this distinction; targeted SPARK checks cover
the handle registry contracts, not the trusted hardware retirement assertion.

The handle registry now has a separately tested `Replace_Retired` metadata
operation. It requires an exact closed previous-session identity and a trusted
retirement assertion, validates replacement backing against retained neighbors,
and advances the full handle by the slot count without wrapping. Session close
checks stored entries across all generations. Targeted SPARK checks cover the
registry contracts; hosted tests exercise 128 replacement generations and stale
names. Cross-session replacement additionally requires the old reservation's
acknowledged released state. The request layer retains that previous owner
separately from the new authenticated owner while allocation is pending. Closing
the old session cannot cancel the new owner's pending request or close its new
handle. Tests cover 128 successive owners sharing one acknowledged slot;
hardware zeroing/retirement remains an integration obligation.
Acknowledged allocation replacement invokes this operation.
It neither establishes retirement nor releases storage by itself, and does not
remove the private allocation limits above.

Integration must preserve fresh identities across all three layers: driver
handles/tickets, asynchronous allocation completions, and supervisor slices.
The existing allocation reply key includes slot and generation, but that alone
does not make recycling the driver's pending state safe. Reused storage also
needs the existing zero/flush/readback initialization before new grants or GPU
publication. Old handles, close messages, and completions must never address
the next occupant. This path can reuse the existing slice allocator; it does
not require a new physical allocator or returning retained DMA RAM to the OS.

`Buffer_Memory` now uses a fresh completion serial for every submission,
including supervisor retry and extent-fetch requests, and rejects serial
wraparound. Hosted regressions check that stale replies cannot advance or
cancel the current transaction; the native driver compiles with this path.
This protects completion matching but does not yet recycle allocation tickets
or establish the retirement evidence needed to acquire another generation.

The next integration must distinguish name closure from backing retirement:
close admission to the allocation generation, drain GPU work, eliminate all
GPU bindings and confirm the required translation invalidations, and confirm
all CPU grants (including Desktop descendants) retired. Only then may trusted
pool code zero and reuse backing under a fresh identity. CPU grant retirement
alone is insufficient; a still-mapped GPU address can remain reachable from a
later command batch. Uncertain publication, owner loss, or missing completion
evidence must keep the allocation quarantined rather than free it.

This is a remaining production-driver requirement, not a claim that the
existing state-machine proofs establish native reclamation safety. The older
bring-up evidence below records intermediate milestones, not current feature
availability.

## Whole-session cleanup audit (2026-10-01)

The Linux reference separates scheduling-disable handling from context
destruction. Its deregistration-completion handler releases the GuC ID and
destroys a context marked destroyed; it also exposes separate engine and GuC
TLB invalidation operations. See the current browsable
[GuC submission implementation](https://codebrowser.dev/linux/linux/drivers/gpu/drm/i915/gt/uc/intel_guc_submission.c.html#intel_guc_deregister_done_process_msg).
This is a source comparison, not a pinned firmware-ABI validation or evidence
that every allocation requires both invalidation operations.

CuBit's remaining whole-session integration must classify each retained parent
allocation: application PPGTT leaves, replacement tables, original root/scratch,
and GGTT-visible context/ring storage. A deregistration event does not by itself
clear our retained GGTT ledger entries, CPU descendants, or allocator records.
Before reclaiming context parents, audit the applicable translation domains
and the actual unpublication/completion path. Do not infer a GuC translation
flush from the existing native engine-TLB completion. Already acknowledged
application-slot reuse is separate from this unfinished context cleanup.

Each original context parent now retains its allocation ticket before the
asynchronous allocation begins. Completion verifies that exact ticket and
session before accepting backing, and GGTT cleanup checks the current retained
ticket owner. The ticket survives failure and session closure. This preserves
the slot/generation identity needed for a later supervisor release; it does
not itself make the parent reclaimable or replace alias/translation checks.

The native ADL-N dispatcher now connects the existing application-image
retirement helper to a bounded, one-candidate-per-loop cleanup pass. It requires
the session's terminal lifetime, acknowledged GuC deregistration, completed
application work, retired CPU grants, and acknowledged scheduling stop for all
retained contexts. A separately captured exact GGTT range authorizes only
replacement with the driver's retained scratch page. The helper preflights
every old PTE against the retained allocation, replaces and reads back the PTEs,
then waits for RCS/OA and GuC MMIO invalidation completion. ADL-N uses CEE8,
not the later-platform GuC CT invalidation action.

An attempted retirement is never replayed. Uncertainty after replacement
quarantines the runtime. `retired context GGTT DETACHED` explicitly retains
backing and its ledger claim: it is not permission to free the context parent,
root, scratch, replacement tables, or application BOs. Hosted image tests cover
the remapping helper and failure paths; they do not validate the native gate,
MMIO completion, or this cleanup sequence on physical hardware.

`Intel_GPU_Retirement_Invalidate` is the dispatcher's actual one-shot
engine-then-GuC completion composition. Run its hosted fault-injection fixture
with `nix develop -c bash -c 'gprbuild -p -P tests/intel-gpu/retirement_invalidate.gpr && tests/intel-gpu/build-retirement-invalidate/retirement_invalidate_tests'`.
It checks every I/O, ownership-check and clock failure on the successful path,
bounded termination with a frozen clock and pending hardware bits, and rejection
of a second execution without further callbacks. These are mock-MMIO tests,
not hardware validation or a SPARK proof of the complete cleanup coordinator.

`nix develop -c python3 tests/intel-gpu/test-retirement-gate.py` additionally
compiles the exact `Image_Retirement_Owner` function extracted from the native
dispatcher. Its hosted fixture varies lifecycle, scheduling, drain, grant,
range, reset and backend facts. This validates the gate's decisions with mocked
observations; it does not establish that those observations hold on hardware.

For a session that attempted context registration, the retirement query now
also waits for this GGTT detachment. An unattempted cleanup remains pending;
a rejected/quarantined attempt reports uncertainty rather than successful
drain. Existing pending work and uncertainty are never cleared by this check.
The same extracted-function fixture exercises all 48 combinations of these
flags and cleanup outcomes. The response still does not certify allocator
release or reuse of original parent storage.

## Opt-in bring-up retention implementation

Following the kernel handoff, ALLOC_DMA arg3 now selects 0 (existing process
lifetime) or 1 (retain successful backing until reboot); other values reject.
The existing CAP_PROCESS/RIGHT_GRANT authorization still applies. Mode 1 has
a global 16384-page/64MiB boot budget under its own short critical section.
Failed allocation calls refund their reservation; successful calls never do.
This conservative global limit is not a per-adapter production quota system.

The process DMA record carries retainUntilReboot. Both releaseDMAAllocations
callers still wait for CPU loans as before and clear frame-owner tags; retained
blocks skip buddy free and remain allocated across PID reuse. There is no
user-requested reset/release path. This is a deliberately bounded bring-up
mechanism, not the general state-machine reclamation protocol above.

Main and private kernel changes compile in the private workspace. Native
exit/deferred-loan/quota regressions remain required. No application requests
mode 1 yet, and no published hardware image contains this new kernel.

Native private QEMU fixture `x0741b__` now passes the reusable
tests/intel-gpu/native/DMA_Retention_Check scenario: eight8MiB allocations
remain pairwise disjoint across owner exit/PID reuse; failed reservations do
not consume quota; unknown mode/order reject; the64MiB cap persists across
exits; ordinary mode still allocates outside retained ranges. Desktop, Intel
RAM diagnostics and the log viewer also pass. The earlier inline fixture
`og9oqhzo` passed too. The extracted test initially missed its explicit GPR
source listing; that was fixed before `x0741b__`. This is not the deferred
CPU-loan test or proof of hardware DMA safety. GPU mode1 use remains disabled.

Validation: the Nix-hosted test enumerates all five-event sequences through
length eight (390625 length-eight sequences), including owner loss at every
stage and out-of-order confirmations. GNATprove level 2 proves the Next
postcondition, dependency contract and termination. These are local policy
checks, not a proof of native DMA safety.

## Current gap

`Process.IPC.releaseDMAAllocations` releases frame ownership and returns the
whole buddy allocation. Both immediate process teardown and the final CPU
grant return can reach it. Neither path establishes GPU quiescence. CPU loan
pins therefore cannot substitute for a device lifetime pin.

Further allocation-path review: ALLOC_DMA tags every frame with the target
process, maps it PG_USERDATA, and records the buddy block in that process's
DMA array. The current cleanup releases those tags then frees the block.
The allocator already rejects freeing higher-order blocks with outstanding
pins; adding a device pin without changing cleanup would therefore cause an
allocator exception, not implement quarantine. All cleanup paths must be
changed together. Page-table destruction frees table pages, not these DMA
leaf allocations; ordinary process frame-list reclamation is a separate path
that must also be checked when changing allocation bookkeeping.

## One allocation generation

The normal sequence is Private -> GPU reachable -> Draining -> GPU stopped
-> Reclaimable. Record GPU reachability BEFORE making a PTE, descriptor or
command address visible. Even partial publication failure then retains memory.
Draining closes new submissions/references. GPU stopped means all outstanding
access has completed and cannot restart. Finally remove GPU mappings and wait
for the relevant translation invalidations before marking reclaimable.

An unused private allocation can be retired immediately. Unexpected owner loss
after publication quarantines the allocation, even if a stop was observed.
Quarantine and reclaimable are terminal for this generation. No timeout,
process exit, CPU fence, or submission-completion notification alone is a
release certificate. A completed command does not erase the engine's ability
to fetch the still-mapped buffer again.

The pure transition model proves these local ordering rules. Hardware evidence,
serialization, cache visibility, kernel frame pins and identity validation are
integration obligations, not consequences of the proof.

## Required kernel contract

Use allocation identities including generation, bound to a device ownership
epoch, never a raw PID/address as the release key. The broker obtains the
device-scoped allocation authority; an application does not obtain arbitrary
physical mappings. Before returning publishable memory, kernel accounting must
retain a device pin independently of CPU mappings and memory grants.

Owner death must atomically close admission for that device epoch and preserve
device-pinned allocations through both existing DMA-release paths and address
space teardown. Retained records must outlive the process table entry, or keep
that entry non-reusable. Do not reset their metadata on PID reuse. Reserve the
quarantine accounting capacity before allocation succeeds; a full table fails
allocation without exposing an untracked buffer.

Initial bring-up may retain abandoned GPU allocations until reboot rather than
claiming recovery. Account them against a bounded device budget that survives
driver restarts, so restart loops cannot consume unbounded RAM. This is a
fail-closed bring-up policy, not the final normal memory-management strategy.

A later reset/isolation broker may release quarantine only after authenticated
device-epoch recovery has eliminated DMA and stale translations. A GPU driver
with unrestricted MMIO and bus mastering cannot be trusted to certify its own
isolation against malicious behavior. IOMMU isolation and controlled reset/
ownership are required for that stronger security claim; proof of this state
machine does not make arbitrary bus-mastering drivers safe.

## Integration tests required before submission

Native4GiB QEMU `ya3c1fm0` passes constrained versions of the eight retained
allocations and ordinary allocation. Before each retained8MiB request,
ceilings1/4095/8MiB-1 fail without consuming quota. Full-range/alignment checks
and deferred CPU-loan owner exit pass. This tests early impossible-size
rejection and quota exhaustion, not exhausted/fragmented address-zone search.

ALLOC_DMA arg4 now supplies an exclusive physical address ceiling; zero
preserves unconstrained allocation. Both ordinary and retained modes use the
same constraint. `BuddyAllocator.allocBelow` searches free lists under their
existing lock, admits only `base <= ceiling - requested_size`, and splits the
selected block's left prefix using existing metadata transitions. Failed
placement does not consume the retained budget. The ordinary fast path is
unchanged; the constrained slow path can scan many nodes with the lock held
and has not been latency-profiled. Physical overlays remain outside SPARK;
this addition is not a claim that native allocator integration is fully proved.

Native QEMU4GiB run `v0ee3vry` passes the formerly failing firmware-buffer
case: physical2097152000 fits below4GiB, complete copy/padding readback and
Desktop/logviewer pass. This is one allocation on the real kernel path, not
exhaustive coverage of fragmentation, exhaustion, free/reallocate or races.

Firmware-buffer integration now requests a fixed1MiB retained allocation via
authenticated supervisor request022C. Caller-controlled size/address fields
are not accepted. The driver copies the admitted firmware and zeroes/readbacks
the entire tail, but does not publish GPU PTEs or claim DMA coherence. Native
4GiB QEMU run050ywr9x returned physical6343884800 and correctly rejected it
under the initial below4GiB policy. This exposes a missing allocator placement
constraint, not an excuse to truncate the address or silently widen policy.
A lower-memory VM can test CPU copy plumbing; it cannot establish general
allocation success. Constrained DMA allocation or separately justified wider
hardware address admission remains required before relying on this on NUC.

Native QEMU regression `cubit-usb-live.xiopwdp2` passed the deferred CPU-loan
case: a child grants its retained DMA page, the supervisor acquires and checks
a sentinel, kills the owner, observes that another child cannot reuse its PID,
then returns the acquisition. Subsequent children reuse that PID but never
the retained physical range. The full eight-allocation/quota/ordinary-mode
test and Desktop/Intel diagnostic fixtures pass. This exercises the final
CPU-acquisition cleanup route, not GPU quiescence or concurrent interleavings.
The first test failed because events intentionally have NO_PROCESS senders;
the fixture now responds through its granted endpoint using capSubmit and
the supervisor checks the authenticated caller. Kernel policy was not relaxed.

- Kill the driver after allocation, after publication, during drain and after
  stop; verify physical pages are not reissued in every exposed case.
- Exercise both immediate teardown and deferred final CPU-grant return.
- Reject stale allocation generations, stale device epochs and other senders.
- Race owner exit against publication/release under the authoritative lock.
- Exhaust quarantine capacity and restart budget; fail before publication.
- Test normal drain, mapping invalidation completion and exactly-once release.
- Establish CPU-to-GPU memory visibility separately from lifetime handling.

Shared kernel edits require coordination with the networking/thread work.
The private Intel snapshot must implement and test this boundary before any
firmware DMA or command submission is enabled.
