# GPU DMA retirement boundary

Status: proposed kernel integration, with a tested/SPARK transition model in
`Intel_GPU_DMA_Lifetime`. The model is NOT wired into allocation or teardown;
current native GPU service still does not publish buffers or submit GPU work.

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
