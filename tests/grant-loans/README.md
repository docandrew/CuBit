# Bounded derived-loan lifetime core

`Memory_Grants.Loans` is a pure SPARK state machine for one forwarding scope
bound to a generation-checked parent grant. This is a **foundation, not a live
derived-grant syscall**. The parent grant lifetime and receiver teardown now
understand a separate kernel forwarding hold; no native caller creates one yet.
Grant permissions and display mapping behavior remain unchanged. The desktop
still uses its copying path.

```
nix develop -c make -C kernel test-grant-loans prove-grant-loans
```

The first supported shape is an explicitly forwardable driver -> display loan,
with terminal display -> compositor children. A child cannot itself authorize
another forwarding scope. This deliberately does not claim a general recursive
grant graph. The rules can be extended later without relaxing ordinary grants'
prohibition on treating borrowed addresses as owned memory.

## Security and state

The parent owner chooses `No_Forwarding` or `Forward_Once`. This will have to be
kernel-stored authority; a borrower's unchecked syscall field cannot supply it.
Read access is not permission to delegate a live mapping. This restriction does
not prevent copying data that a process already has authority to read.

One dedicated kernel parent hold is retained for the whole scope. Child loans
are nonempty page subranges and cannot add write permission. There are at most
16 children; reservation counters never wrap. References bind parent slot and
generation, child slot and reservation sequence. Rejected operations preserve
the complete state. A scope is configured once, cannot reopen after retirement,
and must never be reset or recreated under the same parent grant identity.

Child phases:

```
Mapping -> Available -> Draining -> Unmapping -> Absent
   |            |                       ^
   +------------+--- revoke, no readers -+
```

Reserve before mapping so partial mapping failures also retain the parent.
Publish only after all mappings succeed. Acquisitions are admitted only while
available. Revocation closes admission; the last reader return allows unmapping,
but does not itself retire the mapping or release the parent. Mapping rollback
also needs explicit retirement confirmation, even if zero pages were installed.

Parent close stops every new reservation, publication and acquisition. It does
not release memory, erase readers or declare DMA quiescent. Only after every
child is unmapped/retired may `Release_Parent` emit its one-shot release
obligation. Ordinary reader returns cannot consume that parent hold.

The private Ghost invariant ties every live child to a held parent, enforces
range/access attenuation, nonzero bounded reservation identity, zero readers
while mapping/unmapping, and positive readers while draining. Runtime branches
handle real request rejection; Ghost invariants/contracts are proof scaffolding,
not a requirement to turn on kernel runtime assertions.

## Parent hold implementation (2026-09-21)

`Memory_Grants.Lifecycle`, the actual parent state used in the kernel, now
distinguishes ordinary acquisitions from one dedicated forwarding hold.
`Retain_Forwarding_Hold` is one-shot per initialized grant lifetime and only
works while available. The separate `Release_Forwarding_Hold` is a kernel
operation, not a userspace syscall; ordinary `Record_Return` cannot reach it.
Retaining does not consume any of the 127 user-acquisition slots. Owner-selected
forwarding permission and scope/identity authentication are still adapter work.

`Force_Close` was removed, not kept as a compatibility alias. Its replacement,
`Close_Receiver`, clears a dead receiver's ordinary acquisitions but preserves
the forwarding hold and pending revocation. Native receiver teardown unmaps
and acknowledges shootdown while page tables still exist, then sets installed
page count to zero. It invalidates the grant identity only if no hold remains.
A retained identity cannot be acquired or reused; later retirement of its
zero-page mapping must not touch the dead receiver's page tables.

**Frame pins and a forwarding hold are different.** Each derived mapping will
need its own pins on root-owned physical frames. The parent hold preserves
grant identity and deferred owner resources (notably whole DMA allocations),
not a dead intermediary's virtual address space. Before a native forwarding
syscall is enabled, scope closure must be integrated with parent revocation
and receiver exit, and each child mapping must get independent pins. The new
held-receiver branch is not reachable through today's native grant syscalls.

Validated: 6,084,701 additional hosted checks against an independent model,
including all seven-event histories from available/inactive states, every
ordinary-reader count through 127, duplicate returns/releases, receiver exit,
and parent/child model composition through acknowledged-retirement boundaries.
The combined proof gate passes 197 diagnostics, checking that every new hold
operation is actually covered, with no skips or assumptions. This proves the
pure state transitions, not the native unmap/shootdown implementation.

The native kernel builds without enabling runtime assertions. The state fields
are grouped to use existing padding: GNAT reports an 8-byte `Lifecycle` on this
target, the same as before the forwarding hold. Four-vCPU KVM regressions passed
for `display-grants-virtio-vga` and `desktop-protocol`, including dead-client
buffer release. Those exercise existing native grants, not yet a live forwarded
child. The final-layout one-vCPU loaded virtio input fixture also passed integrity;
latency remains above target. Measurements are in the ignored local directory
`tests/performance/results/forwarding-holds/`, separate from the earlier GPU
wakeup experiment. No pixel copies were removed by this lifetime change.

## Integration obligations before exposing a syscall

1. Authenticate the caller, parent reference and intended recipient through the
   existing endpoint/reply authority rules, checking process generations and
   closing state under the existing lock/lifetime discipline. The pure model
   accepts already-authorized inputs; it does not prove identity or capability
   enforcement in the kernel adapter.
2. Add explicit owner-controlled forwarding metadata with a deny-by-default
   value. Derived grants are terminal. Reject creation from a parent pending
   revocation, even if an existing acquisition still keeps its mapping alive.
3. Retain a dedicated, **non-user-returnable** parent hold. Do not mix
   scope holds into the existing scalar count in a way that ordinary
   `returnGrant` calls can drain them. Do not rely on a caller promising to keep
   its normal acquisition. The parent bookkeeping exists now; the scope adapter
   must acquire/release it exactly once and enforce owner-controlled permission.
4. Reserve under serialization, install mappings with attenuated permissions,
   and publish only on success. Partial failure must remove installed mappings
   and acknowledge TLB shootdown before retirement. Pixel memory is shared;
   the state machine introduces no pixel copying.
5. Parent revocation/owner exit must close associated scopes. Receiver exit
   must retire children before releasing their ancestor hold. The existing
   `revokeAllGrantsTo` now preserves the parent hold, but it does not yet close
   associated child scopes (none exist natively). Child mapping pins must
   survive removal of a dead intermediary's mappings. Do not wait for readers
   while holding the global grant spinlock.
6. Invoke `Finish_Retirement` only after real unmap/shootdown evidence. Release
   the parent acquisition only after all children retire; commit the model's
   release transition only when the real return succeeds. The model's output
   is an obligation, not evidence that the kernel performed it.
7. Device work needs driver-owned completion/quiescence evidence in addition to
   CPU mapping retirement. Process death and a userspace message cannot prove
   a GPU has stopped reading. Quarantine uncertain storage.
8. Add native three-process tests: actual aliasing with no data copy, denied
   forwarding/escalation, partial-map rollback, parent revocation, borrower
   and intermediate-service death, slot/PID reuse and SMP shootdown. Only then
   enable the display/compositor buffer path.

## Tests and proof boundary

### Multi-output composition tests

`output_lifetime_test` combines the production grant/hold model with a concrete
instance of the production `CuBit.Presentation_State` model. This is a hosted
test of their intended composition, not a new display subsystem or native
multi-output implementation.

In six scenarios, output A stops while queued, being read, or held by scanout;
output B completes 128 further frames. Mappings cover either separate pages of
one arena or overlapping read-only pages. Closing A cannot close B or consume
the parent hold; A's reader survives until the test supplies explicit final-use
evidence. Delayed duplicate returns are rejected. Reusing A's loan slot does not
make its stale reference valid. Closing the whole parent subsequently prevents
new work on B and waits for all mappings to retire.

The test explicitly shows that two sessions can both have frame ID 1. Output,
service incarnation and session authentication must precede frame dispatch in
the future native adapter. The generic frame state alone is not an identity or
authorization check. Likewise the test's backend-quiescence and unmap events
are simulated observations; real unplugging must never synthesize them.

Validated in Nix on 2026-09-21: **14,926 additional hosted checks**, with the
existing 4,532,184 loan checks and 6,084,701 parent checks still passing. The
combined component proof gate now passes **200 diagnostics**, including actual
postconditions for the concrete presentation instance. The cross-component
scenario is regression-tested, not itself SPARK-proved. No scheduling, refresh,
pixel correctness, DMA or latency guarantee follows from this hosted test.

Native `display-discovery-multi-output` was rerun on KVM successfully: CuBit
discovers 1024x768, 1920x1080 and portrait 1080x1920 virtio outputs. The last two
remain `Detected_Only`; the fixture does **not** show a desktop on all outputs.

### Original loan core

Initial loan-only validation on 2026-09-21 in Nix: **4,532,184 hosted checks** and **195 proof
diagnostics** across the loan instances and existing parent grant policy, with
no assumptions, skips or unproved obligations. The generic also compiles against
the freestanding kernel runtime with `-gnatp` and without `-gnata`. This is
compiler compatibility evidence, not a native syscall or QEMU integration test.

Hosted tests check permission combinations, boundary ranges up to 4096 pages,
16-child capacity, all 127 reader counts, stale handles, foreign parent slots
and generations, sequence exhaustion, reset rejection, mapping rollback and
every seven-event sequence of publish/acquire/return/revoke/unmap/close/release
against an independently represented reader/boolean model.

The generic is proved for both its production sequence bound and a three-entry
sequence budget to exercise exhaustion. `check-proof.py` rejects missing proof
obligations, assumptions, skips and unproved checks. Proven properties concern
this serialized state machine, **not** live mapping, TLBs, DMA, scheduling,
process identity, a recursive grant graph or the future native integration.
