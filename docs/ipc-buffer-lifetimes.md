# Shared IPC buffer lifetimes

Status: existing kernel enforcement audited; desktop client-buffer migration
implemented, 2026-09-09. This is one grant mechanism, not a display-specific
authority system. See [storage](storage-io.md), [audio](audio-zero-copy.md), and
[the desktop protocol](desktop-protocol.md).

## Identity, authority, lifetime, contents

These are separate questions:

1. **Identity:** `(slot, generation)` identifies a particular grant incarnation.
   `CuBit.Grant_References` is the shared pure userspace representation; the
   existing `CuBit.Memory_Grants` API aliases it. No address is encoded in it.
2. **Authority:** the kernel checks the current grantee, expected owner, rights,
   generation and range. A server uses the kernel-authenticated request sender
   as expected owner. Capability-directed acquisition resolves service identity
   through the endpoint instead of accepting an advertised PID as evidence.
3. **Lifetime:** an accepted acquisition prevents grant retirement until return.
4. **Contents:** acquisition does not freeze bytes. A read-only receiver mapping
   does not remove the sender's writable mapping or other authorized aliases.

CCL ownership types help cooperative programs obey these rules; kernel/service
enforcement must still protect against hostile native code.

## Existing kernel mechanism

`Process.IPC` serializes grant creation, acquisition, return and revocation using
the grant lock. Under that lock, acquisition checks identity, receiver, owner,
write permission and nonempty byte range before recording an acquisition.
Range checks use subtraction after bounding the offset to avoid overflow.

Each published mapping already owns physical-frame pins, even before acquisition.
Acquisition governs deferred revocation; it does not first create those pins.
Retirement removes mappings, waits for acknowledged TLB invalidation, then drops
the pins. The pure `Memory_Grants` model does not prove those machine operations.

Revocation with no acquisitions or forwarding hold retires immediately. Otherwise
it becomes pending, rejects new acquisitions and waits for both forms of use to
end. Generation exhaustion retires a slot instead of wrapping its identity.
Owner teardown retains backing storage and PID identity until outstanding loans
finish. Grantee teardown closes its ordinary acquisitions and retires its mappings
before page-table disposal, without consuming a separate kernel forwarding hold.

The forwarding hold is one-shot and independent of user returns. Its state core
is implemented/proved, and receiver teardown preserves a held parent record
after unmapping (installed page count becomes zero). The native adapter now
connects child mappings, independent frame pins, owner permission checks and
scope-close cascades. `Derive_Via_Capability` uses syscall 122 with recipient
endpoint slot, parent slot/generation, page offset/count and a 0/1 write flag.
Success returns the canonical packed child reference atomically; failure returns
U64'Last. The child is terminal. Recipient generation is rechecked under the
grant lock; received memory still cannot be re-granted through ordinary creation.
The three-party `grant-forward` fixture exercises this native path separately
from the pure SPARK policy tests. This does not establish GPU DMA quiescence.
Companion native exit fixtures also retain a child while the intermediary or
original owner exits. Both passed four-CPU QEMU on 2026-09-30: admission closes,
the retained mapping remains readable, and returning it retires the child.
The intermediary-exit fixture additionally confirms root retirement through
the surviving owner. These do not test later owner PID or physical-frame reuse.
See [derived-loan implementation and proof boundaries](../tests/grant-loans/README.md).

The focused kernel SPARK gate passed 116 checks on 2026-09-09. This is evidence
for the analyzed model/contracts, not a proof of the lock, page tables, allocator,
TLB implementation, hardware DMA quiescence or the whole IPC system.

## Desktop adoption

The compositor now acquires a generation-bearing BGRA buffer for its checked
byte extent. It retains that acquisition across frames, and returns it on
replacement, destruction, session goodbye, close or dead-client reaping.
Replacement acquires the candidate before returning the previous loan, including
when both refer to the same grant. Rejected replacement preserves the old buffer.

There is no extra acquisition syscall per frame, pixel copy, polling loop, or
new service hop introduced by this migration. Returning an acquisition may
complete a pending revoke and consequently require a TLB shootdown.

Current drawing is single-threaded and copies client pixels into compositor-owned
storage. No deferred renderer retains a client pointer beyond that synchronous
copy. A threaded renderer, DMA or direct scanout must retain its own obligation
until actual last use; retaining a raw copied surface record is not sufficient.

Client exit is no longer a check-before-use race: the acquisition keeps memory
mapped even if the owner dies after `processAlive`. Reaping currently occurs
during scene composition; idle dead-client cleanup is not time-bounded. An
authenticated process-death notification is follow-up work, not justification
for forced release while a consumer can still access memory.

The shared toolkit revokes old grants after successful replacement, and its
current grant on close. Its `sbrk` allocation strategy still cannot free prior
pixel allocations on resize: this change fixes grant-slot leakage, not that
separate heap-allocation limitation.

## Common asynchronous contract: next slice, not yet implemented

Keep request progress separate from grant lifetime:

- **Accepted:** validation/admission succeeded; this is not completion.
- **Buffer released:** this operation will no longer access the indicated loan.
  Other operations may still hold acquisitions of that grant.
- **Completed:** the service-specific result is known. Presentation, durable
  storage and audio playback each have different completion semantics.
- **Cancellation requested:** not completion or permission to reuse memory.
  Some operations cannot be cancelled. Cancellation must drain any actual use
  before releasing its loan.

Specify bounded queues, generation-bearing operation identities, exactly-once
terminal accounting, completion-overflow recovery and explicit backpressure.
Do not wait for a batch to fill. Prove the small state machine one property at
a time before changing the transport or adding display presentation fences.

## Remaining migration and enforcement work

- Application-to-desktop and desktop-to-display attachments now use checked
  acquisitions. The display-to-GPU scanout mapping remains numeric-grant/address
  arithmetic; see [display buffer lifetimes](display-buffer-lifetimes.md).
- `CuBit.Streams` still exposes legacy grant-base subscription state. Migrate it
  and remaining callers to authoritative generation-checked acquisition.
- Derived subrange loans must retain parent lifetime and never amplify rights;
  ordinary regranting of received pages remains rejected today.
- Pinned-memory quotas and bounded teardown/resource retention remain needed.
- Mutable pixel streams may tear. Untrusted command descriptors must be copied
  into bounded private storage before validation/use, or be enforceably immutable.
- CPU mapping lifetime is not DMA isolation; IOMMU domains and device quiescence
  are separate requirements.

## Validation

Run proofs and portable tests in Nix:

```sh
nix develop -c make -C kernel prove-memory-grants
nix develop -c make -C kernel test-desktop-protocol prove-desktop-protocol
```

Both proof gates fail on unproved checks and use two workers. Hosted tests use
assertions; production does not require runtime assertions for these contracts.
No `pragma Assume` or SPARK-Off escape was added.

The extended `desktop-protocol` QEMU test passed replacement balancing,
malformed/short/stale grant rejection, pending revocation, release on destruction,
and owner exit followed by reaping. Workbench, Files, DOOM and storage-grant
regressions passed after migration. These checks do not measure the latency goal.
