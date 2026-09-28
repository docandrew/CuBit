# Legacy PID-Directed IPC Audit

Status: migration ledger

CuBit's authority-bearing IPC API is capability-directed: a caller names a
capability-table slot, and the kernel validates the capability type, rights,
object generation, and destination. The older PID-directed entry points name a
process directly. `SYSCALL_SEND`, `SYSCALL_CALL`, and `SYSCALL_SUBMIT` bypassed
endpoint-capability mediation and have now been removed from the production ABI.

This audit is deliberately source-based. Prebuilt applications must be rebuilt
before an entry point is disabled, and the final removal milestone must also
scan linked images for the legacy syscall numbers.

## Kernel entry points

| Entry point | Current mediation | Status |
|---|---|---|
| `SYSCALL_SEND` | Removed | Kernel handler, syscall constant, and userspace wrappers removed |
| `SYSCALL_CALL` | Removed | Kernel handler, syscall constant, and userspace definition removed |
| `SYSCALL_SUBMIT` | Removed | Syscall 23, kernel handler, public IPC entry and runtime/C aliases removed; remaining callers use held endpoints and `capSubmit` |
| `SYSCALL_SEND_EVENT` | Kernel caller, any `CAP_IRQ`, or matching writable endpoint | Live compatibility path; `CAP_IRQ` currently permits delivery to any PID |
| `SYSCALL_REPLY` | Pending-reply state in IPC core | Replace public PID spelling with the kernel-minted reply capability path |

`SYSCALL_RECEIVE` is not destination-directed and does not create authority to
contact another process. Its queue-selection and typed-message behavior still
belong in the IPC review, but it is not the same ambient-destination flaw.

### Submit removal regression

The body-local enqueue helper requires the generation obtained by endpoint
resolution and is only called by `capSubmit`. Shell, logstore and procmgr find
an existing writable endpoint rather than obtaining one through PID lookup.
Missing endpoint authority now denies delivery. Native capability tests reject
retired syscall 23 and an empty-slot capability submission; the asynchronous
IPC headless test continues to pass. No grant is minted merely to preserve an
old unchecked call site's behavior.

## Live PID-spelled reply callers

NetStack is migrated: immediate replies consume the kernel-minted capability in
slot 63, deferred operations move that authority into a typed pending slot, and
completion selects the exact saved slot. A failed save does not create a pending
operation. The kernel table operation also refuses to overwrite an occupied
destination.

The remaining first-party PID-spelled reply sites are:

| Area | Callers | Migration note |
|---|---|---|
| Core services | filesystem, process manager, config, log store, desktop, device manager, network manager | Convert immediate replies to slot-63 `replyCap`; introduce typed pending records before any deferred conversion |
| Device and media services | ATA, NVMe, HDA, mixer, virtio-gpu, virtio-net | Same immediate conversion; preserve interrupt and fire-and-forget distinctions |
| Stream adapters | Ada `CuBit.Streams`, C `cubit_streams.c` | Make reply authority explicit in the stream service adapter rather than retaining only the sender PID |
| Test/support services | IPC test server, CCL test host, clock service | Convert the ordinary reply helpers; retain explicit saved-cap tests |
| Auxiliary old ATA tree | `userspace/drivers/ata` | Not part of `kernel/Makefile world`; migrate or retire with that tree |

`replyWait` users (display and the IPC benchmark server) still spell the prior
sender PID in userspace. The kernel validates and consumes matching reply
authority, but the final ABI should select the reply handle directly so multiple
saved replies to one process cannot be ambiguous.

## Live raw-submit callers

| Caller | Purpose | Required migration |
|---|---|---|
| `apps/shell`, foreground launch | Subscribe to a newly launched child's stdout stream | Make launch return an attenuated child/session endpoint handle, then use `capSubmit` |
| `apps/shell`, stream inspection | Ask an arbitrary selected PID for its stream list | Route through an explicitly authorized inspection broker; do not mint arbitrary process endpoints |
| `apps/shell`, log query | Query the registered log store | Migrated: manifest slot 23 plus `capSubmit` |
| `services/procmgr`, process event | Notify the dynamically registered log store | Hold or acquire a log-store endpoint handle and use `capSubmit` |
| `services/logstore`, producer subscribe | Subscribe to a newly announced producer | Stream registration must transfer or broker an attenuated producer endpoint handle |
| `services/logstore`, retry subscribe | Retry the producer subscription above | Same handle as the original attempt; never rediscover authority from a PID |

The log-store subscription body is presently disabled by an early return, but
the raw calls remain compiled and are included in the audit.

## Removal sequence

1. ~~Remove the unused raw synchronous send/call syscalls and their userspace
   wrappers.~~ Completed; CuBit has no legacy-binary ABI commitment.
2. Add endpoint/session handles to process launch, stream discovery, and stream
   registration protocols.
3. Convert every raw `submit` caller to `capSubmit` or an authorized broker.
4. Replace arbitrary-PID event delivery. IRQ possession authorizes handling an
   interrupt; it does not by itself authorize messaging every process.
5. Replace PID reply with `CAP_REPLY`, rebuild every first-party image, scan the
   linked artifacts, then delete the constants and kernel handlers.

The reply-capability table core now proves these narrower properties without
assumptions: save is an exact move into an empty slot; take returns the exact old
capability and clears only that slot; two consecutive takes from one slot cannot
both succeed; and ordinary derivation or authority tag minting excludes `CAP_REPLY`.
The syscall boundary and concurrent `Process.IPC` machinery remain outside this
focused proof boundary and must not be described as end-to-end proved yet.

Until steps 2–5 are complete, CuBit does not satisfy complete IPC mediation.
The remaining entry points are compatibility debt, not an alternate public IPC
model.
