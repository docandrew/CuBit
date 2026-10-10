# Kernel locking: ownership, ordering, and latency

Status: first implementation/audit pass; **not a proof of whole-kernel deadlock
freedom, scheduler correctness, or bounded interrupt latency**.
Date: 2026-09-06.

## Invariants

1. A published lock has one atomic owner word: either unowned or one logical
   CPU. Only an unowned lock can be acquired; only its owner can release it.
2. Local maskable interrupts remain disabled throughout acquisition, waiting,
   and ownership. Nested exclusion preserves the outer restoration policy.
3. Ordinary critical sections never sleep, schedule, or acquire locks in reverse
   dependency order. Masking interrupts does not exclude other CPUs or NMIs.
4. The sole scheduling exception is the documented transfer of exactly one
   CPU-owned `Process.lock`, with IF clear and exclusion depth one.
5. A process's blocked-state publication, queue membership transition, and
   saving of its running context must be serialized against wakeup.
6. Removing a mapping is not sufficient to recycle memory: all required remote
   TLB acknowledgments must precede dropping its lifetime pin.

The security implication is direct: memory and capability lifetime depend on
these protocols. Atomic lock acquisition by itself cannot make a caller's
unprotected observations or premature frees safe.

## Primitive and proof boundary

`Locks` is a pure SPARK ownership-policy package. Its private, 32-bit `State`
encodes unowned or a CPU from the configured bounded CPU subtype. `Acquire`
returns an enumerated acquired/contended/reentrant result; `Release` succeeds
only for the owner. Failed operations preserve the state.

`Spinlocks` uses these actual production transitions on a local snapshot and
commits a successful proposal using locked x86 compare/exchange directly on its
private atomic record component. There is no scalar in-out assembly wrapper
that might operate on a copy instead of the shared word. Availability and owner
identity are published together, replacing the old separate flag and CPU fields.

Contended waiters repeatedly load the atomic word, service pending TLB
acknowledgments, and execute `PAUSE`. They attempt another locked RMW only after
observing an unowned word. Release also uses compare/exchange. Compiler memory
clobbers and the locked instruction supply ordering at this trusted boundary.
No fairness, starvation, or wall-clock acquisition bound is claimed.

The lock representation is private. Initialize only before publication; never
copy/reinitialize a live lock. Static default initialization and the boot setup
procedures do not require a heap, secondary-stack return allocation, or a new
kernel runtime facility.

The old `not isLocked(S)` release postcondition was removed: another CPU can
legitimately acquire the lock before the releasing procedure returns.
`isLocked` and `ownedBy` are observations, not stable ownership tokens.
An owner-only operation's safety depends on the surrounding exclusion protocol,
not on a previously sampled Boolean.

### Proved

`make -C kernel prove-lock-ownership` verifies the production `Locks` policy:

- acquiring an unowned state assigns it to the requesting CPU;
- a different CPU cannot acquire or release an owned state;
- recursive acquisition is rejected without changing ownership;
- only the owner can transition the state back to unowned.

The focused report has 16 checks: 9 flow and 7 prover checks, none unproved,
no warnings, no `pragma Assume`. The ghost exclusive-owner theorem calls the
actual transition implementations. `Interrupt_State` separately proves nested
restoration and scheduler-handoff policy independence (35 checks).

These are sequential state and functional proofs. Atomic instruction semantics,
the adapter's compare/exchange loop, hardware IF/GS, scheduling, and the complete
lock-order graph remain trusted/review obligations. In particular this does
**not** prove that concurrent acquisition terminates or all clients obey the
ownership discipline.

Kernel compilation still suppresses runtime checks; `-gnata` is not enabled.
Assertions/contracts are discharged statically in proved code. Explicit fatal
branches for recursive acquisition or foreign release are executable boundary
decisions, not runtime evaluation of SPARK contracts.

## Acquisition dependencies

### Owned anonymous memory

`Process.Owned_Memory` serializes its bounded allocation inventory with a
registry lock. Allocation takes registry -> owner address-space -> allocator
locks. Release takes grant -> registry -> owner address-space, then performs
PTE removal and acknowledged TLB shootdown before detaching/freeing frame-list
nodes. Source grant lookup and pin acquisition take grant -> source
address-space; they drop the source lock before acquiring a receiver lock.
This is an implementation review boundary, not a whole-kernel SPARK proof.

Legacy physical mapping admission holds the registry lock through mapping
publication. It rejects aliases to retained owned frames. This does not revoke
preexisting raw physical mappings; holders of arbitrary physical mapping
authority remain trusted. Process reclamation holds the registry lock across
freeing its frame list and forgetting its owned descriptors, after execution
has quiesced. No owned release may free backing before remote TLB acknowledgment;
grant-pinned frames additionally remain in the allocator's deferred-free state
until the last grant pin is returned.

This inventory covers the kernel `Spinlocks` call sites inspected in this pass.
It is a reviewed dependency map, not a mechanically proved acyclic call graph.
Transitive dependencies also apply.

| Outer lock | Nested locks / work observed |
| --- | --- |
| Mailbox (`mailtab(pid).lock`) | Process table, its send/receive queues; IPC sleep wake takes process then sleep queue |
| Process table | Ready/sleep/send/receive queue locks; grants; PID tracker; memory cleanup through slabs/buddy |
| Grant table | TLB round serialization; buddy frame ownership/pins; DMA registry enqueue; reportLock then PID bitmap for deferred PID retirement |
| DMA registry (Process.DMA.Registry_Lock) | May be taken under ordered mailbox locks, Process.lock, grantLock or addressSpaceLock; takes the buddy lock for metadata and ownership tags. Never takes those outer locks. Cleanup is bounded to 64 tags per step and runs outside grantLock |
| Memory_Accounting ledger | Below process/mailbox/grant/address-space/DMA-registry locks. Serializes original-owner quotas and global retained charges together. Never holds ledger across buddy allocation/free; preparation and deferred metadata frees run after unlock |
| DMA mapping publication | Ordered caller/target mailbox locks precede addressSpaceLock; metadata commit then takes the DMA registry lock. This pins the target incarnation, but current allocation/mapping loops still extend the interrupt-off interval |
| Slab pool | Buddy allocation when expanding the pool |
| TLB round | No further spinlock acquisition in request/acknowledgment service |
| Individual process queue | No further spinlock acquisition; release before moving to another queue |
| Buddy allocator / PID bitmap | Leaf locking in the inspected allocation/bitmap paths |
| Kernel notices (2026-10-07, docs/ipc-delivery.md) | `reportLock` (exit and fault reports) and `controlLock` (control messages) are leaves; a PID a report read releases is freed after `reportLock` is dropped. Grant notices live under the grant-table lock. Receive paths take notices while holding the mailbox lock (mailbox, then grant, then report or control). Producers set a notice under its own lock, release it, then take the recipient's mailbox lock to ring its doorbell, so no notice path holds the grant lock while taking a mailbox lock |
| Console output (`TextIO` output lock) | Leaf: held for one string print or `println`, including each already-copied chunk of a user `SYSCALL_WRITE`. Enabled by `Process.setup`; unlocked in early boot, after a panic, and for nested prints on the owning CPU |

Buddy charge completion (2026-10-07): charge identities are separate from
access-owner PIDs. `freeFrame` keeps the charge on deferred/pinned frames;
`unpinFrame` detaches it only at actual reclamation. Both paths, and whole-block
`free`, invoke the once-installed refund handler only AFTER dropping the buddy
lock. A handler must not raise or reacquire an outer process/mailbox/grant lock:
its caller may still hold any of those. The ledger sits below those outer locks
and never holds its lock across buddy operations. Process.setup installs the
handler before creating accounts. Ordinary pages, ordinary/retained DMA and
owner-slab metadata reserve against the same original-owner quota; retained
DMA also reserves the global ceiling in that transaction. Quota adoption at
RESUME rejects an already-overcharged child, preserving zero-as-unlimited.
The original charge ledger outlives owner death and PID reuse.

DMA registry metadata uses reclaimable owner arenas and packed 64-record slabs.
Registry operations may enter the ledger, release it, then allocate/bind/free
through Buddy; physical frees call the ledger only after the Buddy lock drops.
Sparse frame-charge metadata and account-store caches remain separately globally
budgeted, not fully attributed to individual owners. No whole-allocation GPU
release interface exists yet: retained orphan backing and its live records stay
charged. A slice acknowledgement cannot trigger a physical refund.

Never acquire a mailbox lock while holding `Process.lock`. Async submission
acquires two distinct mailboxes in ascending PID order (self-submission takes
one), then the process lock if needed. Request-ID commitment occurs inside this
same critical section. PID-based reply selection also acquires both mailboxes
in order, but releases the caller's lock before entering the target-locked
completion/handoff path. Explicit saved-reply retirement locks its caller's
cspace, releases that lock, and only then locks the reply target. Reply saves
use the caller mailbox lock too, serializing with authorized policy edits.
See [request/reply lifetimes](ipc-request-lifetimes.md). 

NMI handling and `TLB_Shootdown.Service` must not take locks, allocate, or enter
the scheduler. A CPU spinning with IF clear services a shootdown itself, avoiding
the cycle where the requester holds a lock needed by a CPU that cannot receive
the maskable acknowledgment IPI.

The old four-line mailbox/process/ready/sleep hierarchy omitted grants,
allocators, PID retirement, and the shootdown dependency. The timer path also
formerly nested a ready-list lock under the sleep-list lock. Queue locks are now
leaves in the sleep/wakeup paths.

## Concrete repairs in this pass

### Sleep and wakeup

`Process.sleep` now acquires process then sleep-queue locks, publishes SLEEPING
and inserts the process, releases the sleep lock, and enters the scheduler
without releasing `Process.lock`. The resumed context releases that lock.

Previously it published SLEEPING under only the sleep lock, released that lock,
then called `yield` to obtain the process lock. A timer or remote IPC wake could
change the process state/queue membership in this gap before the running context
was saved.

Both timer wakeup and IPC's `wakeFromSleep` now hold `Process.lock` while
checking/removing a sleeper and making it ready. They release the sleep queue
lock before acquiring a ready queue lock. Duplicate wake attempts do not enqueue
the same already-awake sleeper again. An async-submit receiver wake also obtains
`Process.lock` around dequeue/ready.

These changes do not complete the audit of all other IPC blocked-state
publication and fast-reply paths.

### Process queue structure

The native regression caught a missing `q.tail := pid` in `enqueue`. After the
first append, another append used the old tail and could overwrite the link to
a waiter. The tail update is now part of the same locked mutation.

`popFront` and `popBack` now select and remove their endpoint under one lock
acquisition. Previously selection happened before acquiring the removal lock.
The removed endpoint's links are cleared consistently. These public endpoint
helpers currently have no live kernel callers; the enqueue repair affects live
IPC queues.

## Measurements and tests

The existing optional trace facility now includes `lock_wait_tsc` and
`lock_hold_tsc` histograms. Normal boots keep tracing disabled; no timestamp
reads occur in the lock path when disabled. The enabled path uses per-CPU
histograms, without serial output, allocation, or another lock.

Samples are diagnostic raw TSC intervals, include instrumentation overhead, and
are not serialized-instruction WCET measurements. Nested hold durations overlap.
A scheduler handoff remains one physical lock-ownership interval on that CPU.
These two histograms do not measure the entire hardware-IF-disabled interval,
including interrupt entry/exit or masked time outside a lock.

`tests/kernel-locking` is a **Linux-hosted** test executable:

- Compiles the actual production Spinlocks/Locks implementation, including its
  compare/exchange and PAUSE instructions.
- Replaces privileged CLI/GS/TLB operations with explicit fixtures. Each host
  task has its own simulated CPU and exclusion state.
- Forces four waiters to exercise the contention/TLB-service path, then checks
  400,000 protected updates, nested locks, balanced exclusion, and foreign release.
- Compiles the actual production `Process.Queues` against a minimal PCB fixture.
  Its ready adapter requires process-lock ownership and no held sleep lock.
  Checks timer and IPC wakeup, duplicate wakeup, preserved delta delays, and
  multi-entry queue endpoints. This test failed on the missing tail update
before that production bug was corrected.

`-gnata` belongs only to these independent host-test projects. They neither
import the kernel GPR nor install their fixtures into a CuBit image. The host
test does not execute the real scheduler, process teardown, or remote TLBs.

Reproduce with Nix:

```sh
nix develop -c make -C kernel prove-lock-ownership prove-interrupt-state
nix develop -c make -C kernel test-locking test-reclamation-state
nix develop -c make -C kernel check-spark-boundaries cubit_kernel
nix develop -c tests/headless/run.sh --test bench-ipc --accel kvm --timeout 30
```

The kernel build includes the stack-usage gate. The legality check still emits
existing overlay/storage-model warnings and is not a whole-kernel proof. CI
includes the ownership proof and native adapter regression; the IPC benchmark
requires both lock histogram markers.

The final four-CPU KVM regression runs passed `bench-ipc` (30 seconds),
`ccl-workbench-virtio-vga` (25 seconds), `desktop-doom` (35 seconds), and
`storage-grants` (40 seconds, including the reclamation stress marker).
The IPC run completed 2,000 synchronous calls and 512/512 asynchronous requests,
and emitted both lock histograms. These are functional regression observations,
not a comparison against Linux or a worst-case latency bound.

## Remaining work, in order

1. **Process retirement integration coverage.** The follow-up implements
   explicit execution presence, stop/claim states and worker-stack reclamation.
   The lifetime core proves 30 checks. Extend the host concurrency and QEMU
   self-exit tests with forced remote user termination and repeated PID reuse;
   then formalize the scheduler/assembly refinement, not just the state core.
2. **Mailbox ownership proof.** Teardown now follows mailbox-before-process
   locking. Submit locks both owners in PID order; pending removal and completion
   publication share a lock, with space reserved at admission. Generation checks
   and stale queued-identity cleanup protect final publication and PID reuse.
   Prove this accounting/queue protocol against actual callers. See
   [process retirement](kernel-process-retirement.md) for implementation,
   regression coverage, asynchronous kill semantics and remaining limitations.
3. **Complete IPC state/queue ownership.** Audit the send/receive/direct-reply
   paths that observe or publish state before obtaining the process lock.
   Establish one runnable-queue membership and stable saved context per process.
   The old, uncalled `wait`/`goAhead` channel wait (which marked READY without
   enqueueing) was removed on 2026-09-24; futexes replace it (docs/threads.md).
4. **Reduce interrupt-off work safely.** Teardown, table scans, grant page walks,
   slab expansion, and synchronous all-CPU shootdowns can hold raw locks for too
   long. First retain lifetime with explicit retirement ownership, then detach
   bounded work under short locks and perform cleanup outside them. Do not
   merely unlock around objects that can then be freed/reused.
5. **Measure contention tails and full interrupt blackout.** Add per-lock-class
   attribution and full IF interval instrumentation before claiming the 1 ms
   p99 OS-added-input target. A five-second shootdown fail-stop timeout is a
   reclamation safety policy, emphatically not a latency guarantee.
6. **Stronger verification.** Prove the intrusive queue/state invariants and
   lock-order obligations against actual callers; separately justify the atomic
   hardware boundary. Debug-only lock dependency checking could supplement
   this but would not replace proofs or run in the shipping kernel by default.

No sleeping mutex, ticket lock, RCU, or new userspace synchronization ABI is
introduced by this pass. Add a new primitive only for an identified use case
after the existing ownership, lifetime, and latency constraints are understood.
