# Process retirement and mailbox ownership

Status: implemented with a proved lifetime state machine and focused host/QEMU
regressions. This is **not** an end-to-end proof of kernel memory safety,
concurrent queue correctness, or bounded retirement latency.

## Invariant

A stop request closes admission; it does not free memory. A process's stack,
address space and PID remain retained until its execution has stopped and one
reaper has exclusively claimed cleanup. Outstanding grant borrowers can retain
backing memory and the owner PID beyond that cleanup.

Scheduling state is not execution presence. In particular, a process can publish
RECEIVING or WAITINGFORREPLY while still executing kernel instructions on its
stack. Neither a waiting state nor INVALID is sufficient evidence of quiescence.

## State and handoff

`Process_Lifetime` is a pure SPARK package with a private, single-word state:

| State | Executing | Closing | Can claim cleanup |
| --- | --- | --- | --- |
| Live_Stopped | No | No | No |
| Live_Running | Yes | No | No |
| Closing_Running | Yes | Yes | No |
| Closing_Stopped | No | Yes | Yes |
| Reap_Claimed | No | Yes | No |
| Fully_Retired | No | Yes | No |

Only fresh process construction resets this state. All live transitions take
`Process.lock`. The PCB component is atomic for closing hints; those hints do not
authorize reclamation. ELF construction is unpublished until `publish` opens
the mailbox and admits the process together, in mailbox-before-process order.

The scheduler records entry before dispatch and acknowledges departure only
after switching to its own stack and kernel page tables. Direct IPC switching
records the transition under the process lock, which remains held across the
assembly switch. The lock cannot be released on the new context until the old
stack has stopped executing. A closing process is never dispatched again.

Forced termination broadcasts a reschedule request. The executing context leaves
at a scheduler handoff, completed syscall, or user interrupt-return boundary.
The requester does not wait under a lock for another CPU. This relies on the
existing interrupt-exclusion/handoff adapter and its audited safe points; SPARK
does not model the assembly, APIC, interrupt delivery, or x86 memory ordering.

## Cleanup worker

A dedicated kernel worker on CPU 0 sleeps when no work is eligible. Stop requests
and CPU departure acknowledgements wake it under the same process lock used to
publish its sleep, avoiding a polling loop and a lost-wakeup window.

The worker claims a stopped victim and removes ready/sleep queue membership
under `Process.lock`. Delta-queue removal preserves successor deadlines. It then
releases that lock and visits mailboxes individually in mailbox-before-process
order. This detaches blocked senders, removes queued references to the retiring
sender, completes outstanding requests, and clears victim-owned mailbox state.
Messages carrying an old sender PID cannot survive until that PID is reused and
cause a receiver to mint a reply capability for the new occupant.

Grant revocation, frame/page-table reclamation and kernel-stack release run on
the worker's own stack, outside the global process critical section. Existing
grant locks, mapping pins and synchronous TLB acknowledgements still govern
shared memory. Final state and bookkeeping are written before immediate or
grant-deferred PID publication. There are no PCB writes after publishing its PID
free. Child-exit events are bound to the parent's saved generation.

`SYSCALL_KILL` success now means **stop accepted**, not cleanup completed. Exit
notifications remain best-effort bounded events, not a reliable join protocol.
They follow cleanup; they need not wait for all grant borrowers to return.

## Mailbox ownership

The owner's mailbox lock protects its message ring, pending requests, completion
queue and admission flag. Async submission changes two owners: it acquires the
distinct sender/destination mailboxes in ascending PID order, then the process
lock if needed for wakeup. Reclamation never acquires a mailbox while holding
the process lock. Reply authority validation, pending removal and completion
publication share the destination mailbox critical section.

Every accepted completion-bearing submission reserves space:

`queued completions + pending requests <= COMPLETION_QUEUE_SIZE`

Reply and target-death paths exchange a pending reservation for a queued
completion. Polling releases a reservation. Overcommit is rejected at submit,
not by silently dropping a completion later. This accounting is regression
tested, not yet isolated into a proved SPARK ADT.

Capability-based sends, submissions, events and memory grants carry the checked
PID generation into final admission. Reply generation checks occur under the
target mailbox lock. PID-addressed grant authorization also checks generations
in both forward and reverse endpoint paths.

The old fused reply-and-wait implementation retained an unlocked target. It now
composes the common checked reply and receive paths. Standalone reply retains
its same-CPU direct handoff; the fused optimization can be reintroduced once its
two-mailbox lifetime/queue protocol is specified and tested.

## Administrative boundaries and limitations

Map-into, capability inspection/minting, resume, CPU assignment and IRQ/service
registration serialize target use against mailbox closure. CPU reassignment
currently requires a stopped, suspended target: changing its home CPU while
running or queued is not a safe migration protocol. Bootstrap does legitimately
mint endpoints into already-started services, so minting preserves that existing
workflow. The mailbox lock prevents lifetime races with retirement; it does not
make every concurrent capability-table reader/writer coherent. A table-wide
publication/replacement protocol remains a separate soundness obligation.

Shared-address-space thread creation had no callers and no complete shared
ownership/teardown protocol. `Process.create(thread => True)` now explicitly
rejects it. Supporting threads requires address-space and shared-mailbox lifetime
accounting; per-process execution presence alone would be insufficient.

Remaining work includes the full intrusive-queue/state ownership proof, coherent
cross-process diagnostic snapshots, globally serialized IRQ/service registries,
DMA controller quiescence before driver memory reclamation, and tests that force
remote user execution/termination and repeated PID reuse in QEMU. A four-CPU
boot does not itself exercise remote killing: the benchmark pair runs on CPU 0.
Neither this change nor the five-second TLB fail-stop policy establishes a
hard-real-time or 1 ms p99 guarantee.

## Verification

```sh
nix develop -c make -C kernel prove-process-lifetime test-locking
nix develop -c make -C kernel cubit_kernel check-spark-boundaries
nix develop -c make -C kernel bench-ipc-client bench-ipc-server
nix develop -c tests/headless/run.sh --test bench-ipc --accel kvm --timeout 30
```

The lifetime proof discharges 30 checks, including the ghost stop/acknowledge/
claim/no-revival scenario, with no assumptions or unproved checks. CI runs it.
Kernel runtime assertions remain disabled. Host-test assertions are Linux-only.

The host regression uses the production lifetime core and spinlock with mocked
CPU/interrupt adapters for 10,000 concurrent stop/acknowledge/reap/reuse cycles.
It also tests actual queue detach operations, including delta preservation.
The QEMU benchmark tests 2,000 synchronous calls, 512 async completions, capacity
reservation/overcommit, server exit with simultaneous sync/async waiters, exactly
one target-death completion, and rejection by the retired endpoint. Its temporary
disk is refreshed with the currently staged benchmark binaries before each run.
The runner also builds the kernel before staging it, avoiding a false pass (or
repeat of an already-fixed failure) from an older kernel ELF.

Final four-CPU KVM validation passed storage-grants (40 s), capability-security
(30 s), desktop-doom (35 s), bench-ipc (30 s), and ccl-workbench-virtio-vga (25 s).
The kernel stack-usage build gate and SPARK legality check passed; the latter
still reports existing hardware-overlay warnings and is not a whole-kernel
proof. Object-symbol inspection confirms the ghost theorem is not emitted in
the production lifetime object.
