# Threads

Status: the process/thread tables, work stealing, threads and futexes are
implemented (see the Status sections). Date: 2026-09-25.

This is step 1 of the plan toward running Servo on CuBit:

1. kernel threads and futexes (this document);
2. a Rust `std` target for CuBit;
3. a text engine (shaping, rasterization, Unicode);
4. Servo's standalone crates;
5. SpiderMonkey, interpreter first;
6. Servo through its embedding API, inside the existing Ada chrome.

Threads are needed by far more than Servo. Every runtime, including C, Ada
tasking and Rust `std`, needs them to use more than one CPU from one program.

## What exists today

The kernel was designed with threads in mind, but the path is switched off:

- `Process.create (thread => True)` returns `NO_PROCESS` on purpose
  (`process.adb:554-559`), and nothing ever sets `isThread`.
- `pgTable` indexes `addrtab`, and `mailtab` and `completionTab` are
  documented as shared by a process's threads (`process.ads:687-709`).
- Several paths already redirect `isThread` to `ppid`:
  - receiver selection (`IPC.getReceiver`);
  - grant owner and grantee;
  - frame ownership in `tryAddPage`;
  - reclaim skipping address-space teardown.
- The redirection is inconsistent:
  - `switchAddressSpace` ignores `pgTable`;
  - the async submit path uses the thread's own (closed) mailbox;
  - `handleAllocDma` maps into `addrtab (target)` directly;
  - page faults check the faulting entry's own ranges and quota.
- `ppid` means both "parent/supervisor" and "address-space owner".
- `TLB_Shootdown.Invalidate_All` already targets every online CPU. It is
  documented as covering threads that share an address space.

Found by the survey and fixed as part of this work:

- **FS base leaks between processes.** `boot.asm` enables `CR4.FSGSBASE`, so
  user code can `wrfsbase`, but FS base is never saved or restored.
  `KERNEL_GS_BASE` holds the placeholder `0x1337d00d` for every process. It
  is not yet confirmed whether the APs enable `FSGSBASE`.
- **The legacy `wait`/`goAhead` wakeup was broken.** It marked waiters
  READY without queuing them, and had no callers. Removed (2026-09-24);
  futexes replace it.
- **`SYSCALL_EXIT` ignores its exit code.**
- **`openDescriptors` is never freed.** Nothing touches it; it is dead state
  to remove.

## Model

Processes and threads are separate kernel objects in separate tables.

- **A process record** (process table, named by `ProcessID` as today) holds
  the address space, mailbox, completion queue, capability table, grants,
  DMA, frames, quota, heap and supervisor. `mailtab`, `completionTab` and
  `addrtab` stay keyed by `ProcessID`, and only processes get entries.
- **A thread record** (thread table, named by `ThreadID`) is small: saved
  context, kernel stack and guard page, state, queue links, scheduling fields
  (priority, last CPU, pinned, turn credit, accounting), FPU and FS state,
  per-thread IPC state (`sendMsg`, `replyMsg`, blocked state, reply
  capability, receive deadline), lifetime and generation. Its `process`
  field names the process whose address space and authority it uses.
- **Every process has at least one thread**, its main thread, created with
  it. A thread's exit is reported to its process. A process ends when its
  last thread ends, and its lifecycle events (exit, fault) go to its
  supervisor (`svpid`, normally procmgr or devmgr). There is no UNIX-style
  parent: `EVENT_CHILD_EXIT` goes to the supervisor, which for everything
  procmgr and devmgr spawn is the process `ppid` names today.
- **`ThreadID` and `ProcessID` are distinct types**, so the compiler rejects
  reading process state through a thread ID. That was the dormant design's
  failure mode: several paths ignored the `isThread` redirect. `isThread`,
  `pgTable`, `mail` and the overloaded `ppid` go away.

The scheduler, ready/sleep/futex queues, `Process_Lifetime` and context
switching use `ThreadID`. IPC, capabilities, grants and syscalls keep using
`ProcessID`, except for the per-thread IPC state.

### Identity seen by other processes

Servers see the sender's **process**, never the thread. Today
filesystem, display, desktop, tls, netstack, mixer, procmgr's ledger and the
`isAdmin` checks all key per-client state on the sender PID. With this rule
they keep working unchanged when a client uses several threads.

The receive path stamps the sending thread's process. Endpoint and process
capabilities name processes, never threads.

**Reused IDs must never inherit anything.** Services cache per-client state
(open file handles, Config permissions, display and desktop ownership,
authority ledgers) keyed on the sender. A sender that is only an ID lets a
process that reuses the ID inherit the previous owner's state. Capabilities
are already protected by generations inside the kernel; service caches are
not. So:
- the identity delivered to services is the full `(ProcessID, generation)`
  pair, a 64-bit process instance that is never reused for the life of the
  system;
- services key per-client state on it, not on the bare ID;
- a regression test reuses an ID and checks that no handle, permission or
  capability carries over.

Reply capabilities stay per thread: a reply wakes the thread that called. A
reply capability records the thread ID and that thread's generation. A thread
that exits invalidates replies owed to it, but not its siblings'.

### The process table

Today `ProcessID` is `0..255`, and every per-PID table is a static array of
full-size records, about 6.2 MB for 255 IDs:
- `proctab`: 3.2 MB, about 12.7 KB each, because each record embeds its
  capability table, grants, pending requests and DMA records;
- `completionTab`: 1.4 MB;
- `addrtab`: 1.0 MB, a 4 KB top-level page table stored inline even for
  unused slots;
- `mailtab`: 0.57 MB.

Threads would exhaust the 255 IDs quickly, and scaling these static arrays
does not work (4,096 IDs would take about 100 MB). The table layout changes
instead.

Processes and threads each get a table.

**Dynamic tables** (one generic implementation, two instances) replace the
static arrays:
- **Two-level lookup:** a directory of pages, each holding a fixed number of
  records. `Lookup (id)` is `directory (id / N) (id mod N)`: two loads, no
  hashing, no lock on the read path. Records never move, so queue links and
  in-kernel references stay valid.
- **Grows and shrinks by page.** A page is allocated from the buddy allocator
  when its ID range is first needed, and returned when all its entries are
  free. IDs are handed out lowest-first, so pages stay dense and can empty.
- **Page reclamation waits for concurrent readers.** Lookups are lock-free,
  so a generation check alone cannot protect a reader that already holds a
  pointer into a page being returned. A page removed from the directory is
  freed only after every CPU has passed a quiescent point (the scheduler loop
  or an interrupt return, with no record pointer held) since the removal.
  This is the epoch-and-acknowledge protocol the kernel already uses for TLB
  reclamation (`TLB_Reclamation`, proved). Rule: no record pointer is held
  across a quiescent point. The grace-period state machine gets a SPARK
  proof like `TLB_Reclamation`'s.
- **Generations** live in a separate compact array, a few bytes per possible
  ID, that survives page release. A stale capability, reply or grant naming
  a freed ID fails its generation check instead of touching freed memory.
  This replaces the per-record `capGeneration`.
- **Proved bookkeeping.** ID allocation, generations and page occupancy are
  pure logic, written in SPARK and proved: IDs are unique, generations are
  monotonic, and a page is freed only when empty. Mapping and unmapping pages
  is a thin, unproved adapter, like the slab.
- **ID spaces** are large (for example 65,535 each). Memory tracks live
  records. Threads are a few hundred bytes each. A low reserved range keeps today's fixed IDs (per-CPU idle
  threads, requested PIDs).
- **Page tables are allocated:** a task record holds a pointer to its
  buddy-allocated top-level page table, not an inline 4 KB table.
- **Frame-owner tags** in the buddy allocator widen from 8 to 16 bits to name
  tasks. That doubles that metadata (about 4 MB more at 16 GB of RAM) and
  touches the `Frame_Pins` and buddy metadata proofs, which are re-run.
  Frames always belong to a process, never a thread.

**Generations must not live in freeable records.** Today `create` preserves
`capGeneration` and every grant slot's generation across PID reuse, which
keeps stale capabilities and grant references from matching the next
process. Once record pages can be freed, and unused IDs read as the `Absent`
record, those values would be lost. The migration:
- **Capability generations** come from the ledger (`Object_Table.Generation_Of`).
  All 37 reads of `capGeneration` move to it. The ledger gains an
  "invalidate while reserved" operation, because teardown advances the
  generation before the ID is released (the ID stays reserved while grants
  are outstanding).
- **Grant-slot generations** are namespaced by the process generation: the
  high half of a slot's 32-bit generation is the owning process's generation,
  and the low half counts grants within that life. Every reuse of an ID
  starts in a fresh range, so no memory is needed per possible ID and the
  grant-reference ABI is unchanged. An ID whose process generation would
  exceed the high half is retired. A slot whose low half is exhausted is
  retired for that life, which `Memory_Grants.Advance_Generation` already
  does.
- **No validity check reads a generation from the `Absent` record.**

**Status (2026-09-24): the kernel's process table is an `Object_Table`.**
- Components: `Id_Ledger` (`tests/id-ledger`, 54 checks proved, including
  `Invalidate` and the generation limit), `Quiescent_Reclamation`
  (`tests/quiescent-reclamation`, 15 checks) and `Object_Table`
  (`tests/object-table`, a hosted concurrency test).
- `proctab (pid)` is a lock-free lookup. `PIDTracker` is a wrapper over the
  table.
- `generationOf` (all 33 former `capGeneration` reads) is the ledger's
  generation plus 1. Teardown invalidates the generation, and the later free
  does not advance it again.
- Grant-slot generations start at `Memory_Grants.Life_Base` for each life
  and advance with `Advance_Generation_Within`. The ceiling comes from the
  grant's own generation (`Ceiling_Of`), all proved.
- Quiescent points are the scheduler loop and the idle loop. `Reclaim` runs
  at each reaper pass, so pages emptied by one retirement are freed at a
  later one.
- IDs are still 1..255 (`ProcessID`). Page tables (`addrtab`), mailboxes
  and completion queues are still static per-PID arrays. Widening IDs,
  allocating those, and 16-bit frame-owner tags come next.

**Status (2026-09-24): the thread split.**
- **The thread table exists.** Scheduling, context, kernel stack, FPU/FS
  state, queue links, lifetime and per-thread IPC state live in `Thread`
  records (`Thread_Table`, distinct `ThreadID` type). Every process has a
  main thread, allocated and released with its PID by `PIDTracker`.
- **Ready, sleep and mailbox queues link threads.** The scheduler dequeues
  threads.
- **Conversions are explicit.** `mainThreadOf` and `processOf` are the only
  ones. Each remaining `mainThreadOf` marks a place that must become
  thread-aware before processes get more threads (IPC enqueue of the
  caller, receive deadlines, event and IRQ wakeups, retirement detaches,
  `ready`/`sleep` entry points).
- **Removed:** the dormant `isThread`/`mail` redirects, `create`'s `thread`
  parameter, `addToProctab`, and the unused `wait`/`goAhead`.
- **Remaining before a second thread can run:** done, see the next status.

**Status (2026-09-25): threads and futexes.**
- **Per-CPU current thread.** The scheduler, accounting, context save and
  restore, and every caller-side IPC path work on `getCurrentThread`.
- **Thread-addressed IPC.** Each thread has its own `replyCap`. A synchronous
  reply capability names the blocked sender thread and carries that
  thread's generation; an asynchronous one names the submitting process and
  its generation. `RingEntry.senderThread` records the caller for
  synchronous requests. Slot 63 in the ABI (`REPLY_AND_CONSUME`,
  `MOVE_REPLY_CAPABILITY`) means the calling thread's current reply. A reply
  by PID alone (`REPLY`, `REPLY_WAIT`) uses the caller's own reply
  capability first, then a deferred one; deferred replies to two threads of
  one process are ambiguous by PID and fail (use the explicit slot).
- **Wakeups.** Unsolicited work (events, IRQ doorbells, retirement events)
  wakes one blocked receiver and every thread in the mailbox's
  `notifyQueue` (threads blocked for an event or a completion); each
  rechecks its condition.
- **Thread IDs** are 1..1023. A main thread takes the number of its PID when
  free, else any free ID (non-main threads may hold low numbers).
- **Address-space lock** (see Address-space mutation).
- **`THREAD_CREATE`/`THREAD_EXIT`** with a per-process quota of 128 threads,
  a sibling list headed by the main thread, process-wide kill (every thread
  is stopped), and reaping: a process is reclaimed only when all its threads
  are off-CPU; an exited thread of a live process is reclaimed alone.
- **Futexes** (see Futexes), with proofs and an interleaving explorer in
  `tests/futex-queues`.
- **Tests:** `userspace/apps/futex-check` (headless case `futex`).

The existing slab allocator is not used for these tables:
- growth is capped at eight blocks;
- blocks are never returned;
- it has no ID lookup or generation;
- its free list lives inside freed objects, so a stale raw pointer would
  corrupt it.

A per-task thread limit remains as an ordinary resource quota (see
Authority and resources), not as a guard for the ID space.

## Syscalls

| # | Name | Effect |
| --- | --- | --- |
| 90 | `THREAD_CREATE (entry, stack, arg, fs_base, exit_word)` | New thread in the caller's process, READY on the caller's CPU, same priority. Returns its thread ID, or all ones on failure. |
| 91 | `THREAD_EXIT` | Ends the calling thread. The main thread ending ends the process. |
| 92 | `FUTEX_WAIT (addr, expected, deadline_ms)` | Block while the 32-bit `*addr = expected`, until woken or the absolute monotonic deadline (all ones: none). Returns 0 woken, 1 retry (the value differed), 2 timed out, all ones for a bad address. |
| 93 | `FUTEX_WAKE (addr, count)` | Wake up to `count` waiters on `addr`, oldest first. Returns the number woken. |

`exit_word` (0 for none) is the join mechanism: when the thread ends, the
kernel stores 0 to that 32-bit word and futex-wakes every waiter on it
(Linux's `CLONE_CHILD_CLEARTID`). A joiner waits while the word is nonzero.

Four syscalls, deliberately. Two things need no syscall:
- **FS base:** user code sets it with `WRFSBASE` and the kernel saves it per
  thread; `THREAD_CREATE` takes the initial value.
- **The thread ID:** the runtime keeps it in thread-local storage from
  `THREAD_CREATE`'s result. `GETPID` keeps returning the task.

`EXIT (code)` becomes **task** exit: every thread stops and the exit code is
delivered to the parent with `EVENT_CHILD_EXIT`. `KILL` of a task stops all its
threads. A thread cannot be killed individually from outside the task.

A fault in any thread kills the whole task. Threads share memory, so a
wild write in one thread makes every other thread's state suspect. The
supervisor is told which thread faulted.

`THREAD_CREATE` validates (as implemented):

- `entry`, `stack` and `fs_base` are nonzero user-half addresses (`fs_base`
  may be zero); `exit_word` is 4-byte aligned;
- the process's thread quota, and that the process is not exiting.

It does not check that `entry` lies in the image or that the stack is mapped:
a bad value faults the new thread, which ends the process, exactly as the
same mistake in the creating thread would.

The new thread starts with RSP = `stack`, RDI = `arg`, FS base = `fs_base`,
and a clean FPU state. A SysV function expects RSP + 8 to be 16-byte aligned
at entry; the runtime passes its stack pointer accordingly.

## Authority and resources

Thread creation and futexes are **not** guarded by capabilities.
- **A thread adds no authority.** It runs inside its own task, with exactly
  the task's address space, capabilities and identity. Only the task itself
  can create one (no remote creation), it takes the creator's priority and
  cannot raise it, and servers see the task, not the thread.
- **Futexes are private to the task:** keyed by (task, address), so they are
  not a signalling channel between tasks and cannot get around capabilities.
  They allocate nothing: a waiter uses its own process-table entry, and the
  bucket table is fixed.

Two things are bounded instead:
- **Kernel resources, by quota.**
  - Threads consume entries in the thread table (1..1023, shared by all
    processes) and a kernel stack (two pages with its guard) each.
  - Implemented: a fixed per-process limit of 128 threads, main thread
    included. Planned: the limit from the process's resource quota (set by
    procmgr at launch), and kernel stacks charged to its frame quota.
  - `THREAD_CREATE` fails at the limit without affecting other processes.
- **CPU share, per task.** Round robin over threads gives a task with many
  busy threads more turns than a single-threaded peer at the same priority.
  Charging CPU time to the task (the existing `Scheduling_Budgets`
  framework) fixes this. It is not needed for the first threads
  implementation, but it is before Servo's thread pools share a machine with
  the desktop.

If futexes are ever extended across tasks through shared memory (grants),
that is cross-task signalling and needs its own capability decision.

## Stacks

The first version has no new memory API. The runtime allocates a thread's
stack from its heap. The kernel maps the heap eagerly on `sbrk`, so a thread
stack never takes a demand-paging fault. Page-fault admission keeps its
current rule: the primary stack range and the task's heap range, both read
from the task.

There are no guard pages below thread stacks in this version: a stack
overflow runs into the neighbouring heap block. Guard pages need an
address-space region API (`map`, `unmap`, `protect`). SpiderMonkey's executable
memory and Rust's stack guards need the same API, so it is designed together
with them as a separate step. Until then, runtimes should keep thread stacks
generous (Rust's default is 2 MiB) and probe stack use in tests.

## Futexes

Futexes are private to a task. The key is `(task, user virtual address)`,
and the address must be 4-byte aligned and inside the task's heap, image
or primary stack.

- A kernel table of 64 hashed buckets, each with its own spinlock and 32
  waiter slots, plus one overflow set with a slot for every possible thread
  (both instances of the proved generic `Futex_Queues`). A full bucket spills
  into the overflow set, so `WAIT` is never refused for lack of space and one
  process cannot crowd another's futexes out of a shared bucket. Waiters carry
  tickets from a global counter, drawn under the lock of the structure they
  join; a key's waiters always enqueue under its bucket lock, so a wake takes
  the lowest ticket across the bucket and the overflow set: FIFO per key. A
  waiting thread records where it waits, so a timeout or teardown removes it
  directly.
- **Wait**:
  1. Take the bucket lock.
  2. Read `*addr` through the task's page tables (`User_Page_Walk`, without
     faulting).
  3. If it differs from `expected`, return `EAGAIN`.
  4. Otherwise enqueue, publish the `FUTEXWAITING` state and deadline
     metadata, release the bucket lock and yield. A waker needs the bucket
     lock, so it always finds the published state; if it readies the thread
     before the thread has switched out, the scheduler only saves its
     context (the existing IPC pattern).

  This follows the `sleep` and IPC receive pattern. Checking the value and
  queuing under the same bucket lock is what makes wait/wake race-free.
- **Wake**: under the bucket lock, dequeue up to `count` waiters matching the
  key and `ready ()` each.
- **Deadlines** are metadata, expired by the BSP's millisecond tick. A
  per-bucket count of timed waits lets the tick skip idle buckets.
- **Teardown:** the reaper withdraws each dying thread's wait
  (`Process.Futex.cancelWait`), outside `Process.lock`, before freeing it.
  A kill stops waiting threads directly: they are off-CPU, so reapable.
- User words are read and written through the process's page tables
  (`User_Memory.Load_Word32`/`Store_Word32`, a new `Writable_Frame` walk),
  with the frame pinned for the access. Only the caller's own normal RAM,
  never the grant aperture.

Lock order: futex bucket → `Process.lock` → ready-list locks. A bucket lock
is never taken while holding a mailbox lock or `Process.lock`.

### Futex proofs (required)

1. **Kernel queues:** a pure SPARK `Futex_Queues` package, used by the
   kernel. The bucket lock makes each operation an atomic transition, so the
   following are provable:
   - `WAIT` never sleeps when the word differs from `expected`;
   - `WAKE (addr, n)` wakes at most `n` threads, only threads waiting on that
     exact key, and never the same thread twice;
   - a thread is on at most one futex queue, and a deadline expiry and a
     wake cannot both remove it;
   - keys are private to a process (a wake in one process never reaches a
     waiter in another);
   - addresses are aligned and in range, and bucket indexes are in bounds;
   - process teardown removes every waiter the process has.

   Unproved adapter: the lock, hashing of real addresses, and reading the
   user's word.
2. **The user-space protocol**, as an explicit transition system in SPARK.
   Each thread has a program counter (CAS, set-waiters, in `WAIT`, exchange,
   in `WAKE`), and the shared state is the futex word plus the kernel sleep
   queue. Each atomic step is a procedure that must preserve the invariant,
   which proves it for any number of threads and any interleaving:
   - mutual exclusion;
   - no lost wakeup: if any thread sleeps, the word is 2, or an unlocking
     thread has set it to 0 and not yet called `WAKE`.

   Condition variables get the same treatment when they are added.
3. **Tested, not proved:**
   - atomicity of the steps on x86 (an assumption) and fence placement;
   - the correspondence between the model and the C, Ada and Rust runtime
     code (Rust `std` brings its own futex mutex);
   - coverage: a hosted exhaustive explorer over every interleaving of the
     real algorithm for 2–3 threads, guest stress tests in `thread-check`
     with real cross-CPU contention, and mutation checks showing the tests
     catch a broken algorithm.

## Address-space mutation

Several threads can now fault, grow the heap or map grants in one address
space at the same time. A per-task **address-space lock** (a spinlock in the
task record) serializes:

- `tryAddPage` and demand faults;
- `sbrk` (heap growth and rollback);
- grant map and unmap into the task;
- DMA mapping.

As implemented it is taken after a mailbox lock (`MAP_INTO`) or the grant
lock (grant map and unmap), and nothing but allocator and TLB-round locks is
taken while it is held. Holding it across a shootdown wait is safe: spinlock
waiters service shootdown requests. Page faults decide admission and map
under it, and a fault on a page a sibling thread has just mapped returns
without mapping twice.

Lock order: mailbox or grant lock → address-space lock → TLB round, buddy.

## Lifetime and teardown

`Process_Lifetime` stays per thread: one execution presence per thread.

As implemented:

1. **Thread exit** (`THREAD_EXIT`, not the main thread):
   1. clear and futex-wake the exit word;
   2. wake a client blocked on a reply this thread owed (empty reply);
   3. mark it exiting, request its stop and leave the CPU;
   4. the reaper then withdraws any futex wait, frees its kernel stack,
      releases its thread ID (the generation advances) and unlinks it from
      its process, decrementing the thread count.
2. **Process exit, kill or fault** (a fault in any thread):
   1. close the mailbox and request a stop for every thread, under the same
      locks `THREAD_CREATE` takes to link a new thread (so a racing create
      either is stopped too or sees the kill and backs out);
   2. the reaper reclaims the process only when every one of its threads,
      exited ones included, is off-CPU; no thread can still be using the
      address space;
   3. then today's `reclaimProcess` sequence (mailboxes, grants, DMA,
      capability table, address space), each thread's futex wait and kernel
      stack, and the thread IDs.

Planned, not done: the `Task_Membership` SPARK package (thread count and
closing state, proving a process record outlives its threads and no thread
joins a closing process). Today these rest on the reasoning above and the
`futex` headless test, which exits a process with threads blocked and
running.

## IPC changes

- The receive path stamps the sender as the task.
- Endpoint wakeups (`trySendEvent`, `notifyIRQ`, retirement events) wake any
  thread waiting on the task's mailbox.
- **Async completions are dispatched to the thread that submitted them.**
  Pending requests live in the process record under its mailbox lock, and
  each records the submitting thread. A thread's completion wait or poll
  returns only its own completions. Waking every waiter and letting any
  thread take any completion would let one thread consume another's, and
  runtimes drop completions they do not recognise. A thread that exits has
  its outstanding requests cancelled, never handed to a sibling.
- Reply authority moves out of the shared capability table. Slot 63 becomes a
  per-thread `replyCap` field, so two threads serving requests at once cannot
  overwrite each other's reply authority. The ABI is unchanged: slot 63 still
  names "my reply capability", resolved per thread.
- Capability reads in `capCall`, `capSend` and `capSubmit` take the task's
  mailbox lock, as capability-table edits already do. A thread in the middle
  of a call is never exposed to a half-edited table.

Implemented (2026-09-25): reply authority per thread; async completions per
submitting thread (a pending request records its thread, the kernel-only
`CompletionQueue.owners` array tags each entry, and a thread's wait, poll and
activity wait see only its own; `THREAD_EXIT` cancels the thread's pending
requests and drops its queued completions). The user-visible
`CompletionEntry` layout is unchanged. Endpoint slots for
`capCall`/`capSend`/`capSubmit` are resolved under the caller's mailbox lock
(one snapshot of the checks and the generation the send pins).

Found by review before the first native run, and fixed:
- timer sleep wakeups readied the main thread instead of the sleeping
  thread (`Process.Queues`); the process-addressed `ready`/`notify`
  overloads are removed so the compiler rejects that mistake;
- `CAP_CALL`/`CAP_SEND` copied the reply from the main thread's buffer;
- the reaper could skip withdrawing a futex wait that a waker was still
  finishing (it now decides under the bucket lock);
- one process could fill a futex bucket and make other processes' waits
  spin (now the overflow set);
- thread creation could exhaust the thread IDs every new process needs
  (non-main threads are capped at 753, keeping 255 for main threads).

Known, not fixed: a kernel-mode caller's `reply` by PID goes to the target's
main thread (no kernel thread replies to user requests today).

## Scheduling: work stealing

Each CPU keeps its own ready list and its own scheduler loop, as today.

- **Home CPU becomes "last ran".** Wakeups queue on the CPU where the thread
  last ran, so cache warmth is kept. A new thread starts on its creator's
  CPU, and stealing spreads it.
- **Stealing.** When a CPU finds its own list empty, it takes the
  highest-priority unpinned READY entry from the CPU with the most ready
  entries before running idle. All of this happens under `Process.lock`, so
  it adds no new lock order. The thief records itself as the entry's last CPU.
- **Pinning.** `SET_CPU` (devmgr only) pins a process to one CPU, and
  stealing never moves it. This keeps devmgr's placement of storage, input,
  network and GPU services. Kernel threads (idle, reaper) are pinned.
  Everything else floats. There is no affinity-mask syscall: nothing needs
  one. If a caller appears (a real-time thread wanting its own core, hybrid
  P/E-core CPUs), placement should be a policy declared in the manifest and
  applied by procmgr, not a new application syscall.
- **Soft affinity.** Waking on the last-ran CPU keeps caches warm. A process
  moves only when that CPU is busy and another CPU in its mask is idle.
- **Migration is safe.**
  - FPU state is saved eagerly on switch-out and restored on dispatch.
  - PCID is off, so a CR3 load flushes the TLB.
  - Each dispatch sets the TSS and saved kernel stack.
  - `Process_Lifetime` already prevents a PID from being dispatched on two
    CPUs at once, and a READY entry is by definition not executing.
- **Direct IPC hand-off** is unchanged. It takes the fast path only when the
  peer is on the same CPU, and otherwise uses `ready ()` and an IPI.
- **Unchanged:** priority ordering and turn credit, which moves with the
  thread (`savedTurn`).
- **Only aged work moves.** An entry is stealable after waiting at least 500
  µs. A freshly woken IPC partner normally runs within microseconds on its
  own CPU; stealing it split client/server pairs and made sync IPC about 4×
  slower. The rule is the proved `Work_Stealing` package
  (`tests/work-stealing`). Idle CPUs look for aged work at their timer
  opportunities (at most every millisecond).

Status: implemented and on by default (`WORK_STEALING=1`). Results in
`tests/performance/results/2026-09-24-work-stealing.md`: CPU-bound
throughput scales about 3.8× on four vCPUs, and IPC is unchanged. Input
latency while the desktop repaints under load regressed in two of three runs.
That is the open item for later scheduler tuning.

This also applies to single-threaded processes. procmgr-launched services and
apps, which today all run on CPU 0, spread across CPUs.

## FS base and user GS

- The context-switch path saves FS base (`rdfsbase`) on switch-out and
  restores it (`wrfsbase`) on dispatch, per thread. Every CPU enables
  `CR4.FSGSBASE` identically; verify the APs.
- User GS is not supported: the switch path loads zero into
  `KERNEL_GS_BASE`, so it is swapped in as user GS on return to ring 3. This
  removes the `0x1337d00d` placeholder and any cross-process leak.
- `THREAD_CREATE` sets the initial value; `wrfsbase` from user code is
  captured by the save on switch-out.

## Userspace

| Runtime | Change |
| --- | --- |
| C (`libcubit`) | `cubit_thread_create`/`join`/`exit`, futex-based mutex and condition variable, TLS block via FS (`errno` becomes thread-local), a lock around `malloc` and the stdio table, and crt0 TLS setup from the executable's `PT_TLS` |
| Ada runtime | per-thread secondary stack found through TLS (the `s-secsta` pattern), a real lock for protected objects (`s-taprob`) built on the futex mutex. Ada tasking itself is out of scope. |
| Rust | the allocator lock becomes a futex mutex. Threads come with the `std` target (step 2). |

The loader gains `PT_TLS` support: it records the TLS template (address, file
size, memory size, alignment) and hands it to the runtime. The runtime copies
the template per thread, since there is no dynamic linker.

## Proofs and tests

**Proved (SPARK):**
- `Task_Membership` (above);
- the existing `Process_Lifetime`, lock-ownership and TLB-reclamation proofs
  keep passing;
- a small `Futex_Keys` package: key validity and bucket selection.

**Regression-tested, not proved:**
- futex wait/wake races;
- stealing;
- the context-switch FS base path.

**A new guest test app, `thread-check`**, checks:
- threads created across CPUs, and seen running on at least two;
- a futex ping-pong with deadlines and spurious-wakeup handling;
- TLS isolation (FS base values differ and survive migration);
- concurrent `sbrk` and page faults from several threads;
- several threads in `capCall` to one server at once, with the server seeing
  a single task identity;
- a grant acquired by a second thread, released on task exit;
- thread exit, then task exit with running threads;
- a fault in a second thread kills the task, and the supervisor is told.

The full existing regression suite must keep passing: network, TLS, desktop,
DOOM, storage, capability security and IPC benchmarks. The IPC benchmarks
matter because stealing and the task indirection touch hot paths.

## Order of work

1. FS base save/restore and user GS clearing. This fixes today's leak and has
   no dependency on threads.
2. Work stealing for existing processes. This helps today's desktop and has
   no dependency on threads.
3. Dynamic tables, then the thread split:
   1. the generic dynamic table, proved (IDs unique, generations monotonic,
      a page freed only when empty);
   2. the process table moved onto it, with allocated page tables and 16-bit
      frame-owner tags, with no behaviour change;
   3. the thread table, with every process given exactly one main thread.
      Schedulable fields move over one group at a time, with no behaviour
      change.
4. The address-space lock.
5. Per-thread reply authority, task-keyed async IPC, and waking any mailbox
   waiter.
6. `Task_Membership`, `THREAD_CREATE`/`THREAD_EXIT`, task-wide exit and kill.
7. Futexes.
8. C runtime threads and TLS, then the Ada runtime changes, then
   `thread-check`.

Steps 1 and 2 stand alone and are useful immediately. Steps 3 to 5 are
refactors that the existing regression suite checks. Step 3 is the largest
kernel change in the plan.

## Decisions

Settled in review:

1. **Servers see the process, not the thread**, since services key per-client
   state on the sender.
2. **A fault in any thread kills the process**, because the threads share
   memory.
3. **Separate dynamic process and thread tables** replace the 255-entry
   `proctab`, before any thread creation. There is no UNIX-style parent:
   lifecycle events go to the supervisor.
4. **Four new syscalls only.** No affinity syscall; `SET_CPU` pinning stays
   devmgr's.
5. **No capability for thread creation or futexes.** Thread count and kernel
   stacks are bounded by quota.

Open:

6. **Thread stacks come from the heap, without guard pages**, until the
   address-space region API exists.
7. **No thread creation in other processes** (no remote thread injection).
   procmgr and supervisors get no new authority over threads.
