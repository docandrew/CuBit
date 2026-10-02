# Scheduler: virtual deadlines, inherited urgency

Status: design (2026-09-29). Nothing here is implemented yet except the interim
idle-CPU placement described in "Interim change". This document sets the CPU
scheduling policy. [input-latency.md](input-latency.md) remains the source of
the latency requirements, the measurement boundaries and the authority rules
("latency class is resource authority"). The budget model in
`Scheduling_Budgets` ([tests/scheduler-budgets](../tests/scheduler-budgets/README.md))
supplies the accounting for authorized urgency.

## Goals, in order

1. **Daily-driving latency first.** Keypress-to-photon and input-to-focused-app
   latency under load are the primary measure. Throughput must not regress,
   but it never outranks latency.
2. **No hand-assigned service priorities.** A service's urgency comes from the
   work it is doing for someone, not from a number chosen when it was started.
3. **Scales to large servers.** Desktop and laptop sizes (2–16 CPUs) are
   optimized first. The structure must also work at hundreds of CPUs and on
   NUMA machines, well beyond today's `MAX_SMP_CPUS = 8`.
4. **Provable bounds.** Starvation freedom and budget conservation are proved
   at level 1, not approximated by a periodic sweep.
5. **Urgency is an authority.** A driver or service may make a wakeup urgent
   only through a capability granted to it. Asking for it grants nothing.

## Why the current scheduler must change

The current policy is strict priority with FIFO within a level, on per-CPU
lists, plus work stealing (docs/threads.md). Services get fixed priorities and
CPUs from devmgr (`assignCPU`). What we measured and found on 2026-09-29:

- **Apps start on CPU 0**, where the filesystem, NVMe, PS/2 and xHCI drivers
  are pinned. `resetThreadRecord` leaves `cpu = 0`.
- **Polling starves callers.** fs-bench runs at priority 3. The filesystem
  service runs at priority 5 and polls its queue for up to 50 µs after
  activity. While it polls, fs-bench cannot run on CPU 0 at all.
  - An open+read+close loop cost 29.5 µs per iteration.
  - The service's own work in that loop was about 4.3 µs.
- **Idle CPUs are slow to help.** An idle CPU takes waiting work only after it
  has queued for 500 µs (`Steal_Age_Microseconds`), and it checks only on its
  1 ms tick.
- **Equal priority means waiting.** A thread woken at equal priority waits for
  the 100 µs "awakened peer" alarm, not the IPI. Input drivers share priority 5
  with the filesystem and NVMe on CPU 0, so a keypress can wait behind a
  polling filesystem.
- **Every fix is another priority.** Each adjustment moves the inversion
  somewhere else. That is the knock-on pattern this design removes.

## Prior art and what we take from it

**Con Kolivas: Staircase Deadline, BFS (2009), MuQSS (2016).** Kolivas argued
that interactivity heuristics, which guess who is interactive from sleep
patterns, were the cause of desktop stalls and not their cure. He replaced
them with one deterministic rule.

- **Earliest virtual deadline first.** A task gets a deadline of
  `now + rr_interval × ratio` when it receives a new time slice. With equal
  weights, `ratio` is 1 and `rr_interval` is about 6 ms.
- **A sleeper keeps its deadline** and the rest of its slice. A task that runs
  briefly and sleeps (an input driver, a compositor, an editor) therefore
  nearly always has an earlier deadline than a CPU-bound task, and runs first
  when it wakes. A task that uses its whole slice gets a later deadline.
- **Bounded waiting.** No runnable task waits more than about one
  `rr_interval` per other runnable task, by construction.
- **Wake placement.** A woken task goes to an idle CPU if there is one, cache
  distance first. Otherwise it preempts the CPU running the latest deadline.
- **SCHED_ISO.** Soft real time any process may use, capped at about 70% of a
  CPU. It falls back to normal scheduling beyond the cap.
- **SCHED_IDLEPRIO.** Runs only on otherwise idle CPUs.
- **BFS used one global queue**, which limited it on large machines. MuQSS
  gives each CPU its own deadline-ordered skiplist. A CPU picks the earliest
  deadline among its own head and the heads it peeks at on other CPUs,
  without taking their locks.
- **interbench.** It measures latency and missed deadlines of simulated audio,
  video, X and game loads under background loads (burn, compile, read, write,
  memory pressure).

We take:
- the virtual-deadline rule;
- sleepers keeping their deadlines;
- the bounds;
- the placement rule;
- MuQSS's per-CPU structure;
- ISO and IDLEPRIO;
- interbench's method.

**Windows** (per *Windows Internals*; the values are approximate). Windows uses
strict priorities, 1–15 dynamic and 16–31 real time, with temporary boosts
that decay by one level each quantum:
- I/O completion boosts chosen by the driver: keyboard and mouse +6, sound +8,
  disk +1;
- a GUI boost when a window message arrives;
- a foreground boost and longer foreground quanta;
- a once-a-second sweep that briefly raises threads starved for about 4 s;
- priority inheritance through locks and ALPC;
- MMCSS: registered multimedia tasks run in the real-time range for about 80%
  of each period.

We take one idea: **urgency comes from events, not guesses.** A driver knows
that a completion is a keypress. We do not take the priority ladder, the
decaying boosts or the starvation sweep. Those are the special cases that
cause knock-on effects, and they cannot be given a proved bound.

**seL4 MCS** (scheduling contexts). A client lends its time budget along with
its call, and the server runs on the client's budget. We take donation, in
the bounded form input-latency.md already requires: donation "cannot outlive
or broaden the originating request".

## Design

### 1. Virtual deadlines replace priorities

Each thread has a **weight** from its latency class (below), a **slice
remaining** and a **virtual deadline**, all in TSC-derived nanoseconds.

- **Refill.** When a slice is refilled:
  - `slice := Slice_Length`;
  - `deadline := now + Slice_Length × Ratio (class)`.
- **Charging.** Running charges elapsed time against the slice. Each dispatch
  also charges at least `Minimum_Dispatch_Charge`, so a thread that wakes and
  sleeps every microsecond still pays for its context switches. This is the
  dispatch-credit lesson from `Scheduling_Budgets`: a 1 µs wake/sleep spammer
  caused 120,600 dispatch changes in 200 ms when only time was charged.
- **Exhaustion.** When the slice is used up, it is refilled. A later deadline
  moves the thread behind the others.
- **Sleep and wake.** Sleeping keeps the slice and the deadline, even a
  deadline that has passed by the time the thread wakes. A deadline changes
  only when a slice is used up (Kolivas's rule).
  - So a thread that slept past its deadline runs first when it wakes, but
    only for the rest of its slice.
  - A first version refilled passed deadlines on wake ("no credit for
    sleeping"). That queued every woken thread behind the CPU hogs: under
    CPU burn, wake latency went from 234 µs to 3 ms.
- **Selection.** The earliest deadline that is eligible runs. The same classes
  cover everything, so there is no second ordering.

Classes (`LatencyClass`, already in the ABI):

| Class | Ratio | Admission | Behavior |
|---|---|---|---|
| `BACKGROUND` | runs only on a CPU with nothing else ready (IDLEPRIO) | anyone may choose it | never delays other work |
| `NORMAL` | 1 | default | fair share by deadline |
| `INTERACTIVE` | 1, plus authorized urgency (§4) | capability | wakes it is authorized to make urgent get a short deadline |
| `REALTIME` | its own admitted period and budget (§5) | capability plus admission | ISO-like, capped |

Services get **no class of their own** by default. They are `NORMAL` and run
at the urgency they inherit (§3). devmgr's priority table and `assignCPU` go,
except where hardware requires affinity (§9).

### 2. Run queues and placement at any scale

- **Per-CPU queues** ordered by deadline, each with its own lock. A CPU picks
  the earliest deadline among:
  - its own queue's head;
  - the published head deadlines of the other queues in its **peek domain**,
    read without their locks. It then takes the chosen queue's lock and
    checks again.

  At first the queue can be a sorted list. A skiplist or a proved bounded heap
  comes when queue lengths measure long.
- **Peek domains** come from `CPU_Topology`: SMT siblings, then the
  last-level cache, then the package, then NUMA node.
  - Desktop and laptop, up to 16 CPUs: one domain. Selection is effectively
    global, like BFS.
  - Large servers: a CPU peeks within its last-level-cache domain. A periodic
    balancer moves work between domains, bounded per tick. This is MuQSS's
    "interactive off" mode, chosen by topology, not by a tunable.
- **Wake placement**, cheapest first:
  1. If the wakee's deadline is earlier than its current CPU's running deadline
     by at least `Preempt_Margin`, it preempts there.
  2. An idle CPU: the previous CPU, then an SMT sibling, then the last-level
     cache domain, then the package.
  3. Otherwise, preempt the CPU in the domain running the latest deadline, if
     the wakee's is earlier by `Preempt_Margin`.
  4. Otherwise, queue on the previous CPU.

  The margin prevents ping-pong between near-equal deadlines. A remote choice
  sends a reschedule IPI.
- **Scaling dependency.** Today one `Process.lock` serializes all scheduling.
  Per-queue locks, and the lock order documented in
  [kernel-locking.md](kernel-locking.md), are prerequisites for server scale,
  not for the desktop.
- **Idle.** An idle CPU halts. Waking a halted vCPU costs about 7–9 µs
  under KVM.
  - Polling before `hlt` (as Linux's guest haltpoll does) was measured and
    dropped for now; see Status.
  - The better lever is keeping communicating pairs on one CPU.
  - Tickless idle, with no 1 ms tick on idle CPUs, comes later.

### 3. Inheritance through IPC and queues

- **Synchronous call.** The caller lends its deadline and remaining slice to
  the server until the reply, as input-latency.md specifies.
  - The server runs at `min (own deadline, lent deadline)`.
  - Time it runs is charged to the lent slice first.
  - The loan ends at reply, or when the request is abandoned.
  - Same-CPU calls keep the direct handoff.
- **Asynchronous queues** (filesystem and netstack queue pairs, frame rings).
  The kernel does not see each request, so deadlines ride on the signals it
  does see:
  - A client's KICK or WAIT carries the client's current deadline. The
    service's wake then inherits the earliest deadline among the pending kicks
    and waits.
  - While serving, a service may publish "serving client X" in a per-thread
    scheduling word. The kernel honors it only for a client bound to that
    service by capability (the queue setup already establishes that binding).
    The service then runs at X's deadline, with its time charged to X. It
    cannot claim a client that is not bound to it, and it cannot hold the
    loan after X has nothing pending.
  - This is the one open design point. The alternative is that the service
    always runs at the earliest deadline among its bound clients with
    outstanding work. That is simpler, but it lets a busy client lend urgency
    to work done for an idle one.
- **Polling.** A polling service burns its own slice (or a slice lent to it),
  so its deadline moves back and waiting work runs. With deadlines, the
  "priority-5 poller starves a priority-3 app" case above cannot occur.

### 4. Authorized urgency (event-sourced)

- **The capability.** A new capability type, `CAP_SCHEDULING`, carries an
  **urgency budget**: time and dispatch credits per period, from the proved
  `Scheduling_Budgets` ledger. devmgr grants it according to policy, and CCL
  configuration names the holders:
  - input drivers (ps2, xHCI HID);
  - `input.svc`;
  - the compositor's input and present path;
  - audio drivers.

  It is never ambient, and requesting a class does not create it.
- **Use.** A holder marks a notification, send or reply as urgent. The woken
  thread gets `deadline := now + Urgent_Slice`, and its dispatch and run time
  are charged to the holder's budget.
- **Propagation.** The urgent deadline passes along the IPC chain like any
  lent deadline:
  - input driver → input.svc → focused app → the filesystem or compositor
    work that app requests.
  - It cannot outlive or broaden the request.
- **Exhaustion.** When the budget runs out, urgency stops: marked wakes become
  ordinary `NORMAL` wakes. Ordinary scheduling is not affected. A buggy or
  hostile holder can therefore spend only its own budget, never starve others.

### 5. Soft real time (ISO)

`REALTIME` is admitted through `setLatencyContract` with a period and budget,
by a holder of `CAP_SCHEDULING` with admission rights:
- Admitted work runs by its period deadline, ahead of `NORMAL` deadlines, within
  its budget.
- Beyond its budget it runs as `NORMAL` until the next period (the ISO
  fallback).
- Total admitted real time per CPU is capped, by default below 70% as ISO
  was. The rest of the system therefore always keeps a share, which is
  provable.
- Audio and vblank submission are the first users.

Concretely:

- **Authority.** A new capability type, `CAP_SCHEDULING`, whose object holds
  the largest real-time utilization its holder may reserve (`ref` = budget
  µs, `param` = period µs).
  - devmgr mints it at spawn (as it mints `CAP_RESOURCE` quotas) for
    `hda.drv` and `mixer.svc`. Apps get it only through manifest policy.
  - `SYSCALL_SET_LATENCY_CONTRACT` with class `REALTIME` is refused unless
    the caller holds a `CAP_SCHEDULING` covering the requested budget/period,
    and the system-wide total stays under the cap.
  - The other classes stay advisory until urgency lands (§4).
- **Admission** is a proved package (`Realtime_Admission`). The sum of
  admitted utilizations, in parts per million, never exceeds
  `Realtime_Share` × online CPUs. Release on exit or on a new contract
  returns exactly what was admitted.
- **Accounting** uses the proved `Scheduling_Budgets` ledger, one per
  admitted thread:
  - periods aligned to the monotonic microsecond clock;
  - a time budget of at most half the period;
  - a context-switch allowance per period;
  - sticky overrun detection.

  The kernel accounts at dispatch and stop.
- **Ordering.** Run keys gain a band. An admitted thread that is `Eligible`
  (budget and switches left this period) has key = its period's end, which
  gives EDF among real-time threads. All `NORMAL` keys are offset above
  every real-time key; idle stays last. `Place`, `Choose` and `Preempts` are
  unchanged: they compare keys.
- **Fallback.** An exhausted or overrun thread gets its `NORMAL` key until
  its next period, so a buggy real-time thread costs at most its admitted
  share.

### 6. What the kernel proves (level 1)

- **Deadlines:** they never move backward except at refill, and a refill sets
  `deadline ≥ now`.
- **Queues:** each is sorted by deadline, and every runnable thread is on
  exactly one queue.
- **Starvation freedom:** a runnable `NORMAL` thread runs within
  `(runnable threads in its domain) × Slice_Length × Ratio` plus the admitted
  real-time share.
- **Urgency conservation:** urgent time and dispatches never exceed the
  granting capability's budget; a loan never outlives its request; a service
  can borrow only from a client bound to it.
- **Real-time admission:** admitted real time on a CPU stays within the cap.
- **Placement:** placement never puts a pinned thread off its CPU.

The arithmetic and ledgers are pure SPARK packages tested hosted, as
`Scheduling_Budgets`, `Scheduling_Turns` and `Scheduler_Timing` are today. The
kernel glue calls them.

### 7. Choosing a scheduler without patching the kernel

Fully userspace scheduling, where a scheduler process makes each dispatch
decision, costs a context switch per decision on the very path whose latency
this design is about. Google's ghOSt manages tens of microseconds per decision
with shared-memory transactions, which suits server batch work, not input.
The designs that work split policy from the hot path:
- **seL4 MCS:** userspace sets priorities and budgets; the kernel dispatches.
- **Linux sched_ext:** BPF policies loaded at run time run inside the kernel.
  A verifier checks them, and a watchdog falls back to the default scheduler.

CuBit takes the same split, in three steps:

1. **One policy interface.** The policy is pure, proved functions of
   scheduler state (`Virtual_Deadlines`): the run key on wake, placement
   (`Place`), which list to run from (`Choose`), and what happens at slice end.
   The kernel keeps only the mechanism: queues, switching, IPIs and
   accounting.
2. **Proved policies chosen at boot** by the CCL boot configuration:
   - `desktop`: short slices, one peek domain up to 16 CPUs, idle polling
     before `hlt`;
   - `server`: longer slices, peek domains per last-level cache, tickless
     idle, and margins that favor staying on a CPU.

   Changing policy is a configuration change, and every policy is proved.
3. **Run-time tuning by a policy service** holding a scheduling-admin
   capability:
   - slice, margin and peek domains;
   - real-time admission;
   - urgency grants.

   These are rare decisions, so they cost nothing on the hot path.

A later research step is loadable policies, like sched_ext. They would be
written in a bounded language we can prove (or given as data tables) rather
than BPF. A ghOSt-style agent owning a set of CPUs could suit specialized
servers.

### 8. Visibility

Visibility is a CuBit principle: security first, and performance too. "The
Linux Scheduler: A Decade of Wasted Cores" (Lozi et al., EuroSys 2016) found
scheduler bugs that went unnoticed for years and caused many-fold slowdowns.
Two tools exposed them:
- an online check of one invariant, "no CPU idles while work waits elsewhere";
- per-CPU heatmaps of queue lengths and load over time.

The scheduler ships with both from the start:

- **A work-conservation checker.** On an idle CPU's timer opportunity, it
  checks for work that another CPU could let it take and that has waited
  longer than a bound, twice `Preempt_Margin` plus the IPI budget.
  - A violation counts toward a per-CPU counter and freezes a trace window.
  - It runs only on idle CPUs, never on a busy CPU's path. An idle CPU
    already scans the other lists on each tick to find work it could take
    (`stealableWorkElsewhere`). The checker only compares the found entry's
    `queuedTSC` against the bound and bumps a counter.
  - It stays on in normal builds only if the latency benchmark shows no
    measurable cost, measured A/B with it on and off. Otherwise it becomes
    a build option like the trace ring.
  - Proved: `Place` and `Choose` never leave an allowed idle CPU unused for
    a thread that would wait. The checker tests what the proof cannot see:
    stale `cpuRunningKey` values, lost IPIs, pinned-thread effects and
    hypervisor delays.
- **Scheduler events in the existing trace ring** (`Trace`):
  - ready, with the target CPU and why it was chosen;
  - dispatch, with the deadline and the list taken from;
  - preemption, with the reason;
  - slice refill;
  - a cross-CPU take;
  - checker violations.
- **Export by capability.** A holder of a trace-read capability can copy a
  frozen window out as records, rather than it being dumped to serial. An
  export can reveal other processes' timing, so it is an authority like any
  other.
- **A viewer.** First a host-side tool that turns an exported window into an
  HTML timeline: one lane per CPU showing who ran, queue length, idle while
  work waited, and deadlines. Later an in-OS workbench view.
- **Per-thread counters in sysinfo:** slices used, preemptions by reason,
  wake-to-run latency histograms, CPU moves.

### 9. What goes away

- Strict-priority insertion and the priority field as a scheduling input.
- The 100 µs awakened-peer alarm.
- The 500 µs steal age and tick-driven stealing, replaced by placement and
  peeking.
- The parts of `Scheduling_Turns` that deadlines replace.
- devmgr's per-service priorities and CPU assignments. Affinity stays only
  where hardware requires it: an MSI vector's target CPU, or a device queue
  bound to a CPU.

## Benchmark before implementation

This is an interbench-style harness under tests/headless, built before the
policy changes so the current scheduler gets a baseline.

- **Simulated workloads:**
  - input events at 125 and 1000 Hz from a test source through input.svc to a
    focused app;
  - a compositor frame loop at 60 and 144 Hz;
  - a 5 ms audio period;
  - an interactive request/response load.
- **Background loads:**
  - none;
  - CPU burn on every CPU;
  - fs-bench;
  - net-bench;
  - a wake/sleep spammer;
  - a polling service.
- **Reported:** wake-to-run and event-to-app latency (p50, p99, max), missed
  frame and audio deadlines, and background throughput.
- **Boundaries:** those of input-latency.md, with Linux-hosted runs reported
  separately from native runs.
- **A/B:** every scheduler change is compared against the previous commit.

## Interim change (2026-09-29, under test)

Idle-CPU placement on top of the current priorities. If an unpinned thread
would have to wait on its CPU, it goes to an idle CPU, and that CPU gets an
IPI. That applies when the thread:
- wakes behind equal or higher priority work;
- is preempted by higher-priority work;
- yields behind higher-priority work;
- starts for the first time.

This is step 2 of the design's wake placement, and it carries over. See
`Process.idleCPUFor` and `placeOn`, and the requeue in `Scheduler`.

## Status (2026-09-29)

- **Proved:** `Virtual_Deadlines` (kernel/src): refill, the dispatch charge, the
  preemption margin, `Place` and `Choose`. 52 of 52 checks prove at level 1, and
  the hosted tests pass (tests/scheduler-deadlines).
- **Native, in the working tree (not committed):**
  - deadline-ordered ready lists (`Queues.insertByKey`);
  - dispatch by `Choose` across all CPUs' lists;
  - wake and requeue placement by `Place`. A thread still switching out stays
    on its CPU; the latency benchmark's spam load found that race;
  - slices kept across sleeps, a minimum charge per dispatch, and preemption
    by deadline and margin;
  - deadlines move only at slice refill (Kolivas's rule).

  Removed: strict-priority ordering, the 100 µs awakened-peer alarm and the
  500 µs steal age.
- **Tested natively:** the latency benchmark (3 runs each) and the headless
  suite: libc, storage-grants, threads, futex, async-ipc,
  capability-security, desktop-display, input-stream, bench-ipc, bench-spread
  and bench-fs.
- **Results** (tests/sched-latency, QEMU/KVM, 4 vCPUs, p50/p99 µs, median of
  3 runs):

  | Workload | Load | Before | After |
  |---|---|---|---|
  | wake | CPU burn | 234 / 245 | 2.1 / 15 |
  | IPC round trip | CPU burn | 386 / 391 | 2.1 / 1998 |
  | IPC round trip | none | 45.5 / 79 | 2.8 / 22 |
  | IPC round trip | spam | 46 / 83 | 15 / 51 |
  | frame lateness | CPU burn | 745 / 1242 | 400 / 1007 |
  | wake | none | 2.2 / 8.5 | 9.0 / 13 |

  - Background throughput is unchanged.
  - For reference, Linux in the same VM wakes in 13 µs with no load and 1.1 µs
    under burn.
  - Unloaded wakes are slower than before because a preempted waker moves to
    an idle CPU, so a ping-pong pair ends up on two CPUs and pays an IPI to a
    halted vCPU on each wake.
  - Frame and audio lateness now sit at the 1 ms clock floor, which precise
    timers remove.
  - About one IPC round trip in a hundred under burn still takes about 2 ms.
- **Haltpoll** (§2, "Idle") was tried: an adaptive window up to 200 µs, with
  the IPI skipped while polling. It was removed, because it gave no gain on
  these workloads. Wakes paced at 1 ms fall outside any sensible window, and
  under burn wake latency rose from 2.1 to 6.8 µs. Revisit with finer timers
  and real IPC-heavy traces.

## Order of work

1. The latency benchmark, and a baseline on the current scheduler.
2. Pure SPARK deadline and queue packages, tested and proved hosted.
3. Deadline selection and wake placement native, with priorities ignored.
   Measure.
4. Deadline inheritance for synchronous IPC. Then the asynchronous queue
   signals (§3).
5. `CAP_SCHEDULING` and authorized urgency, with devmgr and CCL grants.
6. ISO admission for audio and vblank.
7. Remove devmgr priorities and pinning. Haltpoll idle.
8. Server scale: per-queue locks, peek domains, cross-domain balancing.
