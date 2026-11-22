# CuBit Input Latency and Scheduling

## Product requirement

CuBit treats response latency as a correctness property of the interactive
system, not as optional visual polish.

The initial software target is:

> From device interrupt or completion to delivery at the focused application,
> CuBit adds less than one millisecond at the 99th percentile under an admitted
> interactive workload.

Key-to-photon tests additionally require that accepted input appear on the first
display refresh whose submission deadline can be met. Device polling intervals
and physical scanout are reported separately from OS-added latency.

Hard real-time is a stronger future contract. An admitted hard deadline may not
be missed within its documented hardware and workload assumptions; percentile
targets alone are insufficient.

## Measurement boundaries

### Current experimental scheduler (September 2026)

The wake-aware experiment uses adaptive one-shot timer opportunities and a
1.5 ms ordinary execution turn. A higher-priority interruption preserves the unused portion of
that turn; it does not implicitly rotate an interrupted task behind equal peers.
Direct IPC inherits the existing CPU turn without replenishment. Waking a peer
can bring forward a rotation, but does not grant higher priority, bypass FIFO
selection, or create any application authority. See
[`tests/scheduler-turns`](../tests/scheduler-turns/README.md) for the pure credit
model and its proof boundary.

Native higher-priority rescheduling is serviced at both syscall and interrupt
return, after releasing operation-specific locks. A stale reschedule request is
revalidated against the current ready queue; an IPI is not itself permission to
preempt a higher-priority task.

The normalized publication-to-app test has crossed the observed 1 ms p99 target
with four busy peers on one CPU, including concurrent repainting. It remains
a closed-loop diagnostic: it does not establish IRQ-to-app latency, independent
arrival-rate behavior, admission bounds, or physical key-to-photon performance.
Unbounded interrupt-disabled work (including the legacy debug-write syscall),
driver completion latency, and host descheduling remain obstacles to a broader
latency guarantee and require separate measurement/hardening.

`ONESHOT_SCHEDULING=1 WAKEUP_SCHEDULING=1` selects the current experiment.
These are now the defaults; see the
[measured results and assurance limits](../tests/performance/results/2026-09-11-adaptive-latency.md).
Ordinary clock maintenance is armed at up to one millisecond, shortened by
remaining execution turns or a 100-us wakeup opportunity. Local wakeups shorten
the local alarm; remote wakeups request an IPI so the target CPU can do so.
An earlier request cannot postpone an existing alarm. Expiry is checked against
the current CPU-owned `Deadline_Ownership` slot, and early/stale vectors rearm
the remaining interval. The next alarm is armed **before** clock work can yield.
The slot is not a process reservation or permission to boost priority.

Elapsed time is independent of interrupt count. A 100-us **periodic** trial
exposed time dilation because this host's KVM minimum periodic interval is
200 us. Its apparent good latency was invalid and its 60-guest-second load did
not finish in the allotted host time. The corrected clock is PIT-calibrated
before fast interrupts begin, with split/combined update conservation proved
in SPARK. A live host-monotonic comparison validates the native compute-control
interval. KVM applies this minimum specifically to periodic timers; see
[`limit_periodic_timer_frequency` in Linux](https://raw.githubusercontent.com/torvalds/linux/master/arch/x86/kvm/lapic.c).

Remaining guarantee work is explicit: trusted admission, independent/open-loop
arrival testing, bounded IRQ-disabled operations, driver IRQ/completion-to-report
timing, and physical display/audio measurements. In particular, virtio-net's
1-ms polling fallback causes avoidable higher-priority wakeups; replacing it
requires reliable device interrupt routing, not a larger polling interval.

### Required instrumented boundaries

Every sample carries or can be correlated with monotonic timestamps at these
boundaries:

1. hardware interrupt or transfer completion;
2. driver scheduled and report decoded;
3. normalized input published;
4. input router or desktop scheduled;
5. focused application delivery;
6. surface update accepted;
7. compositor submission;
8. scanout or page-flip completion.

Telemetry reports count, minimum, maximum, percentile histogram, queue depth,
coalescing, sequence gaps, and explicit drops. Serial output never runs in the
measured path. Physical key-to-photon measurement remains the final authority.

## Event semantics

Input types do not all have the same loss policy:

* key and button transitions are ordered and lossless;
* relative pointer motion may be coalesced only by adding every delta;
* absolute pointer motion may replace an older undelivered position;
* wheel displacement may accumulate without crossing an ordered transition;
* raw-input streams retain timestamped samples according to a declared bounded
  overflow policy;
* every overflow is observable through a counter and sequence discontinuity.

Coalescing stale presentation work is not permission to discard logical input.

Input delivery is state synchronization as well as event delivery. A consumer
must be able to reconstruct the current pointer position, complete button and
modifier state, focused surface, and device generation after any detectable
transport loss. Merely increasing a queue bound does not establish this
property.

Each normalized seat event therefore needs:

* a monotonically increasing sequence number and device/seat generation;
* a declared delivery class: replaceable state, accumulable displacement, or
  ordered transition;
* an authoritative state snapshot containing absolute pointer position and
  complete button/modifier state; and
* an explicit overflow/resynchronization event. Resynchronization cancels
  capture and transient key/button state before installing the new snapshot.

Pointer motion can use latest-state semantics once it is absolute. Relative
deltas and wheel displacement must be accumulated before replacement. Button
and key transitions may not be silently lost. If a bounded transition stream
cannot accept another event, the system reports the gap and sends a cancel plus
snapshot rather than pretending the stream remained continuous.

IRQ notification has doorbell semantics. A notification means that work may be
pending, and the recipient drains the authoritative controller or shared ring
until empty. Kernel notification state is coalescible and remains pending until
observed; correctness must never depend on retaining one mailbox entry per
interrupt.

## Current reliability audit

The current driver-to-desktop vertical slice uses a shared SPARK protocol for
typed, state-bearing source reports. The kernel stamps source identity from the
capability that authorized publication; producer-supplied authority tags are ignored.
Each PS/2 keyboard, PS/2 pointer, and xHCI pointer source maintains an
independent generation and sequence. Desktop validates delivery class, rejects
exact replay, detects generation or sequence discontinuity, merges pointer
button state deliberately across sources, and cancels transient capture and
modifier state at an explicit resynchronization boundary.

The transport is still the bounded kernel event lane, not the final shared-ring
`input.svc` design. Queue rejection is visible to the producer and makes its
next accepted state-bearing report a resynchronization boundary. This prevents
stuck state and silent continuity claims, but an ordered transition can still
be lost at that explicit boundary. The remaining gaps are:

* replace direct driver-to-desktop publication with typed `input.svc` publish
  handles and a seat-scoped desktop subscription;
* give the input service transport-bound source class identity, so a publisher
  cannot claim a different device class merely by changing report bytes;
* add a bounded lossless lane or complete keyboard state snapshots for ordered
  transitions rather than relying only on cancel-and-resynchronize recovery;
* remove transitional static keyboard/mouse registry roles; and
* enforce latency contracts through scheduler admission, dispatch ordering,
  budget accounting, and deadline propagation.

Kernel device IRQ delivery is now a persistent, coalescing doorbell outside the
ordinary mailbox ring. PS/2 treats it as a wake hint and drains controller bytes
until empty in bounded decode batches. Desktop input channels are stable by
surface ID, have independent bounded queues and serials, and emit an explicit
`INPUT_RESYNC` snapshot rather than silently replacing an ordered transition on
overflow. The shared UI runtime cancels transient capture and installs that
state. Event-driven applications drain pending input and then block through a
deferred one-use reply capability; the old ten-millisecond polling sleep has
been removed. The CCL Workbench's custom platform adapter now follows the same
rule instead of retaining its own ten-millisecond poll. When a compositor frame
deadline is pending, desktop dispatch
uses one combined IPC-or-absolute-deadline kernel wait; publication wakes it
immediately rather than waiting for a millisecond sleep slice. Motion
coalescing respects press, release, and wheel barriers, and the runtime renders
accumulated drag motion before consuming a following barrier.

Input replies carry a bounded `more pending` drain hint. A client awakened with
one isolated event can return directly to the atomic deferred wait instead of
issuing an empty poll first; a queued burst is still drained before sleeping.
The hint conveys no authority and cannot lose a later arrival, because that
arrival either appears in the queue or completes the newly installed waiter.

The target correction is a typed `input.svc` seat stream. Device drivers receive
publish-only endpoints; the input service validates and normalizes per-device
reports and deliberately merges their state; desktop receives a seat-scoped
subscription; applications receive only compositor-routed, surface-scoped
input. Global observation, raw reports, injection, focus control, and latency
admission are separate authorities. Surface IDs remain object names and never
authorize input consumption.

Use per-seat and per-surface bounded queues so one stalled client cannot evict
another client's transitions. Replace idle polling with a wakeable wait/drain
operation or one-shot reply completion. Shared state pages and SPSC rings may
remove hot-path copies, but their mappings, producer/consumer roles, bounds,
and notification endpoints remain capability-authorized.

## Target pipeline

The intended path is:

```text
device completion
  -> bounded driver decode
  -> typed input stream publication
  -> policy-controlled routing
  -> focused application and desktop state
  -> incremental composition
  -> asynchronous scanout
```

Drivers do not choose the focused recipient. An input-routing service owns seat,
focus, device-class, and raw-input policy. Direct raw input is a separately
granted authority and cannot imply global input observation.

Small inline transitions and shared single-producer rings are both valid
transports. Shared rings use edge notifications rather than a syscall for every
sample. Producers replenish buffers before blocking, and consumers drain a
bounded batch before yielding.

## Scheduling contracts

Latency class is resource authority. A process cannot promote itself merely by
requesting a class in its executable or making a syscall.

* `REALTIME` is admitted periodic work with an enforced period, deadline, and
  execution budget. Audio is the first user.
* `INTERACTIVE` is bounded aperiodic work with a short wakeup deadline and a
  replenished burst budget. Input routing and compositor dispatch use it.
* `NORMAL` provides ordinary fair scheduling.
* `BACKGROUND` yields to admitted latency-sensitive work.

An IPC call may donate the caller's effective deadline and only the remaining
bounded execution budget through the service chain. Donation conveys no other
authority. The callee cannot retain it after resolving or returning the request.
This prevents priority inversion without allowing an untrusted client to mint
real-time CPU access.

CPU and IRQ affinity are selected as a pipeline. Same-CPU IPC may use direct
handoff; cross-CPU wakeup uses a prompt reschedule IPI. Busy polling is excluded
from steady-state input and real-time paths.

## Display behavior

The boot framebuffer currently has no hardware cursor or page-flip completion.
Cursor-only damage therefore has an explicit immediate-present operation and
does not poll legacy VGA vertical blank. Ordinary frame damage may remain
vblank-oriented.

The target display protocol provides asynchronous submission, explicit buffer
ownership or fences, vblank completion events, incremental damage, and hardware
cursor planes where available. Presentation policy is explicit in the protocol;
it is never inferred from an undocumented rectangle-size threshold.

## Implementation sequence

1. Remove synchronous vblank polling from software-cursor input feedback.
2. Expose per-process mailbox event drops and input-path latency histograms.
3. Block desktop dispatch on wakeable IPC while no frame deadline is pending.
4. [done] Add a combined IPC-or-deadline wait for compositor scheduling.
5. Keep multiple xHCI interrupt transfers queued and use MSI completion instead
   of one-transfer-at-a-time millisecond polling.
6. Decouple immediate input consumption from software-cursor scanout. Bound
   redundant cursor presents so a high-report-rate mouse cannot serialize the
   desktop on synchronous display IPC.
7. [vertical slice done] Define the typed normalized input event and stream
   schema.
8. Enforce capability-governed interactive scheduling budgets.
9. Propagate bounded deadlines through IPC and test priority inversion.
10. Add asynchronous display fences, vblank events, and physical
    key-to-photon measurement.
11. [done] Replace lossy per-IRQ mailbox entries with pending doorbells whose
    consumers drain the authoritative source.
12. [vertical slice done] Add sequenced normalized source state and explicit
    cancel/resync events. Complete seat snapshots remain with `input.svc`.
13. [done] Split the desktop's global queue into bounded per-surface streams
    with explicit snapshot recovery rather than silent transition replacement.
14. [done] Replace application polling sleeps with a wakeable input wait/drain
    path based on deferred one-use reply capabilities.
15. [vertical slice done] Introduce capability-stamped source identity and
    independent state for every keyboard and pointer before merging them into
    a seat. Transport-bound class identity remains with `input.svc`.

Steps 1 and 3, plus the event-drop portion of step 2, are implemented in the
current framebuffer path. Step 5 now has an initial vertical slice: xHCI
maintains eight independent interrupt-IN requests, replenishes completed
transfers before downstream publication, and blocks on a dedicated MSI/MSI-X
notification. Step 6 is also implemented for the software cursor: input is
consumed immediately, the first update after idle is presented immediately,
and redundant scanout is coalesced to a bounded four-millisecond cadence.

xhci.drv maintains low-rate aggregate diagnostics for transfer events, decoded
reports, motion, button transitions, completion failures, short reports,
unexpected event types, and the most recent four report bytes. Time queries and
formatting are kept out of the per-report hot path. A shell-free QEMU desktop
stress run decoded 845 reports and two button transitions with zero transfer
errors, short reports, or desktop event drops. A deterministic headless source
test additionally publishes 128 paced motions, button and key transitions, and
one deliberate sequence hole. It requires exactly one observed recovery
boundary, zero semantic rejects, and a bounded number of Workbench surface
presents. The real laptop's touchpad is
now usable, but the external USB mouse remained badly jerky and appeared not to
click before cursor-present coalescing. Hardware retesting and timestamped
latency distributions are still required before treating the target as met.

## Properties to verify

The model and implementation should establish that:

* ordered transitions cannot be silently replaced by motion samples;
* accumulated relative motion equals all accepted source deltas;
* sequence gaps or bounded queue overflow are observable;
* an ungranted process cannot observe global or unfocused input;
* requesting a latency class cannot create scheduling authority;
* admitted budgets cannot exceed schedulable capacity;
* deadline donation cannot outlive or broaden the originating request;
* exhausted interactive work cannot starve admitted real-time work; and
* no unbounded allocation, search, logging, or device polling occurs on an
  admitted critical path.

## September 11 measurement gate and scheduling gap

The native `bench-input` fixture now measures normalized source publication to
focused-application receipt through real capability-checked desktop IPC. See
[reproduction and exact boundaries](../tests/performance/README.md#focused-application-input-diagnostic).
It separates input integrity from observed timing: losing a transition never
improves the reported percentile. The optional `< 1 ms` empirical p99 gate is
not an interrupt-to-app guarantee or a hardware-independent CI promise.

Initial idle measurements are comfortably below the target, but equal-priority
CPU contention exposes millisecond-scale delays even with no lost events.
The asynchronous display path removes one source of blocking; it does not
solve scheduling of the compositor and focused client. A four-vCPU guest does
not spread this test's participants across cores.

The current implementation explicitly leaves `SYSCALL_SET_LATENCY_CONTRACT`
advisory. `Process.setLatencyContract` stores its fields; ready queues still
select by priority and equal-priority tasks use FIFO ordering. The initial
measurements used a ten-millisecond timer quantum; the follow-up below replaces
that cadence. `Process.ready` requests immediate
local preemption only for a strictly higher-priority task. Thus there is no
implemented admission envelope within which the product requirement can yet
be claimed. FIFO fairness is necessary but insufficient for submillisecond
delivery. This is an architectural gap, not evidence of an input-stream loss.

A future reservation/admission increment must establish these properties together:

1. A trusted admission authority supplies bounded interactive CPU reservations;
   a process-local hint cannot manufacture them.
2. Actual execution time, including short syscall/block/wake cycles, consumes
   the reservation. Blocking or repeated wakeups cannot replenish it.
3. Exhaustion demotes/defers work until a bounded replenishment; simply yielding
   and immediately becoming eligible again is not budget enforcement.
4. Input dispatch and the focused recipient receive admitted service within
   the end-to-end budget; prioritizing only desktop leaves client delivery
   exposed to the same delay.
5. Any IPC donation conserves the caller's remaining budget, does not broaden
   authority, and cannot survive reply, cancellation or caller retirement.
6. Admission and selection policy should be a small pure SPARK state machine
   tested against adversarial workloads before integration with live queues,
   interrupt-return preemption and hardware time accounting.

Do not enable the existing untrusted latency hints as priorities or hide the
failure by lowering the benchmark peer's priority. The workload envelope,
denial/exhaustion behavior, input losses and deadline misses must remain visible.
Timing proofs additionally need bounds on lock/IRQ-off intervals, nonpreemptible
work, and hardware effects; SPARK functional correctness alone cannot supply
those physical bounds.

### First correction: submillisecond peer scheduling

The immediate fix is ordinary, authority-neutral scheduling, **not** enabling
the advisory latency classes. The calibrated LAPIC runs at four scheduling
ticks per millisecond on each CPU. Every tick checks the local priority-ordered
ready queue under its queue lock; if an equal- or higher-priority peer is ready,
the running task yields through the existing scheduler. FIFO reinsertion puts
an interrupted CPU-bound peer behind already-ready equals. If only lower
priority/idle work is ready, the timer avoids an unnecessary context switch.

The 250-microsecond cadence belongs to the CPU and is never reset by an IPC
handoff, syscall, block/wake cycle or process launch. Both IPC handoff chains
and compute loops therefore reach periodic peer-selection opportunities.
This preserves the existing trusted base-priority model and grants no extra
authority to a self-declared interactive task. The busy peer in the benchmark
retains the same priority as desktop and the client; it is not demoted to make
the result pass.

`Scheduler_Timing.Advance` is a pure SPARK divider: every fourth LAPIC tick
advances one public millisecond. Only BSP advances global time/sleep/deadline
queues, and coarse quota accounting still runs once per local millisecond.
Calibration continues to use ticks per millisecond; the BSP switches from the
PIT calibration clock only while interrupts are disabled, after masking the
PIT and before AP startup. No sleeps, audio periods or timeout units change.

Proof covers the divider's initialization, bounds and exact phase/wrap contract,
not live timer delivery or SMP scheduling. Hosted tests exercise 400,000 ticks
with independent CPU phases and the production queue's readiness/FIFO rules.
Native regressions measure the full dispatch path. The nominal equal-priority
queueing term is proportional to `(ready peers) * 250 us`, not a universal
latency ceiling. Higher-priority work, IRQ-off sections, host scheduling and
large runnable populations still require explicit admission and measurement.

Tradeoff: 4,000 local timer interrupts/second/CPU instead of 1,000. Global
deadline scanning remains at 1,000/second, and uncontended work does not pay
four times as many context switches. A future deadline/one-shot timer can
reduce idle interrupt cost after timer ownership and wakeup races are specified.

Follow-up test-infrastructure task: replace unframed debug-console bytes with
bounded, source-attributed log records. Current SMP writers can interleave even
inside one syscall; this corrupted a required desktop startup marker despite
observed input delivery. Do not solve it with a long global IRQ-disabled lock
around serial I/O, which would undermine the input-latency goal. Single-write
userspace records remove only the same-CPU, between-syscall fragmentation.

### Follow-up experiment: 1.5-ms peer rotation

The current implementation separates timer frequency from peer rotation:
500-us LAPIC ticks feed two independent per-CPU phase dividers. Every second
tick advances a millisecond; every third tick offers peer rotation (1.5 ms).
The divider is still CPU-owned and unaffected by IPC handoffs or task lifetime.
This halves timer IRQ frequency to 2,000/second/CPU and reduces nominal peer
rotation opportunities sixfold compared with the 250-us experiment. Neither
number is a measurement of actual context switches or throughput gain.

There is no new interactive promotion or wakeup-budget enforcement in this
experiment. Equal-priority input work can wait longer behind a compute task;
the submillisecond target must be measured again, not assumed preserved.
The public millisecond clock, sleep/deadline scans, authority checks, and direct
IPC handoff behavior are unchanged. The legacy quota yield remains separate
and can still occur at millisecond boundaries.

The production divider passed 600,000 hosted transitions covering all six
clock/quantum phase combinations. GNATprove discharged all seven obligations:
two initialization checks, four range checks, and one exact functional
postcondition. This proves the logical divider, not hardware timing or live
scheduler correctness. The production queue/locking/lifetime tests also pass.

### Hosted expedited-budget prototype

`Scheduling_Budgets` and `tests/scheduler-budgets` now implement the first
portable policy component. This is **not enforcing native policy**; its 1.5-ms
quantum and advisory latency hints are unchanged. All testing/proving uses
the Nix Linux environment. See [tests and scope](../tests/scheduler-budgets/README.md).

The model separates ordinary scheduling from a future admitted expedited
lane. Per-reservation and shared per-CPU ledgers bound both execution time
and expedited dispatch count. The common experimental period is 2 ms;
expedited time is capped at 50% per aligned period. Periods are CPU-clock
aligned, not restarted by waking, dispatching, or making an IPC call.
Unused time/dispatch credits do not accumulate. Exhaustion removes expedited
eligibility but leaves ordinary scheduling possible. A missed exhaustion
stop is detected and sticky, including when accounting resumes after a
replenishment boundary; it requires explicit trusted recovery, not automatic
amnesty on the next period.

This fixed-window bound is **not** a sliding-window bandwidth or response-time
guarantee. Two adjacent windows can produce back-to-back reserved bursts.
Existing strict base priorities can also starve lower-priority work; a shared
expedited ceiling alone does not fix ordinary-queue fairness.

The toy dispatcher revealed why CPU time alone is insufficient: a 1-us
wake/sleep spammer produced 120,600 synthetic dispatch changes in 200 ms
despite respecting its time allowance. Adding dispatch credits reduced that
to 1,200 for the same idealized workload while all input/audio bursts finished
and ordinary compute retained at least half each aligned period. This is not
a native performance result. Kernel/IPC/interrupt overhead is absent from the
simulation; finite dispatch limits bound modeled admission churn, not all
system activity or arbitrary untrusted IPC traffic.

The component proves all 138 obligations, including exact balance changes,
dispatch-claim behavior, overrun detection, bounds and a ghost split-execution
conservation property. The whole dispatcher, authority model and timing
guarantee are not proved. A separate single-microsecond reference checks
10,000 deterministic accounting events against the constant-time algorithm.

Before native enablement:

1. Bind reservations to kernel-authenticated authority and process lifetime,
   never to the self-set `LatencyContract`. Admission must bound the sum of
   reservations and their dispatch budgets. Do not introduce a root-like
   implicit grant or hard-code benchmark priority exceptions.
2. Account at scheduler entry/exit, `Process.directSwitch`, block/wake, timer
   expiry and process retirement. The current `Scheduler.schedule` duration
   spans direct handoff chains and refreshes the PID only afterwards, so it
   cannot serve as per-process CPU accounting. Charge the actual reservation
   owner and the shared CPU ledger without double charging.
3. Count scheduling/IPC overhead attributable to expedited execution. Preserve
   fractional clock units across short bursts; flooring each handoff to whole
   microseconds would allow sub-microsecond work to escape accounting. The
   prototype's integer time units are not a clock-conversion implementation.
4. Specify a per-CPU timer owner and arm the next budget stop/replenishment
   deadline. A 500-us periodic tick cannot enforce a 200-us allowance precisely.
   Keep the public millisecond clock independent; bound IRQ-off/lock intervals.
5. Under the appropriate lock, update both ledgers to the same timestamp,
   verify both time and dispatch eligibility, claim both dispatch credits,
   then switch. Continuing the same execution does not consume another credit.
   Neither ledger is safe to copy/reset as a way to admit additional work.
6. Specify bounded selection/preemption behavior for local and remote wakeups;
   revalidate when actually dispatching, including safe retirement/reuse.
   Migration must not reset periods or gain a second CPU allowance. Start
   with CPU-pinned reservations until ownership transfer is specified.
7. Specify IPC budget transfer separately: one owner, conservation of remaining
   time and dispatch credit, bounded delegation, and teardown on reply,
   cancellation, exhaustion or retirement. No automatic credit minting for a
   service merely because an interactive caller contacted it.
8. Measure the real loaded-input/IPC/audio regressions, including overload and
   wakeup storms, with budget-exhaustion, overruns and switch counts visible.
   Include multiple ordinary runnable tasks, independent periodic input,
   dispatch-credit exhaustion, and fixed-window boundary bursts before claiming
   a workload envelope for the submillisecond target.

### Native execution accounting, without a policy change

The first native integration step now records scheduled-residency ticks and
ordinary/direct dispatch counts in each process entry. Separate cache-aligned
CPU records track the last ordered TSC and accounting owner without modifying
the assembly-visible per-CPU layout. All accesses use the existing process
lock. Scheduler entry/return and direct IPC switches account the outgoing owner
before the lifetime handoff can permit retirement/reuse. No new switch-time
lock, division, allocation or serial output is added.

This is not yet reservation charging: ticks belong to the executing process,
include kernel/interrupt time inside its interval, and do not distinguish
delegated budgets. The 1.5-ms scheduler policy remains unchanged. Expedited
budgets, admission, precise deadline enforcement, IRQ attribution and migration
are still future work. Bad timestamp/owner state disables accounting without
altering scheduling; this is suitable for telemetry, not a budget-enforcement
failure policy. Future enforcement must explicitly reject unhealthy accounting.

The existing trace-summary syscall can snapshot only its caller's counters;
the snapshot is copied under lock and printed afterwards. Native IPC tests
require positive scheduler and direct dispatch counts and no fault/saturation.
The hosted production-core test verifies exact totals over 100,000 handoff
chains, checkpoints, independent clocks, reuse and failure cases. GNATprove
discharges 19 obligations on a concrete instantiation, including exact span
and owner assertions; whole-kernel locking/clock correctness is not proved.

See [implementation/test scope](../tests/execution-accounting/README.md) and
[paired native results](performance-baseline.md#native-accounting-overhead-experiment).

### Native shadow budget observation

`SHADOW_SCHEDULING=1` enables a diagnostic adapter using the same ordered TSC
timestamp as native execution accounting. Normal builds default to zero, with
the shadow update/output branches compiled out. No queue, priority, quantum,
admission or authority decisions change in either build.

The observer charges actual non-idle process residency to a per-lifetime ledger
and a shared per-CPU ledger under the existing process lock. Both use raw TSC
ticks, not rounded microseconds per handoff. The experimental limits are 200 us
and four dispatches per process, 1,000 us and eight dispatches per CPU, in aligned
2-ms windows. Idle/scheduler gaps are not process execution; checkpoints charge
elapsed time without claiming another dispatch. Resetting a process lifetime
does not reset the shared CPU ledger.

This is a **demand meter, not a counterfactual scheduler simulation**. Even a
hypothetically denied dispatch still executes normally and is charged. Sticky
overruns are expected without precise stop timers and are distinct from clock
or sequencing faults. The measured denials cannot predict real admission
outcomes, since a real scheduler would have changed the execution history.

Caller-only `SHADOW-BUDGET` snapshots are checked against `ACCOUNTING`: identical
ticks and identical scheduled-plus-direct dispatch totals at the same checkpoint.
No shared CPU activity or other application's counters are disclosed by that
interface. Hosted tests and focused proofs cover the pure adapter, not native
locking, retirement ordering, timestamp hardware, or a physical latency bound.
See [test scope and commands](../tests/scheduler-shadow/README.md).

### Expiry ownership and one-shot hardware bring-up

`Deadline_Ownership` now supplies a CPU-local expiry slot with generation-aware
owner identity, monotonically issued arm tickets, stale cancellation rejection,
exactly-once polling, and earliest-deadline selection against an independent
clock deadline. A concrete instantiation plus ghost replacement scenario passes
all 23 SPARK checks. This proves the ADT's transition contracts, not live
interrupt/lifetime synchronization or a physical timing bound.

`DEADLINE_TIMER_TEST=1` enables an isolated pre-scheduler test on each CPU. It
temporarily uses LAPIC one-shot countdowns, deliberately delivers early vectors
after owner replacement, cancels armed owners, and changes generations before
expiry. It restores the 500-us periodic timer before scheduling starts. No live
task is admitted or preempted, and BSP milliseconds deliberately pause during
its boot probe. The normal build compiles out the diagnostic hooks.

One- and four-CPU KVM IPC fixtures pass the timer checks and ordinary IPC/retirement
regressions. The four-CPU run reports a 32-us p99 lateness bucket beyond the
requested 200-us deadline on each CPU; the single-CPU capture reports 64 us.
These are boot-only interrupt-entry observations, **not loaded stop latency**.
See [scope and remaining integration](../tests/deadline-ownership/README.md) and
[measurements](../tests/performance/results/2026-09-11-deadline-timers.md).

Scheduler waiting remains the leading explanation for the measured loaded-input
tail, rather than a new demonstration of indefinite starvation: the earlier
250-us cadence measured roughly 0.26–0.52-ms loaded p99, while 1.5-ms peer rotation
returns roughly 1.77 ms. Neither shadow accounting nor this boot probe changes
that policy. The next live step is clock/quantum/budget timer multiplexing and
safe rearming before yields, followed by trusted admission and bounded wakeup
preemption. Do not promise latency improvement until that policy is actually
connected and measured.
