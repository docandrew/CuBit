# Hosted expedited-scheduling policy experiment

This is **Linux-hosted policy testing**, not an enabled CuBit scheduler change.
The production-shaped pure SPARK component lives in
`kernel/src/scheduling_budgets.*`. An opt-in native shadow observer now calls it
to measure actual demand; it does not make scheduling decisions. See
[shadow accounting](../scheduler-shadow/README.md).
CuBit retains the 1.5-ms peer-rotation experiment.

```sh
nix develop -c make -C kernel test-scheduler-budgets prove-scheduler-budgets
```

The component tracks two resources in a common CPU-aligned 2-ms period:

- Expedited execution time, capped at half a period per ledger.
- Expedited dispatch credits, so tiny or zero-time wakeups cannot authorize
  unlimited context switches while retaining a positive time balance.

Use one ledger per admitted reservation and one shared ledger per CPU.
Eligibility requires both ledgers; claim a credit from each on an expedited
dispatch. Account elapsed expedited execution against both. Continuing the
same execution needs time but no additional dispatch credit. An exhausted
reservation loses expedited eligibility, not ordinary scheduling eligibility.
Creating state is a privileged lifecycle operation, never a wakeup operation.
The shared CPU ledger must survive reservation/PID replacement.

Accounting processes the OLD execution state before changing it. It splits
cross-boundary execution without a loop proportional to elapsed time, does
not accumulate unused allowance, rejects reversed time without mutation, and
retains a sticky overrun flag even after subsequent replenishments. A sticky
overrun disables expedited eligibility until a future trusted recovery path
decides how to re-admit it. It is not an app crash or silent runtime clamp.
The adapter must stop execution on time; detecting an overrun is not proof
that hardware cannot overrun.

Tests cover:

- Exact exhaustion, repeated short execution bursts, and zero-time state toggles.
- Period boundaries, delayed accounting across multiple windows, clock reversal,
  maximum representable timestamps, and sleeping without accumulating credit.
- 10,000 deterministic events compared to a separate one-microsecond reference.
- Replacing individual ledgers without replenishing the shared CPU ledger.
- 10,000 zero-time dispatch claims admitting exactly eight starts.
- Two 200-ms idealized simulations with input bursts, audio bursts, a
  continuously runnable ordinary compute task, and a wake/sleep spammer.

Both simulations complete 400 input and 100 audio bursts, preserving at least
1,000 us of ordinary compute time in every aligned 2-ms window. Time limits
alone permit **120,600** synthetic dispatch changes. With dispatch credits,
the same workload makes **1,200** changes. This is a deterministic demonstration
of switch-storm control, not a native speedup or a prediction of hardware
latency. The one-microsecond simulation steps are not proposed timer ticks.
Dispatch decisions cost zero simulated time, there are no locks/IRQs, and the
toy dispatcher is not CuBit's production priority/FIFO queue implementation.

The bounded simulation uses 100/200/600-us per-reservation allowances and
4/2/2 dispatch credits for input/audio/spammer respectively, with a shared
1,000-us/eight-dispatch CPU ceiling. These are explicit experimental values,
not production admission policy. Ordinary tasks and their quantum are not
modeled in enough detail to compare general compute throughput.

GNATprove discharges **138 obligations**, with no unproved or justified checks.
This includes exact accounting and dispatch-claim contracts, range checks,
initialization, termination, and a ghost proof that splitting same-period
execution at a handoff preserves both the remaining balance and overrun state.
No `pragma Assume` or `SPARK_Mode => Off` is used in the component. Assertions
are enabled only in the hosted test project, not the kernel.

Not proved or implemented: authority admission, aggregate reservation admission,
native timer enforcement, hardware clock accuracy, SMP migration, a proof of
native process lifetime binding, IPC donation, lock synchronization, fairness among ordinary
tasks, or any physical latency bound. See the
[native integration checklist](../../docs/input-latency.md#hosted-expedited-budget-prototype).
