# Native scheduled-residency accounting

Unlike the hosted expedited-budget simulator, this component is now called by
the **CuBit kernel**. It collects measurements only. It does not change queue
ordering, priorities, the 1.5-ms quantum, authority checks, quota enforcement,
or enable expedited reservations.

## Storage and boundaries

- `Process.execution`: raw residency ticks, ordinary scheduler dispatches,
  direct IPC dispatches, and a sticky saturation flag. Reset at PID creation.
- `Process.cpuAccounting`: separate 64-byte-aligned per-CPU records containing
  the current accounting owner, last ordered timestamp, clock/ownership health,
  and scheduler-only time. No changes to assembly-visible `PerCPUData` offsets.
- `Execution_Accounting`: generic pure SPARK state/counter operations, instantiated
  with the kernel's `ProcessID` type. The native adapter performs clock reads,
  owner lookup and synchronization; those hardware operations are not proved.

All updates hold the existing `Process.lock`. Dispatch does not acquire an
additional lock, allocate memory, divide clock values or print diagnostics.
The timestamp is `lfence; rdtsc; lfence` with a compiler memory clobber.
Ticks are retained without per-burst conversion/rounding.

At scheduler dispatch, ownership moves from the scheduler to the process.
Immediately after return to the scheduler, the final running process is charged
and ownership moves back to the scheduler. At `Process.directSwitch`, the old
process is charged and ownership changes to the target before releasing the
old context's execution presence. Thus A->B->A handoff chains do not become one
large interval charged to their last PID. A locked snapshot can checkpoint the
current interval without incrementing dispatch counts.

The process lock and existing execution-presence protocol prevent retirement
or PID reuse before the final interval has been charged. The hosted test models
reset/reuse after that boundary; it is not a proof of the whole live lifetime
protocol. Processes remain pinned to their existing home CPUs.

## Meaning and limitations

These are **scheduled-residency ticks**, not exclusive user CPU time or retired
instruction cycles. They include syscalls, interrupts, lock waits and other
kernel work occurring within each accounting interval. Idle-process residency
includes halted time. Context-switch cost is assigned according to the explicit
boundary placement; it is not independently measured. The existing trace
`run_tsc` remains a scheduler-return interval, potentially spanning multiple
direct IPC owners, and is not the new per-process counter.

Clock reversal or unexpected ownership makes that CPU's accounting unhealthy
and prevents further charging. This never changes execution/scheduling policy.
Overflow saturates counters and marks the record; it cannot silently wrap into
small usage. Native diagnostics must reject unhealthy or saturated snapshots.
Boot/reinitialization is the current reset boundary; no live recovery mechanism
for accounting faults is provided. Future budget enforcement must explicitly
fail closed on invalid accounting rather than consuming these as valid data.

No timer checkpoint is needed for these cumulative observations: the next
handoff or snapshot closes the interval exactly. Enforcing a short budget still
requires deadline programming and periodic/interrupt-return integration. Nor
is this a reservation-owner/donation implementation: ticks currently belong to
the executing process. Sub-microsecond TSC remainders are retained by keeping
raw totals, but conversion into the separate budget model remains future work.

## Diagnostics and tests

The existing `SYSCALL_TRACE_SUMMARY` prints one additional **calling-process-only**
`ACCOUNTING:` record, after taking a locked snapshot and releasing the lock.
It does not expose other processes' counters, add a syscall, or create a new
global enumeration authority. `Trace.Reset` does not reset lifetime accounting.
Console framing can still be corrupted by the pre-existing cross-CPU serial
interleaving issue; incomplete snapshots fail the benchmark validator.

```sh
nix develop -c make -C kernel test-execution-accounting prove-execution-accounting
nix develop -c bash tests/performance/test.sh
nix develop -c bash tests/headless/run.sh --test bench-ipc --accel kvm --cpus 4 \
  --load --timeout 25 --keep-logs --serial /tmp/accounting-ipc.log
nix develop -c python3 tests/performance/report.py /tmp/accounting-ipc.log \
  --require-load --require-execution-accounting
```

The native IPC fixture now requires a healthy, nonsaturated caller snapshot
with positive ticks and both scheduler/direct dispatch counts. This validates
live wiring; it does not independently measure exact native tick attribution.
The hosted test checks exact totals over 100,000 A->B->A chains, checkpoints,
scheduler gaps, retirement/reuse, independent CPU clocks, reversal/owner faults,
and saturating tick/dispatch arithmetic.

GNATprove analyzes a concrete instantiation with the same owner range as the
kernel: **19 obligations discharged**, no unproved/justified checks. It covers
the transition/counter contracts and ghost assertions for each exact handoff
interval and owner. A generic source by itself generates no proof obligations;
the make target therefore selects the concrete proof driver. A separate
modular-sum lemma did not discharge and was removed, not assumed. Aggregate
conservation is covered by the exact hosted totals; the native adapter, assembly,
clock behavior and SMP lock protocol are outside this proof.

All builds/tests use Nix. Assertions are enabled only in the hosted project;
the kernel retains optimized `-O2 -gnatp` builds. No `pragma Assume` or new
SPARK-off section was added.
