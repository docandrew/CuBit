# Shadow scheduler budget accounting

This tests the production pure SPARK observation adapter, both on Linux and
inside CuBit. It is deliberately **not budget enforcement or admission**.
Normal kernel builds retain the 1.5-ms scheduling policy and compile out the
shadow update/output branches. Record storage remains in the process/CPU tables.

```sh
nix develop -c make -C kernel test-scheduler-shadow prove-scheduler-shadow
nix develop -c bash -lc 'SHADOW_SCHEDULING=1 tests/headless/run.sh --test bench-ipc --accel kvm --cpus 4 --timeout 35 --serial /tmp/shadow-ipc.log --keep-logs'
nix develop -c python3 tests/performance/report.py /tmp/shadow-ipc.log --require-shadow-budgets
```

Omit `SHADOW_SCHEDULING` (or set it to `0`) for the normal build. Headless fixtures
temporarily select a test boot profile; rebuild the normal desktop ISO afterwards.

The native adapter updates under the existing process lock at scheduler
entry/exit, direct IPC handoffs, and explicit accounting checkpoints. It uses
the exact same TSC timestamp as the separate execution counter. A reservation
is reset on process creation, while CPU state survives process retirement/reuse.
This depends on the existing native lifetime/lock discipline, not a new proof
of it. CPU migration and IPC donation are not supported by this experiment.

Hypothetical limits: per process 200 us/four dispatches; per CPU 1,000 us/eight
dispatches; aligned 2-ms periods. Calibration scales those limits to TSC ticks
once; execution is never rounded at each handoff. Calibration uncertainty and
synchronized-TSC assumptions remain. No public latency hint authorizes a budget.

All actual non-idle demand is charged, even after a hypothetical denial. That
keeps raw residency comparison exact, but makes this a demand meter rather than
a prediction of what an enforced expedited lane would do. Budget overrun is
sticky and expected; clock/sequencing failure is a separate unhealthy state.
There is no trap, throttle, or priority promotion in the diagnostic path.

Hosted tests cover 20,000 short dispatches, exact TSC charging, checkpoints,
idle gaps, replacement lifetimes without shared-credit replenishment,
independent CPU state, multi-period execution, exact exhaustion, and sticky
clock/sequencing failures. The native IPC fixture requires both dispatch paths
and compares shadow ticks/counts with its own independent execution snapshot.
Parser tests reject missing, duplicated, faulty, and inconsistent snapshots.

GNATprove discharges all 20 adapter checks (13 flow, seven prover), without
unproved/justified checks, warnings, `Assume`, or SPARK-off code. The generalized
budget core independently discharges 138 checks. Four adapter assertions check
the results of budget operations; they are proved and enabled in hosted tests,
not enabled at runtime in the kernel. This does not prove the whole scheduler,
concurrent native integration, or any latency/throughput guarantee.
