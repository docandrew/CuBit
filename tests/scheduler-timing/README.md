# Independent clock and scheduling dividers

```sh
nix develop -c make -C kernel test-scheduler-timing prove-scheduler-timing test-locking
```

The hosted timing fixture compiles the production `Scheduler_Timing` package.
It checks 600,000 ticks across all six combinations of independent starting
phases. A 500-us hardware tick advances the millisecond clock every two ticks
and offers peer rotation every three ticks (1.5 ms). Both units are conserved.
GNATprove covers initialization, range checks, and the exact increment/wrap
postcondition for both dividers. It proves no hardware timing bound.

The locking suite also compiles the production ready-queue implementation.
Its stable-array oracle checks FIFO among equal priorities, readiness at/below/
above the highest queued priority, empty queues, and lock-depth restoration.
That covers the decision to offer a peer scheduling opportunity, not the live
context switch, interrupt controller or arbitrary SMP interleavings.

For live contention use `bench-input --load` from the headless runner, with
`--timeout 75`. Keep benchmark peer priority unchanged and validate the load
coverage and input-integrity counters. See `tests/performance/README.md`.
The CPU-owned periodic tick is not reset by direct IPC handoffs or sleeps;
this fix does not introduce priority promotion or real-time reservations.
