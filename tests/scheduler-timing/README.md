# Interrupt-independent elapsed clock

```sh
nix develop -c make -C kernel test-scheduler-timing prove-scheduler-timing test-locking
```

The hosted timing fixture compiles the production `Scheduler_Timing` package.
It checks 1,000,000 irregular samples across ten independent starting epochs,
including missing/coalesced interrupts, long pauses and clock reversal.
Milliseconds are derived from elapsed TSC ticks, preserving the fractional
remainder, not by counting interrupts. An ordered timestamp type makes the
non-wrapping arithmetic explicit. The native adapter uses a boot-relative epoch
and rejects values outside its supported range.
The 1.5-ms execution turn is now accounted separately by `Scheduling_Turns`:
higher-priority preemption preserves its unused credit; IPC does not refill it.
GNATprove covers initialization, arithmetic safety, exact elapsed-time updates,
and a ghost lemma proving that split and combined updates give identical time.
It proves no hardware timing bound or accuracy of the reference oscillator.

The kernel calibrates TSC against the PIT before starting fast LAPIC interrupts.
It calibrates the LAPIC against the actual TSC interval rather than assuming a
PIT sleep occupies exactly its requested duration. Native sleep-queue catch-up
visits queued sleepers once, not once per missed millisecond; `test-locking`
checks passed deadlines and preservation of the first future delta.

The locking suite also compiles the production ready-queue implementation.
Its stable-array oracle checks FIFO among equal priorities, readiness at/below/
above the highest queued priority, empty queues, and lock-depth restoration.
That covers the decision to offer a peer scheduling opportunity, not the live
context switch, interrupt controller or arbitrary SMP interleavings.

For live contention use `bench-input --load` from the headless runner, with
`--timeout 75`. Keep benchmark peer priority unchanged and validate the load
coverage and input-integrity counters. See `tests/performance/README.md`.
The adaptive one-shot scheduler uses the same elapsed clock and execution-turn
accounting. Direct IPC does not refill turns; wakeups do not reset the clock.
This does not introduce priority promotion or real-time reservations.
