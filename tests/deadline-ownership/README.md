# Deadline ownership and isolated native expiry tests

This is the next **timer foundation**, not an enabled interactive scheduler.
Ordinary peer rotation remains 1.5 ms; no new priority/admission authority is
granted. It does not improve input latency on its own.

## Pure SPARK component

`Deadline_Ownership` owns one pending expiry slot. The adapter's owner type must
include process lifetime identity, not merely a recycled PID. Each successful
arm issues a monotonically increasing ticket. Cancellation requires both the
owner and current ticket. Ticket exhaustion rejects the arm without wrapping
or modifying an existing expiry. Waking, changing ownership, or recycling a PID
must never reset the CPU slot/ticket sequence.

On an interrupt, `Poll` checks the **current** deadline and identity. Early
vectors do nothing. A due expiry is consumed exactly once before any scheduling
action: it returns `Expired` only for the matching owner, otherwise `Stale_Owner`.
It reports lateness separately. No raw process pointer survives in a pending
event. `Next_Interrupt` chooses the earlier of this expiry and an independently
owned clock deadline; it never moves that clock deadline.

```sh
nix develop -c make -C kernel test-deadline-ownership prove-deadline-ownership
```

The hosted fixture checks 10,000 replacement/cancellation/expiry chains,
independent CPU slots, boundaries at zero and the maximum tick value, stale
generations, and exhaustion with a deliberately small ticket space. GNATprove
discharges 23 checks on a concrete instantiation and ghost replacement scenario,
with no unproved/justified checks or warnings. No `Assume` or SPARK-off code is
used in the ADT. Hardware, monotonic clock validity, locking, and live process
lifetime integration are **not** proved. Ticks describe a non-wrapping clock
epoch; the live adapter must reject an unhealthy clock rather than interpret
wrap/reversal as a valid scheduling history.

## Native boot probe

```sh
nix develop -c bash -lc 'DEADLINE_TIMER_TEST=1 tests/headless/run.sh --test bench-ipc --accel kvm --cpus 4 --timeout 35 --serial /tmp/deadline-timer.log --keep-logs'
nix develop -c python3 tests/deadline-ownership/check_native.py /tmp/deadline-timer.log --cpus 4
```

The test temporarily switches each CPU's LAPIC to one-shot mode before its
scheduler starts, runs 128 samples of each scenario, then restores its normal
500-us periodic timer. These are synthetic generation-tagged owners, **not live
process reservations**. The four CPUs are tested during sequential startup,
not under concurrent application load.

- Normal 200-us expiry.
- Old 50-us hardware countdown arrives after replacement by a 200-us software
  deadline. It must not expire the replacement early; the remaining interval
  is rearmed.
- Cancelled owner still receives a hardware vector: no expiry is delivered.
- Owner lifetime changes without cancelling: the due expiry is reported stale.

Interrupts are acknowledged normally; the probe consumes its own vectors before
clockkeeping/scheduling. BSP guest milliseconds intentionally pause during this
boot-only probe, with no live services or user deadlines. This is not the future
live timer multiplexer. Configuration/state updates happen with local interrupts
disabled, each CPU accesses only its own slot, and volatile completion flags
communicate with its ISR. No locks, printing, allocation or yields occur in the
probe ISR. A TSC timeout makes a missing interrupt an explicit boot-test failure.

Reports require all requested CPUs, exact expiry/cancel/stale counts, observed
early vectors, and zero failures. Reported p99 is a histogram upper bound on
**interrupt-entry observation lateness after the requested deadline**, not the
full handler/preemption latency and not key-to-photon. It includes programming,
calibration and host effects; boot-only results establish no loaded latency SLA.
Residual delays round up to a microsecond; TSC and LAPIC rates remain separately
calibrated and may disagree.

`DEADLINE_TIMER_TEST` defaults to zero. Normal boot and interrupt paths compile
out calls to the probe. Kernel assertions remain disabled. After headless tests,
rebuild the normal ISO with `nix develop -c make -C kernel iso`.

The LAPIC programming follows [Intel SDM volume 3A, §12.5.4](https://cdrdv2-public.intel.com/835754/253668-sdm-vol-3a.pdf).
One-shot, periodic and TSC-deadline modes are mutually exclusive. Stopping a
countdown or changing its mode is not a software ownership check; a pending
vector must still be validated. Physical hardware also needs an audit of timer
behavior in power-saving states. TSC-deadline is a possible future backend,
not required by this countdown-mode test.

## Before latency can improve

1. Multiplex the real clock, ordinary quantum and budget deadlines on one local
   timer, including overdue events without unbounded IRQ catch-up loops or
   losing elapsed sleep time. Rearm before a handler can yield.
2. Bind the slot to actual dispatch ownership under the process/lifetime lock.
   Handoffs and retirement must cancel/replace safely; stale expiry cannot act
   on a replacement process. Account both reservation and shared CPU ledgers.
3. Add trusted admission and bounded interactive preemption. Self-declared
   latency hints are not grants. Exhausted work remains ordinarily runnable.
4. Measure ready-to-run latency, loaded input, ordinary throughput, IRQ-off
   intervals and overrun behavior. Test genuinely concurrent CPUs and the laptop.
