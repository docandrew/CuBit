# Work-stealing eligibility

`nix develop -c make -C kernel test-work-stealing prove-work-stealing`

`Work_Stealing` (`kernel/src/work_stealing.ads`) is the pure SPARK rule the
kernel's steal path uses (`Process.Queues.stealFrom`). An idle CPU may take a
ready entry from another CPU's list only when it is:
- ordinary work (priority >= 0, never an idle thread);
- not pinned (`SET_CPU`, kernel threads);
- not being retired;
- not still executing (switching out) on another CPU;
- queued for at least `Scheduler_Timing.Steal_Age_Microseconds`.

**Proved (GNATprove):** 30 checks, none unproved or justified, no
`Assume`:
- `Eligible` equals exactly that rule (its postcondition).
- `Min_Age` saturates instead of overflowing.
- A timestamp earlier than the queued stamp (cross-CPU TSC skew) is never
  aged. The first kernel version computed `now - queued` unsigned, which
  wrapped and made such entries look ancient.
- Uncalibrated time makes nothing stealable.
- In a ready list sorted by descending priority, the first eligible entry has
  the highest priority among eligible entries (`Prove_First_Is_Best`), so
  taking the first one is taking the best one.

**Consequence used by the scheduler:** only a stolen (eligible) entry has
its CPU rewritten, so a pinned process never moves.

**Hosted test:** compares `Eligible` against an independently written reference
(31,360 cases) over
every combination of flags, priorities around the idle boundary, and time,
rate and age boundaries (including reversal and saturation).

**Not proved:**
- the intrusive queue walk itself (exercised by `test-locking` and the
  native suites);
- TSC calibration;
- the idle-CPU timer check;
- the latency effects, which are measured by the `bench-*` fixtures: see
  `docs/threads.md`.
