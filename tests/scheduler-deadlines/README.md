# Virtual deadlines

`nix develop -c make -C kernel test-scheduler-deadlines prove-scheduler-deadlines`

`Virtual_Deadlines` (kernel/src) holds the pure SPARK rules of the MuQSS-style
scheduler ([docs/scheduler.md](../../docs/scheduler.md)):

- `Refill`: the deadline of a fresh slice, `now + slice`, saturating.
- `Dispatch_Charge`: every dispatch costs at least the minimum charge.
- `Preempts`: a key runs ahead of another only if it is earlier by more than
  the margin. An idle CPU (`Idle_Key`) is preempted by any deadline.
- `Place`: where a woken or requeued thread goes. In order:
  1. a pinned (or still executing) thread stays home;
  2. home, if it would run there at once;
  3. an idle allowed CPU, nearest after home first;
  4. the allowed CPU running the latest key it preempts;
  5. otherwise queue at home.
- `Choose`: which ready list a CPU runs from. Its own list wins unless an
  allowed list holds a key earlier by more than the margin. Empty lists are
  never chosen.

**Proved** (level 1, 52 checks): the postconditions above. In particular:
- `Place` never moves a pinned thread;
- `Place` only moves a thread to an allowed CPU where it runs at once (idle,
  or preempting);
- `Choose` takes another list only for its earliest allowed head, and only
  when that head is earlier than the own list's by more than the margin.

**Tested** (hosted, `main.adb`): the same rules on concrete cases. These
include the two edge cases the first draft got wrong: a deadline near the
maximum preempting an idle CPU, and an idle CPU choosing an empty list.

**Not covered here:**
- the kernel glue (Process.ready, placeOn, the scheduler loop), which the
  kernel-locking hosted suite and the native headless suite exercise;
- a woken thread keeping even a passed deadline, which is a rule of the glue
  (deadlines move only at slice refill).

Native latency results are in [tests/sched-latency](../sched-latency/README.md).
