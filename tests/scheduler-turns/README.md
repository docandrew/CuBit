# Ordinary execution turns

`nix develop -c make -C kernel test-scheduler-turns prove-scheduler-turns`

This pure SPARK ADT accounts remaining execution credit, in ordered TSC ticks.
Charging saturates at zero. Moving credit clears its source. The ghost split
lemma proves that splitting a charge across a suspend/resume boundary gives the
same remainder as charging the whole interval. The test also exercises nested
higher-priority preemption, direct-IPC no-refill, and unsigned boundary values.
Hosted assertions are enabled; the kernel still builds with assertions disabled.

Native integration rules:

- A scheduler dispatch starts a 1.5 ms turn, or resumes saved credit.
- Timer checkpoints charge actual residency, including syscall/interrupt work
  attributed to that process. They do not charge time while another task runs.
- A strictly higher-priority preemption saves the remainder. The interrupted
  task resumes ahead of its equal-priority peers, but behind higher priorities.
- Exhaustion, ordinary yielding/blocking, and wake-aware early rotation do not
  bank unused time. Ordinary queue insertion is still FIFO among equal peers.
- Direct IPC changes the running owner without replenishing the CPU turn.
- Creation/reuse resets saved credit. A rejected accounting transition clears
  the affected credit rather than supplying an unaccounted continuation.

The kernel queue implementation is exercised by `test-locking`, including the
distinction between resuming an interrupted turn and inserting an exhausted turn
behind peers. Hardware context-switch glue is not covered by the ADT proof.

These are **ordinary scheduling turns**, not resource admission, enforceable
reservations, a priority capability, or a proof of an application deadline.
