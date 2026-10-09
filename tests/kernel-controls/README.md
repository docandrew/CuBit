# Kernel_Controls: control messages kept per sender

Hosted tests and SPARK proof (level 2) for `kernel/src/kernel_controls.ads`
(docs/ipc-delivery.md, step 2). Run in the Nix shell:
`tests/kernel-controls/run.sh [--prove]`.

What is checked:
- nothing is taken before the target opens, or for another life of it;
- two senders' Stops are two facts; a repeat of an unread kind is one;
- when all 8 sender slots hold other senders' unread messages, a new
  sender is told Busy (it keeps its message), while a sender with a slot
  can still add to it;
- a closed target's messages go with it.

Proved: one slot per sender (`Valid`); `Send` either keeps the message
(`Holds`) or changes nothing, and never loses a message already kept;
`Take` returns a kept message and clears exactly it.
