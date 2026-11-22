# Kernel_Credits: a ring per sender at each receiver

Hosted tests and SPARK proof (level 2, 36 checks) for
`kernel/src/kernel_credits.ads` (docs/ipc-delivery.md, step 3). Run in the
Nix shell: `tests/kernel-credits/run.sh [--prove]`.

What is checked:
- a flooder fills only its own ring and is then Busy, while another
  sender is still admitted into an entry of its own;
- `Take` serves round-robin (the other sender within one turn) and keeps
  each sender's order;
- a later life of a sender waits until the earlier life's ring is
  forgotten;
- `Forget` empties a sender's ring.

Proved: every admitted message has a place in its sender's ring;
isolation (a sender under its credit is admitted, and every other ring is
untouched); `Take` returns the head of a non-empty ring and finds one
whenever any is non-empty.
