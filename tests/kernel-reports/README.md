# Kernel_Reports: exit and fault reports kept until read

Hosted tests and SPARK proof (level 2) for `kernel/src/kernel_reports.ads`
(docs/ipc-delivery.md, step 1). Run in the Nix shell:
`tests/kernel-reports/run.sh [--prove]`.

What is checked:
- a child's exit is kept for its parent and for procmgr until both take it;
- its PID is released only after the last report is read (`Request_Free`
  defers it; `Take` or `Close` says when);
- a recipient that ends drops what it was owed, which releases those PIDs;
- a closed recipient, or another life of it, is sent nothing;
- faults while one is unread are counted in it (`Further`).

Proved: `Valid` (a deferred release always waits for an unread report) is
kept by every operation, and a released PID has nothing unread.
