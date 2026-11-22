# Call sequences

Hosted tests and SPARK proof (level 1) for `kernel/src/call_sequences.ads`:
which answer a waiting caller accepts (docs/ipc-fastpath.md, "Call
deadlines").

    tests/call-sequences/run.sh [--prove]

**Proved:** an answer stamped for an earlier call is refused once the
caller has begun another (`Earlier_Refused`); `Next` only grows and is
never called at the last sequence (rollover retires, never wraps).

**Tested here:** the timeout-then-call history, and the rollover edge.

**Tested in the guest** (tests/headless `async-ipc`, "call deadlines"):
a silent server times the caller out on schedule; a late answer while the
caller waits in its next call is refused; a call that times out while
queued is never seen by the server; a server cannot answer with a label
only the kernel gives.
