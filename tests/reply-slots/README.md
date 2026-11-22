# Reply slots (BUG-004)

Hosted tests and SPARK proof (level 2) for the per-thread reply slot in
`kernel/src/capabilities-operations.ads`: a server's reply authority for
the request it is serving (IPC-000 rule 3, "every request ends").

    tests/reply-slots/run.sh [--prove]
    tests/reply-slots/mutations.sh

**Proved:** `replaceReplyCap` (every receive and poll, and a retiring
thread, go through it) installs the new authority and hands back the one
it replaces, never dropping it; `takeReplyCapFrom` consumes the slot once.
`proveSecondReceiveDisplacesFirst`: a server that receives twice without
answering gets the first request's authority back from the second receive,
and can answer only the second, once.

**Assumed (not proved):** that the kernel's IPC code (`process-ipc.adb`,
SPARK_Mode Off) ends every displaced authority with `endReply` once it
has released the mailbox lock, and that `Call_Sequences.Accepts` is the
whole test for whether the caller still waits on that call.

**Tested here:** the receive-twice history, an empty poll, a caller that
timed out and moved on (left alone), an async request displaced by a
call, and a retiring thread. `mutations.sh` checks that dropping the
displaced authority, or not installing the new one, fails the test.

**Tested in the guest** (tests/headless `async-ipc`, "displaced reply
ended"): the server holds a call and receives again; the call ends at
once with `REPLY_ENDED`, not REPLY_TIMEOUT at its 2 s
deadline; an async request displaced the same way completes
`COMPLETION_CANCELLED`.
