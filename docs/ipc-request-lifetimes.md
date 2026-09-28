# IPC request identity and reply retirement

Implemented hardening, 2026-09-10. This records two bugs found while auditing
desktop input wait lifetimes. The fixes are kernel-wide, not desktop workarounds.

## Request identity

A completion-bearing request is identified within its caller's process
generation by a nonzero, never-reused request ID. The caller-supplied completion
token is correlation data, not reply authority. A kernel-minted reply binds
the caller PID, its generation and the request ID.

Process creation zeroes its record, bypassing Ada component defaults. The old
counter expected an initial value of one. Starting from zero, its first request
selected one but advanced the stored counter only to one: the second request
also selected one. The native fixture reproduced a barrier reply completing the
earlier, deliberately held request's token instead.

`IPC_Request_Ids` now stores the last issued ID; zero naturally means none issued.
Its `Next` function returns a discriminated result, with a nonzero identifier
only on success. Process creation explicitly initializes the sequence too.
At the maximum ID it rejects new completion-bearing submissions; it never wraps.
A failed enqueue may burn an ID but cannot recycle it. Reset occurs during
closed-process cleanup/creation, with PID generations protecting old replies.

The function is intentionally not atomic. `submitResolvedEndpoint` commits its
result under the caller and target mailbox locks, alongside pending/completion
reservation and queue publication. Distinct mailboxes are acquired in ascending
PID order, with one acquisition for self-submission. There is no additional
lock or allocation in the submission path.

## Reply retirement is distinct from delivery

Previously `replyCap` returned early for a closed or generation-stale target,
leaving the selected capability and deferred-slot bit in place. A desktop
waiter could clear its software state yet strand the kernel reply slot; the
next save correctly refused to overwrite that occupied slot.

An explicit reply attempt now retires a selected `CAP_REPLY` regardless of
whether delivery succeeds. `retireReplyCap` returns the exact old authority,
clears only that slot and its bookkeeping bit, and leaves non-reply authority
unchanged. A second attempt has no reply authority. Target liveness/generation
are still checked before publication; retirement does not make stale delivery
permissible. This implements `SYSCALL_REPLY_AND_CONSUME_REPLY_CAPABILITY`'s
one-use semantics even on failure. PID-based reply selection still only takes a
matching live reply; callers disposing a selected deferred reply use `replyCap`.

Policy-authorized cspace edits can occur concurrently. Reply save/retirement
therefore use the target process's mailbox lock, like policy edits and
inspection. Explicit retirement drops the caller lock before taking the peer
lock. PID-based selection acquires both mailboxes in PID order, then drops the
caller lock before completion. Only the peer mailbox remains locked entering
`completeReplyLocked`; no extra lock crosses its scheduling handoff.

Shared-address-space threads are currently rejected by process creation. If
shared cspaces/request state are introduced, they must use the same logical
owner's sequence and lock, not independent counters or per-thread locks.

## Evidence and limits

Hosted ID tests cover 10,000 consecutive IDs from zero, the last two IDs, and
repeated rejection after exhaustion. All four SPARK checks discharge, including
the successor-or-exhausted postcondition and absence of runtime errors. Returning
a value avoids an out-parameter discriminant constraint issue; no caller guard,
assumption or SPARK-Off exception was introduced to hide that issue.

The capability-policy gate discharges 119 checks, including exact retirement
and bookkeeping preservation. These are sequential state properties. They do
not constitute a proof of lock ownership, concurrent cspace policy, native
syscall integration, queue matching or the entire kernel.

The native `async-ipc` fixture covers the first-two-request collision, caller
death, failed delivery/double use, non-reply preservation and same-slot reuse,
alongside existing completion pressure, reply ordering and server-death tests.
Red/green development isolated the fixes: first the barrier failed, then after
the ID fix only the slot-reuse check failed, then both passed.

The native desktop adversary also submits an indefinite wait, malformed poll
and destruction on one async lane. It checks distinct request IDs and exact
token/reply association, waiter resolution before the destroy acknowledgement,
no duplicate completion, and a new timed wait on the reused channel. The host
watchdog bounds a lost-reply failure; the test does not guess installation time
with a delay.

```sh
nix develop -c make -C kernel test-ipc-request-ids prove-ipc-request-ids
nix develop -c make -C kernel prove-capability-policy
nix develop -c make -C kernel ipctest-server ipctest-client
nix develop -c tests/headless/run.sh --test async-ipc --accel kvm --cpus 4 --timeout 30 --keep-logs
```

CI runs the ID test/proof and the native lifetime regression against the focused
security image with two-CPU TCG emulation. That exact configuration passed
locally, as did four-CPU KVM and the expanded native desktop protocol gate.
Single-CPU KVM also passed, exercising the direct reply/context-switch path
without leaving an extra caller mailbox lock held.
The final kernel SPARK legality check and hosted locking/reclamation suites
also passed (including 400,000 lock increments and 10,000 concurrent process
lifetime rounds). These suites do not exhaustively test concurrent cspace edits.
The final four-CPU input-stream gate passed with 6 client presentations and
138 input requests, unchanged from the previous regression interval. This
checks repaint/IPC counts, not end-to-end latency.

Remaining work includes systematic reply/policy/death interleavings, the rest of
the cspace reader/writer audit, input queue serial exhaustion and the bounded
asynchronous commit/release state machine. These changes are not a latency proof.
