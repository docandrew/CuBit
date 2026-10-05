# Conditional endpoint disposal

The production `Capabilities.Endpoint_Disposal` package conditionally clears
one exact endpoint record. It does not authorize a caller, lock a process,
revoke derivations, drain messages, or establish GPU quiescence. No syscall
currently exposes it. A future wrapper must perform those applicable checks
under the process mailbox locks before calling it.

Run in Nix:

```sh
gprbuild -q -p -P tests/endpoint-disposal/disposal.gpr
tests/endpoint-disposal/build/disposal_tests
gnatprove -P tests/endpoint-disposal/disposal.gpr \
  -u capabilities-endpoint_disposal.adb --mode=all --level=2 --report=all -j2
```

The hosted fixture supplies only Config's actual 64-slot table bound, avoiding
the kernel serial dependency. The real capability types and disposal body are
compiled directly. 1,024 cases cover all slots and mutations of every record
field, invalid expectations, already-empty slots, repeated cleanup, and stale
cleanup against fresh-tag replacements. Successful removal preserves all other
slots; unsuccessful removal preserves the entire table.

On 2026-10-02 the suite passed and SPARK proved the exact postcondition and
flow/initialization/termination checks: four results, none unproved or justified.
Separately, compilation with `kernel/cubit.gpr -c -u` passed using the actual
kernel configuration. These are hosted tests, pure sequential proof, and native
compile evidence—not a kernel syscall or concurrent execution test.

The comparison cannot detect reinstalling the identical capability (ABA).
Issuance tags must therefore be fresh and nonwrapping before slot reuse is
enabled. Empty slots return failure, not fabricated cleanup success; callers
need to retain their successful-disposal receipt for idempotent protocols.

## Private native wrapper experiment (2026-10-02)

In `.build-workspaces/graphics-admission-reply-qmsr03hg` only, experimental
syscall127 wraps this primitive. Its six scalar arguments describe destination
incarnation, slot, expected endpoint incarnation, tag, rights and parameter.
It validates ranges, takes caller/destination mailbox locks in ascending PID
order, checks destination liveness/incarnation and a scoped/root CSPACE with
RIGHT_REVOKE, then compares and clears the exact endpoint. Endpoint-target
liveness is deliberately not required: cleanup must still match a stale
endpoint's original generation after that process exits. This is not a
published ABI; the main syscall/runtime files remain unchanged.

The first private run85377 correctly rejected fixture bootstrap: production
devmgr/procmgr CSPACE authority has GRANT, not REVOKE, so the kernel rejected
amplification. The follow-up fixture explicitly supplied REVOKE through its
private kernel/devmgr bootstrap and limited the test client's REVOKE to its
own CSPACE. Its driver-scoped CSPACE retained GRANT only.

Run74655 exited0 with `GPU-DISPOSAL-IPC PASS exact cleanup and stale rejection`,
all admission regressions, baseline async-ipc and final fault scan. Logs are
`tmp/disposal-authorized-{boot,serial}.log` in that workspace. It exercises
wrong tag/rights/object parameter, stale endpoint/destination generations,
grant-without-revoke denial, exact cleanup, repeated cleanup rejection, and
late cleanup after installation of a replacement with a different tag.
This is native CuBit on QEMU with a synthetic GPU controller, not real GPU
quiescence, concurrent-race proof, issuer-counter nonwrap, or session reuse.

Follow-up run41485 exited0 after explicitly rebuilding `make -C kernel
ipctest-client`. Fifteen additional scalar-boundary cases rejected zero or
generation-less identities, out-of-range PID/slot values, zero tags, and
unknown rights bits. Each rejection was followed by inspection and comparison
of all six endpoint-description words, rather than trusting the error return
alone. Both disposal markers, all admission markers, async IPC and the runner's
final fault scan passed. Logs: `tmp/disposal-boundaries-built-{boot,serial}.log`;
build log: `tmp/disposal-boundaries-build.log` in the same private workspace.
This remains sequential boundary coverage, not a concurrent-race test.

The preceding run71037 passed only the old regression: the headless runner
reuses staged IPC applications. Its `disposal-boundaries-*` logs must NOT be
used as evidence for the new cases. Always build the IPC application first
and require the `PASS 15 scalar boundaries preserve endpoint` marker.

Build provenance caveat: an unrelated bulk edit reached this private snapshot
and changed CCL inputs after creation. The changed `ccl-types.adb` and
`config_read_outcomes.adb` copies were preserved under
`tmp/source-drift-preserved/`; both build inputs were restored to their exact
original manifest hashes before successful build94271. Unrelated modified
CCL test files were left intact and are not part of this native fixture.
Initial60360 failed enum representation ordering; corrected privately.
Build15390 then failed CCL warning-as-error checks; no warning suppression was
used. Promotion requires coordinated review of the shared ABI and explicit
bootstrap policy, not copying the private manager/kernel wholesale.

### Two-process contention regression

Private run8325 exited0 on four-CPU TCG with 32 fresh endpoint installations.
After waking the client with a reply, the synthetic server and client each
attempt exact disposal of the same client slot. Every round required one
success and one rejection, then checked that the endpoint was absent. A
remote-only positive control first required successful server cleanup, so an
unauthorized server could not produce a false single-winner PASS. Earlier
local-only and stale-replacement checks also remain in the run.

The private manager supplies the server with REVOKE scoped to each test
client's CSPACE, choosing an empty slot56..58. Client authority over the server
remains GRANT-only, preserving the negative authorization control. This is
test-only authority, not a proposed production bootstrap policy. The fixture
preserves the delegated endpoint's original broker-PID object parameter;
the kernel still compares all fields exactly.

Logs in the private workspace: `tmp/disposal-contenders5-{build,boot,serial}.log`.
All admission regressions, async IPC and the final fault scan passed. This
exercises self/cross-process locking paths but does not establish simultaneous
lock contention, all scheduling interleavings, process-exit races, or GPU drain.
The snapshot predates the main tree's stored-session-tag migration.

Failed fixture runs are retained:34757 misread empty-cap inspection semantics;
69841/81417/27688 used the wrong endpoint parameter. They are not kernel
concurrency failures or passing evidence. The remote-only control and exact
parameter diagnostics exposed the setup mistakes without weakening the kernel.

### Current stored-tag integration

Run59148 subsequently rebuilt both IPC applications with the main tree's
stored-tag registry and control units, then passed the same native suite,
including remote-only cleanup, all 32 single-winner rounds, all four admission
paths, and the final fault scan. The four registry/control files compare
byte-for-byte with the main tree after the run. Evidence is in
`tmp/disposal-stored-tags-{build,boot,serial}.log` in the private workspace.
This supersedes the older-registry limitation for this particular run, not
for the earlier logs. It does not enable slot recycling or promote syscall127.

### Captured runtime wrapper (private)

Run31557 exited0 after rebuilding the private runtime and IPC applications;
the final native async-IPC fault scan passed. The snapshot now names syscall127
and provides `Capability_Grants.Endpoint_Snapshot`, `Capture_Endpoint` and
`Dispose_Endpoint`. Main runtime/ABI files remain unchanged pending ownership
handoff. These are correlation snapshots, not authority or delegation receipts.

The wrapper captures the endpoint's object incarnation, parameter and source
rights; callers retain the separately captured destination process incarnation,
selected destination slot, attenuated rights and fresh session tag. It rejects
invalid snapshots/targets, excess rights and zero tags before invoking the
kernel's exact-match disposal. Capture is not atomic with delegation: the
selected source slot must remain stable through the delegation call.

The native test installed an auxiliary source, captured it, delegated a target,
removed the source, and successfully disposed the target using the retained
snapshot. Invalid snapshots, a CSPACE source, stale tags and repeated disposal
were rejected. The original 15 boundary checks, remote-only positive control,
32 two-caller rounds and all four admission paths also passed. Logs:
`tmp/disposal-wrapper3-{build,boot,serial}.log` in the private workspace.
Earlier wrapper builds94901/84252 failed runtime style checks and are not
passing evidence. No production REVOKE authority or storage recycling is enabled;
this test establishes neither GPU drain nor exhaustive concurrent safety.

### Private registry reclamation prototype

After the native wrapper run, the snapshot's render-session registry diverged
from main again. It now has a dispatcher-only `Reclaim_Retired` prototype:
exact sender/tag, retired phase, no quarantine, unique tag, and all eight
retirement facts with no uncertainty. Accepted reclamation clears only that
record; it never resets the tag issuer. A subsequent reservation can reuse the
free record with a strictly newer identity. Main does not expose this operation.

Run97958 passed 1,024 record reuses and 523,264 incomplete-fact rejections,
plus stale close/finalize/reclaim isolation, wrong sender, active rejection,
quarantine, injected duplicate metadata and issuer exhaustion after reclamation.
Selected registry SPARK analysis reports43 results (9 flow,34 prover), zero
unproved/justified. The initial run46004 left one reclamation postcondition
unproved; explicit duplicate-tag rejection and its loop invariant resolved it.
Run55188 passed the existing registry/controller hosted regressions and native
Intel registry compilation.

This is NOT a caller-integrated resource retirement implementation: the trusted
facts still need actual endpoint-disposal receipts, GPU/alias/backing retirement,
broker completion drain and per-slot state reset. No application may supply
those facts. No claim is made that the kernel/hardware observations are proved.
The native wrapper run31557 predates these new registry changes; do not treat
its logs as a native reclamation test. The source and tests are private under
`tests/intel-gpu/session_reclamation.gpr`; proof log is
`tmp/session-reclamation-proof2.log` and report is in that project's object dir.

### Private controller reclamation

Run10814 adds a dispatcher-only controller entry point around registry reclaim.
It requires the original generation32/PID32 recipient identity to match the
stored association, delegates all retirement checks to the registry, and clears
that recipient association only on successful reclamation. No new IPC operation
accepts these facts from applications or brokers.

Hosted tests pass 128 successive process generations using the same PID and
reused record. They reject reclamation of reservations/active sessions, bare
PIDs, mismatched generation/PID, missing endpoint cleanup, repeat reclamation
and quarantine. Late activation, abort and reclamation of the old identity do
not affect the replacement. Selected control proof reports35 checks (14 flow,
21 prover), zero unproved/justified. Run24376 passes the existing controller
regressions and native control-unit compilation. The new test project is
`tests/intel-gpu/control_reclamation.gpr`; proof log is
`tmp/control-reclamation-proof.log` in the private workspace.

Production remains unchanged. In particular, main's resource owners and GuC
context-ID tombstones still need coordinated retirement/reset; controller
success must not be fabricated from the existing quiescence-only reply.

### Native reciprocal admission/reclamation fixture

Run29885 exited0, including the final async-IPC fault scan. A synchronous client
completed32 reserve/delegate/activate/status/abort/cleanup cycles against the
synthetic native service. Each reservation returned the actual driver recipient
slot; both reciprocal endpoints were installed by the kernel with a fresh tag.
The server required successful exact disposal of the application endpoint and
its own recipient endpoint before setting `Endpoints_Disposed` and reclaiming
the controller record. The client checked its endpoint was absent and the next
tag increased. This crosses the16-lifetime limit in the **private fixture**.

The private manager additionally gives the synthetic server CSPACE REVOKE
scoped to itself in slot61; it retains the earlier separately client-scoped
cleanup grants. This is test-only policy, not production authority. The server
does no GPU work: other resource-retirement facts are explicit synthetic facts,
and synchronous IPC avoids a broker completion ledger in this test. Consequently
this does not remove the main driver's lifetime limit or establish safe reuse
of hardware context IDs, backing, aliases or asynchronous broker records.

All earlier admission, scalar boundary, captured-wrapper and32 two-caller tests
also passed in this run. Logs in the snapshot:
`tmp/session-reuse-native-{build,boot,serial}.log`.
