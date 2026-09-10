# Network authority regression

Known harness limitation: guest serial writes from different services can
interleave inside a readiness marker. One software-emulated run on 2026-09-08
reached inbound round 3 but split `network-check: inbound ready 3` with a TCP
diagnostic, leaving the peer waiting until the test timeout. The harness fails
closed rather than counting this as a pass. Follow-up: use structured/atomic
test readiness reporting so concurrent diagnostics cannot corrupt coordination.
The subsequent KVM run passed all guest and host-peer checks; the serial
interleaving limitation itself remains open.

2026-09-09: the expanded async suite and `async-ipc` passed under KVM. A
separate real-NetSurf check fetched two local HTTP pages (the first used a
meta refresh to the second); the same binary without boot approval rendered
the explicit "Network access not granted" error. A test-only
`CUBIT_NETSURF_HOMEPAGE` bypassed address-field input issues; the normal browser
startup image was restored afterward. These browser checks were manual,
not an automated browser regression in this runner.

The local NetSurf port is git-ignored. Its `netsurf-fetch-cubit.c` denial path
and `libnsfb-cubit.c` direct one-way submission both changed in this migration
and need inclusion in the separate port backup. Audit direct syscall callers
with `rg --no-ignore`, not just the tracked source tree.

Run from the repository root, using the Nix/Alire toolchain:

```sh
nix develop -c make -C kernel test-network-authority prove-network-authority
nix develop -c make -C kernel cubit_kernel user_runtime network-check capability-test procmgr devmgr netstack
nix develop -c tests/headless/run.sh --test network-authority --timeout 90 --keep-logs
```

The hosted test checks scope encoding/validation, CIDR and port boundaries,
direction, DNS permission, containment, owner/tag separation, stale tags and
bounded-table exhaustion. Its runtime assertions are Linux-hosted only.
Focused GNATprove checks the scope and grant ADTs, not the complete netstack.

The headless test uses a temporary copy of the ext2 disk and an alternate init
profile. It launches the same application without and with explicit boot
approval, exercises denied and accepted bind/close operations, forged policy
tags, configuration denial, outbound subnet/port/DNS restrictions and stale
shared-memory generations. An approved connection to 10.0.2.2:18443 exchanges
PING/PONG with a **loopback-only** host TCP peer. Twelve connection lifetimes
split between six synchronous and six asynchronous open/write/read/close
sequences. Async checks preserve full-width completion tokens, deliver the
fourth payload word (grant generation), reject stale grants and verify one
completion per request. The pending-accept test explicitly checks that no
completion exists before listener close, so an early rejection cannot pass
as successful deferred cancellation. The twelve lifetimes
exercise channel/connection reuse and grant-acquisition release. Stale handles
cannot write or close their replacements, and a full-width invalid handle is
rejected without narrowing to an array index. No internet service is used.
It also verifies that retired syscall 23 and an authorityless `capSubmit` fail.

The same test now forwards **only** `127.0.0.1:18444` to the guest's admitted
`10.0.2.15:8080` endpoint. Four native accepts verify split request writes,
buffer retention, EOF after peer half-close, response after half-close, and
accepted-channel survival of listener close. Direction/tag and stale-grant
denials precede IO. An idle accept deliberately waits its 30-second deadline;
duplicate pending accepts are rejected, listener close completes the original
accept exactly once, and two never-accepted connections expire in the backlog.

Both local ports 18443 and 18444 must be free. This is a local functional and
authority regression, not evidence of internet readiness, complete TCP
conformance, or owner-death reclamation. The test includes real timeouts, so use
90 seconds rather than the old shorter outbound-only budget.
