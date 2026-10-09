# Checked archive loading and timeline geometry

Run from the repository root, choosing a new private output directory:

```sh
nix develop -c python3 tests/observatory-metrics/archive-io/run.py \
  /tmp/cubit-archive-io-check --prove
```

The runner snapshots sources, records hashes and commands, and checks the
source hashes again before declaring success. It builds outside the checkout.
`--toolchain-root` can select a separate checkout containing the Alire toolchain.

## Coverage

The stream suite checks 42,841 cases: the real 78-event native capture,
byte-at-a-time input, pages, every truncated footer length, the maximum
4096-event archive, extra bytes, and plot widths 1 through 512 with full-width
timestamp arithmetic. No page becomes ready before checked EOF.

The reader suite checks 303 assertions across normal fragmented reads and
fault scenarios: grant creation/submission denial, malformed or stale replies,
timeouts including stalled retirement, overcounted reads, incomplete input,
invalid close replies, and the real two-word filesystem Open reply shape.
Short positive reads continue; only a zero-byte read establishes EOF. The open
size is not trusted as EOF. Close acknowledgement precedes exposing a page.

The optional deadline parameter retains the existing 250,000 us live-metrics
default. Archive operations explicitly use 2,000,000 us each, with saturated
addition. This is a bounded per-request deadline, not an overall load deadline
or a performance target. The archive itself is capped at 1,049,088 bytes.
Existing live-metrics adapter tests separately passed 1,069 assertions with
this change (private evidence 244).

## Proof and foreign boundary

The stream, plot and query-lifetime units are SPARK On. Scoped proof covers
87 checks (15 flow, 72 prover, zero unproved): readiness gating, bounded stream
state, interval containment, bars staying within the plot width, saturated
deadlines and the declared lifecycle contracts. See `proof.txt`.

The archive-reader specification is SPARK On; its body is SPARK Off because
it manages volatile foreign-written memory and runtime IPC/grant calls.
The body uses one aligned 4096-byte transfer page and one outstanding request.
It consumes bytes only after a matching validated reply AND independently
confirmed grant retirement. Failure quarantines the instance; a late reply
cannot permit reuse. Revoke/retirement is still polled after failure.
An uncertain open handle is left for process-exit cleanup; this adapter does
not promise explicit close after an ambiguous or timed-out operation.

The file is read-only and fixed at `@nvme:0/work/desktop-trace.cubittrace`.
Capability enforcement, service honesty, actual memory visibility and process
cleanup are runtime assumptions, not proved by these units. Hosted mocks test
control flow, not the native filesystem ABI or whole-kernel lifetime safety.

## Native evidence and limits

A separate private native CuBit viewer used these exact policy/reader sources
with the Mesa software compositor. Run 242 passed page 0/1/0, event selection,
reload, and exact returned-page pixel restoration. Run 243 opened the actual
4096-byte capture left by the interrupted native writer (221): no ready page
or timeline bars appeared, and reload retained the incomplete-file warning.
The native Open response is length two (handle, size); tests reject the old
one-word assumption. Evidence paths/hashes are recorded in `evidence.json`.

These QEMU runs are functional evidence, not hardware performance measurements.
The viewer application has a standard `trace-viewer` build target and a
development Apps-menu entry; USB/laptop capture-storage integration remains open. Completion intervals mean recorded submission to collection,
not display latch or physical photons. Missing events remain explicit loss;
there is no inferred cross-event causality or CPU-stack flame graph data.
