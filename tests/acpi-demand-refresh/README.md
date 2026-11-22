# Reader-driven battery freshness

```
export TMPDIR=/home/doc/cubit-build-tmp
nix develop -c env TMPDIR=/home/doc/cubit-build-tmp bash tests/acpi-demand-refresh/run.sh
```

An authenticated native battery measurement read that finds the copied inventory
Too_Old may move its reply into an eight-entry bounded pool. The native owner
queues one fixed battery-family sample and answers that same request only after
one attempt/publication, at the original quiet boundary. No method, node, context,
clock value, provider or address comes from the wire. Sampling retains the existing
shared fuel/deadline, receipt fanout/ACK, provider-idle, result cleanup, context,
source-health and suspended/stopped-family guards.

The minimum demand interval is1,000,000 native microseconds. Concurrent eligible
reads coalesce without resetting that interval or restarting the sample. Clock
loss/regression, counter exhaustion, pool capacity and failed reply saving cannot
start new work. Save occurs before committing the metadata-only demand; a failed
commit retains the moved token for one unavailable reply. A caller-gone answer
consumes its authority once. An unsafe stopping reply prevents owner retirement.
There is no timer and no retry until a caller explicitly requests again.

Fresh cache reads remain immediate. Timestamp-only pages remain descriptive and
do not acquire AML. Auxiliary generation errors stay errors; after a refresh the
original expected cache generation is checked again. Rate-limited reads can remain
Too_Old. Age0 is truthful even when the completed sample is already too old by the
reply turn; the server never loops until age0 succeeds. Request admission records
the actual observed clock into the original family owner, so regression before
collection is rejected before opening another provider window.

The standalone checked/release tests use the actual limiter, codec and native
saved-reader queue with explicit cache/scheduler/request/endpoint/IPC models.
The supplemental joined helpers drive the actual native owner Advance callback,
root/runtime/interpreter, battery collector, endpoint and SDK IPC against modeled
platform/kernel/wait/clock boundaries. A real AML BST counter and a capacity change
without Notify verify fresh-read inactivity, eight-reader coalescing, same-call
fresh data, cooldown, age0, no automatic retry, and full stopping reply ownership.
Clock loss, reversal after admission and overflow take the existing guarded cached
shutdown path in both AML widths. Native execution and hardware acceptance remain
separate coordinator gates.

Scoped SPARK evidence covers the pure limiter's native-default instance only:
state validity/nonzero accepted ticket postconditions, data/initialization flow
and termination. It does not prove clock trust, owner composition, AML acquisition,
transport, freshness or hardware. The rejected generic-only/no-check run is not an
accepted proof. Exact artifacts and counts are named in the packet receipt.
