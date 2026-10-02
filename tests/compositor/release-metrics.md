# Typed software presentation metrics

`Compositor_Release_Metrics` converts a validated frame-trace value into a
typed metrics span without allocation, IPC, formatting or a clock read. The
caller supplies producer timestamps. The metrics-enabled Desktop publisher
now uses this conversion after validated display completion.

| Output | Key | Name | Kind and unit |
| --- | --- | --- | --- |
| 0 | 1 | `desktop.out0.submit_release` | Span, microseconds |
| 1 | 2 | `desktop.out1.submit_release` | Span, microseconds |

Start and end are the existing submission and validated software completion
timestamps. Correlation is the Desktop-wide non-reused frame/request token.
The publisher identity and output key provide the other correlation scope.
Output reopen must retain the shared request allocator; resetting that allocator
within the same publisher lifetime would invalidate this uniqueness argument.
The conversion rejects zero frame/session identities, unavailable clocks and
end-before-start. Equal timestamps and zero timestamps remain valid readings.

No input watermark is involved. This measures software submission-to-release,
not input response, vblank latch, scanout or photons. The current metrics store
aggregates durations and does not retain raw correlation events. Raw trace
export remains a separate requirement.

The level-2 SPARK proof discharged 18 checks: 13 runtime checks, two functional
contracts and three termination checks; zero unproved or justified. Contracts
establish valid declarations, output keys, exact timestamp/correlation fields
and the same admission predicate as the frame trace. They do not authenticate
the caller's completion or prove kernel/service behavior.

```sh
nix develop -c gprbuild -q -p -P tests/compositor/release_metrics.gpr
nix develop -c tests/compositor/build/release-metrics/release_metrics_tests
nix develop -c gnatprove -P tests/compositor/release_metrics.gpr -u compositor_release_metrics.adb --level=2
nix develop -c gprbuild -q -p -P tests/compositor/release_metrics_store.gpr
nix develop -c tests/compositor/build/release-metrics-store/release_metrics_store_tests
```

Hosted tests cover 20,000 real wire-codec round trips, exact names and keys,
invalid identities/clocks and near-maximum timestamps. Actual metrics-store
tests admit both declarations and samples, verify distinct per-output duration
summaries, and retain a series across output reopen with fresh frame tokens.
These use fixture-supplied identities and private pages, not native IPC.

The lease-pressure test fills all 16 source slots, advances past the service's
60-second idle lease and forces eviction. A subsequent undeclared sample is
rejected. Repeating declarations restores successful ingestion under the same
publisher identity. Therefore the production adapter must make each batch
self-describing (declarations followed by samples), rather than relying on a
single startup declaration or assuming collector state survives idle periods.
This also protects against the initial declaration batch being lost. The test
establishes this integration requirement; the adapter now follows it.

The [batching policy](metric-batch-policy.md) supplies the bounded declaration
order and flush decisions; its actual-page hosted test covers overload and
out-of-order release without adding another record queue.

Evidence, 2026-10-01: policy/test/proof session 39272 completed with exit 0,
`/tmp/cubit-release-metrics-policy-v2.log`; actual-store session 70816 completed
with exit 0, `/tmp/cubit-release-metrics-store-v2.log`. Initial compile 67495
failed due to missing visibility of the byte-addition operator; the corrected
source is the proved version. No runtime sources or staged binaries changed.

The metrics-enabled Desktop now has bounded completion routing, self-describing
batches, a generated manifest binding and native observer/malformed-collector
gates; see [Desktop integration](desktop-metric-publisher.md). The general SDK
fix is still pending, with defective paths guarded in Desktop. Default startup
rollout, broader overload/collector recovery, hardware presentation timestamps
and supported-hardware performance measurements remain outstanding.
