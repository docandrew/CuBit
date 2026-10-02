# metrics.svc hosted tests and proofs

```sh
nix develop -c bash tests/metrics/run.sh            # tests + SPARK level 1
nix develop -c bash tests/metrics/run.sh --no-prove # tests only
```

Linux-hosted. Peer PIDs and authority tags are supplied by the harness, not by
kernel IPC; grants and the service loop are not exercised. See
[the design](../../docs/metrics-service.md).

Covered by regression tests (`main.adb`):

- record codec round trips for every kind; rejection of unknown kinds, keys
  0/33, bad declared kind/unit, invalid or embedded-zero names, nonzero
  reserved words and reversed spans; 200,000 pseudo-random slots, where every
  accepted slot must re-encode to identical words (canonical encoding);
- batch header validation (magic, count, sequence 0/max, clock, reserved,
  length mismatch);
- two-page producer batching: full-page and both-in-flight drops are counted
  and reported in the next header, sealing and completion order, sequences;
- store: declarations, latency/counter/gauge/span aggregation, p50/p90/p99/
  p99.9 as bucket upper bounds, wrong-kind/undeclared/conflicting records,
  isolation between publishers and between a reused PID with a new tag,
  sequence gaps, rejected replays, malformed batches allocating nothing,
  source-table exhaustion, lease-based eviction, multi-page summary paging;
- authorization matrix (publisher/observer/zero/log tags).

Proved (GNATprove `--level=1`, 0 unproved): absence of run-time errors in
`CuBit.Metric_Records`, `CuBit.Metric_Batches`, `Metric_Histograms`,
`Metric_Store`; functional contracts for slot writes, decode validity,
header length agreement, the batcher's state machine (`Consistent`,
accept/drop and seal/complete outcomes), histogram count/min/max, and
`Metric_Store.Ingest`'s ghost `Same_Except`: a publication changes no source
except the one owned by the caller's (PID, tag) — an idle source may be
evicted and replaced by the caller's. Mutation check: writing another
source in `Ingest` makes that postcondition unprovable; a no-op control
mutant still proves.

Not proved: percentile accuracy beyond bucket bounds, codec round-trip
(tested), the native service loop/grant adapter (`main.adb`) and client
(`CuBit.Metrics`), kernel IPC authentication.

`procmgr-metrics-issuance.patch` is the candidate procmgr/authority-policy
change requested from the procmgr owner (see `coordination/observability.md`).
