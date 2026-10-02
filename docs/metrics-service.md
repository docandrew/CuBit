# Metrics service, typed logging catch-up and tracing plan

Status: implementation plan plus first slices, 2026-10-01. Architecture and
phase gates are in [observability streams](observability-streams.md); this
document records what is being built against it and what exists. Each claim
below is labelled **proved** (SPARK, stated level), **hosted** (Linux-hosted
regression test) or **native** (CuBit under QEMU). Anything unlabelled is plan.

## Implementation plan

### (a) logsvc (logstore) catches up

Today logstore accepts one 544-byte UTF-8 text record per IPC (`CuBit.Log_Records`),
stamps authenticated PID, issued publisher tag and collector milliseconds, and
fans out to at most eight observers through `Log_Fanout` (512-record queues,
30 s lease, explicit `Gap`). Publication shares one bootstrap credit pool.
Gaps against what users now need:

1. **Structured fields.** Add LogRecord v2: severity, producer timestamp, a
   message template plus up to eight typed fields (integer, unsigned, duration
   in microseconds, boolean, bounded text, metric key reference), each with a
   short identifier name. Field types are a closed enum; CCL sees a record of
   typed values (the Observatory/REPL renders them, filters by field), not a
   string to re-parse. v1 text records stay valid: a v1 record is a v2 record
   with no fields. No compatibility shim beyond that one decode path.
2. **Batching.** Publication carries a page of records per IPC, like metrics
   below, so a busy service does not pay one round trip per line.
3. **Filtered subscriptions.** Observer subscriptions name a minimum severity
   and optional source tag set; filtering happens in the service so a watcher
   does not copy everything. Observer authority stays explicit (log-observer
   role); per-topic observer scopes come with the startup supervisor.
4. **Retention/export off serial.** A bounded in-memory history plus an
   explicit save operation writing a chunked, checksummed file through the
   filesystem service into an approved diagnostics directory (shared design
   with the metrics capture archive). Serial echo becomes optional.
5. **Per-source budgets.** Replace the single bootstrap pool with issuer-chosen
   pools per declared source class (needs procmgr/startup issuance change).

### (b) metricsvc: typed metric events

Service `metrics.svc` (source `userspace/services/metricsvc`). Producers are
latency-sensitive (compositor, desktop, drivers), so the producer path is:
append a fixed 64-byte record into a private page (no IPC, no syscall, no
allocation), and submit the page asynchronously when full or at a natural
boundary (frame end). Two pages alternate: one fills while the other is in
flight. If both are busy the record is dropped and a saturating local counter
increments; the next batch header reports it. Producers never wait.

Record kinds (closed enum, wire codes are named constants):

| Kind | Meaning | Aggregation in metricsvc |
| --- | --- | --- |
| Describe | binds a key (1..32) to a kind, unit and 1–32 char name | series metadata |
| Counter_Add | monotonic increment | saturating total |
| Gauge_Set | instantaneous value | latest value, min/max, histogram |
| Latency | one duration sample | log histogram, min/max/sum, percentiles |
| Span | start/end timestamps plus correlation id | duration histogram; end < start rejected |

Every record carries the producer's timestamp (monotonic microseconds) and an
optional correlation id (input sequence, frame id) so spans can later be joined
into causal chains. Batch headers carry a strictly increasing batch sequence:
a jump is recorded as an explicit batch gap, a repeat or regression is rejected.

Identity and authority:

- A publisher is identified by the kernel-stamped sender PID **and** its
  procmgr-issued publisher tag. Tags are distinct per issuance and never wrap,
  so PID reuse cannot join a new process's data to an old series.
- Observing requires a distinct observer tag (metrics-observer role, startup or
  explicitly approved, like log-observer). Publisher tags cannot query, so
  producers cannot see each other's data. Knowing tag numbers grants nothing:
  the kernel stamps them from the capability.
- Capacity: fixed maximum sources and keys per source; a full table returns
  `Exhausted` (an idle source past its lease may be evicted first). These limits
  become typed CCL launch parameters of `metrics.svc` (bounded above by the
  compiled maxima) once the startup supervisor passes launch values.

Watcher queries are batched too: one `Query_Summaries` call fills a caller-owned
writable page with up to 16 series rows: source PID and tag, key, kind, unit,
name, count, min, max, p50/p90/p95/p99/p99.9 (as histogram bucket upper bounds,
never invented precision), sum/total/latest, sample saturation and sum overflow,
rejected records, batch gaps and producer-reported drops. A cursor pages through
all series.

Later in (b): a raw event subscription (overwrite-oldest ring with per-watcher
sequence cursors and explicit gap results), CCL interface `metrics.summary` so
the REPL/Observatory/Workbench gets typed records and charts, a capture save
operation shared with (a), and the fixed-word snapshot transport of the
architecture document for rates beyond page-per-IPC.

### (c) Tracing plugs in later

- Spans are already a metric record kind with correlation ids; the compositor's
  `Compositor_Source_Trace` drain becomes a metrics producer adapter (graphics
  agent owns those files) instead of a serial exporter.
- Kernel `Trace` (512 events/CPU, histograms) exports through the same record
  vocabulary once a scoped kernel observation authority and read-only export
  exist (kernel owner, phase 3 of the architecture document). metricsvc then
  becomes the collector for kernel lanes too, with CPU identity as the source.
- The Perfetto/Chrome-trace exporter consumes the capture archive offline.

## Delivered slices

### Slice 1: typed metric records, aggregation and authorization core

- `userspace/runtime/gnat/cubit-metric_records.ads/.adb`: wire format, batch
  header, record validation, name charset; shared by producers and service.
- `userspace/runtime/gnat/cubit-metric_protocol.ads`: operations, statuses,
  tag classification, `May_Invoke`, summary row layout.
- `userspace/runtime/gnat/cubit-metric_batches.ads/.adb`: producer-side
  two-page batch builder (append without IPC, explicit drop accounting).
- `userspace/services/metricsvc/metric_histograms.ads/.adb`,
  `metric_store.ads/.adb`: bounded source/series tables, ingestion,
  histograms, summary rows.

### Slice 2: native service, client and acceptance app

- `userspace/services/metricsvc/main.adb` (`metrics.svc`, `-gnatp -O3`):
  authenticates peer PID and tag, gates with `May_Invoke`, copies the
  producer page privately, returns acquisitions before replying.
- `userspace/runtime/gnat/cubit-metrics.ads/.adb`: `Publisher` (Put,
  Has_Room, Flush, Complete, Disconnect; two page-aligned granted pages) and
  `Observer` (Query).
- `userspace/apps/metrics-check`: publishes 1,000 latency samples, a counter
  and a span, queries summaries, and checks publisher-cannot-observe,
  observer-cannot-publish and malformed-batch refusal.

### Slice 3: logsvc structured fields and filtered subscriptions

- LogRecord v2 typed fields in `CuBit.Log_Records` (see
  [typed logging](typed-logging.md#structured-fields-format-version-2)).
- Severity-filtered subscriptions in `Log_Fanout`/logstore/`CuBit.Logging`.
- `userspace/apps/log-fields-check`: native acceptance app.

## Status (kept current)

| Item | Proved | Hosted | Native |
| --- | --- | --- | --- |
| Metric codec, batcher, histograms, store (incl. isolation) | level 1, 161 checks, 0 unproved | `tests/metrics` 24,340 checks | compiled only |
| metrics.svc adapter, `CuBit.Metrics` client | no | no | built and linked (`make metricsvc metrics-check`); end-to-end blocked on procmgr tags |
| LogRecord v2 fields | level 1, 104 checks (run-time safety) | `tests/log-fields` 29,923 checks | headless `log-fields` PASS (TCG, 4 CPU) |
| Filtered subscriptions | `Log_Fanout` level 2 (existing suite) | `tests/log-fanout` | headless `log-fields` PASS |

Native results are recorded in `coordination/observability.md` as they land.

## Requests outstanding

- **procmgr (graphics agent / CCL agent startup work):** issue per-launch tags
  for two new service roles, `metrics` (publisher) and `metrics-observer`,
  exactly as log publisher/observer tags are issued today (observer startup-only
  or explicitly approved). Without this, metricsvc sees tag zero from a generic
  service endpoint and denies everything: native end-to-end needs it.
- **Catalog:** add `(service metrics 24 read-write)`,
  `(service metrics-observer 25 read-write)` and fixed bindings. Additive only.
