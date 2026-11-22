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

## Raw history query (2026-10-08)

`Query_Raw` (0D02) is an observer-only pull operation over a fixed 256-record
service history. Accepted records, including declarations, retain the kernel
publisher PID/tag, source batch sequence, producer-drop count, source batch gaps,
and original metric slot. Rejected records and replayed batches append nothing.
History overwrites the oldest entry without waiting for readers. Sequence exhaustion
stops history appends and counts those losses; aggregate ingestion continues.

Requests are four words: next desired sequence (initially 1), writable grant slot,
grant generation, and 4096-byte capacity. A successful response is four words:
rows written, next sequence, number overwritten before the requested cursor,
and terminal history-drop count. Zero/future cursors are invalid. At most 32 rows
are returned per request; an empty tail is successful. Each 128-byte row contains
sequence/PID/publisher-tag/batch/producer-drops/batch-gaps, two zero reserved words,
and the unchanged 64-byte metric encoding. Unused rows are zero. The service
returns its grant acquisition before acknowledging success.

`CuBit.Metric_Raw_Observer` owns one aligned page and grant. It is synchronous
collector code, never a render-path call. It captures the endpoint's process
incarnation and checks it before/after queries. Serialize use and keep the
capability slot stable during each call. A changed endpoint or uncertain/malformed
reply disables the object; retain it until `Disconnect` confirms retirement.
Reacquire a new observer and reset its cursor after service replacement. Raw
telemetry has no buffer-release or GPU-retirement authority.

Names/declarations may have aged out before a late reader starts. Preserve unknown
metadata and explicit loss; do not apply a current summary's name to older raw
records without a matching declaration. PID reuse is separated by publisher tags.
Source-store lease eviction may also remove metadata, so raw keys alone are not a
historical schema. Preserve typed numeric records even when names are unavailable.

The SPARK history/store/query policies and actual observer adapter tests are
reproducible with `tests/metrics/raw-history/run.py`; native acceptance is recorded
in its evidence directory. The native service loop, mapped-memory access, kernel
identity enforcement and foreign adapter remain outside those policy proofs.
Desktop detailed event encoding and export are implemented below. CCL event
capture/visualization and kernel event export remain separate work; aggregated
summaries alone still do not supply flame graphs or photons.

## Raw trace groups

Record kind 6 (`Trace`) carries raw diagnostic payload only. It does not
declare, create, or update a summary series. Its key is a schema identifier
(1 for the compositor trace codec), independent of ordinary metric keys.

| Slot word | Meaning |
| --- | --- |
| 0 | Kind 6 |
| 1 | Schema key, 1 through 32 |
| 2 | Nonzero producer event ID |
| 3 | Fragment index, 0 through 3 |
| 4–7 | Four payload words, preserving all 64 bits |

Four fragments contain one 128-byte compositor packet. On the publication
wire they occupy 256 bytes; this is bounded diagnostic overhead, not a
zero-copy claim. The existing 4 KiB page format and two-page grant lifetime
remain unchanged. `Metric_Batches.Append_Group` admits all four fragments
into one filling page or changes neither page and counts four dropped
records. It refuses after batch sequence exhaustion. `Metrics.Put_Group`
adds no IPC, allocation or wait. Its native adapter reports a disabled
publisher's group as four rejected records. `Has_Group_Room` is advisory
within the same serialized event loop; callers must still check acceptance.

A trusted collector obtains publisher identity and batch identity from raw
query envelopes, not the trace payload. `Compositor_Trace_Metrics.Assemble`
requires four consecutive history sequences with identical PID, issued
publisher tag, batch, schema and event ID, ordered parts 0–3, a valid full
packet, and equality between the packet event ID and envelope event ID.
The raw observer's endpoint incarnation must remain stable; discard all
partial assembly state on observer failure or reconnection. Even valid
trace payload is a producer claim, never an ownership or retirement fence.

The service retains fragments individually in its bounded 256-record raw
history. Overwrite can remove the beginning of an event, and raw query page
boundaries can split an event. A collector must retain at most the bounded
partial group, reject orphan/mismatched fragments, and expose history gaps,
producer dropped records, and source batch gaps. `Compositor_Trace_Stream` now provides that bounded streaming collector.
Call `Start` with the raw observer incarnation and requested cursor, then
`Feed` for each validated raw row in order. It retains one partial event,
works across raw-query pages, and rejects orphan, malformed, replayed or
mixed-identity rows. `Feed` never switches endpoints automatically. Call
`Discard_Partial` on query failure or capture end; archive the counters before
calling `Start` for a new capture. No IPC, allocation, waits or clock reads
occur in this policy.

Native acceptance publishes synthetic input/source/draw/submit/frame events
through the real SDK and metrics service. It keeps both SDK pages busy,
refuses 100 additional events (400 fragments), dispatches completions, and
verifies 64 exact retained events, an overwrite gap of 268 records from its
prior cursor, reported producer loss, unchanged ordinary summary series,
and confirmed publisher/observer grant retirement. Earlier 1,000-sample
summary and raw authority tests also pass. This is a CuBit QEMU functional
test using explicit kernel/initrd/allocator seeds, not a Desktop trace
callsite test or supported-hardware performance measurement.

The record codec, batching/store policy and compositor fragment/assembly
policy are SPARK. Native SDK IPC, capability/grant truth, service FFI and
clock accuracy remain outside those proofs. Evidence is under
`tests/metrics/raw-history/evidence/trace-*`; the portable runner exercises
atomic capacity boundaries, 1,000 held-page attempts, full-width identities,
mixed publishers/batches/sequences, malformed packets and original metrics
regressions. Desktop export is validated below; CCL capture/viewer integration
remains open.

### Streaming collector validation

`Capture.Success` marks a complete event; all other result fields are meaningful
only when it is true. The result carries observer incarnation, authenticated
publisher PID/tag, batch, first history sequence, producer drop/batch-gap
metadata, and the full compositor event. Identity is still supplied by the
native transport, and event contents remain producer claims. No result grants
resource ownership or establishes hardware presentation.

The collector counts skipped history rows, rejected rows, abandoned partial
events, emitted events and endpoint mismatches. These are different scopes:
skipped rows include unresolved sequence positions after malformed input;
rejected rows include replay/orphan/malformed data; neither is a count of lost
complete events. Entirely overwritten events cannot be counted exactly from
fragments. Producer dropped-record counts and source batch-gap counts are
preserved separately on complete events. Counters saturate rather than wrap.
`Start` resets counters explicitly; `Discard_Partial` preserves them.

The hosted regression passes 89,443 checks across all query-page lengths
1–32, all four initial fragment offsets, replay, altered envelope fields,
endpoint changes, explicit discard and terminal sequence boundaries. A
compiled negative control without the endpoint guard is rejected. SPARK
proves successful output validity and identity binding, final sequence
correlation, exact emitted-count advancement, and a nondecreasing cursor.
The state-size regression bounds its storage to one 4 KiB page; no heap is used.

Native CuBit acceptance deliberately shifts retained history to fragment 1.
The real SDK/service/raw observer test rejects three orphan fragments,
reconstructs 63 exact events across eight query pages, records a 269-row gap,
preserves the 400-fragment producer-loss report, and confirms grant retirement.
Earlier complete-group/authority/summary tests remain covered. The native
fixture uses explicit prebuilt kernel/initrd/runtime/allocator seeds. This is
functional transport evidence, not actual Desktop timing or hardware latency.
Desktop export, archive storage and CCL visualization remain integration work.

## Desktop trace export

Build Desktop with metrics enabled and set the boot configuration to:

```lisp
(setting "desktop.metrics.trace" "true")
```

This is a `system-config v1` setting, read once when Desktop starts. Only the
exact raw value `true` enables detailed tracing; absent/denied/other values keep
it off. Desktop's manifest grants read-only access to `desktop.metrics.`.
Normal builds keep detailed tracing disabled. Serial timing may remain off.
The startup diagnostic goes through Desktop's existing logsvc path.

The exporter covers input dequeue, accepted protected-frame source publication,
writer-attributed source draw, accepted display submission, and completion
collection. Legacy buffer attachments do not supply protected publication
identities; unsupported draw paths are counted rather than given invented
source/writer identities. Submission and completion events still work there.
Use the complete raw envelopes and event identities when correlating records.
Client input watermarks are untrusted; completion collection is not scanout latch.

The existing two SDK pages serve aggregate and trace records together. Four
fragments are admitted atomically, no retries or IPC occur in the append, and
an urgent flush request uses the ordinary event-loop pump. Full/held pages shed
work and expose loss. Health gauges 13/14/15 are `desktop.trace.invalid`,
`desktop.trace.refused`, and `desktop.trace.unsupported`. They are emitted only
when trace mode is enabled. Producer dropped-record counts include ordinary
metrics as well as trace fragments; do not convert them into lost whole events.

Native Desktop validation includes a real protected-frame toolkit client plus
the Mesa software window, raw and summary observers, menu restoration, and
cursor checks at 100% and 125% DPI. Serial timing is disabled. Run188 captured
10 input, 5 source, 19 draw, 20 submission and 21 completion events; its final
producer loss snapshot was 6 records, with zero batch gaps/history loss/rejected
rows in that capture and confirmed observer grant retirement. These are
functional CuBit QEMU results with explicit prebuilt platform seeds, not a
hardware latency, whole-run completeness, GPU or 240 Hz claim.

Reproduction and native fixture sources: [Desktop trace test](../tests/compositor/trace-native/README.md).
Publication ID/flush policy and codec/assembly are SPARK; the existing native
publisher adapter, main event loop, transport truth and clock accuracy are not
proved by these tests. Capture archives, CCL timeline/flame-graph UI, kernel
export, physical input timestamps and authoritative display latch timestamps
remain open.

A repeat with the packaged native fixtures (run194) passed the same complete
workload; its final producer loss snapshot was 4 records. Loss varies with
scheduling and is intentionally visible. Evidence:
`tests/compositor/trace-wire-evidence/desktop-packaged194.json`.
