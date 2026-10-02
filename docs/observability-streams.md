# CuBit observability streams and serial-free profiling

Status: implementation proposal, 2026-10-01. No new transport or collector is
implemented by this document. The goal is first-class on-device diagnostics,
latency timelines, scheduler tuning and sampled CPU profiles without requiring
serial access, while preserving bounded producer work and memory.

## Decision

Build a small `telemetry.svc` collector and a versioned, typed **read-only
snapshot ring** transport. Producers own their storage, publish without waiting
for readers, and retain a bounded recent history. The collector pulls records
in bounded batches, independently of rendering or scheduling. Keep low-rate
human-readable diagnostics in logstore; do not force high-rate binary events
through text logging. Reuse capability/grant authorization and the existing pure
stream policy vocabulary, but do not reuse the current byte-stream data plane
unchanged. Use one collector attachment per producer initially; downstream
viewers subscribe to collector-owned data, avoiding producer-side fan-out cost.

Three semantics must remain distinct:

| Data | Contract | Full or delayed-reader behavior |
| --- | --- | --- |
| Timestamped events: input, wake, switch, submit, complete | FIFO among surviving sequence numbers; `Ordered_With_Gaps` | Overwrite **oldest** history, report sequence gaps |
| Current state: queue depth, memory usage, cumulative histogram | `Latest_Value` per statically declared metric key | Replace prior snapshot; revision reveals skipped updates |
| Durable audit | Separate admitted durable contract | Never pretend lossy telemetry is durable audit |

“Overwrite-last” is appropriate for a latest-value mailbox. Repeatedly replacing
the newest FIFO entry while retaining old history would hide current causal
activity. Recommend overwrite-oldest for event history and latest-value for
state. A collector can arrive at leisure, but only history still within the
budget survives. No lossless or zero-overhead claim is intended.

## Existing foundations and actual gaps

* `kernel/src/trace.ads/.adb` already has 512 records per CPU, raw ordered TSC
  stamps, event counters and 12-bin duration histograms. Recording starts
  disabled. Event kinds cover syscalls, scheduling, readiness, IPC handoffs,
  locks and late timers. Local start/freeze/dump is build-gated; output is serial.
  It is SPARK Off and lacks a published concurrent snapshot ABI, explicit
  sequence/gap contract and authenticated collector transport. Global Reset and
  Summary syscall cases in `kernel/src/syscall.adb` currently invoke global
  trace operations directly; do not promote these into production control
  authority without an authorization and SMP synchronization redesign.
* `kernel/src/scheduler.adb` measures ready latency and run duration. A scheduler
  loop can contain direct IPC handoffs: its run-duration sample is not necessarily
  one thread's runtime. Instrument authoritative `Process.accountBoundary` and
  readiness transitions, including `directSwitch`, rather than infer precise
  per-thread execution from outer schedule-run/stop records alone.
* `userspace/runtime/gnat/cubit-streams.ads/.adb` already exposes producer-owned
  rings and read-only subscriber grants. However `streamWrite` handles a service
  request before writing, walks subscribers, and publishes variable-length
  records using volatile loads/stores. `streamRead` uses a local cursor with no
  per-record overwrite-consistency handshake or explicit gap result. This is
  evidence against treating it as the required bounded hot-path transport;
  schema negotiation alone does not repair concurrent snapshot consistency.
* `CuBit.Logging` has one-outstanding asynchronous grant publication and explicit
  drops/retirement. `Log_Fanout` has bounded 512-record queues, eight subscribers,
  replay and lease expiration. `Log_Records` is bounded UTF-8 text with millisecond
  timestamps. Logstore's current shared publication credit pool (64 burst, one
  refill per 100 ms) is unsuitable for scheduler event rates. See
  [typed logging](typed-logging.md). Independent publisher reconnection is not
  yet implemented. The boot-logs application demonstrates serial-free viewing,
  not a persisted high-rate capture or profiler.
* `CuBit.Protocols.Stream_Policies`, `Stream_Connections` and `Stream_Bindings`
  provide pure policy/admission/lifecycle models. The live typed wiring registry
  and transport are still missing; see [stream wiring](stream-wiring.md).
* `Time.Read_Monotonic` / syscall 114 provides explicit-success microsecond
  readings through `Platform_Monotonic`. Kernel tracing instead records raw TSC.
  Numeric microsecond units are not an accuracy guarantee. Existing benchmark
  TSC calibration does not establish cross-CPU synchronization.
* Compositor source/frame tracing and `CuBit.Timing_Histograms` are useful event
  producers and aggregate models. Their current timing-only serial exporters
  should become adapters into this stream; source timestamps and causal IDs
  must not be reconstructed from collector arrival time.
* Filesystem protocol has scoped file operations and explicit flush; successful
  write is not equivalent to flush. No trace archive/retrieval workflow exists
  merely because these operations exist. There is no general safe sampling
  unwinder established by this audit; panic diagnostics deliberately do not
  assume frame-pointer chains (see development backlog).

## Transport and concurrency boundary

Initial event slots are fixed-size (proposed 64-byte total slot), explicit
little-endian schema words, with no pointers or strings. A slot contains an
atomic version/commit word and seven payload words. Stream descriptors carry
schema/version, producer incarnation, lane/CPU identity, clock domain, slot
count and supported record kinds; records carry sequence-derived identity,
timestamp, thread identity where relevant, kind and bounded arguments. Wider
samples use a separately admitted fixed-size stream, never variable-length
chains that a writer can leave half-published. Reserve immutable metadata and
control pages separately from payload capacity.

Single-writer is an enforced contract, not a presumed consequence of one process:
userspace initially has one ring per event-loop owner or admitted thread.
Kernel rings are per CPU. A kernel adapter masks local IRQs only for the bounded
write, prevents migration, and handles recursive instrumentation with a bounded
skip and counter. NMI tracing is disabled initially; if later required it gets a
separate lane. Never spin on a trace lock from an interrupt. Neither timestamp
capture nor the writer may invoke allocator, IPC, scheduler, symbolizer, logstore
or collector code. Setup/attachment and cleanup occur outside this path.

Use a small audited atomic primitive boundary, initially sequentially consistent
aligned 64-bit atomic accesses for **both payload words and versions**. Writer:
publish busy version; write the fixed payload words; publish the committed
version; advance committed head. Reader: read expected committed version, copy
atomic words into private storage, reread version, accept only the same expected
committed version. Checking a sequence around ordinary concurrently overwritten
Ada/C memory is insufficient: it leaves a language-level data race. `Volatile`
alone is not a concurrency proof. Validate generated code, alignment, lock-free
atomics and ordering in the actual native toolchain. Optimize barriers only after
a written memory-order proof and measured need.

Assign non-reused sequence versions; reject/disable the lane at exhaustion
rather than wrap into an ABA match. A reader behind retained history advances to
the oldest retained sequence and emits a gap. A raced read gets a small bounded
retry allowance, then yields with a retry outcome (not a fabricated valid event).
A later snapshot can establish the exact overrun range. Head can lag a committed
slot transiently; readers may defer it until the next batch. Never spin until a
writer or collector makes progress. Readers retain only private copies, not
borrowed mutable ring slots, so no consumer can pin producer capacity. This
snapshot contract must be added explicitly to typed profiles: it differs from a
consumer-held immutable queue element in the current policy commentary.

Producer attempt/commit counts, overwrite history, disabled/recursive/malformed
loss and collector/export loss are distinct. Use saturating counters with a
saturation flag. Collector cursors are private; no reader-writable consumer index
can corrupt a producer. Header counters and latest-value histogram arrays also
need coherent versioned snapshots, not uncoordinated reads of several atomics.
Every accepted record is validated again from the private copy. A malicious
producer can lie about payload and time; authenticated origin belongs to the
transport envelope supplied by the registry/kernel.

## Ownership, authority and lifetime

Attach via explicit, scoped publication and observation capabilities approved at
startup. A named `metrics` endpoint or schema match does not authorize reading
another process. The registry resolves process **incarnation**, lane generation,
approved schema and page budget, then arranges a read-only mapping. Collector
never maps unrelated producer pages; pages are dedicated and initialized before
release. System-wide kernel traces need separate privileged observation/control
authority. Applications can inspect their own admitted streams; kernel addresses,
user stacks and cross-process scheduling information require tighter authority.
Trace collection should avoid text input, URLs and syscall buffers by default.

Separate stable data lifetime from collector lifetime. Producer keeps its bounded
ring even with no collector. Restarted collector authenticates anew, obtains a
new attachment generation and starts at an explicit oldest-retained or current
position. Export captures retain boot/producer/attachment identities so PID reuse
cannot join unrelated events. A collector crash neither blocks writes nor causes
unbounded new page allocation.

Revocation, process exit and rebind use prepare/commit/retire with actual grant
retirement acknowledgement. A lease timeout, TARGET_DIED completion or successful
Revoke alone is not permission to reuse pages. Retired mappings consume the
admitted budget; if retirement stalls, deny replacement attachment or retain the
old bounded allocation. Do not create unlimited rings on reconnect. Kernel-owned
trace pages require a dedicated controlled mapping/export boundary; ordinary
user-owned grant APIs do not by themselves establish kernel-page export safety.
This kernel boundary must be implemented and tested, not assumed available.

## Budgets and collector behavior

Proposed starting development profile: 64 KiB event payload per enabled CPU,
32 KiB per selected userspace lane, at most 32 user lanes and 16 observed CPUs;
16 MiB collector staging, 64 MiB rotating on-disk capture. Thus payload rings
are at most 2 MiB, excluding explicit page rounding/metadata/snapshot tables;
record those overheads in admission. On a four-CPU NUC with Desktop/Display and
four app lanes enabled, payload is 448 KiB. These are tunable proposed limits,
not measured requirements. Allocate enabled lanes only; do not statically multiply
large buffers by every theoretical process/CPU. Kernel boot-lane reservation must
be included in the boot memory budget.

At 64 bytes/event, 64 KiB retains 1,024 events: only 10.24 ms at 100,000 events/s.
Expose measured retention time and losses. Deferred collection cannot preserve
arbitrary high-rate history. Keep inexpensive cumulative counters/histograms for
long sessions; selectively enable detailed categories, filters and bounded capture
windows for diagnosis. A slow collector may lose event history while cumulative
statistics remain useful.

Collector pulls round-robin with per-lane byte/work budgets, copies accepted
records into fixed staging, then compresses/writes outside producers. Start with
no compression for easier fault diagnosis. Polling needs no per-event wakeups;
use a bounded configured cadence and coalesced optional notifications later.
Collector priority and CPU/I/O budgets must preserve desktop responsiveness and
avoid starving other services. A storage stall fills bounded staging and then
loses records with accounting; it never applies backpressure to rendering or the
scheduler. Downstream subscribers have separate bounded cursors/budgets.

## Serial-free user workflow

Ship a small on-device **Performance** UI plus a CCL control client using the same
scoped control operations: select profile, start bounded recording, stop/save,
show producer/collector loss, and list saved captures. Begin with a compact live
summary (latency histograms, run queue, missed deadlines, bytes and retention)
rather than port a large trace viewer into CuBit.

Collector writes chunked `.cubittrace` archives under a restricted diagnostics
directory: versioned metadata, executable build IDs/load maps, clock descriptors,
records, explicit gaps and per-chunk length/checksum. A partial final chunk is
recoverable and marked incomplete. Completed capture footer + checked flush is
required before UI says saved; storage-full, write failure and unavailable media
are visible. Do not promise power-loss durability beyond the filesystem's proven
flush semantics. RAM-only boot retains a bounded volatile capture and labels it
as such.

First retrieval path: an explicit **Export capture** operation to an approved
writable USB volume, coordinated with the filesystem agent's supported native
mount/flush path. Treat removable-volume discovery and export UI as deliverables,
not existing functionality. Second path: authorized, explicit network transfer
through an independently scoped exporter; no default network server or automatic
upload. Preserve a local capture even when network export fails. Headless tests
may retrieve the archive from the test disk, but the NUC acceptance gate must use
the actual on-device save/export workflow with no serial dependency.

## Useful latency data and honest flame graphs

Scheduler records should identify thread incarnation, CPU and scheduling reason:
wakeup requested, actually made runnable, selected/dispatch, switch out, block,
preempt/yield, migration, direct IPC handoff and timer deadline versus handling.
Include effective priority, bounded run-queue depth and correlation IDs. Measure
wakeup-to-run, runnable queue residence, execution intervals, blocking time,
preemption delay and timer lateness separately. Repeated wake attempts are not
new runnable intervals. Account for ready-state changes on remote CPUs and every
direct handoff; define event placement relative to locking/state commit.

Compositor records link input sequence, handler completion, source publication
(epoch/ticket), output frame/session, render begin/end, driver submit and actual
completion. Distinguish display scanout/vblank observations from a software copy
completion. Unknown causality stays unknown; missing spans are shown as gaps.
Physical keypress-to-photon still requires external input/display measurement.

Histograms retain count, sum with overflow indication, min/max, fixed documented
buckets, rejected-clock samples and saturation. Export p50/p95/p99/p99.9 only with
sample count, bucket bounds and capture interval; never give invented precision.
Clock rollback or unidentified domains invalidates a duration.

A timeline of spans is not a sampled CPU flame graph. Implement profiling in two
steps: first bounded timer sampling of interrupted IP + thread/CPU with explicit
lost/missed samples (flat hot spots); then safe bounded stack capture. Choose a
profiling build with frame pointers across Ada/C/Rust and verify assembly/leaf
exceptions, stack bounds and fault-safe access. No symbol lookup, allocation or
unbounded unwind in the interrupt. If stack bytes are copied for offline DWARF
unwinding, cap them, mark truncation and restrict access as sensitive data. Kernel
sampling must never fault on a user stack or acquire locks held by the interrupted
code. A production unwinder/PMU sampler is additional work, not supplied by the
existing panic handler. Preserve executable build IDs, load addresses and exact
debug symbols for offline symbolization; stale symbols are rejected.

Timer samples estimate CPU residency, not hardware event cycles; disclose sampling
frequency and bias. Off-CPU flame graphs require pairing block/wakeup/run events
with captured stacks and explicit duration weights. Do not use missing records
as zero wait or interpret aggregate durations as complete call stacks.

## Clocks and offline tooling

Use monotonic microseconds with explicit success for first userspace tracing,
and raw per-CPU ordered TSC for bounded kernel fast paths where appropriate.
Capture boot clock identity, frequency rational, per-CPU calibration brackets,
uncertainty and discontinuity epochs. Validate cross-CPU skew on hardware before
merging timelines. HPET read cost and TSC conversion accuracy require measurement;
do not add an MMIO read to every scheduler event casually. Migration must preserve
CPU identity. Wall-clock anchors label a capture, never compute scheduling latency.
GPU clocks need separate calibration and reset identity.

Write an offline CuBit decoder/exporter, not a Perfetto SDK dependency in the
kernel. Primary timeline output: Perfetto protobuf with explicit clock mappings,
thread/output tracks, counters, causal flows and loss annotations. A first Chrome
Trace JSON exporter can shorten bring-up; do not discard native metadata to fit
it. Perfetto supports application slices/counters and clock snapshots; see the
[official track-event documentation](https://perfetto.dev/docs/instrumentation/track-events)
and [external format guide](https://perfetto.dev/docs/getting-started/other-formats).
For verified samples, emit speedscope sampled profiles and optionally folded
stacks for flame graphs; follow the [official custom-source format](https://github.com/jlfwong/speedscope/wiki/Importing-from-custom-sources).
All external conversion occurs after capture; archive remains the source of truth.
Pin schemas/tools in tests and validate an actual exported capture in the viewer.

## Implementation order and acceptance gates

1. **Transport policy and native atomic boundary.** Implement portable SPARK
   sequence/gap, latest-value revision, saturation, budget and attachment-lifetime
   cores with contracts. Prove arithmetic safety, FIFO surviving order, no silent
   accepted-sequence reuse, bounded indexing and failed-operation state preservation.
   Add an actual atomic adapter and concurrent hostile-schedule tests. Check writer
   interruptions between every word, wraps, overrun, stalled readers, sequence
   exhaustion, malformed producer headers, clock failure, restart and revocation.
   SPARK proofs of sequential policy do not prove hardware memory ordering,
   interrupt masking, mappings or compiler atomics; document those audited bounds.
2. **Serial-free vertical slice.** Add admitted userspace attachment and collector;
   feed compositor source/frame records without changing their causal semantics.
   Add start/stop/save and explicit USB export through the UI/CCL. Native test:
   capture real app input through output completion, save, flush, retrieve, decode
   and view a timeline with matching IDs and declared gaps. Complete once on the
   NUC without serial before calling this workflow usable. Keep the serial exporter
   only as a test adapter, not the required path for evidence.
3. **Kernel integration.** Migrate Trace hooks to per-CPU stream policy, add scoped
   kernel observation/control, boot reservations and read-only export, then audit
   every readiness/accounting/direct-handoff transition. Add runqueue and scheduler
   histograms. Native SMP workload with thread migration, IPC handoffs, deliberate
   overload and collector death must retain correct identity/loss semantics and
   make forward progress. Unauthorized apps cannot read/control other streams.
4. **Profiles and safe stack sampling.** Flat-IP samples first, bounded stacks next,
   build metadata and symbolization, then speedscope export and off-CPU attribution.
   Native known-stack fixtures must produce expected stacks and reject stale build
   metadata. Unmapped, guard-page, corrupted, recursive and truncated user stacks
   must never crash or stall the kernel. Verify emitted files in real viewers.
5. **Hardware overhead and long-running collection.** Measure disabled,
   counters-only, detailed events, sampling, collector-on and disk-export profiles
   on the same NUC build/workload. Report producer cycles/event, IRQ-masked time,
   CPU%, memory including mappings/staging, writes/s, missed samples/records,
   scheduler tail latencies and desktop missed deadlines/input latency. Test frozen,
   slow, killed and restarted collectors, full disk, failed flush, removal during
   export, subscriber churn and quota exhaustion. Hardware long-run results must
   demonstrate bounds; QEMU correctness is not a 240 Hz performance measurement.

Use test-scope native fixtures where needed, not new global trace knobs exposed
to arbitrary applications. Every phase requires an input/build manifest, native
fault scan and loss-aware offline validation. Tests should inject gaps into each
causal chain and reject falsely complete latency measurements. Histogram totals
and accepted sequence ranges must reconcile with reported loss; mismatches are
capture errors. Include no-consumer runs proving producers do not wait.

Performance gates should initially report measured regressions rather than invent
unearned cycle limits. Establish a reproducible hardware baseline, then choose
explicit allowed overhead and p99/p99.9 budgets before enabling tracing by default.
Detailed stack sampling remains opt-in; cheap counters can become default only
after these measurements. Always distinguish 4.167 ms frame cadence at 240 Hz
from a 1 ms physical end-to-end stretch goal.

## Concrete ownership handoffs

* **Observability implementation:** new typed record/transport-policy packages,
  atomic adapter, collector, capture UI/control, archive codec and offline tools.
  Coordinate runtime, procmgr and manifest changes before editing shared owners.
* **Compositor owner:** replace deferred serial drains with bounded stream writes;
  preserve original input/source/frame IDs and timestamps; add output-completion
  correlation and loss-aware native fixture. Do not emit strings or do filesystem
  work in rendering. A metrics failure must not alter buffer retirement/scheduling.
* **Kernel owner:** scoped trace control/export, CPU-local writer serialization,
  identity epochs and clock metadata, authoritative scheduler hooks, later safe
  sampling. Review existing global reset/summary syscalls and broad raw syscall
  arguments; replace unsafe diagnostic authority rather than preserve an undeployed
  API solely for compatibility. Keep scheduler policy independent of telemetry.
* **Filesystem owner:** approve scoped archive directory, bounded writes/flush,
  USB export and failure reporting. No claim all removable filesystems work.
* **Servo owner:** emit browser input/paint/submit spans and sampled build metadata
  under the same schema/identity model; async engine dispatch acknowledgement is
  not proof that an input contributed to a presented image.

No user decision is needed to begin phases 1–2. The recommended default is local,
opt-in detailed capture, bounded overwrite-oldest events, latest-value aggregate
metrics, explicit export and no automatic upload. Storage location, retention and
capture profile can be settings with admitted ceilings rather than blockers for
implementation. Do not build the entire generalized runtime wiring editor before
shipping this narrow authorized collector path.

## Implementation status

Kept by the observability implementation agent; the sections above are the
architecture and are unchanged. The first implementation is a page-batched
IPC collector rather than the fixed-word snapshot transport: producers append
fixed 64-byte typed records to a private page without IPC and submit whole
pages asynchronously (a full page is 63 records per IPC). It already
provides the event/latest-value split, explicit producer drops, batch
sequence gaps, per-publisher isolation and observer-only queries. The snapshot
ring remains the plan for kernel lanes and rates where page-per-IPC is too
expensive. Details, status and requests: [metrics service](metrics-service.md).

