# Native Observatory summary boundary

Run `nix develop -c bash tests/observatory-metrics/run.sh` from the repository
root. This uses disjoint hosted outputs and does not stage or boot an image.

`Observatory_Metric_Summaries` validates observer-side summary rows using the
existing metric record codec for key/kind/unit/name metadata. It rejects zero
source IDs, invalid publisher identities, unknown flags, reserved words,
malformed names, empty histograms with nonzero statistics, reversed extrema,
and unordered percentile bounds. Successful views preserve every input bit,
including 64-bit identities, timestamps, totals, loss counts and saturation.
Percentiles are histogram bucket upper bounds and may exceed the observed max;
they must not be displayed as exact order statistics.

The decoder allocates nothing, performs no IPC and changes no producer code.
It is a building block for a CCL-backed native viewer, not an implemented UI or
an authorization boundary. The future host adapter must authenticate the reply,
validate page/count/cursor/lifetime contracts, and expose only owned resources
or bounded typed values to CCL. A numeric publisher tag alone grants nothing.
Summary snapshots are not raw timelines or CPU stack samples.

Validation (session 38225): 64 hosted checks passed, including a page generated
by the actual `Metric_Store`, malformed rows, empty distributions, saturation,
64-bit values above signed/JavaScript ranges, and rounded percentile bounds.
GNATprove level 2 discharged all 12 checks (6 termination, 4 run-time,
2 functional contracts), with zero unproved or justified checks. Contracts
prove success exactly matches validation, preserve all row words, and guarantee
successful declaration access. The metadata codec's existing contracts are a
proof dependency. This does not prove collector authentication or a future IPC
adapter. Log: `/tmp/cubit-observatory-summary-final.log`.

## Query reply admission

`Observatory_Metric_Queries` validates successful completion envelopes before
any row count conversion or page traversal: kernel-valid/success, exact label,
length, flags/reserved fields, bounded total (512 viewer slots), bounded row
count, and a cursor advancing far enough for those rows. Only a full page can
continue; an empty page is accepted only at the advertised end. `Valid_Page`
accepts all reported rows or rejects the entire page, ignoring unused storage.
Its contract proves both envelope admission and validity of every used row.

Session 49905: 2,105 hosted checks passed, including all cursor boundaries,
malformed envelopes, incomplete/non-progressing replies, unused poisoned rows,
and a real 32-series `Metric_Store` traversal through two full pages and its
empty terminal page. Combined level-2 proof: 19 checks, zero unproved or
justified. `/tmp/cubit-observatory-query-verified.log`.

These functions do not issue asynchronous requests yet. The future adapter
must route replies by a live non-reused request token, authenticate the peer,
validate mapped-page lifetime, and bound its deadline. A timeout must quarantine
collector-owned storage rather than reuse it; a completion must establish
quiescence before this policy examines a private copy. The current synchronous
`CuBit.Metrics.Query` remains unchanged and is not suitable for a responsive
viewer's event loop. Pagination is not an atomic multi-page snapshot: source
incarnations can change between replies and must be handled explicitly by the
future snapshot model.

## Asynchronous observer adapter

`Observatory_Metric_Observer` now implements the separate nonblocking observer
path without changing `CuBit.Metrics.Query`. Instantiate it with an authorized
observer capability and keep the instance alive for the process lifetime. It
owns one aligned, volatile 4 KiB page. Begin_Query consumes a caller-shared,
non-reused asynchronous token, creates a writable grant and submits once. The
caller routes completions to Collect and calls Tick on event-loop wakes and
at the 250,000-us deadline; there is no internal wait or retry loop.

A completion alone does not expose the page. Tick revokes its grant and requires
independent kernel retirement confirmation before reading or validating any
rows. Each Tick makes at most one revoke attempt and one retirement query.
Timeouts, close, malformed replies and rejected submission disable the instance;
late completions cannot revive it. Outstanding memory remains resident, with
later Tick calls permitted to finish retirement. Do not put the instance in a
short-lived scope or deallocate it while a foreign acquisition may exist.

Take copies only a ready, fully validated page and returns it to the caller.
Before the next grant, the now-private page is cleared so an incomplete writer
cannot leave valid-looking old rows in its tail. Data copies here are bounded
metadata copies on the observer side, not compositor pixel copies. Consumer
pagination still needs an incarnation-aware model; cross-page atomicity is not
provided by metrics.svc.

The new `Observatory_Query_Lifetime` policy is SPARK. The message/grant adapter
is audited SPARK-Off FFI glue: no synchronous call, blocking receive or wait is
used. Its correctness still depends on the kernel's capability/token routing,
grant retirement and memory ordering, the caller's monotonic microsecond clock,
and a serialized event loop invoking Tick on time. Tests use mocks for those
boundaries; they are not native IPC evidence.

Validation: `nix develop -c bash tests/observatory-metrics/run-adapter.sh` passes
1,069 checks against the actual adapter, including wrong/stale tokens, failed
grant creation/submission, invalid envelopes/rows, timeout before/after reply,
late retirement, 1,000 refused-revoke attempts, token exhaustion, saturated
clock deadlines, successful empty/nonempty replies, and incomplete next-query
writes. Lifecycle proof discharges all 10 checks (zero unproved/justified).
Final hosted check and compile against the real CuBit runtime both pass in
session 14461, `/tmp/cubit-observatory-observer-native-final.log`; lifecycle
proof is in `/tmp/cubit-observatory-observer-final.log` (64359). Native compile
uses `alr exec -- gprbuild -q -p -c -P
../tests/observatory-metrics/native.gpr` from the `kernel` directory.
Hold the shared native-build lock. No image was staged.
Native collector and CCL integration are covered below; visible UI remains outstanding.

## Native collector integration

Session 11373 passed a 120-second, four-CPU CuBit/QEMU TCG `ccl-workspace`
gate including the final fault scan. The native acceptance app completed 15
asynchronous queries over 29 event-loop turns, with independent grant retirement
before each admitted page. It received growing Desktop release measurements
(three frames/three batches), checked the source, metric name/schema, and zero
loss/rejection counters. Workbench saves, reopen and interaction also passed.
All 14 recorded source/binary inputs and the private base disk were unchanged;
the previously staged test observer was restored and byte-checked.

Evidence: `/tmp/cubit-observer-native.serial`,
`/tmp/cubit-observer-native-v2.log`, `/tmp/cubit-observer-native-inputs.sha256`,
and `/tmp/cubit-observer-base.sha256`. The first attempt, 87563, compiled and
linked successfully but stopped before boot because the shared default disk
was absent. The passing retry used a freshly built private ext2 base image.

The repository runner reproduces that orchestration with unique evidence and
base paths (its shell syntax was checked):

```sh
flock --exclusive --timeout 600 coordination/build.lock \
  nix develop -c bash tests/observatory-metrics/run-native.sh
```

It requires existing staged boot prerequisites, a built metrics-enabled Desktop
and `metrics.svc`; it does not build the whole system. The observer test image
is temporarily installed under the existing test profile's filename, with the
original restored on exit. The acceptance app deliberately sleeps between
polling turns as stand-in event-loop work; it is not yet a CCL UI event loop.
This establishes real async query and retirement integration, not CPU flame
profiling, raw capture transport, hardware latency, or an interactive viewer.

## Bounded CCL metrics view

`Observatory_CCL` publishes the read-only `metrics` 1.0 interface described by
`userspace/lib/observatory/metrics.ccl-interface`. Discovery does not confer
permission: the embedding application explicitly installs observe grants.
The context holds one validated summary page (at most 16 rows), replaces it
only from private, retired data, and invalidates the old view if replacement
fails. Serialize replacement and evaluation in the application's event loop.
There is no IPC, waiting, or per-event allocation inside a CCL host call.

`ready` and `count` take no arguments. The remaining operations take a zero-based
row index: `name`, `unit`, `source`, `publisher`, `samples`, `minimum`, `maximum`,
`p50-upper`, `p99-upper`, `total`, `dropped`, `batch-gaps`, `lossy`, `saturated`.
Identifiers and unsigned quantities return exact decimal strings, preserving
all 64 bits. Percentiles are bucket upper bounds; empty distributions return
`unavailable`. Loss and saturation are exposed separately. A single page is
not a complete, atomic snapshot of all collector series.

For example, this expression formats row zero without exporting JSON:

```clojure
(concat (metrics.name 0)
  (concat ": p99 <= "
    (concat (metrics.p99-upper 0)
      (concat " " (metrics.unit 0)))))
```

`nix develop -c bash tests/observatory-metrics/run-ccl.sh` tests the actual CCL
interpreter against a hash-verified private source snapshot. Session 78501
passed 72 checks: missing authority, wrong types, index bounds, exact unsigned
values, composition, empty distributions, loss/saturation, and cache rejection.
GNATprove level 2 for `observatory_ccl.adb` discharged 41 analysis results
(30 prover checks and 11 flow results), zero unproved or justified. This proves
the binding's local checks and valid-cache invariant under imported contracts;
it does not prove the whole interpreter, kernel IPC, or foreign memory ordering.
Evidence: `/tmp/cubit-observatory-ccl-proof.log` and
`build/ccl/obj/gnatprove/gnatprove.out`.

The native acceptance app also evaluates these bindings against actual retired
collector pages, compares formatted output and sample counts to the original
rows, checks missing authority, and clears availability on close. Its runner
requires a distinct `TEST: PASS observatory-ccl live summary expressions` marker.
This exercises data-to-CCL integration; a visible viewer and raw trace/stack
capture remain separate work.

Native validation: session 37281 passed the complete 120-second, four-CPU
QEMU TCG workspace test and final fault scan. The new CCL checks passed against
live Desktop data during nine asynchronous queries over 18 event-loop turns,
with confirmed grant retirement and three observed frames/batches. All 138
recorded source/binary inputs and the private base disk were unchanged; the
previous staged observer was restored and byte-compared. Evidence is retained
in `/tmp/cubit-observatory-native-ccl-evidence/`, with the command log at
`/tmp/cubit-observatory-native-ccl.log`. These are functional integration
results, not physical latency or 240 Hz measurements.

## Native Observatory window

`userspace/apps/observatory` is the first visible consumer. Its native window
uses protected client frames and the shared DPI-aware UI toolkit; CCL formats
metric names, sample counts and percentile bounds from the validated private
cache. It exposes at most 16 rows at a time. `N` advances the collector cursor
(or wraps after the final page); pages are independent observations, not an
atomic capture. `Space` pauses automatic refresh, `R` requests a refresh when
running, and `Esc` closes. Collection failure leaves retained values explicitly
stale and disables new queries. Loss and saturation have separate warnings.

The event loop shares nonreused tokens between async input and the observer,
drains at most 16 completions per turn, admits at most one collector query, and
normally refreshes once per second. It uses a 10 ms retry deadline only for
pending grant/frame work; paused or failed-and-retired windows sleep for input.
This UI orchestration is native integration code, not an additional SPARK proof
of the entire app or an execution-time bound. The proven summary, cache and
query-lifetime policies retain the boundaries documented above.

Build and exercise it using existing native runtime/font and staged boot inputs:

```sh
flock --exclusive --timeout 600 coordination/build.lock \
  nix develop -c bash -c 'bash tests/observatory-metrics/build-viewer.sh && python3 tests/observatory-metrics/test-viewer.py'
```

The test builds a private boot image and disposable disk with `init-viewer.ccl`.
That trusted startup grants observation authority; this does not grant ordinary
applications access or add the viewer to the normal Apps menu. The test never
stages the app over another binary or writes the user's base disk.

Viewer validation: session 65972 passed with 39 nonempty refreshes and 20
visible pause/resume cycles, followed by paging, explicit refresh, repeated
query/grant reuse and clean Escape close. The footer-only pixel oracle requires
the paused state to appear and the live state to return; paused content remains
pixel-identical while collection stops. The final binary has no temporary
render diagnostic. Inputs and private base stayed unchanged, and the fault scan
passed. Evidence: `/tmp/cubit-observatory-viewer-final.log` and
`/tmp/cubit-observatory-viewer-final-evidence/` (PNG/PPM, serial, hashes, result).

Earlier 90614 passed the initial interaction oracle. Visual review initially
misread its paused footer as Live, prompting stronger pixel checks. Subsequent
byte comparison confirmed that original footer exactly matches the verified
paused footer from 65972. The apparent stale-presentation issue was a mistaken
visual reading, not a reproduced compositor defect. A render-diagnostic retry
(25778), six-cycle pixel test (70636), and final twenty-cycle gate without that
diagnostic passed. Initial build 89640 failed on missing display/allocator GPR
source directories, which were corrected.

The native viewer does not yet establish overload UI behavior, atomic
multi-page capture, raw timelines, sampled stacks, or physical display latency.
The current data is Desktop buffer submission-to-release, not keypress-to-photon.

## Native collector faults

`fault-service/build.py` makes private, hash-recorded copies of the current
metrics service and injects one fault on its twelfth nonempty query. The
production service/SDK remain unchanged. Eleven real nonempty pages establish
retained data before an invalid reply reserved word, an invalid row flag, or a
three-second held writable acquisition followed by a late reply. Fixtures keep
normal publisher ingestion and authenticate observer requests using the existing
service implementation. More queries after injection are a test failure.

Session 80571 passed all three cases on the table viewer: collection stopped,
the visible status changed from live, keyboard input and close worked, and late
replies did not revive the cache. Each case retained 11 nonempty updates. Native
logs: `/tmp/cubit-observatory-fault-native.log`. These are functional fault gates;
the deliberate sleeps and QEMU wall times are not latency benchmarks.

Build a fixture and run the selected native case under the shared lock:

```sh
flock --exclusive --timeout 600 coordination/build.lock \
  nix develop -c bash -c 'python3 tests/observatory-metrics/fault-service/build.py stall && python3 tests/observatory-metrics/test-viewer.py --fault stall'
```

`envelope` and `row` select the other two cases. Each private disk contains its
own `metrics.svc`; the tests never replace the staged production collector.

## Latency and activity graphs

The default view now plots the selected series using two native bar charts:
its cumulative p99 histogram upper bound and new samples since the preceding
refresh. These are collection observations, not raw events, a sliding-window
percentile, or samples per second. The horizontal axis is oldest to newest,
limited to 64 observations. No animation or independent rendering timer is used.
`T` toggles the complete table; Up/Down selects another row in the current page.
Red bars indicate collection loss/saturation present in the page. Pause leaves
the graphs intact; the first sample delta after pause is unavailable. An identity
change (source, publisher issuance, key, kind or unit) or decreasing cumulative
counter clears history, preventing unrelated incarnations from being joined.

`Observatory_History` is SPARK with a fixed-capacity ring and integer proportional
scaling. Scaling computes floor(value * height / maximum) without forming the
potentially overflowing product, using at most 512 bounded addition/subtraction
steps (110 at the current chart height). A zero maximum maps to zero. Ring
storage, counter-delta guards, loop bounds and overflow safety are analyzed;
proportional arithmetic is regression checked against ordinary-size exact
products and extreme unsigned values. This is not a proof of UI painting or
wall-clock responsiveness.

`nix develop -c bash tests/observatory-metrics/run-history.sh` passed 519,015
checks, including 1,000 ring updates, wrap ordering, counter/identity resets,
pause gaps, zero activity, monotonic proportional scaling, and unsigned 64-bit
extremes. Session 25743 discharged 25 SPARK analysis results (20 proof checks
and five flow results), zero unproved or justified. Evidence:
`/tmp/cubit-observatory-history-proportional.log` and
`build/history/obj/gnatprove/gnatprove.out`. An earlier scaling approximation
underfilled small-value bars and was replaced; its intermediate native run is
not evidence for the final scaling implementation.

Final graphical native validation: session 41578 passed the normal graph/table
interaction gate (39 nonempty refreshes and 20 visible pause/resume cycles)
and all three collector fault gates (11 nonempty pages before each fault).
The graphs remained available as stale history; late replies and keyboard input
did not revive collection, and close succeeded. Private bases and recorded
inputs remained unchanged. Evidence: `/tmp/cubit-observatory-graphs-verified.log`
and `/tmp/cubit-observatory-graphs-evidence/`, including native screenshots.

The final bar oracle (33947) was evaluated against that exact native capture,
counting bar colors strictly inside the plot rather than background/padding;
missing-latency and missing-activity mutations were rejected. The warning-footer
oracle (31530) verifies identical warning-colored pixels before/after late reply
and input on all three captures, and rejects replacement with the Live footer.
An initial visual misreading of the failure screenshot was contradicted by
these direct pixel checks; no stale-status defect is claimed. Oracle evidence:
`/tmp/cubit-observatory-graph-oracle.log` and
`/tmp/cubit-observatory-stale-oracle.log`. No application code changed after the
four native gates; only the screenshot assertions were strengthened.

## Input opportunities during CCL formatting

A full collector page previously formatted all 16 rows before returning to the
input loop. `Observatory_Format_Budget` now admits one row per event-loop turn.
The viewer drains up to 16 completions before each row, with three bounded CCL
submissions for that row and one additional unit-name submission on the final
commit. Each submission retains its existing 4,096-step interpreter fuel. This
is a work-admission bound, not a proof of compilation, host-call, painting, or
wall-clock execution time.

A fixed second text cache holds the incoming formatted page (about 4.8 KiB of
text records). The existing displayed page, count, cursor, unit and loss flags
are replaced only after formatting succeeds. No second query is admitted while
formatting; input and close can be processed between rows. Formatting failure
cancels the pending work while retaining the old displayed data as stale. Empty
pages commit immediately. Pending formatting uses an immediate activity deadline;
otherwise the existing grant/frame/refresh waits apply. Outstanding dirty paint
also retains a retry deadline instead of accidentally sleeping indefinitely.

`nix develop -c bash tests/observatory-metrics/run-format.sh` passed 306 checks
covering all page lengths, ordered row progression and cancellation at every
position. Session 20203 discharged 22 SPARK analysis results (18 proof checks,
four flow results), zero unproved/justified. Evidence:
`/tmp/cubit-observatory-format-proof.log` and
`build/format/obj/gnatprove/gnatprove.out`. The invariant and contracts prove
bounded progression; native app sequencing and cache commit remain integration
code rather than a proof of the full application.

The `full` fixture mode expands a real received summary into 16 valid test rows
with distinct keys. Those added rows are synthetic and must not be interpreted
as actual published Desktop series. This exercises maximum page formatting and
normal UI interaction; `none` still uses the unchanged production collector.

Incremental-format native validation (52241): all five gates passed with the
final application binary. The synthetic full-page fixture completed 38
nonempty updates of 16 rows; the production collector completed 39 updates.
Both exercised 20 visible pause/resume cycles, graph/table pixel restoration,
paging, refresh and close. Envelope, row and held-grant/late-reply cases each
retained 11 complete updates and passed warning-footer stability, no revival,
input and close checks. All recorded inputs and private base images were
unchanged; the final fault scans passed. Evidence:
`/tmp/cubit-observatory-format-native.log` and
`/tmp/cubit-observatory-format-evidence/`. The full fixture's reporting flag
was subsequently corrected to classify it as a normal interaction test, not
a failure-input test; the original reports are retained with that annotation.

This establishes native integration of incremental formatting, not a worst-case
execution time, input response percentile under CPU overload, or 240 Hz claim.

## Native CPU overload

`--load` adds four distinct priority-3 CPU workers to the disposable profile.
Each waits five seconds for initial startup, then performs volatile-state integer
work for 120 seconds of guest monotonic time. Desktop remains priority 4, while
the production collector remains priority 2 and Observatory priority 3. This
keeps measurement publication below interactive work rather than promoting the
collector just to make the test pass. The fixtures require no production edits.

```sh
flock --exclusive --timeout 600 coordination/build.lock \
  nix develop -c bash -c 'bash tests/observatory-metrics/build-load.sh && python3 tests/observatory-metrics/test-viewer.py --load'
```

Session 16440 passed on four-CPU QEMU TCG: 26 nonempty updates overall, six
visible pause/resume cycles, paused pixel stability, positive latency/activity
bars, paging, refresh, graph/table restoration and close. The serial-order
oracle requires at least ten updates, six pause actions and close between the
last worker's start and the first worker's finish. It then waits for all four
unique workers to finish with positive work counts before the final fault scan.
The test also checks that collection remains available and that recorded inputs
and the private base image are unchanged. Production staging is untouched.

Evidence: `/tmp/cubit-observatory-load-native.log` and
`/tmp/cubit-observatory-load-evidence/`. This is functional progress under CPU
contention, not measured input-response percentiles, scheduler fairness proof,
240 Hz throughput, or physical keypress-to-photon latency. Worker chunk counts
are coverage evidence, not a cross-host performance benchmark.


## Desktop stage visibility

The native viewer accepts `--desktop /absolute/path/to/desktop.svc` to build a
private test image around a specific compositor binary without replacing the
staged service. Session 48297 used the metrics-enabled Desktop with six series:
two output release spans and four execution-stage latency series. The existing
viewer needed no rendering changes. All 39 updates, 20 pause/resume cycles,
positive graph bars, table roundtrip, refresh, paging and close checks passed.
The separate native CCL observer now requires every stage to have a positive
sample count and validates each name/kind/unit and loss state. Stage semantics,
proof boundaries and numerical caveats are in
[`desktop-metric-publisher.md`](../compositor/desktop-metric-publisher.md).
Screenshot: `/tmp/cubit-stage-metrics-evidence/viewer/table.png`.
