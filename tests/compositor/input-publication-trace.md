# Desktop dequeue / publication correlation

With `CUBIT_COMPOSITOR_TIMING=on`, Desktop records a microsecond timestamp after
successfully removing an event from a surface input queue. The record contains
surface, input serial and kind. It precedes the IPC reply: it is not a timestamp
for hardware input arrival, client receipt or application handling. Empty polls
and the default timing-off build perform no additional trace clock read.

The pure SPARK `Compositor_Input_Trace` retains at most 64 records, counts invalid
records and overflow with saturating counters, and preserves prior entries on
append. Its existing level-2 proof has 26 checks, zero unproved: seven flow and
19 prover results. Hosted policy tests cover 1,000 batches, saturation, reset,
invalid identities/kinds and unavailable timestamps. New actual queue tests also
verify timing-off behavior, exact records, overflow and empty polls. Actual
append/drain tests verify 200 records across five batches.

There is no formatting, IPC, allocation or waiting in the dequeue recorder.
The opt-in timing diagnostic drain emits records and count/invalid/dropped totals
once per reporting interval, then clears storage. This temporary serial export
supports integration testing; it does not replace the planned subscribable
metrics/trace transport. The metrics service currently exposes aggregated
summaries, and its publisher adapter review is a separate integration gate.

`check-input-publication.py` joins dequeues with accepted publication records
using **both surface and serial**, independent of batch print order. A client
supplies `Input_After`; the match is metadata evidence only. Repeated watermarks
across different publications are allowed. Unknown or unobserved watermarks are
counted explicitly. Incomplete printed batches, overflowing/invalid records, duplicate identities
and reversed-time records are rejected, as is an observed matching dequeue after publication.
A report requires at least one match. A capture can end before an in-memory
batch is drained; closed printed batches do not establish capture of every
event that occurred during the run. It does not infer pixel causality,
visibility, output presentation, scanout, photons or physical keypress timing.

Run hosted checks in Nix:

```sh
nix develop -c python3 tests/compositor/test-input-queue-integration.py
nix develop -c python3 tests/compositor/test-input-trace-integration.py
nix develop -c python3 tests/compositor/test-input-publication.py
```

For native evidence, hold `coordination/build.lock`, build Desktop with the
pinned Alire compiler from `kernel`, selecting `-XCUBIT_COMPOSITOR_TIMING=on`.
Use the resulting `userspace/services/desktop/build-timing/desktop.svc` as the
absolute `CUBIT_DESKTOP_IMAGE` override for the 120-second, four-CPU TCG
`ccl-workspace` headless test. Validate the captured serial log with:

```sh
python3 tests/compositor/check-input-publication.py SERIAL_LOG > REPORT.json
```

Native timestamps under TCG and serial instrumentation are functional evidence;
they are not supported-hardware performance measurements. Linking accepted
publication identities to the output frames that actually consume them remains
necessary before extending the measured software path to presentation.

The subsequent [render pipeline trace](render-pipeline-trace.md) adds writer,
submission and software-completion associations for successful draw work.
It does not establish which pixels survive or when they become visible.

## Native verification, 2026-10-01

Session 34623 completed with exit 0. Both default and timing-on Desktop variants
built. The four-CPU TCG `ccl-workspace` profile passed its 120-second test and
final fault scan. The trace validator found 247 dequeue records in 19 complete
input batches, 84 accepted publications and 84 matching per-surface watermarks;
unknown/unmatched counts and all reported invalid/drop counters were zero.
These are the exported records, not a claim to capture every event at shutdown.

All seven source/staged-binary hashes remained unchanged through validation:
`/tmp/cubit-input-publication-native-inputs.sha256`. Kernel hash:
`/tmp/cubit-input-publication-native-kernel.sha256`. Evidence:
`/tmp/cubit-input-publication-native-v2.log`,
`/tmp/cubit-input-publication-native.serial`,
`/tmp/cubit-input-publication-native-report.json`.

The first native attempt (75200) stopped before QEMU because the CCL image-tool
binder found stale objects after a `ccl-vm.ads` change. A forced host-tool rebuild
resolved that build-state failure; no CCL source was edited by this work. The
initial drain harness also needed to ignore Ada Text_IO's final blank line;
the corrected harness and source-trace regression passed in 80550. The actual
queue/trace hook checks passed in 45874. These failures were not native passes.

The default staged Desktop includes the hook with timing disabled. The separate
`build-timing/desktop.svc` binary enables records; normal desktop operation does
not emit these trace records. Existing software rendering remains the exercised
backend; this gate does not establish Mesa/GPU presentation or physical latency.


Retained close requests (kind 10) can be redelivered with the same surface and
serial until the client acknowledges them. The analyzer now counts all observed
deliveries (`input_delivery_records`), unique identities (`input_records`) and
retried close identities separately. A publication watermark referring to a
retried close is counted as `ambiguous_watermarks` and excluded from latency
joins: the watermark cannot identify a delivery attempt. The first observed
attempt still bounds clock validation; a publication preceding it is rejected.
Ordinary repeated input identities, mismatched retry kinds, reversed clocks,
loss and incomplete batches remain errors. The standalone checker still
requires at least one unambiguous match by default.

Actual Main enqueue/dequeue/trace glue confirmed two trace records for a retry
with unchanged serial and distinct timestamps (hosted 95674). Retry-aware input
and render-pipeline regressions passed (80089), including refusal to fabricate
a latency for ambiguous retry metadata. Reanalysis of the previous native
render capture retained all 168 input-associated draw joins from 247 delivery
records; it contained no close retries or ambiguous watermarks. This is
reanalysis of that capture, not a new native retry run. Evidence:
`/tmp/cubit-close-trace-retry.log`, `/tmp/cubit-close-trace-retry-final.log`, and
`/tmp/cubit-render-pipeline-retry-aware.json`.
