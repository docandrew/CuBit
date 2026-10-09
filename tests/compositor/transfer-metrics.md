# Desktop transfer metrics

Desktop publishes bounded delta counters through its existing metricsvc publisher:

| Key | Name | Unit | Meaning |
| --- | --- | --- | --- |
| 9 | desktop.gpu_readback_bytes | bytes | Pixel payload of successfully submitted GPU readback regions |
| 10 | desktop.cpu_copy_bytes | bytes | Successful synchronous CPU readback-copy rows, excluding stride padding |

These are not total GPU bus traffic or total desktop memory bandwidth. Submission
is not proof that a GPU transfer completed. CPU copies already performed remain
counted even if later retirement fails. Software-only backends report zero for
this GPU readback path; that does not mean the desktop makes no other copies.

Lifetime owner counters saturate rather than wrap. SPARK delta policy rejects
saturation or decreasing totals. The event loop logs and disables these samples
on invalid counters. An unavailable clock preserves the previous snapshot;
publisher refusal advances it, relying on existing loss counters rather than
retrying potentially accepted samples. Thus stream totals are incomplete when
measurements are dropped; always inspect loss metadata with graphs.

Each page includes twelve declarations, leaving 51 of its 63 record slots for
samples. The SDK retains two bounded pages. No waiting or unbounded work queue
is added. A slow collector causes counted drops, not reuse of an in-flight page.

## Evidence and limits

The accompanying evidence records selected SPARK proofs, Linux-hosted Mesa byte
comparison and mutation tests, collector/publisher fault tests, and native CuBit
software-fallback integration. The owner proof has 66 checks, facade 215, delta
policy 2, and schema/batch policy 42, with zero unproved in each selected report;
these overlap and must not be summed. This is modular proof, not whole-program
verification. Main-loop orchestration, FFI implementation, mapping authority,
actual GPU completion and physical display behavior remain outside those proofs.

Native normal collection passed authenticated metadata and stage delivery,
menus, cursor repair at 100%/125% scaling and log collection. Native overload
held a real grant for 30 seconds: 600 immutable-page checks, three menu
restorations during the hold, then resumed delivery with 567 drops reported.
These are correctness results under QEMU, not latency or throughput benchmarks.
Hardware acceleration, 240 Hz and physical keypress-to-photon remain separate
gates. The frozen hardware candidate24 predates this metrics follow-up.

Run hosted collection and overload tests in the pinned Nix environment:

```sh
python3 tests/compositor/test-transfer-delta.py --output /tmp/cubit-transfer-delta-check
cd kernel
alr exec -- gprbuild -p -P ../tests/compositor/metric_batch_stream.gpr
../tests/compositor/build/metric-batch-stream/metric_batch_stream_tests
alr exec -- gprbuild -p -P ../tests/compositor/transfer_metrics_store.gpr
../tests/compositor/build/transfer-metrics-store/transfer_metrics_store_tests
alr exec -- python3 ../tests/compositor/test-desktop-metric-publisher.py
```

The partial-vulkan-facade runner also checks each frame's counter deltas against
independent C readback/copy counters and verifies rejected writers do not change
those counters. Its existing negative controls reject unnecessary full repairs,
full transfers, and incorrect target history.

For native fixtures, `build-native-metrics-fixtures.py` accepts explicit
`--source-root`, `--platform-root`, `--toolchain-root` and a new `--output`.
It copies the selected runtime, collector/observer/stall sources and startup
object into private output, records hashes and commands, and checks for source
drift before success. The runtime is reused, not rebuilt. Supply stable matching
snapshots (or hold the shared build lock). A successful build is not a native
execution result; boot the returned seed using an isolated test image.

The dedicated `metrics-transfer-observer` checks byte declarations and zero
GPU-readback samples on the software fallback. Select it with the fixture
builder's `--transfer-observer`. It must not be used to assert zero work on a
hardware rendering run. The generic observer is retained separately.

`run-native-transfer-metrics.py` accepts the compositor artifact, matched boot
seed directory and a new output directory. Supply `--platform-root`,
`--metrics-seed` (the fixture builder's seed directory) and `--observer-build`
(a matched authenticated Desktop log-observer build with inputs/result manifests).
The latter is an explicit prebuilt dependency, not rebuilt by this helper.
Use `--approve-render --cursor-motion --scaled-cursor-motion` for the normal
software-fallback regression, and add `--metrics-stall` to exercise backpressure.
Boot seed inputs include kernel, initrd, Display, Clock and Logstore. All are
recorded by hash; prebuilt seeds are not represented as current-source rebuilds.
The portable runner and rebuilt transfer observer passed native run52 with
authenticated metrics, three menu restorations, 32 cursor movements at100% and
16 movements at125%. The separate stall45 test covers slow-collector recovery.

## Completion and diagnostic profiling

Keys 11 (`desktop.completion_dispatch`) and 12
(`desktop.diagnostic_output`) are inclusive wall durations in microseconds.
Existing keys 1–10 retain their meaning. Completion dispatch measures one
bounded collection pass only when a non-metrics completion was processed;
metrics-only and empty passes emit nothing, avoiding a telemetry feedback loop.
It reuses the initial completion-budget clock and samples again after dispatch.
This is handler time, not queue waiting or Display service processing time.

Diagnostic output measures from the first actual opt-in serial trace write
through the end of that report, including subsequent formatting and writes.
It excludes formatting of the first call's arguments. Empty reports and
production timing-off reports emit nothing. The measurement goes through the
bounded metrics stream, not back into the serial report. Neither stage proves
hardware presentation or input-to-photon latency; wall time includes preemption.

The `--profiling-observer` native fixture option requires authenticated positive
sample counts for both new keys, exact names/units and the existing Mesa fallback
transfer counters. Run it with a timing-on, metrics-on Desktop. Ordinary workload
observers remain usable with timing off. Native evidence records frozen
platform/runtime provenance; it is not a current-world build or hardware benchmark.

The scoped schema/batch proof reports 45 checks (11 flow, 34 prover), with none
unproved. It covers checked elapsed-time records and bounded batch policy, not
main-loop instrumentation, SDK transport, Mesa, kernel timing or hardware.
Hosted tests cover 61,260 clock/codec cases, 1,000 batching cycles, held-page
immutability under 898 drops and 18 publisher fault scenarios. Twelve metadata
records leave 51 samples per page; no page allocation or queue bound increased.
