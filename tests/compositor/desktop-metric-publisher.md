# Desktop metrics publisher boundary

`Desktop_Metric_Publisher` is the serialized Desktop-side adapter for the two
SDK pages. The metrics-enabled Desktop instantiates it using the executable's
generated manifest capability binding. The default build selects a disabled
implementation and does not contain a publisher object.

Recording a validated completion or execution-stage duration uses the proved
release/stage conversion and self-describing batch policy. It appends directly to the SDK pages without a
second queue, extra clock, IPC or allocation. `Pump` may submit one batch, using
the shared `Compositor_Requests.Allocate` sequence and at most two tracked
requests. Caller scheduling must provide the 100 ms idle flush opportunity.
The first grant setup occurs in Pump; its syscall cost still requires hardware
measurement. It is not a proven real-time bound.

Before forwarding a completion to the SDK, the adapter requires a matching
outstanding token and a definitive envelope. `Compositor_Metric_Completion`
checks kernel validity/status, tag length/flags/reserved bits, unused words,
and exact accepted-plus-rejected count for the submitted batch. Its functional
release-safety contract and termination both prove (two checks, none unproved).
Known terminal refusal statuses require zero payload words. Unavailable,
unknown, malformed or invalid replies quarantine telemetry and retain pages.

Telemetry quarantine does not call Desktop's presentation quarantine. Later
measurements become locally counted drops, and later pumps perform no calls
or token allocations. Pages and grants remain alive until process teardown;
the adapter deliberately does not try to revoke/reuse uncertain allocations.
The whole adapter is a SPARK-off foreign-interface boundary; the conversion,
batch decisions, reply predicate and request state are separate SPARK units.
Kernel token authenticity, serialized calls, SDK behavior under its admitted
inputs and grant lifetime are boundary assumptions, not proved by these units.

The current SDK has the separately documented invalid-completion and disabled-
publisher defects. This adapter avoids those paths: invalid completions never
reach SDK.Complete, and disabled/quarantined publishers never receive SDK.Put.
This caller-side protection permits independent integration without applying
the pending runtime-owner patch. It does not fix or excuse the defects for
other SDK callers. The original SDK regression still reproduces them.

```sh
nix develop -c python3 tests/compositor/test-desktop-metric-publisher.py
nix develop -c gprbuild -q -p -P tests/compositor/metric_completion.gpr
nix develop -c tests/compositor/build/metric-completion/metric_completion_tests
nix develop -c gnatprove -P tests/compositor/metric_completion.gpr -u compositor_metric_completion.ads --level=2
```

The hosted fixture compiles the actual adapter with the current unpatched SDK,
real portable page/record/policy units, and mocked IPC/grants. Eighteen cases
cover two outstanding pages, overload drops, out-of-order release, repeated
retired completion, unrelated tokens, unavailable clocks, grant/submission
failure, token exhaustion, invalid CQEs, transport failure, malformed reply
fields/counts, denial and definite temporary refusal. Quarantine cases perform
1,000 additional record/pump iterations and assert no more submissions or token
allocation. This is functional fault coverage, not native timing evidence.

Session 68234 passed those cases and the original SDK defect reproductions;
`/tmp/cubit-desktop-metric-publisher-v2.log`. Session 48262 passed the explicit
completion contract proof and all per-page accepted/rejected count splits;
`/tmp/cubit-metric-completion-policy-v2.log`. Session 89515 compiled a real
generic instantiation with the pinned Alire native compiler/runtime and private
objects under the shared lock; `/tmp/cubit-desktop-metrics-native-compile.log`.
The first hosted compilation (62963) failed because the generic spec lacked
the body's explicit SPARK-off annotation; it was corrected before those passes.

## Native collection integration

Main routes matching metric completions before presentation dispatch, records
validated display releases and execution-stage durations, and pumps at most one metric batch after
presentation work. Pending data adds a rounded-up flush deadline to the existing
non-consuming idle wait; it does not delay an earlier input/status deadline or
pending paint. No extra tracing clocks or publisher pages are used when metrics
are disabled. Metrics can be enabled without the verbose timing trace exporter.

Build from the repository root under `coordination/build.lock` in Nix:

```sh
bash tests/compositor/build-metrics-desktop.sh
```

The helper builds/stages normal Desktop and metrics.svc, then generates a
separate Desktop manifest with a metrics request and builds
`userspace/services/desktop/build-metrics/desktop.svc`. The normal manifest is
unchanged. It also builds a native observer requesting only observer authority.
Run the CCL workspace gate under the same lock:

```sh
CUBIT_DESKTOP_METRICS_TEST=1 \
CUBIT_DESKTOP_IMAGE="$PWD/userspace/services/desktop/build-metrics/desktop.svc" \
bash tests/headless/run.sh --test ccl-workspace --accel tcg,thread=multi \
  --cpus 4 --timeout 120 --serial /tmp/desktop-metrics.serial --keep-logs
```

Native session 58905 completed with exit 0: both Desktop variants compiled,
the observer saw a growing real Desktop series (three frames/three batches),
and the complete 120-second CCL workspace gate and final fault scan passed.
The observer checks source PID, publisher tag family, metric name/kind/unit,
growth and zero reported loss/rejection/gaps. All 18 source/binary hashes
matched at exit. Evidence: `/tmp/cubit-desktop-metrics-native.log`, `.serial`,
`-inputs.sha256`, and `-kernel.sha256`. This was four-CPU TCG functional testing,
not hardware latency measurement. The fixture's service priorities are not
production tuning or evidence of behavior under CPU saturation.

Native ELF inspection shows the enabled publisher object reserves 16 KiB,
including the two 4 KiB payload pages, state and alignment. The disabled build
has no corresponding publisher object. The earlier description of two pages
describes payload storage, not the entire aligned SDK object size.

Default startup profiles remain unchanged. Normal startup enablement, collector
failure/overload coverage, collector scheduling under load, user-facing querying,
raw trace subscription/export and hardware measurements remain separate gates.
The first integrated build (10625) stopped on the reserved Ada identifier
`Delay`; it was renamed before the passing build. Hosted rerun 83786 passed the
updated 21-check wake policy, adapter fault matrix and render-trace glue.

## Native malformed-collector isolation

The test-only collector in `metrics-fault/` registers the metrics service role
in an isolated startup profile. It accepts publisher requests but deliberately
replies with an impossible accepted-record count. It never acquires a grant.
The normal metrics service binary and SDK sources remain unchanged.

After the ordinary metrics build, run under the same Nix/shared-lock convention:

```sh
bash tests/compositor/build-metrics-fault.sh
CUBIT_DESKTOP_METRICS_TEST=1 CUBIT_DESKTOP_METRICS_FAULT=1 \
CUBIT_METRICS_FAULT_IMAGE="$PWD/tests/compositor/metrics-fault/build/desktop-metrics-fault.svc" \
CUBIT_DESKTOP_IMAGE="$PWD/userspace/services/desktop/build-metrics/desktop.svc" \
bash tests/headless/run.sh --test ccl-workspace --accel tcg,thread=multi \
  --cpus 4 --timeout 120 --serial /tmp/desktop-metrics-fault.serial --keep-logs
```

Native session 67661 completed with exit 0. Desktop reported telemetry
quarantine with one invalid completion and no rejected records. After that
message, Workbench sampled its live labels and saved/opened workspace files;
the full 120-second interaction gate and final fault scan passed. The collector
checks that it receives no more than the two requests allowed in flight.
All 11 source/binary hashes matched at exit, including the real metrics service
and unchanged SDK sources. Evidence: `/tmp/cubit-desktop-metrics-fault-native.log`,
`/tmp/cubit-desktop-metrics-fault.serial`, `-inputs.sha256`, `-kernel.sha256`.

This verifies malformed-reply isolation with the actual kernel completion path.
It does not cover a collector holding acquired grants indefinitely, service
restart/reconnection, all transport faults, or hardware performance under load.
Pages remain deliberately retained after quarantine; no automatic reconnect is
implemented. Normal startup rollout and raw trace/collector tooling remain open.

## Native stalled acquisition and recovery

The test-only `metrics-stall/` collector acquires the first real Desktop
publisher grant and retains it through 600 sleeps of 50 ms. At each wake it
compares every published word against its initial snapshot. It then returns
the acquisition before replying and resumes ordinary batch acknowledgments.
The gate requires a subsequent batch after the first two to report producer
loss, demonstrating that telemetry drops samples and recovers after capacity
is exhausted. The ordinary CCL workspace interaction and final fault gates
also run. This tests a finite stall; indefinite retention and service restart
remain separate scenarios. The sleep durations are guest scheduling delays,
not calibrated host or physical display latency measurements.

After the normal metrics build, use Nix and the shared build lock:

```sh
bash tests/compositor/build-metrics-stall.sh
CUBIT_DESKTOP_METRICS_TEST=1 CUBIT_DESKTOP_METRICS_FAULT=1 \
CUBIT_DESKTOP_METRICS_STALL=1 \
CUBIT_METRICS_FAULT_IMAGE="$PWD/tests/compositor/metrics-stall/build/desktop-metrics-stall.svc" \
CUBIT_DESKTOP_IMAGE="$PWD/userspace/services/desktop/build-metrics/desktop.svc" \
bash tests/headless/run.sh --test ccl-workspace --accel tcg,thread=multi \
  --cpus 4 --timeout 120 --serial /tmp/desktop-metrics-stall.serial --keep-logs
```

Native session 80435 completed with exit 0: the full 120-second, four-CPU
TCG CCL workspace gate and final fault scan passed. All ten source/binary
hashes matched at exit. The capture validator observed two live-label samples
and four saves strictly between the grant-held and hold-complete markers;
all 600 comparisons succeeded. Batch 3 resumed publication and reported 83
dropped measurements, with no telemetry quarantine. This confirms a finite
collector stall does not stop this native desktop interaction workload.

Run the additional ordering check (required to establish interaction during
the hold rather than merely before/after it):

```sh
python3 tests/compositor/check-metrics-stall.py /tmp/desktop-metrics-stall.serial
```

The checker passed a positive fixture and rejected seven invalid captures
(missing samples/saves/recovery, zero loss, premature batch, quarantine or
ambiguous hold intervals). Native evidence is retained in
`/tmp/cubit-desktop-metrics-stall-native.log`,
`/tmp/cubit-desktop-metrics-stall.serial`,
`/tmp/cubit-desktop-metrics-stall-report.json` and
`/tmp/cubit-desktop-metrics-stall-inputs.sha256`. The initial waiter 69529
exited before edits because Nix could not access its cache; 80435 is the
completed retry. No production source changes were needed for this gate.

This does not establish CPU-saturated scheduling performance, recovery from
collector death/restart, or physical input-to-photon latency. The fixed two-page
bound is established by the publisher implementation and hosted fault tests;
this native fixture observes one acquired page, loss, and later recovery,
not a complete native heap or grant-allocation census.

## Lower-priority collector with CPU-bound workers

`init-metrics-load.ccl` runs real metrics.svc at priority 2, Workbench and four
CPU workers at priority 3, and Desktop at priority 4. Each worker sleeps five
guest seconds, then repeatedly performs arithmetic for twenty guest seconds,
checking the clock once per 100,000 iterations. Worker start/end markers and
positive work counts are required; the capture checker requires a Workbench
save while all four workers are active. These markers establish overlapping
CPU-bound work, not measured occupancy of all CPUs or a calibrated utilization.

The observer waits thirty guest seconds, validates the real Desktop series,
and announces its baseline. The input injector then toggles the Apps menu to
cause fresh Desktop drawing. Only growth after this baseline passes. The
observer still checks identity/schema, no rejected records and no batch gaps;
producer drops are reported and allowed because bounded loss under overload
is intentional. Every emitted batch contains its own metric declarations.

Run in Nix with the shared build lock:

```sh
bash tests/compositor/build-metrics-load.sh
CUBIT_DESKTOP_METRICS_TEST=1 CUBIT_DESKTOP_METRICS_LOAD=1 \
CUBIT_DESKTOP_IMAGE="$PWD/userspace/services/desktop/build-metrics/desktop.svc" \
bash tests/headless/run.sh --test ccl-workspace --accel tcg,thread=multi \
  --cpus 4 --timeout 120 --serial /tmp/desktop-metrics-load.serial --keep-logs
```

The first native run (68202) completed all four worker loops and three saves
during their overlap, but failed the observer growth gate: it did not explicitly
request drawing after the observer started. This is not a passing test. The
corrected fixture supplies those post-baseline redraws. Retry 68284 passed the
complete 120-second native CCL workspace gate and final fault scan. Three saves
occurred during four-worker overlap; all workers completed positive CPU work.
After the collector baseline of 82 frames, the explicit redraw grew the series
to 83 frames across 75 batches, with zero producer drops, rejects or batch gaps.
All eight recorded source/binary hashes matched at exit. Both Desktop variants
were rebuilt with the close-request coalescing fix included.

Evidence: `/tmp/cubit-metrics-load-v2-native.log`,
`/tmp/cubit-metrics-load-v2.serial`, and
`/tmp/cubit-metrics-load-v2-inputs.sha256`. This establishes the lower-priority
collector's functional integration under this CPU workload, not that every
CPU was continuously saturated or that deadlines met a hardware target.
The capture checker passed a synthetic positive case and rejected six invalid
captures (23447). No production compositor or scheduler change was made for
this workload. Hardware input/display latency, quantitative CPU saturation,
service restart and normal-startup rollout remain open.


## Normal desktop-session rollout

`make -C kernel desktop-metrics` is now the supported production software
fallback build with metrics enabled. `desktop-session-content` depends on this
target, so `run-desktop` stages it. The desktop disk overlay includes
metrics.svc, and `init-desktop-session.ccl` starts the collector at priority 2
before Desktop at priority 4. Bare `make desktop` remains metrics-off for test
and startup profiles that do not launch a collector. The generic `world`
startup and hardware-specific profiles are not changed by this rollout.
Experimental Mesa/timing/storage variants retain their explicit GPR paths;
the new target selects the standard production fallback explicitly.

The manifest/compiler logic now lives in `tools/build_desktop_metrics.sh`,
shared by the supported target and the compositor test build helper. The
manifest uses generated capability bindings. A full `run-desktop` build is
needed before relying on the fast launcher: it reuses whatever binaries were
last staged, which can include a subsequent metrics-off test build.

Native build 67016 passed and compared the staged Desktop byte-for-byte with
the metrics-enabled output. Packaging check 14712 ran the actual
`prepare-desktop-disk` Makefile target against temporary ext2 images, extracted
Desktop, metrics.svc and init.ccl, verified each byte-for-byte, checked service
order/priorities and filesystem integrity, and confirmed the base was
unchanged. Reproduce under Nix/shared build lock:

```sh
make -C kernel desktop-metrics
python3 tests/compositor/test-metrics-session-disk.py
```

Evidence: `/tmp/cubit-metrics-rollout-build.log`,
`/tmp/cubit-metrics-rollout-inputs.sha256`, and
`/tmp/cubit-metrics-session-disk.log`. The build and disk checks do not boot
the full normal desktop profile. That gate remains pending; the earlier native
collector tests used isolated CCL workspace profiles. NUC hardware packaging,
raw event-stream export and hardware latency measurement remain open.


Normal-profile boot gate 94329 subsequently passed. The test
`test-normal-session-boot.py` boots the exact normal `init-desktop-session.ccl`
from a private ISO and disposable disk produced with the real normal overlay.
It records twelve inputs, including the staged kernel/initrd and service
binaries; these are existing build inputs, not a claim that every component
was freshly rebuilt. All seven startup services launched. Three real emulated
keyboard Apps-menu open/close cycles changed the screen and restored a
252,000-pixel wallpaper region exactly. The guest fault scan and telemetry
quarantine check passed, recorded inputs stayed unchanged, and the temporary
base image stayed unchanged. No development disk was written.

Reproduce in Nix under the shared build lock with staged normal-session
prerequisites available:

```sh
make -C kernel desktop-metrics
python3 tests/compositor/test-normal-session-boot.py
```

Evidence is retained in `/tmp/cubit-normal-session-native.log` and
`/tmp/cubit-normal-session-evidence/` (serial, result, input hashes, QEMU log and
representative screenshots). Original private boot artifacts remain at
`/tmp/nix-shell.F8AmP5/cubit-normal-session-yz8qv_up/`. This closes the normal
startup gate for this four-CPU TCG configuration. The earlier real collector
observer/load gates establish metric publication; this exact normal-profile
test does not add an observer to the shipped startup profile and does not
measure physical display latency or hardware acceleration.


## Execution-stage series

The metrics-enabled Desktop also publishes these inclusive durations, in
microseconds: key 3 `desktop.input_dispatch`, key 4
`desktop.request_dispatch`, key 5 `desktop.scene_draw`, and key 6
`desktop.submit_call`. Keys 1 and 2 retain their per-output submit-to-release
span meaning. The input and request timers start immediately before their
handlers; they exclude earlier queue waiting. Drawing includes the existing
scene/cursor/retained-target repair scopes. Submit measures the call into Display
(and optional trace bookkeeping when verbose timing is enabled). None is a
scanout or physical input measurement. Handler scopes can include drawing and
its instrumentation, so summing these series would double-count nested work.

`Compositor_Stage_Metrics` rejects unavailable or reversed clocks, preserves the
completion timestamp and exact elapsed value, and sets correlation to zero:
these aggregate durations do not claim a causal input/frame join. The wrapper
records at most six declarations before the first sample in each page. The
existing two pages still hold at most 63 records each (57 measurements plus six
declarations); exhaustion drops measurements without a third page or queue.
Stage hooks stop reading extra clocks after telemetry quarantine unless the
separate verbose timing mode needs them. Existing release-span accounting is
unchanged. Timing calls and bounded metadata encoding still have a cost; no
instrumentation-overhead or worst-case execution-time claim is made.

Hosted session 64334 passed 40,840 clock/codec cases, declaration name/kind/unit
checks, 1,000 complete batch lifecycles, and real two-page overload/redeclaration
checks (114 admitted measurements, 886 drops while both pages remain held).
All 18 actual Desktop adapter scenarios passed, including mixed release/stage
records. The stage unit has 22 SPARK analysis results (19 prover checks, three
termination flow results); the updated batch policy has 21 (13 prover checks,
eight flow results). Both have zero unproved or justified results. Evidence:
`/tmp/cubit-stage-metrics-host.log` and their `build/*/obj/gnatprove/gnatprove.out`.
The SPARK boundary does not include the legacy Desktop handlers, SDK, clock
source, collector, or kernel grant mechanics.

Reproduce within Nix, from `kernel`, with `alr exec -- gprbuild` and
`alr exec -- gnatprove` against `../tests/compositor/stage_metrics.gpr`,
`metric_batch_policy.gpr` and `metric_batch_stream.gpr`, plus
`alr exec -- python3 ../tests/compositor/test-desktop-metric-publisher.py`.
Native validation is tracked separately; hosted passes do not establish it.


Native session 48297 completed successfully: the 120-second four-CPU TCG CCL
workspace gate collected all four stage kinds with positive sample counts,
checked their authenticated Desktop source, names, units and latency kind,
and evaluated live names/counts/loss state through CCL. The release series
continued growing (three frames/five batches at the observer checkpoint).
The complete workspace and final fault scan passed; recorded source/binary and
private base hashes matched. The observer was restored after testing.

The same Desktop binary then passed the native Observatory interaction gate:
39 nonempty six-row updates, 20 visible pause/resume cycles, graph bars,
graph/table restoration, paging, refresh and close. Viewer inputs and its
private base image remained unchanged. The metrics-disabled Desktop also
compiled successfully in session 71780; its binary was not staged. Logs:
`/tmp/cubit-stage-metrics-native.log`, `/tmp/cubit-stage-metrics-off-build.log`;
retained screenshots/reports: `/tmp/cubit-stage-metrics-evidence/`.

The paused native table reports cumulative p99 histogram upper bounds of
1536 us input dispatch, 896 us request dispatch, 98304 us scene draw, 3072 us
submit call, and 229376 us output-0 submit-to-release. Output 1 has no samples
in this single-output fixture. These values include startup, the software
emulated graphics path, and nested scopes; they are neither steady-state
benchmarks nor hardware latency claims. They justify inspecting repeated
scene repair and presentation copies, not claiming a NUC bottleneck.
