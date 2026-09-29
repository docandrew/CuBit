# Performance baseline: scheduling, IPC, audio and storage

## Status and scope

2026-09-09, working tree based on `cc995bd`. First exploratory measurements,
not an established performance envelope. Native CuBit applications in QEMU;
not the Linux CCL emulator. **No claim yet of p99 < 1 ms end-to-end input,
display or audio latency.** No Linux comparison or hard-real-time guarantee.

The first measurements exposed an **equal-priority scheduling starvation bug**.
That FIFO-ordering defect is now fixed and regression-tested. Loaded SMP audio
still underruns at the smallest queue depth: fairness is necessary, but does
not establish low wakeup latency. See the follow-up measurements below.

## Reproduce

For the September 11 input follow-up see
[focused-application input measurements](#focused-application-input-september-11).

Commands and fixture details are in [tests/performance](../tests/performance/README.md).
All builds and analysis used Nix; native benchmark apps use `-O2`, the CuBit
userspace runtime and explicit 16 MiB ELF stack reservations. Assertions were
enabled only in Linux-hosted tests, not native services or the kernel.

Environment: AMD Ryzen 7 5800X host, QEMU 11.1.0, explicit KVM acceleration,
q35, Broadwell virtual CPU, 128 MiB guest RAM, NVMe disposable test disk.
QEMU warns that host PCID/HLE/RTM features are unavailable; the guest also
warns that invariant TSC is not advertised. Host scheduling was not isolated.
These numbers must not be compared to TCG emulation or physical laptop input.

## Measurement boundaries

| Metric | Begins | Ends | Important exclusions / inclusions |
|---|---|---|---|
| Synchronous IPC | Before capability call | Caller resumes with reply | Includes both kernel crossings and service work; reply verification and histogram update outside interval |
| Async observed completion | Before submission | Client observes matching completion | Depth 16; includes queueing and client completion-drain delay, not just transport time |
| Mixer execution | Before `Mixer.mixPeriod` | After final DMA-period output | One producer; includes mixing and final output write, not waiting for hardware |
| Audio notification delivery | HDA before period-message submission | Mixer returns from receive | Excludes IRQ-to-driver dispatch, device interrupt generation, and physical output |
| Producer refill interval | One producer iteration | Next iteration | Explicit 1 ms sleep model; this does not introduce polling into the mixer |

Ordered TSC reads (`lfence; rdtsc; lfence`) preserve raw ticks. Three 200 ms
guest-clock calibration intervals must agree within 2%; conversion uses their
min/max midpoint. This is a consistency check, **not external-clock accuracy**.
Observed rates varied between runs (~3.54–3.79 million ticks/guest ms), another
reason to retain raw values and avoid overinterpreting small differences.
Cross-CPU TSC agreement is assumed, not measured. Four virtual CPUs alone do
not make this a cross-core IPC benchmark; these participants run on CPU 0.

The fixed-size histogram has four buckets per power-of-two octave and admits
one million samples before saturation. p50/p95/p99 are **bucket upper bounds**;
min/max are observed values. Counter-read overhead is reported separately and
is not subtracted. Logging and histogram updates are outside measured IPC and
mix execution intervals, but still perturb the surrounding workload. HDA and
mixer timing instrumentation is enabled in the current service builds; existing
periodic mixer diagnostics remain enabled. This is an instrumented baseline.

IPC has 64 warmups, 20,000 synchronous round trips and 8,192 async completions
per phase. It runs first without kernel tracing, then with tracing enabled to
make observer costs visible. The old integer `avg_us=0` summary was removed;
aggregate `total_ms` is coarse elapsed loop time, not per-message latency.

## Initial results

### Focused-application input, September 11

Native `bench-input` runs through the asynchronous desktop/display build.
Source and focused recipient are the same process; these are **closed-loop
normalized-publication-to-app timings**, not device IRQ-to-app measurements.
The compositor/client/busy-peer participants remain on CPU 0 in either guest
configuration. Each cell summarizes 2,048 validated transitions, with no
warmup samples excluded. Each complete run delivered all 6,144 transitions
with no observed loss, duplicate, resync or publication failure.

| KVM guest / load | Continuous p99 upper (µs) | Paced p99 upper (µs) | Repaint-request p99 upper (µs) |
|---|---:|---:|---:|
| 4 vCPU, idle | 4.66 | 9.32 | 223.76 |
| 1 vCPU, priority-4 busy peer | 1,029.61 | 1,029.61 | 1,176.69 |
| 4 vCPU, priority-4 busy peer | 8,304.42 | 8,304.42 | 8,304.42 |

The exact >=1ms sample counts were respectively `0/0/0`,
`1379/1606/1770`, and `2038/2047/2046` (continuous/paced/repainting).
Thus the idle run meets the *observed* target and both loaded runs fail it;
the conclusion does not depend on a histogram bucket straddling 1 ms.
Publication-call p99 in the four-vCPU loaded run was about 0.72 µs continuous,
2.90 µs paced, and 0.72 µs repainting. Millisecond delays lie in the subsequent
input-receive call interval, which includes desktop/client scheduling and
dispatch. This does not by itself attribute every delayed cycle to a kernel
function or explain the difference between UP and SMP-enabled guests.

An earlier four-vCPU attempt using the existing 12-second busy peer was
**rejected**: load ended before the scenarios completed. These final loaded
runs use the same busy loop for 60 seconds; the runner checks that the entire
measurement is bracketed by load start/completion. Do not report the rejected
run's mostly idle later scenarios as loaded performance.

The existing scheduler preserves equal-priority FIFO fairness but has no
enforced interactive reservations. The current latency-contract syscall is
deliberately advisory. Enabling its untrusted hints as scheduler priorities
would introduce a CPU denial-of-service authority bypass, not fix admission.
See [next scheduling obligations](input-latency.md#september-11-measurement-gate-and-scheduling-gap).
No kernel scheduling policy or authorization check was changed in this
measurement increment.

Summaries: [idle SMP](../tests/performance/results/2026-09-11-input-kvm4-idle.json),
[loaded UP](../tests/performance/results/2026-09-11-input-kvm1-load.json),
[loaded SMP](../tests/performance/results/2026-09-11-input-kvm4-load.json).
All builds and hosted tests used Nix. Histogram boundary/rank/extrema/saturation
tests and seven report tests pass. No new latency proof is claimed. Calibration
uncertainty, periodic service logging, non-isolated host scheduling, finite
sampling, and closed-loop coordinated omission remain limitations.

### Experimental 250-microsecond peer scheduling

The follow-up changes the LAPIC scheduling cadence, not priority assignments or
advisory latency-class authority. See
[implementation and limits](input-latency.md#first-correction-submillisecond-peer-scheduling).
It is a responsiveness baseline for a future selective scheduler, not a claim
that 4 kHz periodic timers are the final policy or free of throughput costs.

Same input fixture, loaded at the same priority, 2,048 events/scenario:

| Configuration | Continuous p99 upper (µs) | Paced p99 upper (µs) | Repainting p99 upper (µs) |
|---|---:|---:|---:|
| 1 vCPU | 262.68 | 8.21 | 450.30 |
| 4 vCPU, first run | 298.36 | 261.06 | 447.53 |
| 4 vCPU, repeat | 295.22 | 258.32 | 516.64 |

All three runs delivered every transition and passed the observed p99 timing
gate with full load coverage. The SMP repeat had `1/2/0` samples >=1ms;
occasional observed outliers still reached about 6.2 ms. Therefore this meets
the measured percentile target for these closed-loop runs, **not** a hard
deadline or interrupt-to-app guarantee. The new busy-peer progress counters
reported 14,164,028 / 14,230,394 batches over 60 guest seconds (UP / SMP repeat),
with a maximum observed inter-batch gap of 2 guest milliseconds in each. Each
batch is 4,096 volatile arithmetic iterations. These are progress observations,
not a proof of general starvation freedom or reserved CPU capacity.

Loaded four-vCPU IPC with kernel tracing disabled completed 20,000 synchronous
round trips in 35 guest milliseconds and 8,192 pipelined asynchronous completions
in 5 milliseconds. These totals are coarse elapsed measurements. Histogram
p99 upper bounds were approximately 3.47 µs synchronous and 27.74 µs asynchronous.
The older recorded baselines had smaller tails, but they are not a matched
same-tree A/B experiment and cannot isolate timer overhead. A controlled
cadence/throughput comparison remains necessary before selecting a permanent
scheduler policy. This IPC fixture's busy peer is lower priority; its progress
gap reached 100 ms, unlike the equal-priority input fixture. Base-priority
scheduling has not become a guaranteed bandwidth allocation system.

Validation: the divider's four obligations (one flow initialization, three
prover checks including the phase/wrap contract) discharged with no assumptions
or justifications; 400,000 hosted tick transitions and production ready-queue/
locking tests passed. Native async IPC and multi-app DOOM passed, including
game pixels and a responsive Apps menu. This is not an audio-quality test.
Eight report tests pass. All builds and tests used Nix; kernel runtime
assertions remain disabled.

The subsequent physical-input desktop regression exposed fragmented telemetry:
display-service statistics interrupted every multi-write desktop statistics
record. The raw log showed keys, motion, dragging and both title-bar double
clicks, but the checker saw a header without its fields and reported zero
events. Another run caught the same issue in the backend startup banner.
Desktop now constructs its statistics, backend banner and pointer-trace records
before one debug write each; the regression oracle is unchanged. This addresses same-CPU
writer interleaving, not general cross-CPU log framing. Timing captures above
precede this logging-only correction. Serial diagnostics still perturb timing
and ultimately need a nonblocking structured logging path.

The final `desktop-display` run still failed its startup-marker gate: the single
`desktop: internal shell active` write interleaved **character by character**
with another CPU's Config/FS ACL diagnostics. The log retained complete input
statistics and both expected maximize/restore title-double events, but that
does not make the automated gate pass. This is recorded as an unresolved
cross-CPU debug-console/log-framing defect; no retry-to-green or parser
relaxation was used. Capture: `/tmp/cubit-quarter-records-final.log`.
Earlier failures are `/tmp/cubit-quarter-desktop.log` (split statistics) and
`/tmp/cubit-quarter-desktop-records.log` (split backend banner).

Artifacts: [UP input](../tests/performance/results/2026-09-11-input-quarter-kvm1-load.json),
[first SMP input](../tests/performance/results/2026-09-11-input-quarter-kvm4-load.json),
[repeat SMP input](../tests/performance/results/2026-09-11-input-quarter-kvm4-repeat.json),
[loaded IPC](../tests/performance/results/2026-09-11-ipc-quarter-kvm4-load.json).

### Follow-up: 1.5-millisecond peer scheduling

The next experiment uses 500-us timer interrupts, an independent two-tick
millisecond divider, and a three-tick scheduling divider. It reduces nominal
timer interrupts from 4,000 to 2,000 per CPU per second and peer-rotation
opportunities from 4,000 to about 667. Actual context switches are not counted;
IPC handoffs and blocking remain unchanged. No new interactive priority or
budget policy is enabled. See [implementation](input-latency.md#follow-up-experiment-15-ms-peer-rotation).

Same native loaded-input fixture, 2,048 transitions per scenario:

| Configuration | Continuous p99 upper (µs) | Paced p99 upper (µs) | Repainting p99 upper (µs) |
|---|---:|---:|---:|
| 1 vCPU | 1039.83 | 1039.83 | 1485.48 |
| 4 vCPU | 1790.05 | 1790.05 | 1790.05 |

Both captures pass integrity and full busy-peer coverage: all 6,144 events
delivered in each run, no failures. Both **fail the observed <1-ms p99 target**.
Exact >=1-ms counts are 421/744/636 (UP) and 1647/1726/1772 (SMP), out of 2,048
per scenario. Histogram numbers are bucket upper bounds, not exact quantiles;
the exact miss counts establish that this is a real target failure, not merely
a bucket straddling 1 ms. Largest observed SMP latency was about 6.01 ms.

The busy peer completed 14,641,395 batches (UP) and 14,431,608 (SMP), with a
maximum inter-batch gap of two guest milliseconds each. These totals cover
the entire 60-guest-second load interval, including time after input completion.
They are not an isolated throughput comparison: host scheduling, calibration,
different contention duration, and earlier desktop log-record changes confound
comparison with the preceding 250-us captures. Do not claim a percentage
throughput improvement from these single runs.

The result supports separating ordinary compute slices from bounded interactive
wakeup service; lengthening a universal quantum alone does not retain the
submillisecond input goal. The 1.5-ms setting remains an experiment, not an
accepted latency guarantee. The hosted production divider passes 600,000
transitions and all seven SPARK obligations; production queue/locking/lifetime
tests and eight benchmark-report tests pass. Kernel assertions remain disabled.

Artifacts: [UP input](../tests/performance/results/2026-09-11-input-1500-kvm1-load.json),
[SMP input](../tests/performance/results/2026-09-11-input-1500-kvm4-load.json).
The loaded four-vCPU IPC fixture also passes: untraced 20,000 synchronous
round trips in 33 guest milliseconds, 8,192 pipelined async completions in
4 milliseconds. Histogram p99 upper bounds are 2.90 us synchronous and
16.24 us asynchronous. Traced totals are 41/6 ms. The prior 250-us capture
reported 35/5 ms untraced and p99 upper bounds of 3.47/27.74 us; these coarse
single-run totals and tails do not establish a throughput improvement. The
priority-4 busy peer remains below the priority-5 IPC participants, with a
maximum observed inter-batch gap of 103 ms; this is not equal-priority fairness.
See [IPC artifact](../tests/performance/results/2026-09-11-ipc-1500-kvm4-load.json)
and `/tmp/cubit-1500-ipc-kvm4.log`.

Raw input captures: `/tmp/cubit-1500-input-kvm1.log` and
`/tmp/cubit-1500-input-kvm4.log`. These are closed-loop publication-to-app
measurements, not physical input-to-display or a hard scheduling bound.

### Native accounting overhead experiment

Captured a fresh four-vCPU KVM baseline before enabling accounting, then the
same fixtures after. All participants retain their prior priorities and the
1.5-ms scheduling cadence. This is one paired capture, not a statistically
controlled overhead estimate; host scheduling and guest calibration vary.

| Metric | Before | Accounting enabled |
|---|---:|---:|
| 20,000 sync IPC round trips, untraced elapsed guest ms | 28 | 30 |
| 8,192 pipelined async completions, untraced guest ms | 3 | 4 |
| Sync IPC p99 upper, us | 2.008 | 2.015 |
| Async IPC p99 upper, us | 16.064 | 16.117 |
| Loaded input p99 upper, all three scenarios, us | 1767.93 | 1759.22 |

The IPC and input p99 raw-tick histogram buckets are identical across the pair;
the small converted differences reflect guest clock calibration. Sync/async
coarse elapsed totals suggest a modest cost, not zero overhead. Traced IPC
totals are 37/6 ms before and 39/6 ms after. Do not infer a speedup from lower
single-run maxima or assign precise percentage overhead from millisecond totals.

Both input runs delivered all 6,144 transitions with zero failures and full
load coverage. Both still fail the observed <1-ms p99 target. Exact >=1-ms
counts (continuous/paced/repainting) were 1706/1683/1773 before, and
1684/1698/1771 after. The busy peer made 15,174,225 / 15,195,893 batches over
60 guest seconds, with a maximum inter-batch gap of 2 guest milliseconds each.
These are progress observations, not a guaranteed CPU allocation.

The native IPC caller snapshot reports 364,294,638 residency ticks, 1,179
scheduler dispatches and 40,128 direct dispatches, with zero accounting fault
or saturation. These are lifetime totals including startup and diagnostic
residency, not just the timed loop. It confirms both live hooks are exercised,
not independently exact native time attribution. Retirement regression passed.
The lower-priority IPC busy peer's longest gap was 95 ms before / 98 ms after;
ordinary strict-priority fairness is unchanged.

The multi-app desktop/DOOM regression passed, including game pixels and a
responsive Apps menu. This is not an audio-quality assertion. The hosted
accounting/locking tests, nine report tests, and focused 19-obligation accounting
proof pass. See [accounting scope](../tests/execution-accounting/README.md).

Artifacts: [before IPC](../tests/performance/results/2026-09-11-accounting-before-ipc.json),
[after IPC](../tests/performance/results/2026-09-11-accounting-after-ipc.json),
[before input](../tests/performance/results/2026-09-11-accounting-before-input.json),
[after input](../tests/performance/results/2026-09-11-accounting-after-input.json).
Raw captures are `/tmp/cubit-accounting-{before,after}-{ipc,input}.log` and
`/tmp/cubit-accounting-desktop-doom.log`. All builds, proofs, tests and report
generation used Nix. No new priority or latency authority is enabled.

### Earlier IPC and audio results

Microseconds below use guest-clock calibration. Each row is one run, not a
confidence interval or repeated-run stability claim.

| Configuration / metric | p99 upper bound (µs) | Observed max (µs) |
|---|---:|---:|
| 1 vCPU, idle, sync IPC, untraced | 1.73 | 22.72 |
| 1 vCPU, idle, async observed completion, untraced | 13.86 | 17.77 |
| 1 vCPU, priority-4 busy peer, priority-5 sync IPC, untraced | 2.31 | 24.83 |
| 1 vCPU, priority-4 busy peer, priority-5 async completion, untraced | 11.57 | 19.12 |
| 4 vCPU guest, same-CPU sync IPC, untraced | 1.72 | 27.29 |
| 4 vCPU guest, same-CPU async completion, untraced | 11.49 | 25.62 |
| 1 vCPU, idle, mixer execution, queue sweep | 5.40 | 87.81 |
| 1 vCPU, idle, HDA publication → mixer, queue sweep | 43.24 | 82.52 |
| 1 vCPU, idle, producer refill interval, queue sweep | 1106.85 | 2237.35 |

The first idle IPC run predates explicit single-flight async warmup; subsequent
runs include it. Do not treat small differences as an optimization result.
The loaded IPC test has a **lower-priority** competitor; it is not an
equal-priority fairness test or a saturated four-core benchmark.

Machine-readable snapshots, including raw ticks, tracing phases and sample
counts: [audio sweep](../tests/performance/results/audio-sweep.json),
[IPC with busy peer](../tests/performance/results/ipc-load.json),
[SMP-enabled IPC](../tests/performance/results/ipc-smp.json).

## Audio quality and buffering

The fixture generates stereo S16 at 48 kHz directly into reserved ring spans:
a 500 Hz triangle plus a bounded impulse once per generated second. No decoder,
floating-point synthesis or intermediate producer copy. It has only mixer
endpoint authority and reads statistics for its own stream, not other clients.

Four two-second phases target 2032, 1024, 512 and 256 queued frames. Each phase
settles for 500 ms before collecting its occupancy/underrun window:

| Target frames | Target duration at 48 kHz | Observed queued range | Reported phase underruns |
|---:|---:|---:|---:|
| 2032 | 42.33 ms | 1776–2032 | 0 |
| 1024 | 21.33 ms | 768–1024 | 0 |
| 512 | 10.67 ms | 256–512 | 0 |
| 256 | 5.33 ms | 0–256 | 0 |

The whole run reported zero overruns and no missed period notifications.
Occupancy is a sampled producer/consumer snapshot, not a continuous guarantee.
HDA has four 256-frame DMA periods (21.33 ms of cyclic capacity), in addition
to the client queue and QEMU/host output buffering. **A 5.33 ms client queue
does not mean 5.33 ms speaker latency.**

QEMU WAV output was stereo S16 at 44.1 kHz: 7.98 seconds, estimated tone
501.14 Hz using a discrete zero-crossing median, no detected quiet windows
among 378 interior 20 ms windows, and no clipped samples. Start/stop edges
are excluded. This does not rule out shorter glitches, harmonic distortion,
phase errors, or output-backend problems on actual speakers.

DOOM remains a separate follow-up. Inspection found `sndUpdate` advances
channel positions through all 1536 generated frames, then ignores the number
accepted by `CuBit.Audio.write`. That API explicitly permits partial writes.
A short write therefore discarded an already-advanced tail: a concrete
data-loss path consistent with abrupt effects. **This port bug is now fixed:**
the existing output buffer retains its unwritten suffix, which is retried
before mixing another batch and even after all channels become inactive.
The [hosted regression](../tests/doom-audio/README.md) failed on the original
implementation and verifies exact PCM preservation after the fix. We have not
attributed every audible glitch to this path; nearest-neighbor resampling and
game-loop-driven production remain separate concerns.

## Original scheduling finding: invalid loaded audio result

The continuous priority-4 busy peer started during audio calibration. The
equal-priority producer did not finish calibration/start playback until after
the peer's 12-second interval ended. It subsequently played normally. The
first priority-5 version also blocked filesystem-dependent startup, which is
a separate strict-priority/dependency concern.

The equal-priority failure matched `Process.Queues.insert`: both comparisons
used `>=`, placing newly enqueued tasks **before existing equal-priority tasks**.
The scheduler reinserts a running task on quantum expiry through that path,
allowing the same runnable task to win again. This conflicts with the FIFO
policy documented in `Scheduler`.

The original loaded run was invalid, despite eventual fixture completion.
The runner now requires the busy-peer interval to cover the entire measured
workload and recorded `load_covers_measurement: false` for that original run.

## FIFO fix and follow-up measurements

Both priority comparisons now use strict `>`, preserving FIFO arrival order
within each priority. Only ready-queue insertion uses this operation; sleep
delta ordering is unchanged. No new priority classes, CPU-budget enforcement,
time-slice changes or admission policies are included in this fix.

The hosted locking suite executes the real `Process.Queues` implementation
with fixture PCB storage. It checks 300 simulated quantum expirations and
10,000 seeded arrival/blocking operations against an independent stable-array
oracle, including forward/backward links and empty endpoints. The first FIFO
assertion failed before the fix and the complete suite passes afterward,
including existing locking, sleep-queue and process-lifetime regressions.
This is regression evidence, not a SPARK proof of scheduler fairness or SMP.

Native KVM follow-up results:

| Workload | Observation |
|---|---|
| One-vCPU loaded audio | Genuine overlap; zero reported underruns across all four queue depths; no detected quiet 20 ms capture windows or clipping |
| Four-vCPU loaded audio | Genuine overlap; settled 2032/1024/512-frame phases had zero underruns, 256-frame phase had 31; overall post-warmup count was 39 |
| Four-vCPU IPC | Sync/async correctness and retirement regression passed; untraced p99 upper bounds 2.30 µs / 9.20 µs |

In the clean four-vCPU audio run, producer refill-interval p99 upper bound was
about 7.07 ms, versus the 5.33 ms capacity of a 256-frame queue. Mixer execution
p99 was about 2.88 µs. HDA-publication-to-mixer p99 was about 55.2 µs, with an
observed maximum around 1.21 ms. The long producer refill intervals are a
measured concern; the then-current 10 ms time slice and equal-priority wakeup policy
are follow-up targets, not a proven exclusive cause. A previous SMP attempt
also reported underruns, but final timing lines were corrupted by concurrent serial
output and were not accepted as a timing baseline.

Final-stream mixer close now completes hardware-stop handling and its timing
reports before replying to the closing client. This orders producer/service
reports without adding a serial lock or wait to the active mixing path.
It does not provide general multi-CPU serial-log serialization.

Snapshots: [one-vCPU audio](../tests/performance/results/audio-fair-load.json),
[four-vCPU audio](../tests/performance/results/audio-fair-load-smp.json),
[four-vCPU IPC](../tests/performance/results/ipc-fair-smp.json).
As before, these are individual exploratory runs, not confidence intervals.

The 40-second four-vCPU `desktop-doom` smoke test also passed after the fix
(desktop/application startup and injected input). This is not evidence that
DOOM's sound quality is fixed: its mixer log contained long silent/empty-ring
intervals, and the continuous-tone analyzer is not appropriate for sparse game
effects. Producer-side accepted/dropped-frame instrumentation is still needed.

## First filesystem latency fixture

`bench-storage` uses a single reusable capability-directed 4 KiB grant and
native synchronous FS requests. It exclusively creates a new 64 KiB scratch
file on the runner's disposable NVMe disk; never truncates an existing file.
Each metric includes 512 samples after 32 warmups. Every read is checked
against block-specific contents; the last overwrite is read back and checked.

The timed interval is the capability call through receipt of the FS reply.
Grant creation, seek, path/buffer preparation, content verification, and the
close after each measured open are excluded. The sequence includes file open
authorization and grant validation within the service, not bypass calls.

| One-vCPU KVM operation | p50 upper bound | p99 upper bound | Observed max |
|---|---:|---:|---:|
| Open existing file read/write | 368 µs | 589 µs | 2134 µs |
| Warm 4 KiB sequential read | 55 µs | 442 µs | 2510 µs |
| Warm 4 KiB seeded random read | 55 µs | 129 µs | 126 µs |
| 4 KiB overwrite completion | 368 µs | 884 µs | 1422 µs |

A bucket upper bound can exceed the exact observed maximum; this is not a
percentile calculation error. Sequential and random passes run at different
times, so these single-run tails do not show that random I/O is intrinsically
faster. Working-set, metadata and host caches are warm; there is no cache-flush
protocol or direct physical-device timing here. Write acknowledgement is not
a durable-write/flush guarantee. This is not an async throughput benchmark.

All content checks and expected sample counts passed. Raw ticks and converted
distributions are in [storage-warm.json](../tests/performance/results/storage-warm.json).
The open/overwrite paths merit stage-level profiling before optimization;
their cost is much larger than a bare IPC echo. Test cached metadata policy
checks, block operations and grant lifetime costs separately, preserving the
same authority decisions.

## Filesystem throughput: CuBit against Linux

`tests/fs-bench` runs one POSIX program, `fs-bench.c`, natively on CuBit
(libc → filesystem.svc → NVMe, `run.sh --test bench-fs`) and on a Linux
guest (`tests/fs-bench/linux.sh`, Linux's ext2 driver). Both use the same
QEMU configuration: q35, Broadwell, 4 vCPUs, 512 MiB, KVM, and a QEMU
`nvme` device holding an ext2 volume with 4 KiB blocks and 384 MiB. The
figures are medians of three rounds from one run each on 2026-09-28. CuBit
has no page cache, so on CuBit every result is a device-path result.

| Operation | CuBit native | Linux guest |
|---|---:|---:|
| 64 MiB sequential write + fsync | 0.5 MB/s | 901 MB/s |
| sequential read, warm | 206 MB/s | 19,894 MB/s (page cache) |
| sequential read, cold | 205 MB/s | 4,594 MB/s (after drop_caches) |
| 4 KiB random read p50 / p99 (warm) | 200 / 2,350 µs | 0.4 / 0.8 µs |
| 4 KiB random write p50 / p99, no fsync | 1,279 / 2,330 µs | 0.7 / 1.5 µs |
| 4 KiB random write + fsync p50 / p99 | 3,445 / 6,878 µs | 1,203 / 1,774 µs |
| create + write 4 KiB + close | 15 files/s | 8,011 files/s |
| open + read 4 KiB + close | 43 files/s | 715,850 files/s |
| unlink | unsupported | 9,522 files/s |

Inferred causes (the code locations are in `tests/fs-bench/README.md`;
there is no per-stage profile yet):

- ext2 appends allocate one block at a time, with bitmap, group-descriptor,
  superblock, pointer and inode writes all going through at once (about ten
  synchronous commands per 4 KiB).
- No page, dentry or inode cache, and no readahead or write-back.
- One request at a time at every stage: the libc's single lock and 256 KiB
  bounce, the single-threaded filesystem.svc, and one outstanding NVMe
  command, polled with a 1 ms sleep step. That step is the ~2.3 ms p99.
- Two or three copies per byte (NVMe bounce → 512 KiB filesystem staging →
  client grant → caller).

Caching is the first-order gap for reads and metadata, and write-back with
batched allocation is the first-order gap for writes. The fsync'd random
write, where Linux also waits for the device, is the nearest comparison
(2.9x).

## Verification and next work

The portable histogram was tested at bucket edges, U64 extremes, known ranks
and saturation. GNATprove analyzed all eight subprograms/packages: **15 checks
discharged (nine run-time, one initialization, five termination), zero unproved**.
No `Assume` or SPARK-off escape in that unit. This does not prove percentile
functional correctness or latency: tests cover the former; measurements cover
only observed timing. Four Python tests cover reports, overlap and captures.
Native IPC fixtures passed including the existing retirement check, as did the
idle audio fixture. The original invalid loaded-audio run was rejected; after
the FIFO fix, loaded runs satisfy overlap, but SMP audio-quality limits remain.

Next, in order:

1. Address bounded wakeup service and genuine CPU-budget enforcement, using
   the loaded SMP producer delay as a regression scenario. Review strict-priority
   dependency starvation separately. FIFO fairness is fixed, not the whole policy.
2. Retest DOOM sound after the pending-output fix and measure refill deadlines
   and underruns separately before changing resampling. Keep diagnostic counters
   out of hot-path serial logging. Short writes no longer discard a PCM tail.
3. Extend the same timing vocabulary to input sequence IDs: driver publication,
   desktop routing, client consumption, present acceptance, compositor work and
   display completion. Distinguish present acknowledgement from physical scanout.
   Do not add ambient global-input observation authority for benchmarks.
4. Add representative full-window drags, editor selections, mixed audio streams
   and explicitly cross-core IPC, with repeated idle/contended runs. Retain host
   metadata and instrumented/uninstrumented comparisons.
5. For physical key-to-photon or speaker latency, use external measurement.
   Establish stable per-machine baselines before imposing timing thresholds in CI.

See also [input latency](input-latency.md), [zero-copy audio](audio-zero-copy.md),
[IPC buffer lifetimes](ipc-buffer-lifetimes.md) and
[kernel locking](kernel-locking.md).
