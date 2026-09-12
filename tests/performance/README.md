# Native performance measurements

See [methodology, initial results, and findings](../../docs/performance-baseline.md).
These run **inside CuBit**, not the Linux Workbench. Linux hosts QEMU and
analyzes artifacts. The small histogram test alone is Linux-hosted.

Native scheduled-residency accounting is described in
[execution-accounting](../execution-accounting/README.md). The IPC fixture now
requires a healthy caller-only `ACCOUNTING:` snapshot exercising scheduler and
direct IPC dispatches. Raw lifetime ticks include kernel/interrupt residency;
they are not exclusive user CPU time and are not reset by tracing controls.

```sh
nix develop -c make -C kernel bench-ipc-client bench-ipc-server bench-audio bench-load bench-storage
nix develop -c make -C kernel test-locking
nix develop -c bash tests/performance/test.sh
nix develop -c tests/headless/run.sh --test bench-ipc --accel kvm --cpus 1 \
  --timeout 25 --keep-logs --serial /tmp/ipc.serial
nix develop -c tests/headless/run.sh --test bench-audio --accel kvm --cpus 1 \
  --timeout 25 --keep-logs --serial /tmp/audio.serial
nix develop -c python3 tests/performance/report.py /tmp/ipc.serial
# Substitute the WAV path printed by the audio runner:
nix develop -c python3 tests/performance/report.py /tmp/audio.serial --wav /tmp/capture.wav
```

Requires an already built base system/disk. For a fresh workspace, build
`nix develop -c make -C kernel world` first. Do not run headless tests concurrently:
they rebuild a shared boot image, but use disposable copies of the data disk.
Benchmark binaries are refreshed from `kernel/isodir/boot`, not stale disk copies.
For retained WAVs, specify an absolute serial path outside Nix's temporary directory.

Add `--load` for a continuous priority-4 CPU-bound peer; `--cpus 4` exercises
an SMP-enabled guest, **not necessarily cross-core IPC**. Current benchmark
participants are on CPU 0. IPC participants run at priority 5; audio producer
and busy peer at priority 4. The load runs for 12 guest seconds after a 250 ms
startup delay, with GETTIME calls but no voluntary yields during that interval.
The runner rejects a loaded run if the fixture did not overlap the whole
measurement. The equal-priority audio case originally failed this check:
the ready queue reinserted equal-priority tasks ahead of their peers. FIFO
insertion fixes that starvation. The smallest queue still underruns in the
four-vCPU loaded scenario; fixture completion does not mean glitch-free audio.
Do not bypass the overlap check.

### Optional host vCPU pinning (Linux/KVM)

```sh
# Inspect CPU/core/SMT topology; select one allowed logical CPU per physical core.
lscpu -e=CPU,CORE,SOCKET,NODE,ONLINE
nix develop -c python3 -m unittest discover -s tests/headless -p 'test_qemu_affinity.py'
nix develop -c tests/headless/run.sh --test bench-ipc --accel kvm --cpus 4 \
  --vcpu-cpus 2,3,4,5 --load --timeout 25 --keep-logs --serial /tmp/ipc-pinned.log
```

The list maps vCPU 0,1,... in order, **not** a shared CPU pool. These CPU numbers
are an example, not a portable topology assumption. Invalid counts, duplicate
CPUs and CPUs outside the calling process's allowed affinity are rejected before
boot-image preparation. The wrapper starts QEMU paused, discovers actual host
thread IDs through a private QMP socket, sets and reads back each affinity, and
resumes only after every vCPU is verified. Setup failure stops QEMU. Existing
test QMP clients use a separate socket. `AFFINITY:` lines in the runner output
record the mapping, thread IDs and SMT siblings; retain that output alongside
the guest serial log. The normal unpinned runner path is unchanged.

This does **not** reserve cores, move Linux workloads, isolate interrupts, alter
host scheduler policy or constrain QEMU's other threads. SMT siblings remain
available to other host work. Pinning reduces migration variability but can hurt
if the selected cores are busy. Compare repeated interleaved pinned/unpinned runs
with the same host load. Pinning does not change CuBit's own process placement:
the present IPC/input fixtures still concentrate their participants on guest CPU 0.

For a genuine four-CPU contention comparison of the current spinlock unlock
against an experimental release store, use the isolated
[native SMP spinlock benchmark](../spinlock-bench/README.md). This does not
replace the normal kernel's lock implementation.

Filesystem latency fixture:

```sh
nix develop -c tests/headless/run.sh --test bench-storage --accel kvm --cpus 1 \
  --timeout 45 --keep-logs --serial /tmp/storage.serial
nix develop -c python3 tests/performance/report.py /tmp/storage.serial
```

This creates `@nvme:0/cubit-latency.dat` exclusively on the disposable disk,
initializes 64 KiB, then measures 512 operations after 32 warmups for each of
open, 4 KiB sequential reads, seeded random reads, and overwrites. It verifies
every read and the final overwritten contents. Seek, buffer preparation,
verification and grant creation are outside the timed calls. One outstanding
synchronous operation, one reused capability-directed grant, native FS IPC.
These are warm-working-set completion latencies, not cold-device latency,
async throughput or durable-write guarantees. Outside the runner the app
leaves its scratch file and refuses to reuse an existing one.

Histogram proof (run `test.sh` first to stage production sources):

```sh
nix develop -c bash -c 'cd kernel && alr exec -- gnatprove \
  -P ../tests/performance/histogram.gpr -u cubit-timing_histograms.adb --level=1 -j2'
```

The histogram proof covers run-time checks, initialization, and termination;
not percentile functional correctness, clocks, kernel scheduling, or latency.
Host assertions test bucket boundaries, quantile ranks, extrema and saturation.
Python tests cover conversion, load overlap, signal gaps and QEMU's unfinalized
WAV headers. The WAV analyzer repairs recognized zero-length headers in memory
only; original captures are never modified.

Fixture completion is not an audio-quality PASS. Inspect underruns, missed
notifications, queue depths and capture diagnostics. No regression threshold
for microseconds is imposed yet: establish repeated per-machine distributions
before making noisy timing numbers a CI gate.

## Focused-application input diagnostic

### Wake-aware scheduling

`WAKEUP_SCHEDULING=1 ONESHOT_SCHEDULING=1` are now the defaults. When comparing
alternative settings, use the same environment for **both** the build and the
headless invocation: the runner checks and rebuilds the kernel, so setting a
flag only on an earlier build is insufficient.
The normal turn is 1.5 ms of charged execution. Adaptive one-shots bring forward
a 100-us opportunity for newly awakened same-priority work; otherwise they
cover clock maintenance and remaining turns. This is not a 100-us compute
quantum. With `ONESHOT_SCHEDULING=0`, the comparison uses periodic 250-us ticks.

The compute control runs two identical, authorityless compute workers on one
CPU for overlapping 12-second intervals:

```sh
nix develop -c make -C kernel bench-load
nix develop -c env WAKEUP_SCHEDULING=1 ONESHOT_SCHEDULING=1 bash tests/headless/run.sh \
  --test bench-scheduler --accel kvm --cpus 1 --timeout 25 --keep-logs \
  --serial /tmp/compute-control.log
nix develop -c python3 tests/performance/report.py /tmp/compute-control.log \
  --require-compute-control
```

Use `--load-workers 4` instead of `--load` for the heavier `bench-input` fixture;
pass `--load-workers 4 --require-load --require-reference-clock
--require-input-target` to the report. All four distinct worker lifetimes must
cover the measurement, with positive progress. Integrity and observed timing
are separate gates. Neither is an IRQ-to-app or hardware SLA proof.

The `bench-scheduler` runner automatically starts `clock_reference.py` before
boot and requires its independent clock check to pass. It timestamps live
markers against host monotonic time and rejects more than 5% time dilation over
the 12-second interval, saving the result beside the serial log as
`<serial-log>.host-clock.json`. It cannot validate microsecond oscillator accuracy.
`--require-reference-clock` additionally checks benchmark calibration against
the kernel's pre-LAPIC PIT reference. This prevents a clamped periodic timer
from making latency appear artificially small by slowing the guest clock.

`ACCOUNTING` counts all scheduled dispatches, including higher-priority
interruptions and resumption of unfinished turns. `TURNS` separates new/resumed
turns and higher-priority, exhausted-quantum, wake-aware, and voluntary stops.
Do not infer equal-peer switch frequency from total dispatch count alone.
Guest calibration varies between runs: raw batch totals are not by themselves
evidence of a host-throughput improvement.

```sh
nix develop -c make -C kernel bench-input bench-load display desktop
nix develop -c bash tests/headless/run.sh --test bench-input --accel kvm --cpus 4 \
  --timeout 20 --keep-logs --serial /tmp/input-idle.log
nix develop -c bash tests/headless/run.sh --test bench-input --accel kvm --cpus 1 \
  --load --timeout 75 --keep-logs --serial /tmp/input-load.log
nix develop -c python3 tests/performance/report.py /tmp/input-load.log \
  --require-load --require-input-integrity
# Optional observed-sample timing gate, distinct from integrity:
nix develop -c python3 tests/performance/report.py /tmp/input-load.log \
  --require-load --require-input-target
```

`bench-input.app` is a native CuBit desktop client with **test-only keyboard
publication authority**. It creates a focused window and measures just before
publishing a canonical normalized key report until its surface-scoped
`Wait_Input` reply returns. No production protocol or authorization bypass is
added. It is not in the normal boot profile. No real keyboard driver or QEMU
input injection is involved; this does **not** measure interrupt-to-app latency.

Three scenarios each deliver 2,048 alternating up-arrow press/release events:
continuous closed-loop, closed-loop with a one-millisecond sleep between
events, and closed-loop with a 320x200 surface-damage request before each event.
The latter exercises compositor work on an otherwise empty client surface;
it does not measure a rich application's painting, glyph layout or buffer
uploads. There is one outstanding transition, no warmup exclusion or retries,
and unexpected events, serial gaps, resync, duplicate, timeout or rejected
publication fail integrity. The one-second wait deadline bounds transport
failure; it is not the latency target. QEMU's overall timeout is a second
failure boundary if the service stops responding entirely.

Additional histograms split publication-call time from input-receive-call time.
These are client-observed call intervals, not exclusive kernel CPU times;
either can include scheduling. Their percentiles must not be added. One extra
TSC read splits the stages, and its overhead is included in the end-to-end
measurement. Production periodic desktop diagnostics remain enabled; the test
itself prints only between scenarios, never per event.

The input fixture and busy peer both run at priority 4, desktop at 4, display
at 5. They currently share CPU 0 even in an SMP-enabled guest. Input uses the
60-second `bench-input-load.app` variant of the same busy loop; other fixtures
retain their 12-second load. The runner rejects incomplete load coverage and
incomplete/inconsistent timing counts. Its **PASS is integrity, not timing**.
The optional timing gate requires at most 1% of samples to be >= one calibrated
guest millisecond in every scenario, using an exact miss counter rather than
a histogram bucket that might straddle the threshold.

The busy peer also reports its completed 4,096-iteration batches and longest
guest-millisecond gap between batches, aggregated only at exit. Inspect these
alongside load coverage: a bracketed START/COMPLETE alone is not a CPU-progress
guarantee. The input fixture remains unchanged when comparing the scheduler
cadence fix: it does not give either interactive participant a priority boost.

The current scheduler experiment offers equal-priority peers a scheduling
opportunity every 1.5 milliseconds, using a 500-microsecond timer and independent
clock/quantum dividers. The preceding 250-microsecond experiment established
the scheduling bottleneck relative to the original 10-millisecond cadence.
See the September 11 follow-ups in the performance document for measured
tradeoffs and limitations; a larger quantum alone cannot preserve the input
target without bounded wakeup service.

This is a bottleneck diagnostic, not a guaranteed performance envelope. A
closed-loop source stops offering input while waiting, so it underexercises
backlog (coordinated omission). A single guest calibration and finite sample
set do not establish a statistical population bound; TSC/hypervisor/host
scheduling assumptions still apply. Next requirements are independent paced
producers, IRQ/driver timestamps, bounded scheduler admission, full critical
section attribution, and measurements on the laptop.

## Optional shadow scheduler accounting

Set `SHADOW_SCHEDULING=1` in the environment when running the headless IPC
fixture to enable demand-only budget observation. Its normal-build default is
zero. The runner then additionally requires `--require-shadow-budgets` validation:
caller ticks must exactly match native execution accounting, and shadow
dispatches must equal scheduled plus direct dispatches. Faults, saturation,
missing/duplicate records, and inconsistent totals fail the run. Budget overrun
itself does not fail: execution is deliberately never throttled in shadow mode.
See [scope and commands](../scheduler-shadow/README.md). Compare enabled and
disabled builds using the same accelerator, CPU count and benchmark settings;
this diagnostic adds work and is not a scheduler performance optimization.
