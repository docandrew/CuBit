# Native performance measurements

See [methodology, initial results, and findings](../../docs/performance-baseline.md).
These run **inside CuBit**, not the Linux Workbench. Linux hosts QEMU and
analyzes artifacts. The small histogram test alone is Linux-hosted.

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
