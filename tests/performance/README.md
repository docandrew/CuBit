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
