# Scheduler latency benchmark (interbench-style)

`sched-latency.app` is the before/after yardstick for the scheduler rework in
[docs/scheduler.md](../../docs/scheduler.md). It follows Con Kolivas's
interbench: simulated latency-sensitive workloads, each run alone against
each background load, reporting latency percentiles, missed deadlines and
how much work the background load got done. One C source (`sched-latency.c`,
POSIX plus futexes) runs on CuBit and on Linux.

Everything is an ordinary application: the probe and its IPC peer start at
priority 3 (what desktop.svc gives a launched app), nothing is pinned, and
nothing holds scheduling authority. Background loads and the waker are
threads of the probe process, from a pool of CPUs + 2 threads created at
start.

## Workloads

| Workload | What happens | Sample |
|---|---|---|
| `wake` | A waker thread sleeps to the next 1 ms grid point, takes a timestamp, then `FUTEX_WAKE`s the main thread, which is blocked in `FUTEX_WAIT`. It waits until the waiter has seen the wake before pacing the next. 2 s. | Waker's timestamp to the waiter running again. |
| `interactive` | At most every 1 ms (the next grid point), a request/response round trip to another process. On CuBit, a synchronous IPC call (`CALL_VIA_ENDPOINT_CAPABILITY`) to `bench-ipc-server`'s echo; on Linux, an 8-byte pipe ping-pong with a forked child. 2 s. | Round trip. |
| `frame` | Sleep to the start of each 16 ms period (62.5 Hz), then 2 ms of calibrated CPU work. 4 s (250 periods). | Wakeup lateness against the period start. |
| `audio` | The same with a 5 ms period and 0.5 ms of work. 2 s (400 periods). | Wakeup lateness. |

`missed` counts, for `wake` and `interactive`, samples over 1 ms (the target
in [input-latency.md](../../docs/input-latency.md)) plus echoes that failed.
Their pacers skip grid points that have already passed, so a late pacer
never fires two requests back to back. For `frame` and `audio` it counts
periods whose work did not finish before the next period started. Late periods are not
dropped: the next one then starts late, and its lateness is a sample too.

Work is a fixed number of loop iterations calibrated with nothing else
running (`work units_per_us`), so preemption stretches it, as it would a real
frame.

## Background loads

| Load | What runs | `bg_rate` |
|---|---|---|
| `none` | nothing | - |
| `burn` | CPUs + 1 threads spinning on calibrated work | thousands of work units per second, all threads |
| `spam` (run last) | CPUs / 2 thread pairs; each pair ping-pongs through a futex with 2 µs of spinning per turn, so each thread wakes and sleeps every few µs | handoffs per second |
| `poll` | one thread polling a flag for 50 µs, then `sched_yield`, forever (a polling service) | yields per second |
| `io` | one thread doing 4 KiB `pwrite`s over the first 64 blocks of one file, `fsync` every 16 writes. On CuBit through filesystem.svc and NVMe (`@nvme:0/sched-latency/io.dat`); on Linux, the initramfs (no device) | writes per second |

Each (workload, load) pair refits the clock origin (CuBit), starts the load,
lets it run 250 ms, runs the workload, then stops the load and waits for its
threads to finish. `bg_rate` covers the workload's run.

Output, one line per pair, then `sched-latency: done`:

```
sched-latency: workload=wake load=burn samples=2000 p50_us=.. p99_us=.. max_us=.. missed=.. bg_rate=.. bg_unit=kunits/s
```

Percentiles are nearest-rank.

## Boundaries: what this does and does not measure

- **Closed loop.** Every workload is paced by its own sleeps and waits for
  its own completions. This is not open-loop arrival testing, and it measures
  no device path: no IRQ, driver, input.svc or compositor. It measures the
  scheduler's wake, dispatch, IPC and timer behavior as an application sees
  it.
- **Timer granularity on CuBit.** CuBit's `clock_gettime` and sleeps count the
  kernel's millisecond clock, which timer interrupts advance. So timestamps
  are TSC readings. The clock is updated lazily by CPU 0's timer ticks, so
  its TSC origin and rate are fitted from about 1,000 observed edges: the
  lower envelope of when each millisecond value is first seen (1 s at
  start; the origin is refit over 100 ms before each pair). Periods are
  whole milliseconds on that fitted grid, and a sleep is to a whole
  millisecond. `ms_lag_us` reports how far first sightings trail the fit.
  `frame` and `audio` lateness therefore includes when the millisecond clock and its sleep deadline
  actually fire, as an application would see it. `wake` and `interactive`
  are timed from TSC to TSC and do not depend on the timer.
- **Linux-hosted vs native.** "CuBit native" results are this app running on
  CuBit in QEMU/KVM (the `bench-latency` headless case). The Linux reference
  (`linux.sh`) is the same source running on Linux in the same QEMU
  configuration, reported separately. Neither is a hardware measurement.
  Both include host (KVM) scheduling noise.
- **Interactive transports differ.** CuBit uses its real synchronous IPC path.
  Linux uses pipes between two processes. Compare each against its own
  `load=none` row rather than CuBit against Linux directly.
- These are regression-tested observations. Nothing here is proved.

## Running

CuBit (4 vCPUs, KVM). Needs the libc, `ccl-manifest` and `bench-ipc-server`
built (`make -C kernel libc ccl-manifest bench-ipc-server`, or `make -C kernel
world`). The case stages `logstore.svc`, `bench-ipc-server.app` and
`sched-latency.app` on a disposable disk, makes `sched-latency/`, and quits
QEMU when the guest prints `sched-latency: done`, a FAIL line or a kernel
panic (`stop-on-done.py`, over QMP). A passing run takes about 90 s,
including boot. `summarize.py LOG...` tabulates one or more runs, giving
the median of each field and the p99 range.

```sh
flock --exclusive --nonblock coordination/build.lock nix develop -c bash -c \
  'tests/sched-latency/build-cubit.sh &&
   tests/headless/run.sh --test bench-latency --accel kvm --timeout 300 --keep-logs'
grep -a '^sched-latency:' <serial log printed by run.sh>
```

The case fails on a missing marker, any `sched-latency: FAIL` line (the IPC
peer did not answer, or an echo failed), or an unavailable load.

Linux reference (nixpkgs kernel, busybox initramfs, the app built static with
musl, same QEMU machine, CPU model and vCPUs):

```sh
flock --exclusive --nonblock coordination/build.lock nix develop -c tests/sched-latency/linux.sh
```

For a quick smoke test, build with `-DRUN_DIVISOR=10`. This divides every run
and the warmup by 10.

## Baseline

### CuBit native, 2026-09-29

- Tree: HEAD `0cb61009` plus uncommitted work. That work includes the interim
  idle-CPU placement change in `kernel/src/process.adb`, `process.ads` and
  `scheduler.adb` (docs/scheduler.md, "Interim change"): strict priority,
  FIFO within a level, work stealing, plus idle-CPU placement. The
  unreferenced `virtual_deadlines` package was also present. `run.sh`
  rebuilds the kernel from these sources for each run.
- QEMU/KVM, q35, Broadwell, 4 vCPUs, 128 MiB. The host was not otherwise
  idle: its load average was 2 to 4, mostly this VM, and other agents
  worked between runs.
- Five runs of the final app. Each row is the median over the runs that
  reached that pair (the `runs` column). The p99 range is across runs. Only
  one run of five reached the end: see "Kernel panic under spam" below.
- Clock fit: `ticks_per_us` about 3767.5, about 980 millisecond edges per
  second. The millisecond clock trails its true boundary by 0 to 1 ms,
  uniformly (`ms_lag_us` p50 about 505 µs, max about 1000 µs).

| workload | load | runs | p50 µs | p99 µs (range) | max µs | missed | bg_rate |
|---|---|---:|---:|---:|---:|---:|---:|
| wake | none | 5 | 23.4 | 51.8 (49–55) | 115.7 | 0 | - |
| wake | burn | 5 | 135.0 | 146.8 (141–148) | 168.8 | 0 | 4,448,675 kunits/s |
| wake | poll | 5 | 20.7 | 41.8 (32–56) | 50.9 | 0 | 19,499 yields/s |
| wake | io | 5 | 26.8 | 44.6 (42–49) | 61.3 | 0 | 9,261 writes/s |
| wake | spam | 2 | 22.8 | 57.8 (57–58) | 78.0 | 0 | 94,423 handoffs/s |
| interactive | none | 5 | 40.8 | 92.7 (84–100) | 158.2 | 0 | - |
| interactive | burn | 5 | 386.8 | 402.9 (288–433) | 417.2 | 0 | 4,431,957 kunits/s |
| interactive | poll | 5 | 39.8 | 82.5 (72–91) | 105.6 | 0 | 19,516 yields/s |
| interactive | io | 5 | 43.2 | 73.9 (67–75) | 201.1 | 0 | 9,319 writes/s |
| interactive | spam | 1 | 43.6 | 92.3 | 119.2 | 0 | 94,644 handoffs/s |
| frame | none | 5 | 527.0 | 1,030.9 (1,025–1,057) | 1,051.8 | 0 | - |
| frame | burn | 5 | 594.1 | 1,118.1 (1,104–1,133) | 1,138.9 | 0 | 4,398,083 kunits/s |
| frame | poll | 5 | 516.8 | 1,026.6 (1,010–1,056) | 1,051.1 | 0 | 19,559 yields/s |
| frame | io | 5 | 385.8 | 1,029.1 (998–1,052) | 1,053.8 | 0 | 9,255 writes/s |
| frame | spam | 1 | 103.5 | 200.5 | 209.0 | 0 | 95,308 handoffs/s |
| audio | none | 5 | 529.3 | 1,036.5 (1,011–1,059) | 1,059.0 | 0 | - |
| audio | burn | 5 | 638.9 | 1,124.0 (1,111–1,147) | 1,132.3 | 0 | 4,453,611 kunits/s |
| audio | poll | 5 | 520.5 | 1,032.7 (1,016–1,044) | 1,047.8 | 0 | 19,553 yields/s |
| audio | io | 5 | 354.1 | 1,043.6 (1,010–1,078) | 1,053.8 | 0 | 8,964 writes/s |
| audio | spam | 1 | 91.3 | 203.8 | 227.7 | 0 | 92,959 handoffs/s |

### Linux reference (Linux-hosted guest, not CuBit), 2026-09-29

`linux.sh`: Linux 6.18.45, same QEMU machine, CPU model and 4 vCPUs, 256 MiB.
Two runs. `io` writes go to the initramfs (no device), so its rate is not
comparable with CuBit's. `interactive` uses pipes here.

| workload | load | runs | p50 µs | p99 µs (range) | max µs | missed | bg_rate |
|---|---|---:|---:|---:|---:|---:|---:|
| wake | none | 2 | 13.4 | 26.2 (26–27) | 43.3 | 0 | - |
| wake | burn | 2 | 1.1 | 3.5 (3–4) | 1,803.3 | 2 | 4,648,108 kunits/s |
| wake | poll | 2 | 12.4 | 21.3 (19–23) | 29.9 | 0 | 19,910 yields/s |
| wake | io | 2 | 11.5 | 22.8 (22–23) | 443.7 | 0 | 3,850,851 writes/s |
| wake | spam | 2 | 3.2 | 8.4 (8–8) | 16.7 | 0 | 410,204 handoffs/s |
| interactive | none | 2 | 19.5 | 37.3 (26–48) | 77.0 | 0 | - |
| interactive | burn | 2 | 2.7 | 6.4 (6–7) | 2,069.0 | 4 | 4,634,817 kunits/s |
| interactive | poll | 2 | 19.1 | 37.5 (24–51) | 545.4 | 2 | 19,920 yields/s |
| interactive | io | 2 | 21.4 | 35.8 (34–37) | 236.3 | 0 | 3,863,704 writes/s |
| interactive | spam | 2 | 6.9 | 13.1 (13–13) | 22.1 | 0 | 413,935 handoffs/s |
| frame | none | 2 | 69.3 | 83.8 (82–86) | 93.7 | 0 | - |
| frame | burn | 2 | 53.8 | 2,511.2 (2,324–2,699) | 2,514.6 | 0 | 4,512,241 kunits/s |
| frame | poll | 2 | 72.1 | 83.4 (80–87) | 91.1 | 0 | 19,926 yields/s |
| frame | io | 2 | 68.3 | 79.9 (79–81) | 83.2 | 0 | 3,855,746 writes/s |
| frame | spam | 2 | 56.6 | 64.1 (62–66) | 65.1 | 0 | 414,119 handoffs/s |
| audio | none | 2 | 64.5 | 80.8 (78–84) | 192.5 | 0 | - |
| audio | burn | 2 | 53.4 | 57.3 (57–57) | 484.4 | 0 | 4,525,025 kunits/s |
| audio | poll | 2 | 64.7 | 75.2 (74–76) | 107.3 | 0 | 19,922 yields/s |
| audio | io | 2 | 63.5 | 73.3 (73–74) | 77.8 | 0 | 3,845,888 writes/s |
| audio | spam | 2 | 56.4 | 62.2 (60–64) | 65.6 | 0 | 403,180 handoffs/s |

### What the baseline shows

- **Timed sleeps are late by the millisecond clock, not by the scheduler.**
  With no load, `frame` and `audio` wake about 0.5 ms late at p50 and
  1.03 ms late at p99. That matches `ms_lag_us`. Sleep deadlines expire only
  when CPU 0's timer tick advances `msTicks`, which happens at up to 1 ms
  intervals and not aligned to millisecond boundaries. Under `spam`, lateness
  falls to about 0.1 ms, because the busy CPUs take more timer opportunities.
  Linux wakes 65 to 70 µs late. A sub-millisecond sleep API or a precise
  deadline timer is needed before scheduler work can show up here. Nothing
  missed a deadline, since the work fits the period easily.
- **CPU-bound peers delay wakes by hundreds of microseconds.** Under `burn`
  (5 spinning threads at the same priority on 4 CPUs), a futex wake takes
  about 135 µs to run, with a tight distribution. On Linux it takes 1.1 µs at
  p50. An IPC round trip takes about 387 µs; on Linux the pipe round trip
  takes about 2.7 µs. Across runs, `interactive`/`burn` p99 ranged from 288
  to 433 µs. In an earlier version of the app (thread per pair, not a
  pool), some runs gave 1.8 µs at p50: same-CPU direct handoff, when the
  placement happened to put client and server together. Placement dominates
  this case.
- **Cross-CPU wake is slow when idle.** With no load, a wake takes about
  23 µs at p50 (Linux about 13 µs), and an IPC round trip about 41 µs.
- **`poll` (a `sched_yield` spinner) and `io` barely affect these probes**
  on 4 vCPUs. `io` reaches about 9,300 4-KiB writes/s through filesystem.svc.
- **Background throughput:** `burn` reached about 4.45 M kunits/s against
  Linux's 4.65 M, with the probe running. `spam` handoffs were about
  94,000/s against Linux's 410,000/s.

### Kernel panic under `spam` (a kernel defect, found by this benchmark)

Of the 15 native runs, across app versions, that reached the `spam` load, 11 halted with

```
CUBIT KERNEL PANIC
EXCEPTION: Dispatch of executing or retiring process
```

(`Process.noteContextStarted`, `kernel/src/process.adb`). It happens
while the `spam` load runs: futex ping-pong pairs, each thread waking and
sleeping every few µs across CPUs. That is why `spam` is the last load, and
why the case fails on this kernel. Every earlier pair still reports.

This has not been bisected. The likely cause is the interim placement
change. `ready()` and the scheduler's put-back can now queue a thread on
another, idle CPU and send it an IPI while the thread is still executing on
its current CPU (woken or requeued before it has switched out). The idle
CPU can then dispatch it before the first CPU leaves it. Before the change,
such a thread was always queued on its own CPU. Checking this needs the
same run on a kernel without the interim change.

### Thread creation refused after about 36 create/join cycles

An earlier version of the app created and joined its load threads for
every pair. In all 5 runs that got that far, `pthread_create` failed with
`EAGAIN` at the 37th creation, while `mmap` of a 256 KiB stack still
succeeded. So
`THREAD_CREATE` itself refused. Its limits are 128 threads per process and
the system-wide extra-thread limit, and all earlier threads had been
joined, so exited threads were probably not yet reclaimed by the reaper.
The cause was not investigated further. The app now uses a fixed pool of
CPUs + 2 threads created once.

### Missing or limited APIs (not added here)

- No sub-millisecond clock or sleep in the libc: `clock_gettime` and
  `nanosleep` count milliseconds. The kernel has
  `READ_MONOTONIC_MICROSECONDS` (114), but sleeps and futex deadlines are
  still whole milliseconds on `msTicks`.
- No cross-process pipe or socket, so the Linux and CuBit `interactive`
  transports differ.
- `sysconf(_SC_NPROCESSORS_ONLN)` is fixed at 4 (`sched_getaffinity`).
