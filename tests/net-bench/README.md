# Network benchmark: CuBit against Linux

One client, `net-bench.c` (sockets only), runs on both systems in the same
QEMU configuration: q35, Broadwell CPU model, 4 vCPUs, 512 MiB, one
`virtio-net-pci` device on QEMU user networking, no packet capture, KVM.
Both talk to the same host fixture, `server.py`, which QEMU's user
networking reaches as 10.0.2.2.

| Workload | What is measured |
|---|---|
| download | 64 MiB from the host (port 18480): receive throughput |
| upload | 64 MiB to the host, acknowledged at the end (18481): send throughput |
| rr | 2,000 one-byte request/response round trips (18482): latency |
| connect | 200 connect, receive one byte, close (18483): connection rate |
| serve-accept | the guest as server: it listens on port 8080 and asks the host (18484) to connect in 200 times through QEMU's forward (host port 18486), sending one byte on each: accept rate |
| serve-download | then 64 MiB from the guest to the host on one more accepted connection, until the host has read it all |

Each workload runs three rounds.

```sh
# Linux reference: nixpkgs' kernel and a busybox initramfs; builds everything.
flock --exclusive --nonblock coordination/build.lock nix develop -c tests/net-bench/linux.sh

# CuBit: build netstack and the client, then the headless case.
flock --exclusive --nonblock coordination/build.lock nix develop -c bash -c \
  'make -C kernel netstack && tests/net-bench/build-cubit.sh &&
   tests/headless/run.sh --test bench-net --accel kvm --timeout 180 --keep-logs'
```

Caveats:

- QEMU user networking (slirp) is part of the path on both systems and may
  bound either result; the comparison is between guests, not against a
  physical link.
- CuBit's `clock_gettime` has millisecond resolution; each measurement is
  long enough for that not to matter.
- The client prints through the debug console on CuBit (`-DCUBIT`) and
  stdout on Linux; the workloads are identical.
- Short connections through slirp slow down as a run goes on, on both
  systems (host-side state); compare rounds, not runs of different length.

Diagnostics:

- `NET_BENCH_CFLAGS` (both `build-cubit.sh` and `linux.sh`) adds compiler
  flags: `-DCONNECTS=3000` for a connection-churn stress, `-DCONNECT_ONLY`
  to run only that workload.
- `BENCH_NET_PCAP=1 tests/headless/run.sh --test bench-net --pcap FILE`
  keeps the packet capture (off by default: bulk transfers make it large).
- On a kernel built with `LATENCY_TRACE=1`, the client dumps a scheduling
  timeline mid-download (dump 0) and after the round trips (dump 1);
  `python3 trace-profile.py SERIAL_LOG [DUMP]` shows where one CPU's time
  went, per process and system call, and scheduler stalls (ready work
  waiting behind a late timer). Use one vCPU: a dump covers the dumping
  CPU only. The trace calls do nothing on an ordinary kernel.
