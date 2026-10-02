# Filesystem benchmark: CuBit against Linux

One program, `fs-bench.c` (POSIX file calls only), runs on both systems in
the same QEMU configuration: q35, Broadwell CPU model, 4 vCPUs, 512 MiB,
KVM, and one QEMU `nvme` device (`serial=cubitnvme`, raw image, QEMU's
default host cache mode) holding an ext3 volume (4 KiB blocks) used in
data=ordered mode: CuBit's JBD2 journal, and Linux's ext4 driver.

- CuBit: `fs-bench.app` over the libc's file layer and filesystem.svc, on a
  disposable copy of `kernel/nvme_disk.img` (384 MiB, 4 KiB blocks,
  98,304 inodes of 256 bytes; about 176 MiB free), files under
  `@nvme:0/fs-bench`. The case is `tests/headless/run.sh --test bench-fs`.
- Linux: nixpkgs' kernel (6.18) and a busybox initramfs; the same program
  built static against musl; a fresh ext3 image with the same geometry made
  by `mke2fs` for each run, mounted (ext4 driver, data=ordered) at `/mnt`.
  `FS_BENCH_FS=ext2` runs the unjournaled one.

| Workload | What is measured |
|---|---|
| seq-write | 64 MiB to a new (truncated) file in 1 MiB writes, then fsync: MB/s over both |
| seq-read-warm | the file in 1 MiB reads right after writing it |
| seq-read-cold | the same after `sync; echo 3 > /proc/sys/vm/drop_caches` (Linux); CuBit has no way to drop its caches, so it runs the same read again |
| rand-read | 2,048 `pread`s of 4 KiB at random blocks: p50/p99 latency, IOPS |
| rand-write | 2,048 `pwrite`s of 4 KiB at random blocks, no fsync |
| rand-write-fsync | 256 `pwrite` + `fsync` pairs |
| create | 1,000 files: open(O_CREAT\|O_EXCL) + 4 KiB write + close, in a new directory per round |
| open-read | open + 4 KiB read + close of each |
| list | readdir of that directory |
| unlink | unlink each |

Each workload runs three rounds; every read is checked against per-block
contents. MB/s is 10^6 bytes per second.

Before the rounds, coherence checks print `fs-bench: coherence NAME PASS`
or `FAIL`: handles of one file see each other's writes; buffered writes
reach later openers; sizes after extension; unlink, mkdir and rmdir. The
`reopen-*` checks cover CuBit's parked handles
(docs/filesystem-data-plane.md, "Metadata operations"): reopening a name
after it was unlinked and created again, after a rename, and after
another process wrote the file, unlinked and created it again, or
renamed it away and created it again; and the bytes another process
wrote and left open when it exited without closing (`_exit`). The other process is a fork on
Linux; on CuBit the startup (`init-bench-fs.ccl`) starts two fs-bench
instances, and the one that creates `claim` first runs the benchmark.

```sh
# Linux reference (builds everything, makes its own disk image).
flock --exclusive --nonblock coordination/build.lock nix develop -c tests/fs-bench/linux.sh

# CuBit: the libc, the client, then the headless case (about 2 minutes;
# QEMU quits at the done marker).
flock --exclusive --nonblock coordination/build.lock nix develop -c bash -c \
  'make -C kernel libc && tests/fs-bench/build-cubit.sh &&
   tests/headless/run.sh --test bench-fs --accel kvm --timeout 1000 --keep-logs'
```

`FS_BENCH_CFLAGS=-DFILE_MIB=N` (both scripts) changes the file size; on
Linux `FS_BENCH_FILE_MIB` does too at run time.

## Results (2026-09-30, KVM, 4 vCPUs)

Round 4 final, on a quiet host after a reboot (load average about 1).
CuBit is the median of 9 rounds from three runs, each run passing all 21
coherence checks. Linux is the median of 3 rounds from one `linux.sh` run,
made straight afterwards on the same host (ext3, data=ordered).

| Operation | CuBit native | Linux guest | CuBit / Linux |
|---|---:|---:|---:|
| seq-write 64 MiB + fsync | 645 MB/s | 897 MB/s | 0.72 |
| seq-read warm | 20,394 MB/s | 22,077 MB/s | 0.92 |
| rand-read 4 KiB IOPS | 2,931,696 | 2,615,715 | 1.12 |
| rand-write 4 KiB IOPS | 599,844 | 16,783 | 36 |
| rand-write + fsync IOPS | 592 | 257 | 2.3 |
| create + write 4 KiB + close | 121,002 files/s | 92,239 files/s | 1.31 |
| open + read 4 KiB + close | 2,216,162 files/s | 713,664 files/s | 3.1 |
| list (readdir), 1,000 entries | 6,060,832 entries/s | 5,316,321 entries/s | 1.14 |
| unlink | 267,611 files/s | 288,925 files/s | 0.93 |

Round 4 (seq-write and unlink) changes:
- **Unlink** went from 138k/s (round 3) to 268k/s.
  - The directory-entry removal walks record headers with an inlined,
    header-only step. Called out of line, the record result made
    store-forwarding stalls that cost about 18k cycles per 4 KiB block
    instead of 4k. `Directory_Blocks` is still proved at level 1
    (170 checks, 0 unproved).
  - The orphan-list scans are skipped while the list is empty.
- **Seq-write** was profiled but not changed.
  - The fsync is at parity: two barriers, about 55 ms.
  - The write phase is bound by the NVMe path at about 2 GB/s: per
    32 MiB, doorbells take 4.3 ms and waits 13 ms. A Linux guest reaches
    4 GB/s at queue depth 1 with 512 KiB commands, against CuBit's 128 KiB,
    so larger commands are the next step.
  - Doorbell batching was tried; it was neutral and was reverted.

### Round 3 results (for comparison)

| Operation | CuBit native | Linux guest | CuBit / Linux |
|---|---:|---:|---:|
| seq-write 64 MiB + fsync | 584 MB/s | 848 MB/s | 0.69 |
| seq-read warm | 19,953 MB/s | 22,677 MB/s | 0.88 |
| rand-read 4 KiB IOPS | 3,011,131 | 2,538,895 | 1.19 |
| rand-write 4 KiB IOPS (p50 0.5 µs) | 586,089 | 24,190 | 24 |
| rand-write + fsync IOPS | 603 | 264 | 2.3 |
| create + write 4 KiB + close | 110,998 files/s | 86,590 files/s | 1.28 |
| open + read 4 KiB + close | 1,742,825 files/s | 565,093 files/s | 3.1 |
| list (readdir), 1,000 entries | 5,497,722 entries/s | 4,767,353 entries/s | 1.15 |
| unlink | 138,379 files/s | 250,307 files/s | 0.55 |

Notes:

- CuBit rounds:
  - seq-write 569-623 MB/s;
  - rand-write 410k-1.18 M IOPS;
  - create 110k-114k/s;
  - unlink 137.6k-140.5k/s.
- Open-read is served by parked handles, reused with no request while
  names are unchanged. Linux still resolves each name in its dentry
  cache.
- rand-write buffers in the client's dirty arena under a write
  delegation.
- Unlink was 262k-358k/s mid-round with two shortcuts that the security
  review removed:
  - a client-supplied list of dirty entries;
  - discarding a parked handle's pages before the unlink was known to
    succeed.

  docs/filesystem-data-plane.md explains why.
- Seq-write: the fsync (about 55-68 ms) is mostly the device flush, as
  on Linux. The gap is the write phase: client cache memory allocation,
  and the NVMe driver's 1 MiB DMA window (1.5-2 GB/s).

What is proved and what is tested:

- **Proved** (SPARK, level 1):
  - the handle/object table the service uses to find a file's handles
    (`Shared_Objects`: holder lists, probe window, key uniqueness, a
    denying handle alone on its file; 246 checks, 0 unproved);
  - the directory-record scans (`Directory_Blocks`, 170 checks). This
    includes the in-place removal `Remove_In_Place`: the same refusals
    as `Prepare_Remove`, and at most four changed bytes, reported
    exactly;
  - the queue ring bookkeeping (`CuBit.Submission_Queues`);
  - the ext2/JBD2 pieces listed in coordination/filesystem-journal.md.
- **Tested, not proved:** everything above the table: the service's
  handling of parks, namespace generations, delegations, deferred
  harvest, unlink's dropped pages and dead-process release (main.adb is
  not SPARK); inode allocation from a hint (ext2.adb); the libc client
  (C). These are covered by the coherence
  checks above (native and Linux), storage-grants, the queue layout
  check (`queue-layout-check.py`, Ada against C), and for ext2 the
  hosted suites (power-cut and injected-error namespace and journal
  tests, Linux round trips with e2fsck).

## Baseline (2026-09-28, KVM, 4 vCPUs, before the data plane)

Linux ran on ext2 here.


The median of three rounds. The CuBit numbers are from one native run that
passed (`headless: PASS bench-fs`); the Linux numbers are from one run of
`linux.sh`. The host was not otherwise idle: another agent's builds and
tests ran between these runs, though not at the same time.

| Operation | CuBit native | Linux guest |
|---|---:|---:|
| seq-write 64 MiB + fsync | 0.5 MB/s (142 s writing, 9 ms fsync) | 901 MB/s (16 ms writing, 56 ms fsync) |
| seq-read warm | 206 MB/s | 19,894 MB/s (page cache) |
| seq-read cold | 205 MB/s (no cache: same path as warm) | 4,594 MB/s (device, host-cached image) |
| rand-read 4 KiB p50 / p99 | 200 / 2,350 µs | 0.4 / 0.8 µs (page cache) |
| rand-read IOPS | 1,255 | 2,020,000 |
| rand-write 4 KiB p50 / p99 | 1,279 / 2,330 µs | 0.7 / 1.5 µs (page cache) |
| rand-write IOPS | 769 | 1,146,000 |
| rand-write + fsync p50 / p99 | 3,445 / 6,878 µs | 1,203 / 1,774 µs |
| rand-write + fsync IOPS | 310 | 770 |
| create 4 KiB file | 15 files/s | 8,011 files/s |
| open + read 4 KiB + close | 43 files/s | 715,850 files/s |
| list (readdir), 1,000 entries | 52,958 entries/s | 13,978,000 entries/s |
| unlink | unsupported (ENOSYS) | 9,522 files/s |

Round-to-round spread on CuBit: seq-write 132-142 s, create 15-16 files/s,
open-read 42-48 files/s, rand-read p50 196-1,151 µs. An earlier CuBit run
(same build, timed out before round 3) gave rand-read p50 144-159 µs,
create 19-22 files/s and open-read 51-63 files/s.

What that comparison showed (2026-09-28):

- Linux's warm numbers are page-cache numbers; CuBit has no cache, so its
  warm and cold results are the same and every operation reaches the device.
  Linux's cold read (drop_caches) still benefits from readahead and the
  host's cache of the image file; CuBit's reads come from the same host cache.
- With fsync on every write, where both go to the device, Linux is 2.9x
  faster at p50. For bulk writes CuBit is about 1,800x slower, and for
  metadata-heavy work (create, open) 500x to 16,000x slower.
- The ~2.3 ms p99 on CuBit is the NVMe driver's sleep-poll step:
  a command that is not done after the spin phase waits for a timer tick.

## Where CuBit's time went (2026-09-28)

That baseline predates the block cache, the journal's write-back, the
client page cache and delegations, the request queue and parked handles.
Its causes (block-at-a-time allocation written through, no caches, one
request at a time end to end, staging copies) are what
docs/filesystem-data-plane.md and FS-005 replaced; the history is in git.

## Caveats

- CuBit's `clock_gettime` counts milliseconds. On CuBit the program times
  individual operations with the TSC, calibrated against that clock over
  200 ms (it prints `ticks_per_us`). Linux uses
  `clock_gettime(CLOCK_MONOTONIC)`.
- CuBit prints through the debug console (`-DCUBIT`), Linux through stdout.
  The workloads are identical.
- The 64 MiB file is reused through `O_TRUNC` on both systems.
- The CuBit disk starts with the development image's files. The Linux disk
  starts empty. Both have the same geometry.
