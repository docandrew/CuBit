# Hosted ext2 operation benchmark

The production Ext2 code (CPU side) over an in-memory copy of a Linux-made
ext3 image: the fixture device copies bytes, so the timings measure the
filesystem code, not I/O. Workloads (median of 7 rounds, ns per operation,
and the device commands each issued): 1,000 creates with a 4 KiB write, the
commit, 1,000 warm resolve+readInode+4 KiB reads, 10,000 warm readInodes,
1,000 lookup misses, 100 listings of the 1,000-entry directory, 16 MiB
sequential write+fsync and read in 512 KiB requests, 1,000 unlinks.

```sh
nix develop -c bash -c 'mke2fs -q -t ext3 -b 4096 -F /home/doc/cubit-build-tmp/bench.img 128M &&
  cd tests/filesystem-ops-bench && gprbuild -p -P ops_bench.gpr &&
  build/ops_bench /home/doc/cubit-build-tmp/bench.img'
```

`-XFS_SRC=<dir>` builds against another copy of the filesystem sources (an
A/B against a baseline), `-XFS_OBJ=<dir>` keeps its objects apart and
`-XLTO=-flto` links with link-time optimization, as filesystem.svc is.
This is not the native benchmark: tests/fs-bench (headless `bench-fs`)
measures the whole service under QEMU/KVM.
