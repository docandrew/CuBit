# Linux / CuBit Ext2 interoperability

All images are disposable test copies. No mounting, root privileges, private
filesystem extensions or repairs are involved. `e2fsck -fn` must exit cleanly;
the tests do not accept a repaired filesystem as success.

## Crowded-directory rename regression

```sh
nix develop -c bash -c 'cd kernel && alr exec -- gprbuild -p -P ../tests/filesystem-interop/rename_probe.gpr'
nix develop -c bash tests/filesystem-interop/run-rename-probe.sh
```

This diagnostic copies `kernel/nvme_disk.img`, then adds the storage-grants
fixture names to one copy. It does not launch CuBit. On the 2026-09-24 image,
the ordinary root rename fits, but the crowded copy's 1 KiB source block
cannot hold `cubit-renamed-longer.dat`. `Prepare_Rename` reports
`Insufficient_Space`; production Ext2 returns `Rename_Range_Unsupported`
(native reply 0xF005), preserving the source inode/name and no destination.

The native test formerly assumed spare space in that shared root. It now
renames to the same-length `cubit-alt.dat` there and tests longer names in
`lost+found`. The diagnostic checks both revised paths on both copies. It
retains the crowded-block rejection check rather than hiding that limitation.
The seed reproduces directory names/order, not the native workload's data or
deliberately corrupted metadata. Temporary artifacts are retained for inspection.

This is a **test-fixture correction, not new cross-block rename support**.
Production rename still requires the changed records to fit within the source
block. Cross-block/directory-growth rename needs a separately designed mutation
and recovery protocol; it must not reintroduce unlink-before-insert data loss.

## Hosted production-driver matrix

```sh
nix develop -c bash -c 'cd kernel && alr exec -- gprbuild -p -P ../tests/filesystem-interop/interop.gpr'
nix develop -c python3 tests/filesystem-interop/run.py
```

This compiles the production CuBit Ext2 implementation. Only its block IPC/grant
adapter is replaced by Linux-hosted Ada file I/O; it does **not** exercise native
CuBit IPC, grants, NVMe or scheduling. Ada.Command_Line exists only in this hosted
test executable.

The matrix covers 18 Linux `mke2fs` images: 1/2/4 KiB blocks, 128/256/512-byte
inodes, and either local default Ext2 features or a minimal FILETYPE/EXT_ATTR
profile. It verifies:

- Clean `e2fsck` before and after production-driver mutations.
- Creation using a free inode whose extended bytes contain stale nonzero data.
  The entire new inode tail must be zero, not merely its first 128 bytes.
- Sparse growth into single-indirect mappings, zero holes and filling a hole
  below EOF, checked by Linux `debugfs` payload extraction.
- Rename, truncate-to-empty and subsequent write/read.
- Nonzero shrink retaining a partial indirect block, sparse regrowth, and
  shrink across the indirect/direct boundary followed by a positioned write.
  Linux verifies the full retained prefix and zero-filled growth gaps.
- In-place overwrites inside and across leaves of a Linux-created double-
  indirect tree. Full content and every inode byte must match expectations.
- CuBit-created sparse double trees, shrink retaining a partial leaf, and
  reallocation of a discarded leaf. A second Linux-created sparse double tree
  is shrunk/regrown too; complete contents, zero tails and fsck must agree.
- Preservation of existing extended inode bytes and a standard
  `user.cubit.test` xattr while overwriting file data.

Before the fix, `run.py --one` reproduced `stale inode tail exposed on reuse`.
Afterward all 18 images passed. `/tmp/cubit-interop-before.log` records the
reproduction and `/tmp/cubit-inode-slots-hosted.log` the final matrix/regressions.
These ephemeral logs are not committed.

## Native CuBit round-trip

```sh
nix develop -c make -C kernel filesystem bench-storage
nix develop -c bash tests/headless/run.sh --test bench-storage --check-ext2 \
  --accel tcg,thread=multi --timeout 60 \
  --serial /tmp/cubit-interop-native.serial --keep-logs
```

`--check-ext2` is restricted to `bench-storage`: some other workloads deliberately
introduce malformed filesystem metadata. It checks the runner's temporary image,
seeds the next free inode tail, and then boots real CuBit. The native benchmark
creates a 64 KiB file through filesystem IPC/grants/NVMe, performs reads and
overwrites, explicitly flushes, verifies and closes it. After QEMU exits, Linux
checks inode reuse, the complete expected payload and `e2fsck` cleanliness.

The run passed in four-vCPU TCG. This is native integration evidence, not physical
disk performance, a power-loss recovery test, or proof of whole-filesystem safety.
The original base image is not modified. The temporary image is removed by the
runner; the small `.ext2.json` sidecar next to the serial log records the seeded
inode for reproducibility within the run.

The separate `storage-grants` workload now also seeds a sparse Linux-created
double-indirect file using `seed-double.py`, then requires
`FILE-DOUBLE-OVERWRITE-CHECK: PASS` from the native application. That checks
payload, unchanged surrounding bytes/file size, flush and close through real
IPC/grants/NVMe. The complete storage workload intentionally includes malformed
metadata elsewhere, so whole-image fsck remains restricted to `bench-storage`.

## Inode-slot fault coverage and proof boundary

`tests/filesystem-truncate/build/inode_slots` checks 128/256/512/1024/2048/4096-byte
slots. All bytes in the new slot are cleared; neighboring inode bytes stay
unchanged. **930 injected failures** sweep before/partial/after I/O and five
error/malformed reply styles. Uncertain initialization must publish no inode
number, stop further I/O, and quarantine the volume.

The accounting/admission/path/inventory SPARK project passes all 87 checks, including exact
sector subtraction for resize and bounded direct/single/double path decoding.
The raw byte overlay and transport integration used for full-slot initialization
are regression-tested here, not newly SPARK-proved. No proof assumptions or
runtime assertion options were added to native CuBit.

The native storage workload also reports `FILE-DOUBLE-RESIZE-CHECK: PASS` after
16 MiB sparse growth, alias-coherent shrink/regrowth, zero-tail reads and flush.
Its later truncate exercises retirement of the allocated double tree. The
combined native run passed under four-vCPU QEMU TCG; it is not a disk benchmark.
