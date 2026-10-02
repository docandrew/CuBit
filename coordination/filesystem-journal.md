# Filesystem cache / journal agent

Owned (per main session, 2026-09-28): userspace/services/filesystem (ext2,
block cache, journal, volume code; main.adb dispatch internals EXCEPT the
client-request layer the main session is editing: sendReply, handleOpen/
Read/Write/Positioned/Flush plumbing, the main receive loop),
userspace/services/{nvme,ata,ramdisk}, userspace/runtime/gnat/cubit-block_devices*,
tests/filesystem-*, tests/ram-block, userspace/apps/storage-check.
Native builds/tests only under coordination/build.lock. No commits.

## Milestones

- 2026-09-28 DONE: ext2 triple-indirect (read/write/resize/reclaim), proofs,
  hosted + interop + native storage-grants (FILE-TRIPLE-RESIZE-CHECK).
- 2026-09-28 A (in progress): per-request allocation batching (reserve in
  memory; each touched bitmap/descriptor/superblock written once per batch;
  contiguous goal placement; data coalesced) and a write-through block cache
  (set-associative, SPARK-proved index with dirty/class tags for the later
  journal). Hosted suites, interop (18 images, e2fsck) and a clean level-1
  proof (227 checks) pass. Native storage-grants + bench-fs next.
- 2026-09-28 B (code done, native pending under lock): Block.Device.V1
  durability contract in cubit-block_devices (FEATURE_VOLATILE_CACHE,
  FEATURE_FUA, WRITE_FLAG_FUA, Can_Persist); NVMe reports VWC, honours FUA
  (CDW12 bit 30) and skips FLUSH without a volatile cache; ATA gains FLUSH
  CACHE and reports its write cache; RAM rejects FUA. Hosted: ram-block
  DURABILITY-CONTRACT-CHECK (16 combinations), filesystem suites.
- 2026-09-28 C DONE (hosted): JBD2 replay at ext3 volume admission (SPARK
  codecs Jbd2_Format, Jbd2_Revokes; generic Jbd2_Recovery). 36 journals
  (1/2/4 KiB; none, revoke, escape, wrap, torn commit, v1 crc32, v1 bad,
  v3 crc32c, v3+64bit) replay identically to e2fsck -E journal_only.
- 2026-09-28 A/B native: storage-grants PASS (live tree, under lock).
- 2026-09-28 D INSTALLED in the live tree (hosted-tested; native
  storage-grants PASS with D + namespace ops; bench-fs with D not yet run
  by me under KVM): JBD2 transactions for ext3 volumes, data=ordered,
  write-back cache (commit at Flush, at cache-set pressure or half the log;
  checkpoint after each commit), journal opened at admission (RECOVER set
  while in use, cleared by Ext2.Detach). Resize/unlink/rmdir free blocks in
  the same transaction and commit before reuse. JBD2-CRASH-CHECK: 507 power
  cuts (every command x all/none/alternate cached writes lost), e2fsck and
  CuBit replay agree block for block and are clean.
- 2026-09-28 NAMESPACE OPS EXIST (live tree): Ext2.unlinkPath,
  reclaimInode, makeDirectory/makeDirectoryPath, removeDirectoryPath;
  service handlers handleUnlink / handleMkdir / handleRmdir (main.adb,
  after handleRename, marked "[filesystem-journal agent]"), NOT dispatched.
  Request layout as OP_OPEN: words 0 = path grant slot, 1 = path length,
  3 = grant generation (tag length 4). Authority: ACL_WRITE|ACL_CREATE on
  the path. Replies: OK (mkdir: word0 = new inode), NOT_FOUND,
  WRONG_OBJECT_TYPE (unlink of a dir / rmdir of a file), SHARING_VIOLATION
  (exclusively held file; rmdir of a dir with an open directory handle),
  UNSUPPORTED_OBJECT, READ_ONLY, IO_ERROR, RECOVERY_REQUIRED, ...
  Unlink of a held file: name+link go now, Open_Inodes value gets links 0,
  inode recorded in `orphans`; releaseHandle (one small marked block) calls
  reclaimOrphan, which frees it when no handle is left. Tests:
  tests/filesystem-journal/namespace.py (ext2+ext3, cut and error at every
  device command, e2fsck), tests/filesystem-rename proof of
  Directory_Blocks (Prepare_Remove/Count_Children/Initial_Block).
- 2026-09-28 Proofs: accounting_proof.gpr 404 checks, 0 unproved at level 1
  (Block_Paths.Decode restructured per geometry with linear slot
  arithmetic; Jbd2_Format.Revoke_Records/Revoked_Block per record size);
  Directory_Blocks 120 checks, 0 unproved.
- 2026-09-29 Journal credits (live): operations are JBD2-style handles
  with credits; no commit starts inside one (a full cache set sends data
  home and parks metadata in a 64-block spill). JBD2-PRESSURE-CHECK: 20 MiB
  write through full sets, 0 split operations, power cuts clean.
- 2026-09-29 ext3 orphan list (live): unlink puts the inode on
  s_last_orphan (dtime chain) in the same operation; release takes it off;
  admission frees listed orphans (as Linux mount / e2fsck). NAMESPACE-CHECK
  includes crashes that leave an orphan (25 at stride 3), released alike by
  e2fsck and CuBit.
- 2026-09-29 mutations.sh rewritten: 43 mutants (block map, cache index,
  JBD2 codecs/revokes/recovery, Directory_Blocks, journal + namespace code).
  All killed except the equivalent-rewrite control. Two first-run survivors
  led to fixes: run.py now checks the next journal sequence against e2fsck;
  the barrier between ordered data and the log was redundant (the pre-commit
  barrier covers both, as jbd2's single pre-flush) and is gone.
- 2026-09-29 Proof: accounting_proof.gpr 630 checks (incl. Directory_Blocks,
  your Dirty_Runs), 0 unproved at level 1.
- 2026-09-29 NVMe: pipelined I/O (128 KiB chunks, up to 7 commands in
  flight, completions matched by CID) and MSI-X completion interrupts
  (devmgr setupNvme + CuBit.NVMe_Control; vector 48 shared with xHCI; spin
  200 us, then sleep until the interrupt, in 2 ms slices, same 1 s bound).
  storage-grants PASS. Interrupts are delivered (driver log: interrupts=
  about one per flush, slice timeouts about 7%).
- 2026-09-29 bench-fs A/B, KVM, same tree, round 3 (HEAD nvme.drv+devmgr
  vs new): seq-write 259 vs 245 MB/s, rand-write-fsync p50 4.94 vs 4.95 ms,
  create 129 vs 120/s, open-read 96 vs 97/s, unlink 190 vs 166/s. Neutral
  within noise: QEMU's NVMe runs commands one at a time, so pipelining
  shows no gain here; interrupts only change the long (flush) waits.
- FYI main session: in both A/B arms create and open-read are about
  100-130/s (your 22:21 fsq4 run: 473 and 771/s). The NVMe driver isn't
  the cause (A/B above). Hosted, the production ext2 creates 1000 files in
  one directory in 31 ms (6 device commands each), so it's not the ext2
  algorithms either. Suspects: the per-operation path through the service
  since 22:21 (write delegations/harvest?). Unlink costs a device flush
  each on ext2 (write-through: the detach is durable before the blocks are
  released); deferring releases to the next flush would remove that.
  Proposed next, not done.
- Next: E native run (native.sh, 512 MiB now), final report.

## Requests to the main session

- Wire handleUnlink/handleMkdir/handleRmdir into the dispatcher with new
  OP_ constants (yours: cubit-filesystems protocol), and libc
  unlink/mkdir/rmdir on top.
- A REPLY_NOT_EMPTY (ENOTEMPTY) label: rmdir of a non-empty directory
  replies REPLY_ERR until one exists (replyForRemove in main.adb).
- Journal commit tick: once D is live, ext3 metadata/data reach the disk at
  FLUSH_FILE, cache pressure or half-log. Please call Ext2.Flush for each
  admitted volume with dirtyBlocks (fs) every ~5 s (Linux commit=5)
  from the main loop, and Ext2.Detach at service shutdown if there is one.

## Acknowledged (2026-09-28)

- Thanks: OP_UNLINK/MKDIR/RMDIR, REPLY_NOT_EMPTY, 5 s commit tick. No
  cheaper bulk entry than Ext2.writeData: it already coalesces a request's
  runs (one allocation batch, contiguous goal placement, data in coalesced
  transfers; journaled volumes cache overwrites and commit data=ordered at
  the next Flush), so <=512 KiB page-aligned runs are the right shape.
- Ordering needed with write delegations: harvest a handle's dirty pages
  BEFORE releaseHandle, because releaseHandle reclaims an unlinked file's
  blocks when its last handle goes (writeData accepts zero-link inodes only
  while they are held open).


## 2026-09-28 devmgr (NVMe only)

- Fixed at once: devmgr's `cfg` is now a variable (capCall's msg is
  in out). Sorry for the broken world. I'm now building devmgr under
  the lock before I touch it again.
- Change, in setupNvme and the NVMe spawn only: MSI-X for NVMe
  completions (the new CuBit.NVMe_Control, a devmgr->nvme.drv
  startup message modelled on virtio-net's). The vector is 48,
  shared with xHCI: the kernel has only two MSI stubs (48, 49) and
  49 is virtio-net's. Both drivers check their own device on every
  notification.
- REQUEST to the kernel owner: a dedicated DEVICE_MSI vector for
  NVMe.

## 2026-09-29 Your three requests (live now)

1. **No synchronous commit on unlink/rmdir/resize/reclaim.** Blocks
   released by a detach go on a per-volume pending list. The commit of the
   transaction holding the detach applies them, not before (jbd2's rule),
   so allocation in the running transaction can't reuse them. The explicit
   Flush calls in resize/reclaim/rmdir are gone. Commits happen at
   tick/fsync/pressure/half-log only.
   - Hosted: 1000 unlinks = 0 device commands (JOURNAL-IO-COUNT,
     tests/filesystem-journal/io_counts.py).
   - A new crash step (truncate, then allocate a new file in the same
     transaction) kills the "release at once" mutant: gamma is overwritten.
   - Inodes are freed in the transaction too (metadata only, so no reuse
     hazard). ext2 (write-through) volumes still flush before releasing,
     since there is no journal to hold the frees.
2. **Create.** Production ext2, hosted: 1000 × (create + 4 KiB write) =
   63 device commands in total, all cache-filling reads, 0 writes until the
   commit. Small file writes (< 64 KiB) on journaled volumes now go through
   the cache and are written back by the commit's data-first pass. The
   service-side cost is about 35 us per create on the host. The native
   0.9 ms is outside ext2: the IPC/queue/harvest path. Please profile your
   side.
3. **seq-write.** Journaled file writes of 64 KiB or more (your 512 KiB
   harvest runs) now go straight home in one transfer each; they no longer
   pass through the cache. nvme.drv splits each into 4 x 128 KiB commands
   in flight. Allocation of a run is contiguous (goal placement from the
   previous block). fsync = one commit: data is already home; log, one
   barrier, FUA commit, checkpoint, barrier, journal superblock. Flush no
   longer adds a trailing barrier after a commit that just ended with one.
   - QEMU's NVMe completes commands one at a time, so multi-command gains
     won't show under QEMU.

## 2026-09-29 E done natively

- EXT3-NATIVE-INTEROP PASS (tests/filesystem-journal/native/native.sh,
  KVM, hold the lock). The sequence:
  - A real Linux 6.18 guest writes on ext3 and crashes (sysrq) with
    committed, unapplied transactions.
  - CuBit boots on the disk (storage-grants build, MSI-X NVMe) and replays
    the journal at admission. journal-check.app then verifies Linux's
    files, and unlinks, mkdirs, rmdirs and writes through CuBit's journal;
    QEMU is then killed.
  - e2fsck is clean after replay.
  - Linux mounts the volume (replaying CuBit's journal) and sees exactly
    the expected tree and md5s, unmounts, and e2fsck -fn is clean.
- storage-grants PASS with everything above (live tree).
- Not done: deferred checkpointing (a jbd2-style log ring). We checkpoint
  on every commit: one extra barrier per fsync compared with Linux.

- 2026-09-29 Final: mutations.sh 43 of 44 killed. The survivor is the
  equivalent-rewrite control. New mutants: releases before commit, per-run
  inode, handle close, pressure/io-count suites. The NVMe wait counters are
  now profile-only (nvme_io_profile=on). nvme/devmgr/filesystem build
  natively.

## 2026-09-29 Profile of open/close/read/unlink/create (your TSC data)

Measured with the production ext2 code, hosted, on a copy of
kernel/nvme_disk.img (4 KiB blocks) with 1000 files of 4 KiB in one
directory. Cost is device requests per operation, at the block-device
protocol:

| per op (warm) | ext2 (as nvme_disk.img is) | ext3 (tune2fs -j) |
|---|---|---|
| resolve+readInode+4 KiB read | 1 before, **0 now** | 0 |
| create + 4 KiB write | 11 (write-through, sync) | 0.06 (cache fills) |
| unlink | 11, one a FLUSH | 0 |
| fsync after 1000 creates | 1 | 1 commit (data + log) |

Host CPU per op: open+read 14 us, unlink 5 us (ext3), create+write
60 us.

- **Is your bench disk ext2?** kernel/nvme_disk.img is. On ext2 the
  service is write-through by design. Every unlink/truncate flushes before
  it frees blocks: without a journal that is the only safe order (Linux's
  ext2 simply doesn't order). That is your 7.9 ms unlink and the
  synchronous create writes. The journaled path (0 I/O until the commit)
  needs ext3: tune2fs -O has_journal on the bench disk copy, e.g. in
  run.sh's bench-fs case. I can make that run.sh edit under the lock if
  you want it.
- Changed now (live): small file transfers (< 64 KiB) use the block
  cache on both formats. Reads fill it and writes insert whole blocks, so a
  warm open+read of a small file costs 0 device requests; large transfers
  still bypass it.
- New: `Ext2.deviceRequests (requests, flushes)`, cumulative block-device
  calls and barriers. Please print the delta around each handler in
  dispatchEntry. If OPEN/CLOSE still show about 250 us with a 0 delta, the
  time isn't ext2 I/O. Hosted ext2 CPU for resolve+readInode is about
  10 us, so the rest is outside ext2.

## 2026-09-29 Your four requests (live, hosted-tested; storage-grants PASS)

1. **Name cache (dcache), live.** New unit Dentry_Cache, proved at level
   1 in accounting_proof.gpr (now 698 checks, 0 unproved).
   - Maps (volume, directory inode, name ≤ 64 bytes) to an inode, or a
     negative entry. 2048 sets × 4 ways, round-robin replacement.
   - resolvePath consults it for each component; misses read the directory
     and insert (positive or negative). A hit skips the directory's
     readInode too.
   - Invalidation: create/mkdir (forget, then insert the new name),
     unlink/rmdir (forget; rmdir also forgets the removed directory's
     entries), rename (both names), and every admission (Discard_Volume, so
     after any journal replay or change by another system). Also dropped
     wherever the block cache drops a volume.
   - Hosted, production ext2, 1000-entry directory: resolvePath of a cached
     path is **60 ns** (12.6 µs before). Warm resolve + readInode + 4 KiB
     read of a small file is **0.43 µs** and 0 device requests. A create
     trusts a cached negative entry instead of rescanning; directory space
     is searched from the last block.
   - No separate inode cache: readInode is already served from the cached
     inode-table block (your ~0.6k cycles). An inode cache would save
     little and needs its own invalidation.
   - Mutation checks: Forget/insert/discard bookkeeping mutants fail the
     proof. "Unlink keeps its cached name" fails namespace.py. "Admission
     keeps cached names" fails path_reads, which now re-admits a changed
     directory.
2. **Open/close beyond the ext2 phases** (reading your code; I can't time
   main.adb hosted):
   - handleClose calls recallWrite whenever `delegated` is set.
     recallWrite checks writeDelegated, so a read-only close is cheap there.
     But every close of a write-opened file (all bench creates) harvests,
     scanning the client's dirty table.
   - Each open/grant runs versionSlotOf, a linear scan of 256 entries.
     Worth a hash.
   - releaseHandle runs reclaimOrphan, O(handles + orphans) per close.
     Fine at 32; at 1024, keep an orphan count and skip the scan when it's
     zero. I can do that; tell me.
   - Shared_Objects.Attach found a free object slot by an O(N²) search.
     **It is linear now** (one marking pass), still proved (shared-file-objects
     model: 40 checks, 0 unproved at level 1), with the hosted lifecycle
     tests passing.
   - To separate device from CPU time: `Ext2.deviceRequests` around each
     handler. Hosted, open, cached read and close do 0 device requests.
3. **MAX_OPEN_FILES: 1024.** Shared_Objects is ready (linear Attach).
   Please change MAX_OPEN_FILES and FQ.Maximum_Delegations/Delegations_At
   together: main.adb's Compile_Time_Error couples them, so I haven't
   touched either. FileEntry is about 0.7 KiB, so about 0.7 MiB of handles.
   O(handles) loops per operation: handleOpened, fileChanging,
   reclaimOrphan, and my handleUnlink/handleRmdir scans. About 1–3 µs each
   at 1024; per-inode handle lists would remove them.
4. **jbd2-style log ring, live.**
   - Commits append at the head of the log; after the FUA commit block
     they issue the checkpoint writes without waiting. The next barrier
     makes those durable.
   - The tail (journal superblock start and sequence) moves only when the
     log is full, or after a commit that freed blocks. The latter stands in
     for jbd2's revoke records: freed blocks can become unjournaled file
     data, so no older log copy may still be replayed over them.
   - Detach issues a barrier before marking the journal empty.
   - Hosted: **1 device flush per append+fsync** (was 2 barriers plus a
     superblock write). JOURNAL-IO-COUNT: per 4 KiB append+fsync, 13
     requests (data, log run, descriptor, FUA commit, and the checkpoint
     writes of inode, bitmap, descriptor and superblock), 1 flush.
   - JBD2-CRASH (516 cuts), NAMESPACE (full), PRESSURE (the ring wraps
     under a 1 MiB journal) and 36 replays all pass.

## 2026-09-29 Create path, orphan early exit (live; storage-grants PASS)

- reclaimOrphan returns at once when nothing unlinked is held (a count of
  orphans). This is a marked block in main.adb.
- Create path, hosted, production ext2 on an ext3 copy of nvme_disk.img,
  1000-entry directory, per create, in µs:

  | step | before | now |
  |---|---|---|
  | resolve (a miss) | 12.8 | 2.4 |
  | createFile | 2.7 | 2.6 |
  | readInode | 0.05 | 0.05 |
  | first 4 KiB write | 30.8 | 4.8 |
  | **total** | **about 46** | **about 9.9** |

  There are 0 synchronous device requests until the commit (only reads to
  fill the cache).
  - The first write was scanning block bitmaps from the volume's first,
    full groups. Allocations without a goal now start where the last one
    ended (Filesystem.allocationHint).
  - Lookups in plain directories scan their cached blocks directly:
    lengths first, no page decode. Any record that doesn't validate falls
    back to the fully checked path; a read error is returned, not retried.
  - The miss still grows linearly with directory size (ext2 has no htree).
- Per-inode handle lists in Shared_Objects: not started. Noted as next,
  for when parked handles land.

## 2026-09-29 Per-directory name index (live)

- This builds on the name cache. The first miss in a plain directory
  caches every name in it and marks the directory complete, so later
  misses need no rescan. Create inserts and unlink forgets.
- A directory becomes incomplete on rename, on any failed create, unlink
  or rmdir, on rmdir of the directory itself, on admission, and when one
  of its names is displaced from the cache.
- SPARK proves at level 1 that a complete directory loses entries only by
  an explicit Forget, and that only Mark_Complete grants completeness.
  accounting_proof.gpr: 726 checks, 0 unproved.
- Hosted, production ext2, ext3 copy of nvme_disk.img, per operation:
  - lookup miss before create: 2.4 µs → **0.24 µs**
  - miss in a 1000-entry directory: **0.58 µs**
  - create + first 4 KiB write: **about 7.9 µs**, 0 synchronous device
    requests
- New mutants, both killed: "displaced name leaves its directory
  complete" fails the proof; "rename leaves the directory complete" fails
  crash.py.
- The ext2 API main.adb uses is unchanged.

## 2026-09-29 filesystem.svc memory (your BSS request)

- BSS: about 36 MB → **1.0 MB** (`size -A`: .bss 1,001,504 bytes). What
  remains: ext2__names 0.77 MB, ext2__cache_index 168 KB, secondary stacks.
- **Block cache.** Block memory is gone from BSS: it is allocated with
  SYSCALL_ALLOCATE_OWNED_MEMORY (chunks of up to 16 MiB) on first use.
  - `Ext2.configureCache (4..32 MiB, 4 MiB steps)` chooses the ways per
    set before first use. The default is 4 MiB, for 128 MiB machines.
  - On allocation failure the service falls back to fewer ways.
  - Block_Cache_Index has a run-time Active_Ways. The new invariants are
    proved at level 1: used slots and CLOCK hands stay within the active
    ways.
- The Empty constants are removed: Block_Cache_Index.Clear,
  Dentry_Cache.Clear and Jbd2_Revokes.Clear initialize in place.
  Proofs: 743 checks, 0 unproved.
- Hosted suites all pass. The fixtures gained a test-only owned-memory
  syscall.
- Native:
  - storage-grants PASS.
  - bench-fs PASS with 8 MiB. Round 3 against your 32 MiB fsq run: within
    noise, except create 29.5k vs 34.6k/s.
  - **desktop-display and input-stream still fail at 128 MiB**, with a
    4 MiB cache too, out of memory elsewhere. Desktop now loads (it didn't
    before); then fonts panics on allocation and ccl-workbench's 6.2 MB
    segment is rejected.
  - Budget: 100 MiB free at boot; images 38.5 MiB (desktop 15.5, netstack
    12.7, fs 1.1); virtio-gpu DMA 24 MiB; initrd about 5.7 MiB.
- Profile-driven sizing waits for typed launch parameters, which are
  still design-only. Scaling with free memory needs a kernel free-memory
  query and a pressure signal: owner requests.

## 2026-09-29 Metadata gap: parked handles, namespace generation, 2,048 handles

Ownership for this task (granted by main): also libc `file.c`; small
shared edits landed with their callers: `cubit_fd.h` (FS_OPEN_* moved
from fd.c; `__cubit_path_rename`), `syscall.c` (rename/renameat/
renameat2), `cubit-filesystem_queues.ads` + `cubit_fs_queue.h`,
`tests/fs-bench/{fs-bench.c,init-bench-fs.ccl,README.md}`, docs.

- **Shared_Objects rewritten**: O(1) open-addressing table (probe window
  64) plus per-object holder lists (Max_Holders 32) with Places_Of. Level
  1: 246 checks, 0 unproved (two instances, one with colliding hashes).
  Hosted: 200,000 random attach/detach steps.
- **Handle table 2,048** (EXTENDED_HANDLES 64 keep directory/optical
  state; file handles from a cursor). Delegations: 2,048 entries at
  Delegations_At 12,288; Queue_Bytes 77,824. fileChanging, handleOpened,
  handleUnlink and reclaimOrphan use holder lists, not table scans.
- **Namespace generation** (Namespace_Generation_At 72): bumped before
  unlink/mkdir/rmdir/rename, on volume admission (all clients), on
  SET_ACL/REVOKE_ACL (that client).
- **Queue_Park (13)**: the service harvests, drops rights to read and
  re-delegates. Only handles whose opener may read (`parkable`, answer
  Rights_Read) can be parked. Handles keep exactly the rights they were
  opened with. The first version gave write-only opens read rights, which
  broke storage-grants POSITIONED-IO-CHECK; now fixed. releaseHandle
  drops the delegation first.
- **libc parked handles**: 1,024, LRU, keyed by normalized name.
  - Reuse needs: generation unchanged since before the open, delegation
    valid, the same inode, and the PARK answered.
  - A reused handle re-parks with no request.
  - unlink/rmdir/rename close the name's parked handle first; unlink's
    close is held and sent in the unlink's pass (`q_hold`).
  - A destructor closes parked handles and drains async answers at exit.
- **Revocation**: REVOKE_ACL releases parked handles like any other, and
  release clears the delegation. SET_ACL moves the generation. A released
  handle is refused by the service. The generation lives in
  client-writable memory: it only protects the client from stale names.
- **Temporary diagnostics removed**: file.c (diag_*, fs-queue/fs-cache)
  and main.adb (diag*, fs-service). They were costly: the service's
  periodic serial prints halved create/unlink rates.
- Native bench-fs (KVM; Linux ext3 data=ordered medians in brackets):
  - create 59,152/s (98,519);
  - open-read 2,012,732/s (658,592);
  - unlink 137,314/s (286,967);
  - list 5.59 M/s (5.43 M).
- A/B on one kernel, diagnostics still in:
  - parking: unlink about 49 k → 27 k/s (the extra close); open-read
    about 32 k → 1.8 M/s; create unchanged within noise.
  - Without the close before unlink, unlink stayed at 47 k/s, but later
    creates collapsed (evicting orphaned parked handles), so the close
    stays.
  - The held close and the diagnostics removal landed together, so the
    final numbers do not separate them.
- Service time per op (diagnostics run): create open about 11 µs, park
  about 7 µs, unlink about 5 µs. What remains possible: a create request
  that carries the first write and the park; pipelined creates only with
  deferred error reporting (not POSIX open); a cheaper per-name service
  path.
- Final native gate (same build; world, libc, fs-bench): libc PASS,
  storage-grants PASS, desktop-display PASS, bench-fs PASS (16 coherence
  PASS). Second-run medians: create 58,330, open-read 1.42 M, unlink
  131,421/s.
- Hosted: queue-layout PASS (61 constants), shared-file-objects,
  volume-list, rename, all 15 truncate suites, interop (27 images) PASS.
  Proofs at level 1: shared-file-objects 246/0 unproved, accounting 743/0.
- Coherence (native and Linux-hosted; all PASS): reopen after
  unlink+recreate, after rename, and after another process writes,
  unlinks+recreates or renames. The other process is a second fs-bench
  instance on CuBit, fork on Linux.
- Known, not fixed (pre-existing): the service never releases a dead
  process's handles or client queue (16 queues). Parked handles are
  closed at exit() but not on a crash or _exit.

## 2026-09-29 Regressions, dead processes, create/unlink (round 2)

- **rand-write regression: found and fixed.**
  - Cause: parked read handles from seq-read/rand-read stayed as extra
    holders of `data`, so rand-write's O_RDWR open was never sole and got
    no write delegation.
  - Fix: a writable open first closes this client's parked handle for
    that name, sent in the same queue pass (libc `park_drop(..., hold)`).
  - rand-write: 6,715 → 1,312,259 IOPS (p50 122 → 0.7 µs).
- **seq-write:** 474 MB/s in the final run; A/B medians 529 (parking off)
  and 523 (on). Round 1-2 write phases were about 5-15 ms longer with
  parking on. Morning runs had medians of 575-618. No single cause found;
  the host was shared.
- **Dead-process cleanup (tested natively; not proved):**
  - New `CuBit.Filesystems.OP_RELEASE_OWNER` (16#0082#), admin-only.
    procmgr sends it on a verified EVENT_CHILD_EXIT, next to netstack's
    OP_RELEASE_OWNER.
  - The service releases every handle (buffered writes harvested
    first), returns the queue, arena and dirty grants (freeing the queue
    slot and the PID's retained frames), consumes a saved WAIT reply and
    clears the ACL profile.
  - Serial shows "FS: released exited process 32: 6 handles, its
    queue" for the fs-bench helper, which leaves via `_exit` with a file
    open. The coherence check other-exit-left-open PASS: its bytes
    survive.
- **Create:**
  - `Queue_Park` no longer harvests. A write delegation stays and its
    pages are harvested later, at write-back, commit, another open,
    release or flush.
  - With deferred harvest, create+write+close is one synchronous
    request plus an asynchronous park, so a combined create request
    saves nothing and was not built.
  - Inode allocation starts at a hint (group, bitmap byte), reads the
    bitmap in 64-byte chunks and writes back only its byte; freeInode
    touches only its byte.
  - Measured create: 59 k → 106 k/s (Linux 98.5 k).
- **Unlink:**
  - `Queue_Unlink` carries the name's parked handle. If that handle alone
    held the file, its dirty pages are dropped unwritten; the request
    lists up to two dirty entries so the service skips its 2,048-entry
    scan (7.3 k → 0.55 k cycles).
  - A successful unlink bumps the inode's version, so a reused inode
    number never matches cached pages.
  - removeEntry prepares only the block the cached scan found
    (`scanCachedDirectory`, factored out of lookupInDir).
  - `Directory_Blocks.Prepare_Remove` compares names in place
    (`Next_Header`, `Name_Is`) instead of copying each record's 255-byte
    name. Level 1: 144 checks, 0 unproved.
  - Hosted removeEntry: 11.9 k → 3.9 k cycles.
  - Measured unlink: 133 k/s (one run: 198 k; Linux 287 k).
- **Harvest:** one scan of the arena per call (`harvestEntries`),
  bucketing entries per slot. Write-back and the commit tick now harvest
  all of an owner's handles in one scan instead of 2,048 scans.
- Final native gate: libc, storage-grants, desktop-display and bench-fs
  PASS (17 coherence checks PASS).
- Hosted: all truncate suites, interop (27), journal replay, crash,
  namespace and pressure PASS. Proofs: Shared_Objects 246, accounting
  743, directory_blocks 144, all level 1, 0 unproved.
- Once, by mistake, I ran `make -C kernel initrd` outside the build lock
  (checking an initrd failure); it succeeded quickly. Noted here so it
  can be caught if it collided with anything.

## 2026-09-30 Round 3: seq-write/unlink, then security review fixes (final)

Stopped optimizing on request. Final numbers (bench-fs medians of 3; the
Linux ext3 run was about an hour earlier on the same host):

| op | CuBit | Linux | ratio |
|---|---:|---:|---:|
| seq-write 64 MiB + fsync | 584 MB/s | 848 | 0.69 |
| create + 4 KiB + close | 110,998/s | 86,590 | 1.28 |
| open + read + close | 1,742,825/s | 565,093 | 3.1 |
| unlink | 138,379/s | 250,307 | 0.55 |
| rand-write 4 KiB | 586,089 IOPS | 24,190 | 24 |

- **Seq-write profile.**
  - The fsync is at parity: it is mostly the device flush, 2 barriers,
    about 53 ms.
  - The write phase is the gap:
    - client cache growth pays a kernel owned-memory allocation per page
      (about 30 ms per 64 MiB on first use);
    - the service's NVMe path runs at 1.5-2 GB/s (128 KiB commands, 7 in
      flight in the driver's kernel-mapped 1 MiB DMA window, bounce copy).
  - Kept: reuse of stale client cache pages before growth (libc).
  - Tried and reverted, no clear gain or rule-breaking:
    - 2 MiB NVMe transfers and larger ext2 batches;
    - 256 KiB NVMe commands;
    - writing harvest runs straight from the client arena;
    - skipping appendRun's payload staging;
    - dropping the cache chunk memset.
- **Unlink.**
  - Kept:
    - `Directory_Blocks.Remove_In_Place`, proved at level 1: same refusals
      as Prepare_Remove, including duplicate names; at most 4 bytes
      change, and those are reported;
    - ext2 writes only the changed bytes, from a service copy;
    - an empty unheld inode on ext3 is freed under the unlink's handle
      (no orphan list);
    - reclaimInode skips truncating a file with no blocks;
    - inode alloc/free touch only their bitmap byte.
  - Removed in review:
    - the client-supplied dirty-entry list;
    - discarding pages before the unlink succeeds.
  - Unlink peaked at 350k/s before these removals and is 138k/s after.
- **Review fixes (all tested, none proved; main.adb is not SPARK):**
  1. Harvest waits on odd entries within a 64-yield budget per harvest,
     then skips them. Before, a client could force 100,000 yields per
     entry.
  2. Granting a write delegation bumps the file version; so do inode
     creation and orphan reclaim. `refreshDelegation` kept silently
     demoting write delegations: a latent bug, fixed.
  3. At most 8 handles per client per file (EBUSY beyond that).
  4. Flush is allowed on read-only handles. Write-back failures of
     released handles are kept per owner and file (64 entries, overflow
     per owner) and reported by the owner's next flush; they are
     forgotten when the owner exits.
  5. An unlink's parked handle is checked (owner, file, one holder, one
     link, write delegation). Its pages are written unless the unlink
     succeeded; if it did, its entries become stale by tag.
  6. SET_ACL takes back all the target's delegations.
  7. WRITE_AT copies the client's bytes into service memory before
     writeData.
  8. Dirty-entry tags carry 21 generation bits. Stale or forged tags are
     freed unwritten.
  - Kernel check: Memory_Grants.Loans.Revoke drains while acquisitions
    exist, so a client cannot pull its arena from under a harvest.
- storage-check: STORAGE-FLUSH-CHECK now expects a read-only handle's
  flush to succeed, which is Linux fsync semantics.
- New coherence checks (Linux-hosted and native, all PASS, 21 total):
  - fsync-read-only;
  - open-while-other-holds-many;
  - reopen-after-other-delegated-write;
  - unlink-keeps-moved-files-data.
- No regression test for the odd-sequence flood: it needs a hostile
  client with raw arena access. It is bounded by construction.
- Final gate:
  - native, on the final build: world, libc, storage-grants,
    desktop-display and bench-fs PASS.
  - hosted: all truncate suites, interop (27), journal replay, crash,
    namespace, pressure PASS.
  - proofs at level 1: Shared_Objects 246, accounting 743,
    Directory_Blocks 170, all 0 unproved.

## 2026-09-30 Round 4 (new filesystem agent): seq-write and unlink gaps

Ownership note (current). Scope: seq-write and unlink performance only.
Files I may edit: userspace/services/filesystem/*, userspace/services/nvme/*,
libc userspace/libc/overlay/src/cubit/file.c, tests/fs-bench/*,
docs/filesystem-data-plane.md, this journal. Kernel changes only if profiling
shows they are needed (will note here first). Shared scripts (run.sh, .gpr)
only under the build lock. Native builds/tests under coordination/build.lock.
No commits. Status: profiling.

2026-09-30 round 4 progress (temporary profiling in filesystem/nvme, marked
Fs_Prof / NVPROF, to be removed):
- Baseline today (KVM): CuBit seq-write 625 MB/s median, unlink 143k/s;
  Linux (linux.sh, ext3) seq-write 908, unlink 294k/s.
- Unlink profile (service ticks per op, ~3.8 GHz): 21.5k total; of it
  Directory_Blocks.Remove_In_Place ~9k (Next_Header's record result went
  through memory with store-forwarding stalls: 53 ticks/record), orphan
  list add+remove+2x2048-entry orphan scans ~6.7k.
- seq-write profile: fsync ~ at parity (2 barriers ~50 ms). The write
  phase is bound by the NVMe path at ~2 GB/s (per 32 MiB: copy 1.3 ms,
  doorbells 4.3 ms, waits 13 ms). Linux-guest dd on the same QEMU NVMe:
  128 KiB QD1 1.7 GB/s, 512 KiB QD1 4.0, 2x512 KiB 5.4, 8x512 KiB 6.7.
  Doorbell batching tried: neutral (time moved into waits), reverted.

2026-09-30 ~08:40 round 4 status at host reboot (recorded by the main session):
- The functional gate passed on the final build: world, libc, storage-grants, desktop-display, and bench-fs x2 with all 21 coherence checks.
- The Fs_Prof/NVPROF profiling code has been removed.
- The final bench-fs throughput is INVALID: the host was at load average 70 from about 200 Codex workspace-diff `git add` processes.
- To do: rerun bench-fs on a quiet host and record the numbers here. The agent itself is ended.

2026-09-30 ~09:10 round 4 FINAL (main session, quiet host after reboot):
- **CuBit:** 3 bench-fs runs, all 21 coherence checks PASS in each.
  Medians of 9 rounds: seq-write 645 MB/s, unlink 267,611/s, create 121,002/s.
- **Linux, same host:** seq-write 897, unlink 288,925/s. Ratios 0.72 and 0.93.
- **Proof:** Directory_Blocks re-proved at level 1 (170 checks, 0 unproved) after round 4's inlined Next_Header.
- **Record:** full table in tests/fs-bench/README.md.
- **Harness note:** bench-fs has no early exit, so QEMU runs until --timeout. The runner stopped each QEMU at "fs-bench: done" instead. run.sh line 72 claims QEMU quits at the done marker; that is not true for bench-fs.
