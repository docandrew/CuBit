# Ext2 allocation, creation and metadata-publication failures

## Acknowledged pointer blocks stay warm

After a successful single/double-indirect attachment, the existing read cache
retains the complete pointer blocks that were just acknowledged by the block
provider. It no longer discards them only to read them back at the next lookup.
This is not write-back caching: bitmap, group, superblock, data, leaf, root and
inode writes retain their previous ordering. Ambiguous pointer writes still
invalidate the cache and quarantine further mutation. The cache has the same
single-owner/exclusive-volume assumptions as before; it is not a new shared
metadata cache or a power-loss guarantee.

`sector_counts` tests immediate read-back after attachment across all three
block geometries: only payload I/O is issued. Changing the provider endpoint
forces pointer metadata reload. Its injected failures also compare reads against
an independent raw-disk pointer walk **without clearing caches** after the fault,
including partial/malformed pointer publication. This is a forensic test of
cache contents, not permission for the service to republish an uncertain inode.
The existing 8,880 allocation/publication fault cases remain enabled.

`main` and `resizing` now use real volume admission when replacing the simulated
disk, matching the cache-lifetime boundary used by the native service rather
than overwriting a volume in place behind its live cache.

The cache/I/O behavior is regression-tested, not newly SPARK-proved. The
separate 87-check accounting/mapping proof does not prove hardware or cache
coherence.

## Allocation descriptor scan

`allocateBlock` reads adjacent block-group descriptors as one minimum-sector
snapshot instead of fetching each 32-byte record separately. The snapshot is
local to one search; no metadata mutation occurs before the candidate is
selected, and every mutation path returns. Nothing persists across allocation
calls or crosses a write. Bitmap/group/superblock updates and their failure
ordering are unchanged. This is read coalescing, not a write-back cache.

`allocation_scan` checks 513 cases at and across the 16-descriptor boundary,
including before/partial/after read failures and malformed completions. A
search reaching descriptor 31 now uses two descriptor-sector requests instead
of 32; the seven subsequent bitmap/metadata requests are unchanged. A second
allocation observes a newly available earlier group, regression-testing that
the local snapshot isn't reused. The geometry in this focused fixture is
synthetic; native tests on Linux-created ext2 images and independent e2fsck
cover admitted volumes. This is not a whole-allocator SPARK proof.

After building the hosted project below:

```sh
nix develop -c tests/filesystem-truncate/build/allocation_scan
```

## Nonzero resize and EOF exposure

`resizing` exercises production `resizeFile` with zero, aligned and unaligned
sizes, direct/single-indirect transitions, sparse extension, imported mappings
past EOF, the single-indirect boundary, no-op resize, read-only/volatile media,
missing flush support and inconsistent allocation counts. A shrink followed by
growth, or by a positioned write past EOF, must not expose the discarded bytes
still present in the retained final block.

**4,839 injected failures** cover before/partial/after completion, error labels
and malformed replies. Resize sweeps omit `Short_Transfer` because a flush's
zero-byte reply is valid; gap-write sweeps include it. The fixture checks each
bitmap clear against the last successfully flushed inode and pointer block:
reclaimed blocks must no longer be reachable. It checks no subsequent I/O after
a rejected completion, no published result on failure, and no retry I/O after
quarantine. A read failure after completed zeroing strictly beyond EOF can
leave the old size/prefix usable; it has not published or freed anything.

SPARK proves exact subtraction and unchanged accounting on underflow rejection
in `Sector_Accounting.Plan_Removal`; the combined local proof has **87 checks,
zero unproved**. This is not a proof of the pointer walk, IPC authorization,
disk ordering or crash recovery. No kernel/runtime assertions were enabled.

The native `storage-grants` test requires `FILE-RESIZE-CHECK: PASS`, testing
owned write-authorized handles, rejected read-only requests, alias coherence,
cursor preservation, malformed messages and unsupported lengths. Independent
Linux image/content/fsck validation is in [filesystem-interop](../filesystem-interop/README.md).

Double-indirect allocation/resizing is covered below. Exclusive database
ownership, async queueing, journaling and the persistent Turso adapter remain
separate work.

Run on the Linux host, in Nix:

```sh
nix develop -c bash -c 'cd kernel && alr exec -- gprbuild -p -P ../tests/filesystem-truncate/truncate_tests.gpr && ../tests/filesystem-truncate/build/main'
nix develop -c tests/filesystem-truncate/build/indirect_reads
nix develop -c tests/filesystem-truncate/build/path_reads
nix develop -c tests/filesystem-truncate/build/admission
nix develop -c tests/filesystem-truncate/build/overwrites
nix develop -c tests/filesystem-truncate/build/sector_counts
nix develop -c tests/filesystem-truncate/build/object_admission
nix develop -c tests/filesystem-truncate/build/inode_slots
nix develop -c tests/filesystem-truncate/build/resizing
nix develop -c tests/filesystem-truncate/build/path_decoding
nix develop -c tests/filesystem-truncate/build/double_resize
```

This builds the **production Ext2 implementation**, not a duplicate algorithm.
Only the IPC transport and grant declaration are replaced by test fixtures.
The fixture supplies a sector device, a grant buffer, and a durable image
snapshot updated by successful simulated flushes. Assertions/checks are enabled
in this hosted executable; none were added to the native service.

### Double-indirect paths and existing mappings

`path_decoding` exhausts 1,378,087 supported/boundary paths for 1/2/4 KiB blocks,
plus large rejected 64-bit indices. `Block_Paths.Decode` now returns a bounded
discriminated value; it cannot be passed a caller-constrained output record of
the wrong variant. Its Ghost contract and range safety prove without `Assume`
or SPARK-Off sections. The combined local proof now has **87 checks, none
unproved**; this does not prove disk state, cache lifetime or hardware behavior.

`indirect_reads` adds **105 before/partial/after injected faults** for double-
indirect overwrites and cross-leaf lookahead. Checks cover unchanged metadata,
cache recovery, a single payload request on warm contiguous overwrites,
single/double and leaf boundaries, the final double block and completed-prefix
reporting at the unsupported triple boundary. New allocation and resizing use
the real bitmap/accounting fixtures described below.

`double_resize` adds **13,356 injected failures** for shrink/growth at all three
geometries, including partial leaves, discarded leaves, double-root retirement,
and data-vs-metadata/cross-leaf duplicate rejection before writes. Every reclaimed
block must be unreachable in the last flushed tree. The inventory sort also
checks independently counted permutations and the maximum 1,050,638 entries
with extreme block IDs; no million-block disk allocation is needed.

The direct + single-indirect truncate fixture has 36 transport boundaries.
Each is failed before I/O, after a partial write (64 bytes), and after I/O
completion with an error reply: 108 cases. Every reclamation bitmap write checks
that a flush has already persisted detachment of each block being reclaimed.
Failures after publication begins must quarantine the volume; a second truncate
must issue no I/O. Early read failures leave storage unchanged.

Six additional cases fail the indirect-pointer and inode publication writes
during file growth, before/partially/after completion. They check that the newly
referenced block stays allocated, the operation reports recovery required with
no reliable completed prefix, and subsequent writes issue no I/O.

Other cases cover successful reclamation/accounting, duplicate/out-of-range
pointers, unsupported triple-indirect trees, read-only devices, missing flush
support, and a block session advertising volatile storage.

Allocation and creation add these before/partial/after fault sweeps:

| Operation | Transport boundaries | Injected cases |
| --- | ---: | ---: |
| Block reservation | 8 | 24 |
| Inode reservation and blank initialization | 11 | 33 |
| Create in an existing directory block | 19 | 57 |
| Create with directory growth | 28 | 84 |
| Create rejected for inode exhaustion, reclaiming unpublished directory space | 18 | 54 |

Together with the previous 99 cases, **351 injected-failure cases** run against
the production implementation. Additional cases cover normal no-space/read-only
admission, reserved inodes, partial final allocation groups, duplicate names,
malformed directory records, unsupported indexed directories, and RAM creation.
Directory-publication writes assert that the new inode was reserved and fully
initialized first. This checks completion order, not persistence across power loss.

## Checked indirect reads and cache lifetime

`indirect_reads` also compiles the actual Ext2 implementation. It reproduced
the old bug before the fix: a failed single-indirect read was reported as a
successful sparse read, because the unchecked reader zeroed the pointer buffer
and the resolver cached it anyway.

The resolver now returns a physical block **and a checked status**. Only a
successful lookup with physical block zero means a sparse hole. Direct data
pointers, indirect roots, double-indirect leaf pointers, and final data pointers
are checked against the volume bound. Pointer-cache keys are invalidated before
buffer replacement and installed only after the entire block read succeeds.
Both reads and writes propagate failed lookup; writes do not allocate a
replacement for an unreadable existing mapping.

The new executable exercises:

- 75 before/partial/after fault/retry cases over single- and double-indirect
  metadata and payload reads, including error labels, short completions, wrong
  word counts, and nonzero reply flags/reserved fields.
- Warm-cache behavior: repeated reads require only payload I/O.
- Eviction of each of the three warm pointer caches, failing the second
  transfer after the first half has overwritten the buffer. Revisiting either
  the old or new key must reload valid metadata rather than reuse mixed data.
- Completed-prefix reporting and failed speculative lookup during batching.
  A speculative error aborts before issuing that batch's data read; only
  earlier completed batches are reported. It is not hidden by an implicit retry.
- Legitimate sparse holes, malformed pointers, single/double range boundaries,
  rejected triple-indirect ranges, and no allocation on write-side lookup error.
- Endpoint isolation and reopening the same endpoint after its image changes.
  Volume initialization clears pointer caches; RAM uses the same block protocol.

Read replies validate flags/reserved fields as well as status, length and byte
count. Volume superblock reads now use the checked interface and preserve the
existing bounded startup retry; the obsolete zero-on-error raw read wrappers
were removed.

`path_reads` adds 90 nested-path read/reply failures, 30 final-inode read
failures and 45 rename-parent failures. Checked inode/path APIs now replace
the status-discarding overloads. Missing names remain distinct from unreadable
or malformed metadata; automatic file lookup only tries another volume after
Not_Found, not a lookup error. Lazy device initialization still has a Boolean
admission result and needs a separate absent-device versus failed-device model.

## Production behavior

- `OPEN_TRUNCATE` calls the zero-length case of checked `resizeFile` through
  `truncateToEmpty`; there is no separate emptying implementation. Native
  `Resize_Request` supports nonzero sizes within the same direct/single range.
- Read/validate and snapshot the supported block tree before mutation.
- Publish the detached, empty inode; on persistent devices flush it before
  any free-bitmap update. Devices lacking a flush barrier reject the request
  before mutation. RAM is supported but does not claim persistence.
- Check bitmap, group descriptor, and superblock writes during reclamation.
  Stop and quarantine on uncertainty rather than continue reusing blocks.
- Return typed errors. Recovery-required truncate/write failures retire all
  live regular-file handles sharing the affected inode in the FS dispatcher;
  successful truncate updates their shared metadata without changing cursors.
- Failed indirect-pointer publication never frees its possibly referenced
  blocks. Failed final inode publication does not publish a candidate inode to
  the open-object table or report a reliable completed byte prefix. An error
  does **not** imply that no bytes changed; automatic retry is not a recovery
  protocol.
- Block/inode reservations return no object after an uncertain metadata write.
  The old best-effort bitmap/counter rollbacks were removed: the volume stops
  accepting writes instead of assuming rollback succeeded. Ordinary exhaustion
  does not quarantine the volume.
- Creation plans a directory slot before allocating an inode, checks inode
  initialization, and only then publishes the name. Growing directories reserve
  their block first, so block exhaustion consumes no inode. If inode space is
  exhausted, the never-published directory block is reclaimed with checked I/O.
  Uncertain publication keeps the reservations instead of freeing an inode that
  a directory may already reference.
- Create errors now reach clients as typed filesystem replies, including no
  space, already exists, I/O error and recovery required. The old Boolean
  allocator wrappers, unchecked insertion/free-inode helpers, unused directory
  removal path and unchecked raw metadata-write APIs were removed.

## Scope and remaining work

These are regression tests, **not a SPARK proof**, hardware power-cut test,
full malicious-filesystem validator, or journaled transaction. The device must
honor its flush contract. Initial block ownership is assumed consistent; local
pointer range/duplicate validation is not a global cross-link or metadata-block
ownership proof. Triple-indirect truncation, external attributes, and
fragments are rejected rather than partially handled.

Interrupted reclamation can leak blocks or leave free-space counters inconsistent.
Quarantine is currently in-memory: it is not a persistent recovery marker.
Offline checking/recovery and persistent dirty-state handling are still required
before claiming safe recovery across service restart or power loss.

Creation currently supports regular files in plain direct-block directories
(up to 12 blocks), rejecting indexed/indirect-directory mutation before allocation.
It does not implement mkdir, unlink, directory compaction, or general transactions.

`Block_Inventory` has SPARK-proved bounds safety and a strict-order result
predicate. Its permutation preservation is regression-tested, not proved.
The production traversal and leaf-sized detach/flush/reclaim sequence remain
outside that proof boundary. Scratch space scales with admitted allocated blocks
(four bytes each, at most 4,202,552 bytes), not sparse logical file length.
Unknown disk ownership/cross-links remain outside the tested assumptions.

General data-write atomicity, allocation-before-reference **persistence** ordering,
and persistent recovery state still need work. Successful
create is not a durability acknowledgment; error quarantine does not substitute
for a journal. No new per-allocation flush was added. There is no new concurrency
or thread-safety claim: FS admission remains serialized, essential to the current
shared-inode update/handle-retirement sequence.

The native counterpart is:

```sh
nix develop -c make -C kernel filesystem storage-check
nix develop -c bash tests/headless/run.sh --test storage-grants --accel tcg,thread=multi --timeout 45 --serial /tmp/cubit-truncate-native.serial --keep-logs
```

That checks live CuBit grants, positioned I/O, and multi-handle truncate
coherence on NVMe. The hosted fixture covers injected faults; the native test
does not inject device errors. TCG timing is not a performance comparison.

## Device admission

`admission.adb` exercises the production Ext2 admission routine with faults at
Describe and superblock-read completion: errors, short transfers and malformed
tags, before/partially/after simulated I/O. Failed completions must not be retried.
Additional cases cover canonical no-device replies versus malformed absence,
invalid geometry/description, insufficient grant size, filesystem extent beyond
the offered device, invalid local session parameters and clearing a previously
successful output. Rejections publish no usable session and issue no writes.

The test exhaustively checks the shared `May_Search_Next` policy: only an
unregistered provider or explicit no-device outcome permits automatic fallback.
This is a hosted regression test, not proof of the native dispatcher or the ATA
hardware-identification implementation. Native boot checks exercise ordinary
ATA-absence-to-NVMe fallback; malformed media are supplied by the hosted fixture.

The Nix-hosted run passed 55 admission scenarios plus three invalid local-session
checks, alongside the existing allocation/truncate, indirect-read, path-read and
volume-list suites. Native four-vCPU TCG `storage-grants` and `bench-storage`
workloads completed with the stricter admission behavior. No new SPARK proof or
hardware-performance claim is made for this change.

## Data-only overwrite fast path

`overwrites.adb` measures real block-request counts with 1 KiB and 4 KiB filesystem
blocks. Successful overwrites preserve the inode and all unrelated bytes. Skipping
unchanged-inode publication removes two reads and one write per nonempty operation:
Initially, 4 KiB payloads went from 7 to 4 requests with 1 KiB blocks, or 4 to 1
with 4 KiB blocks. Bounded contiguous-write coalescing now reduces the first case
further to **one data request**, when the grant and provider both permit 4 KiB.
A seven-byte sector-crossing write goes from 7 to 4 requests. No flush is removed.

345 fault cases sweep aligned/unaligned transfers with full and constrained grants
across all three failure modes and five reply styles. A rejected completion cannot
trigger another I/O in these data-only cases; returned length contains only fully
completed earlier chunks/batches.
Failed writes may already have changed data; zero acknowledged bytes is not rollback.
Empty/read-only/quarantined operations do no I/O. EOF extension and sparse-hole
mapping changes still publish the inode. The existing pointer/publication fault
tests continue to cover uncertain metadata and quarantine.

These are exact transport-count and functional regressions, not a whole-filesystem
proof or a hardware latency comparison. The separate accounting proof below
covers the inode value transformation, not this overwrite I/O control flow.

`indirect_reads.adb` additionally tests coalesced writes across direct/single-
indirect mappings, with 75 failure cases at pointer-read and data-batch completion.
Failed speculative lookup discards the unsubmitted batch, preserving any earlier
completed prefix. Other cases check provider versus grant bounds, larger device
sectors, holes/fragmentation, unrequested invalid mappings, original EOF and the
unsupported triple-indirect-write boundary. These batches never allocate blocks.

## Allocated-sector accounting

Ext2 `numDiskSectors` counts allocated 512-byte units, including pointer blocks,
not rounded-up logical EOF. `sector_counts.adb` independently traverses direct
and single/double-indirect pointers and checks the inode count, on-disk candidate and
free-block accounting. It covers 1/2/4 KiB filesystem blocks, tiny writes,
extension inside an existing block, sparse writes, hole filling below EOF,
first/reused indirect roots and leaves, overwrites and truncation back to empty.
The 4 KiB geometry also reaches its last double-tree slot, checks the high file-
size word, and requires LARGE_FILE before growth above 2 GiB.

The production value transformation lives in `Inode_Mappings`, with shared
128-byte on-disk types in `Ext2_Inodes` and checked arithmetic in
`Sector_Accounting`. Both regular-file attachment and directory growth use it.
Run the proof in Nix:

```sh
nix develop -c bash -c 'cd kernel && alr exec -- gnatprove -P ../tests/filesystem-truncate/accounting_proof.gpr --level=1 --timeout=5 --checks-as-errors=on -j2'
```

Combined result: **87 checks, 0 unproved** (29 flow/initialization/termination,
49 runtime, 9 functional contracts). No `Assume`, suppressed proof obligations or SPARK-Off
escape hatches are used in these units. No runtime assertion option was added
to the native service.

The proof establishes, for all inputs to the transformation:

- Accepted attachments add exactly the data and new pointer blocks' sectors
  (one, two or three blocks); arithmetic never silently wraps.
- Invalid/overflowing requests return the original inode unchanged.
- Accepted direct attachments change only the selected empty pointer and count;
  indirect attachments change only the root and count, preserving an existing
  root. File size and every unrelated inode field are preserved.

This is a **local preservation proof**, not proof that an arbitrary disk's initial
count was correct. Fresh/unique block reservation, the indirect block's contents,
caller ordering, raw address overlays, cache coherence and the actual I/O remain
outside this SPARK boundary. It does not prove power-loss consistency or the
whole filesystem; accepted device writes are not assumed to be durable flushes.

The hosted production-path tests additionally pass **9,105 injected failures**
(including the additional positioned-write gap zeroing boundaries):
all request boundaries for direct, first/reused single/double roots and leaves,
two-block direct and direct-to-indirect writes at all three sizes, plus definite out-of-space
cleanup; three failure timings times five malformed/error completion styles.
They check no follow-on I/O after rejection, quarantine after uncertain mutation,
and no reported committed prefix when inode publication is uncertain. Tests also
cover count overflow before reservation and invalid value transformations.

These tests exposed and fixed an unsafe fallback: a failed data/pointer write
used to call an unchecked free wrapper, issuing more I/O after rejection. That
wrapper and its public declaration are removed. Reservations are retained and
the volume quarantined after uncertain I/O; only a definite lack of space for
an unpublished indirect root permits checked reclamation. An earlier successful
attachment cannot trigger inode publication after a later transport failure.

Native validation also passed `storage-grants` and `bench-storage` in sequential
four-vCPU TCG runs with the rebuilt filesystem service. The overwrite fast-path
hosted checks still require exactly one request per contiguous 4 KiB overwrite.
These runs validate live CuBit integration, not physical-media latency or injected
hardware failures. Logs: `/tmp/cubit-accounting-native.log`,
`/tmp/cubit-accounting-storage.serial`, `/tmp/cubit-accounting-bench.serial`.

## Feature and link admission

The same proof project now includes `Ext2_Support`: **23 total checks, none
unproved**, including the ordinary-file decision's equivalence to a Ghost
predicate. This does not prove caller integration or disk-wide absence of aliases.

`admission` now passes **159 cases**, sweeping all 32 bits of each feature class
and checking independent on-disk offsets. Unknown features cannot publish a
session, including unknown RO_COMPAT features. Initial support is deliberately
limited to Linux revision 1 with FILETYPE and the documented feature allowlist.

`object_admission` checks **53 rejected inode cases**: every nonregular type,
zero/multiple link counts, every inode flag bit, deletion and fragment metadata.
Rejected data operations issue no I/O; truncate reads metadata but changes
nothing. Listings still expose the objects, a symlink cannot be an intermediate
directory, and directory link counts are not subject to the regular-file rule.

The native `storage-grants` test now includes `LINK-POLICY-CHECK`: Linux-created
short and block-backed symlinks, and both names of a hardlinked regular file,
are rejected in five open modes (25 requests). The full run passed in four-vCPU
TCG, with unchanged ordinary storage/navigation/rename/coherence coverage.
Logs: `/tmp/cubit-link-policy-native.log`, `/tmp/cubit-link-policy.serial`.

See [the interoperability boundary](../../docs/ext2-interoperability.md) for the
standard-format policy, metadata extension direction and remaining limitations.

## Full inode-slot initialization

`inode_slots` tests newly reserved 128/256/512/1024/2048/4096-byte inode slots.
Each begins with stale nonzero bytes. Initialization clears the whole slot while
preserving neighbors, including slots sharing a device sector. **930 injected
failures** require no inode number publication on failure, no follow-on I/O, and
quarantine after uncertain mutation. Existing 128-byte allocation fault counts
remain unchanged. Admission now also rejects aligned but non-power-of-two inode
strides, bringing its total to **160 cases**.

The [Linux-image matrix and native round-trip](../filesystem-interop/README.md)
independently check `e2fsck`, contents and preservation of existing metadata.
The full-slot byte-overlay/I/O path is tested, not covered by the existing local
SPARK accounting/admission proof.
