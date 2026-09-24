# Ext2 allocation, creation and metadata-publication failures

Run on the Linux host, in Nix:

```sh
nix develop -c bash -c 'cd kernel && alr exec -- gprbuild -p -P ../tests/filesystem-truncate/truncate_tests.gpr && ../tests/filesystem-truncate/build/main'
nix develop -c tests/filesystem-truncate/build/indirect_reads
nix develop -c tests/filesystem-truncate/build/path_reads
nix develop -c tests/filesystem-truncate/build/admission
nix develop -c tests/filesystem-truncate/build/overwrites
```

This builds the **production Ext2 implementation**, not a duplicate algorithm.
Only the IPC transport and grant declaration are replaced by test fixtures.
The fixture supplies a sector device, a grant buffer, and a durable image
snapshot updated by successful simulated flushes. Assertions/checks are enabled
in this hosted executable; none were added to the native service.

The direct + single-indirect truncate fixture has 31 transport boundaries.
Each is failed before I/O, after a partial write (64 bytes), and after I/O
completion with an error reply: 93 cases. Every reclamation bitmap write checks
that a flush has already persisted an inode with zero length and no block tree.
Failures after publication begins must quarantine the volume; a second truncate
must issue no I/O. Early read failures leave storage unchanged.

Six additional cases fail the indirect-pointer and inode publication writes
during file growth, before/partially/after completion. They check that the newly
referenced block stays allocated, the operation reports recovery required with
no reliable completed prefix, and subsequent writes issue no I/O.

Other cases cover successful reclamation/accounting, duplicate/out-of-range
pointers, unsupported double-indirect trees, read-only devices, missing flush
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

- `OPEN_TRUNCATE` uses checked `truncateToEmpty`. The unused arbitrary-size
  truncate API was removed; no callers used nonzero lengths.
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
ownership proof. Double/triple-indirect truncation, external attributes, and
fragments are rejected rather than partially handled.

Interrupted reclamation can leak blocks or leave free-space counters inconsistent.
Quarantine is currently in-memory: it is not a persistent recovery marker.
Offline checking/recovery and persistent dirty-state handling are still required
before claiming safe recovery across service restart or power loss.

Creation currently supports regular files in plain direct-block directories
(up to 12 blocks), rejecting indexed/indirect-directory mutation before allocation.
It does not implement mkdir, unlink, directory compaction, or general transactions.
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
4 KiB payloads go from 7 to 4 requests with 1 KiB blocks, or 4 to 1 with 4 KiB blocks.
A seven-byte sector-crossing write goes from 7 to 4 requests. No flush is removed.

195 fault cases sweep each remaining aligned/unaligned transfer across all three
failure modes and five reply styles. A rejected completion cannot trigger another
I/O; returned length contains only fully completed earlier filesystem-block chunks.
Failed writes may already have changed data; zero acknowledged bytes is not rollback.
Empty/read-only/quarantined operations do no I/O. EOF extension and sparse-hole
mapping changes still publish the inode. The existing pointer/publication fault
tests continue to cover uncertain metadata and quarantine.

These are exact transport-count and functional regressions, not a whole-filesystem
proof, sparse-allocation accounting proof, or a hardware latency comparison.
