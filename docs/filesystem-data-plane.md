# Filesystem data plane: client caching and request queues

Status: implemented (2026-09-29); see "Status" and "Metadata operations"
below. The goal is filesystem I/O within 15–20% of Linux on tests/fs-bench
(same QEMU/KVM and NVMe, both on ext3 in data=ordered mode). The service side
is covered by FS-005 (docs/development-backlog.md):
- a block cache;
- JBD2 journaling in data=ordered mode;
- write-back once the journal exists.

This document covers the client side.

## Why the service cache is not enough

Linux answers a warm 4 KiB `pread` in about 0.4 µs, and a warm
open+read+close in about 1.4 µs, entirely in its kernel. A round trip to
a CuBit service costs several microseconds. Even with a perfect cache in
the filesystem service, every read that crosses to it loses by an order
of magnitude.

The fix is the one NFS uses: **the client caches file data, under a
delegation from the service**. It answers warm reads and buffered writes
from its own memory, and talks to the service only for misses, write-back,
opens and closes. Those go over a queue pair, not a call each
(docs/async-rings.md).

## Pieces

### 1. A request queue pair per client

- The client opens three channels on the filesystem endpoint
  (docs/data-plane.md; `CuBit.Filesystem_Queues`), once: the **transfer
  arena** and the dirty arena (arena channels it lends, both sides
  writing), then the **queue pair** (a duplex channel). The queue is
  `CuBit.Submission_Queues` over 64-byte requests and 32-byte answers.
  Since 2026-10-06 each side writes only memory it owns: the client's
  region holds the requests and the indices it writes, the service's
  region (granted back, read-only) the answers, the service's indices,
  its wake word, the namespace generation and the delegations. This
  replaced `OP_FS_QUEUE`, one client-lent region the service wrote into.
- Requests name data as (arena offset, length) within the arena, never
  as a grant per request.
- Entries: `OPEN`, `CLOSE`, `READ_AT`, `WRITE_AT`, `FLUSH`, `RESIZE`,
  `READDIR`, `UNLINK`, `MKDIR`, `RENAME`, and later more.
- The queue acts with the authority of the endpoint it came through.
  Each entry is admitted like its IPC twin. Handles resolve only in the
  owner's table.
- The service answers on the completion ring. Its wake word and the
  client's kick (the channel protocol's `OP_KICK`) work as in netstack's
  control queue.
- Many requests are in flight. The service may complete them out of order
  (tokens pair them).
- The service can also post **unsolicited entries** (token 0, kind
  `RECALL`), one completion slot reserved per delegation it granted. The
  client must reap them. A client that ignores a recall loses the
  delegation at the recall deadline, and its later cached writes are
  refused.

### 2. Delegations

When the service opens a file for a client, it grants one of:
- `None`: every read and write goes to the service.
- `Read`: no other process has the file open for writing. The client may
  cache what it reads, and serve reads from that cache until recalled.
- `Write`: this client is the file's only opener. The client may also
  cache writes (write-back) and the file size, pushing them with
  `WRITE_AT` at flush, close, memory pressure or recall.

A conflicting open recalls every incompatible delegation. The service
defers the open's answer until each holder has returned its dirty data
and acknowledged (`RECALL_DONE`), or until the recall deadline passes.
Revoking a client's access also recalls its delegations. Data it read
before the revoke stays in its memory, as it would with an open file
descriptor on Linux.

`fsync` means the client pushes its dirty data, then asks the service to
`FLUSH` that file, which means a journal commit plus a device flush.

### 2b. Write delegations: a dirty arena the service harvests

A client holding a `Write` delegation (the file's only open handle)
writes into pages of a **dirty arena**. That is a second grant, shared with
the service, with a table of dirty entries in shared memory. Each entry
holds the handle slot, the page number, a byte range and a sequence word.

- The client makes an entry's sequence word odd while it copies into the
  page and even when done (a seqlock). It checks the delegation's valid
  word before starting a write.
- The service **harvests** dirty pages itself on:
  - close;
  - fsync (FLUSH);
  - a writable or conflicting open by another handle (after clearing
    the valid word);
  - its write-back timer.

  It copies each page whose sequence word is even and unchanged across
  the copy, retrying if the client was mid-copy. It writes the page to
  the file and marks the entry clean.
- So a recall needs no cooperation from the client. An idle or blocked
  holder cannot stall another process's open, and no dirty data is lost.
  The only wait is for a client caught inside a single page copy.
- Close and fsync become one queue request each. The data moves once:
  from the dirty arena into the service's block cache.
- The service treats the table as untrusted. It checks every index and
  range against the arena and the handle's file, and it snapshots an
  entry before using it.

### 3. The client cache (libc)

- A per-process page cache keyed by (file handle, page index). It is
  bounded and evicts with CLOCK. Dirty pages are written back before they
  can be evicted.
- `read`/`pread` copy from cached pages. A miss is read ahead
  sequentially: the window doubles up to a bound while access stays
  sequential, as Linux's does.
- `write`/`pwrite` go into cached pages when holding a `Write`
  delegation, otherwise straight to the service.
- `close` pushes dirty pages and then closes. The data then sits in the
  service's cache, and the service's write-back makes it durable.

### 4. Zero extra copies

- Reads land in the transfer arena, which is the client's memory, and are
  copied once into the page cache or the caller's buffer.
- The service copies from its block cache into the arena.
- Later, large reads can DMA straight into arena pages: the service would
  hand NVMe the physical pages the client lent.

## Status (2026-09-29)

- **Queue pair and arena: implemented.**
  - `CuBit.Filesystem_Queues` and `cubit_fs_queue.h`, whose layout is
    checked by tests/fs-bench/queue-layout-check.py.
  - The service's side is in filesystem `main.adb`: entries are handled
    by the IPC handlers, routed through `sendReply` and
    `acquireClientMemory`.
  - libc's open, read_at, write_at, flush, close, unlink, mkdir, rmdir,
    rename and directory reads use the queue.
  - Waiting: a client sleeps on its own event loop with a wake request
    (`OP_FS_WAKE`, docs/filesystem-protocol-v2.md); further queue
    operations are designed there.
  - The service polls its queues for 50 µs after activity, then arms its
    wake word.
- **Read and write delegations: implemented.** A per-handle table (one
  entry per handle slot, 2,048, on its own pages of the queue grant) with
  inode versions; a 128 MiB libc page cache with sequential readahead; a
  dirty arena the service harvests on close, flush, write-back, recall
  and the 5 s commit.
- **Parked handles, namespace generation, 2,048 handle slots:
  implemented** (next section).

## Metadata operations

Linux opens, reads and closes a cached small file in about 1.5 µs. Every
step happens in its kernel, under its dentry and inode caches. Any
design where each open crosses to the filesystem service pays at least
one queue round trip. So opens that can be answered without the service
are:

- **Parked handles** (libc `file.c`). Closing a file handle whose
  delegation is valid, and whose opener's policy lets it read the file,
  keeps the handle open ("parked"), keyed by its normalized CuBit name.
  The client sends `Queue_Park` (asynchronous): the service drops the
  handle's rights to reading. A write delegation stays, with the pages
  buffered in the dirty arena. They are harvested later: write-back at
  half a full arena, the 5 s commit, another handle's open, release or
  flush, as Linux writes dirty pages back after close. The client can
  still add to them, but only to its own file. A policy change
  (`SET_ACL`) takes back all the client's delegations, so a parked
  handle cannot buffer past it. A read-only open of the same name then
  takes the handle back with no request, if:
  - the queue's namespace generation equals the one read before the
    handle's own open;
  - its delegation is valid and names the same inode.

  Otherwise the handle is closed and the open goes to the service. So
  only a plain read-only open is ever answered locally; create,
  truncate, exclusive and writable opens always go to the service.
  A reused handle closed again is parked without a request (the
  service's side is unchanged). At most 1,024 are parked, least recently
  parked closed first.
  - A writable open of a name first closes this client's parked handle
    for it, in the same queue pass. Otherwise the parked handle would be
    a second holder, and the writer would get no write delegation. This
    was a regression in the first version: 4 KiB random writes fell from
    about 2 M to 6,715 IOPS.
  - rmdir and rename close the name's parked handle first.
  - `Queue_Unlink` carries the name's parked handle, checked against the
    service's own table. The handle is dropped only if it alone holds
    the file being unlinked: same owner and file, one holder, one link,
    under a write delegation.
    - Its delegation is taken back before the unlink.
    - If the unlink succeeds, its buffered pages are never written, as
      Linux drops an unlinked file's dirty pages.
    - Any other outcome writes them to the surviving file.
    - Any other parked handle is closed as CLOSE would be, its pages
      written first.
  - At `exit()` a libc destructor closes parked handles and waits for
    every asynchronous answer.
- **Dirty entries are tagged.** An entry's tag is the handle's slot plus
  21 bits of the handle's generation (`Tag_Slot_Bits`,
  `Tag_Generation_Bits`). The service takes an entry only if its tag
  names a live handle of the queue's own client. An entry of a released
  or reused slot never matches: a later harvest frees it unwritten. A
  harvest waits for an entry being written (odd sequence word) within a
  64-yield budget per harvest, then leaves it for a later one. No client
  can hold the service, and a dead process's release always ends.
- **Coherence.** Granting a write delegation moves the file's version
  on, as do creating a file and reclaiming or unlinking an inode. So no
  client's pages cached at an earlier version are taken again after
  another client's buffered writes. `refreshDelegation` keeps a write
  delegation a write delegation. Before this fix it silently demoted
  one, which could strand the client's buffered entries.
- **Limits per principal.** One client may hold at most 8
  (`MAX_HANDLES_PER_OWNER`) of a file's 32 handles, parked ones
  included. The next open is refused with EBUSY, so no single process can
  exhaust a shared file.
- **fsync and write-back errors.** Any handle may flush (fsync of an
  `O_RDONLY` descriptor, as Linux). A write-back failure of a handle
  that is then released (an asynchronous close, a parked handle) is
  kept for its owner and reported by the owner's next flush of that
  file. If the 64-entry table is full, it is reported by the owner's
  next flush of any file.
- **Service-owned copies.** A client's bytes written through the
  service (`WRITE_AT`) are copied into service memory before `writeData`,
  so the disk and the block cache get the same bytes. Harvested pages
  are copied the same way, and checked against the entry's sequence
  word after the copy.
- **Dead processes.** On `EVENT_CHILD_EXIT`, procmgr sends
  `OP_RELEASE_OWNER` to filesystem.svc (as it sends netstack its release).
  This covers exit, `_exit` and crashes. The service:
  - releases every handle of the process, parked ones included;
  - harvests its buffered writes first (its acquisition of the dirty
    arena keeps the frames);
  - returns the queue, arena and dirty-arena grants, freeing the queue
    slot (16 at most);
  - clears the process's access profile.
- **The namespace generation** (`Server_Namespace_At`, a word in the
  service's region of the queue pair). The service moves it on before any unlink,
  rename, rmdir or mkdir, when a volume is (re)admitted (for every
  client), and when a client's access policy changes (`SET_ACL`,
  `REVOKE_ACL`, for that client). A client reads it before sending an
  open, so a change racing the open leaves the handle's generation
  behind.
- **Revocation cannot be bypassed by a parked handle.** A parked handle
  is an ordinary open handle to the service. `REVOKE_ACL` releases all
  the client's handles (parked ones included), and every release now
  clears the handle's delegation first; a policy change also moves the
  generation on. Either makes the reuse check fail, and a released
  handle is refused by the service anyway. A narrowing `SET_ACL` also
  takes back every delegation of the client (buffered writes written
  first). Its open handles, parked ones included, survive it with the
  rights they were opened with, as open descriptors do on Linux. The generation is in
  client-writable memory: it guards the client against stale names,
  not the service against the client.
- **2,048 handle slots.** Directory and optical-file handles live in the
  first 64 (their extra state is kept only there); file handles take the
  rest from a cursor. The service finds a file's handles through
  per-object holder lists in `Shared_Objects`: an open-addressing table
  (probe window 64) with at most 32 holders per object. Attach, detach,
  lookup and "the other handles of this file" are O(1) or bounded by
  those constants. Proved at level 1: 246 checks, 0 unproved
  (tests/shared-file-objects, with a colliding-hash instance).

Measured natively on the final build (fs-bench, KVM, both on ext3
data=ordered, medians of three rounds; the Linux run was about an hour
earlier on the same host);
tests/fs-bench/README.md has the full table:

| Operation | CuBit | Linux | CuBit / Linux |
|---|---:|---:|---:|
| create + write 4 KiB + close | 110,998/s | 86,590/s | 1.28 |
| open + read 4 KiB + close | 1,742,825/s | 565,093/s | 3.1 |
| unlink | 138,379/s | 250,307/s | 0.55 |
| 64 MiB sequential write + fsync | 584 MB/s | 848 MB/s | 0.69 |

- **Create** is one synchronous request (the open). The first 4 KiB
  write stays in the dirty arena, and the close is an asynchronous park
  that doesn't harvest.
- **Unlink** is one synchronous request.
  - The name's parked handle, when it alone holds the file, is kept
    until the unlink's outcome is known and released in the same
    request. Nothing else can run in between (the service is single-
    threaded), so no handle outlives the unlink: the inode is freed with
    its name, as an unheld file's is, instead of joining the ext3 orphan
    list and leaving it at the handle's release (2026-09-30; that trip
    and two 2,048-entry orphan-table scans were ~6.7k of the ~21.5k
    service cycles per unlink). If the name is still there afterwards,
    the handle's pages are written to the file first, as before; once the
    name is gone they are never written (their tags die with the handle).
    A handle that does outlive the request (any other open handle of
    the file) still keeps the inode through the orphan list.
  - An unlink of a file with no blocks and no surviving handle frees the
    inode under the same journal handle as its name, with no orphan list.
  - The directory record is removed in place: `Remove_In_Place` is
    proved at level 1 to change at most four bytes of the block, and
    only those are written. Its walk of the whole block (duplicate names
    are refused) cost about 53 cycles per record: `Next_Header`, called
    out of line, returned its record through memory with store-forwarding
    stalls. It is now `Inline_Always` (same code and contracts, proof
    unchanged): about 12 cycles per record, a 4 KiB block of ~340 records
    in ~4k instead of ~18k cycles (hosted microbenchmark).
  - An earlier, faster version trusted a client-supplied list of dirty
    entries and discarded pages before the unlink was known to succeed.
    Both were removed for correctness; the cost shows in the number.
- **Sequential write** (profiled): about 55 ms of the fsync is the device
  flush itself, as on Linux. The gap is the write phase.
  - First use of new client cache memory costs a kernel owned-memory
    allocation per page. A truncated file's stale pages are now reused
    before the cache grows.
  - The service's device path runs at about 1.5–2 GB/s: 128 KiB
    commands, at most 7 in flight through the NVMe driver's 1 MiB
    kernel-mapped DMA window, with a bounce copy. Linux writes the same
    64 MiB much faster inside its fsync.
  - Closing this needs a larger DMA window or DMA from grant pages
    (kernel), which is out of scope here. Larger NVMe transfers (2 MiB),
    writing straight from the client's arena and skipping the payload
    staging were tried and measured. They made no clear difference, or
    they relaxed the rules above, and were reverted.
  - 2026-09-30 profile (temporary TSC counters, since removed), per
    64 MiB round: the fsync is two barriers (~50-54 ms, the host's
    flush) plus ~3 ms of remaining harvest; the write phase is bound by
    the NVMe path at ~2.1 GB/s (per 32 MiB: bounce copy 1.3 ms, doorbell
    MMIO 4.3 ms, completion waits 13 ms). A Linux guest's `dd
    oflag=direct` on the same QEMU NVMe reaches only 1.9-2.4 GB/s at one
    command in flight and 2.7-3.1 GB/s with deep queues when writing
    never-written regions of the image (3.9-7 GB/s when rewriting): the
    host's first-write cost, not the command size, dominates. Tried and
    reverted as neutral: one SQ/CQ doorbell per batch of commands, two
    ~500 KiB commands instead of four 128 KiB, and 2 MiB device
    requests. The remaining gap is not closable by driver tuning alone;
    the candidates are fewer barriers per fsync (jbd2 revoke records
    instead of moving the log tail after a commit that freed blocks)
    and deeper device queues (a larger DMA window or DMA from grant
    pages: kernel work).

## Order of work

1. Queue pair and transfer arena; libc switches to them. Measure.
2. `Read` delegation and the client read cache with readahead. Measure.
3. `Write` delegation, write-back, and recalls. Measure.
4. `UNLINK` and `MKDIR` (missing today). Directory reads through the
   queue.
5. Lazy close (parked handles) with a namespace generation: done.
   A client directory cache: not needed by fs-bench so far.

## Proof obligations

- Queue and ring bookkeeping: already proved (`CuBit.Submission_Queues`).
- Arena references: every (offset, length) a request names lies inside
  the registered arena. This is a proved decoder, as for netstack's
  entries.
- Delegation table (service): a file never has a `Write` delegation and
  any other opener at once. A `Read` delegation never coexists with
  another process's writable open. A recall completes, or is forced at
  the deadline, before a conflicting open is admitted.
- Client cache bookkeeping: bounded, and a dirty page is never dropped
  without a completed write-back.
