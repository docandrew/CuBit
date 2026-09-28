# Positioned filesystem I/O

Implemented first stage for the Turso/native async storage path. This extends
the existing filesystem service; it adds no policy authority or alternate
storage stack. The existing cursor-based operations remain in use by apps.

## Wire contract

`Read_File_At` (`0x000D`) and `Write_File_At` (`0x000E`) use the existing four-word
IPC message, length 4, flags/reserved zero:

| Word | Meaning |
|---|---|
| 0 | Existing PID-bound, generation-tagged file handle |
| 1 | Canonical grant identity: generation × 2^32 + slot |
| 2 | Requested byte count |
| 3 | Absolute 64-bit file offset |

Only the established slot/generation ranges are accepted. No new reference
truncation, memory header, metadata grant or buffer copy is introduced. Payload
starts at grant byte zero, so existing page alignment is preserved. This does
**not** remove existing FS/block-driver staging copies.

The shared handlers still check handle ownership, object kind and open rights.
The kernel still validates grant owner, recipient, generation, access and range.
Read requires a writable destination grant; write accepts a read-only source.
Request decoding is not authority minting. A valid wire reference can still be
stale or unauthorized and must fail acquisition.

## Semantics

* Never consult or change the seek cursor, including on partial success or error.
  There is no save/seek/restore wrapper. Cursor-based calls share the same backend
  execution path and advance by the completed prefix as before.
* Reject an offset/count range that would wrap before any transfer or acquisition.
  This check now also protects cursor-based transfer arithmetic.
* Zero-byte operations validate handle/open rights and canonical encoding but
  acquire no memory. They do not establish that the supplied grant is live.
* Read at EOF returns zero; a read straddling EOF returns the available prefix.
  Backend failures retain existing reply labels and completed-prefix counts.
* Write may grow the file within backend limits. Immutable media remain immutable.
  Success is a write acknowledgement, not a durability barrier; use Flush.
* The caller must keep its accepted request buffer alive and avoid mutating or
  reusing it until completion. Requesting grant revocation does not substitute
  for completion or establish cancellation. Current execution acquires,
  transfers and returns
  memory synchronously inside the service; `capSubmit` can deliver the reply as
  an async completion but does not create parallel block I/O.

## Evidence and next boundary

The portable codec has a separate SPARK proof (see
`tests/grant-references/README.md`). The live `storage-grants` fixture covers
positioned writes/readback, preserved cursor, ordinary reads after positioned
reads, partial EOF and unchanged buffer tail, zero length, overflowing ranges,
malformed wire fields/tags, stale handles/grants, rights on both files and grants,
repeated acquisition/return, and submission via the async IPC primitive.

Run through the Nix environment (sequentially, since boot tests share staging):

```sh
nix develop -c make -C kernel filesystem storage-check bench-storage
nix develop -c bash tests/headless/run.sh --test storage-grants --accel tcg,thread=multi --timeout 60 --serial /tmp/positioned-storage.serial --keep-logs
nix develop -c bash tests/headless/run.sh --test bench-storage --accel tcg,thread=multi --timeout 60 --serial /tmp/positioned-bench.serial --keep-logs
```

`bench-storage` adds 512 random positioned reads after 32 warmups, verifying each
block. It does not include a separate Seek, unlike the untimed Seek in the old
random-read phase. These phases alone do not measure the end-to-end savings of
eliminating Seek, and TCG runs establish regression behavior, not hardware speed.

Regular-file metadata coherence is now supplied by the shared open-object table
(see `tests/shared-file-objects/README.md`): independently opened handles share
current size/block mappings while keeping ownership, rights and cursors separate.
The native coherence test reproduced the old stale-EOF bug before the fix.

Checked truncate now persists detachment before block reuse, and uncertain
truncate/write-metadata publication quarantines the volume and retires affected
handle aliases. See [fault tests and remaining limits](../tests/filesystem-truncate/README.md).

Allocation and regular-file creation now propagate failures rather than relying
on unchecked rollback/publication.

Indirect file-block reads now propagate transport failures instead of caching
them as holes. Cache fills publish only on complete reads; volume reopenings invalidate
the cache, and malformed physical pointers fail before data I/O or allocation.

Inode/path resolution now uses checked APIs too; lookup failures cannot masquerade
as missing names before creation or automatic volume fallback. RAM storage now
uses a real block-driver endpoint, with explicitly unsupported durability.

File operations now select an internal volume-list entry, independently of the
hardware driver. Handles and shared metadata retain the volume identity.

Device admission now has typed outcomes too: automatic lookup can skip explicit
absence but not a failed device or malformed filesystem.

Next: declarative bootstrap bindings, metadata durability/recovery paths, bounded pending-request
ownership and actual FS-to-driver concurrency. Exclusive database ownership
is still not implemented. These remain requirements before persistent Turso
adoption; sharing metadata does not itself make concurrent mutation safe.
