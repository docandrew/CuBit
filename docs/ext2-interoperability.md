# Ext2 interoperability boundary

CuBit uses standard Ext2 on disk and CuBit authority semantics above it. These
changes add no private journal, inode layout, reserved-field encoding or feature
bit. This is a limited support profile, not complete Ext2 conformance.

## Admitted profile

- Linux-created revision 1 volumes, 1/2/4 KiB blocks, typed directory entries
  (`FILETYPE`); existing geometry checks still apply.
- Compatible features: `EXT_ATTR`, `RESIZE_INODE`, `DIR_INDEX`, or subsets.
  This does not implement an xattr API, online resizing or indexed-directory
  mutation. Existing indexed directories are rejected for mutation.
- Read-only-compatible features: `SPARSE_SUPER`, `LARGE_FILE`, or subsets.
- Other bits, revisions, creator OSes and legacy untyped directory records
  are rejected before a usable session is published.

Unknown RO_COMPAT features reject the volume altogether; there is no implicit
read-only downgrade. Unknown COMPAT bits are conservatively rejected too. The
signature alone never permits treating Ext3/Ext4 journal, recovery, extent or
checksum layouts as plain Ext2.

Reference: [Ext2 feature compatibility](https://docs.kernel.org/filesystems/ext2.html#feature-compatibility)
and [on-disk structures/constants](https://github.com/torvalds/linux/blob/master/fs/ext2/ext2.h).

## Links and object admission

Ordinary opens require an actual regular-file inode with exactly one reported
link, no deletion timestamp, no unsupported inode flags and no fragment address.
The gate runs before handle/shared-inode attachment or truncation. Read, write
and truncate entry points enforce it too. Directory-entry type labels alone
are never trusted to authorize interpretation of the inode's block pointers.

- Symlinks, both inline/fast and block-backed, are not followed or opened as
  files. Device nodes, sockets and FIFOs are rejected too. No link creation API
  is introduced.
- Multiply linked regular files are rejected through every name. Zero-link
  regular inodes are rejected as unusable live objects as well.
- Directories are exempt from the regular-file link-count rule: their normal
  counts describe structural relationships. Listings remain available without
  opening the objects they describe.
- `REPLY_WRONG_OBJECT_TYPE` identifies nonregular file-open attempts;
  `REPLY_UNSUPPORTED_OBJECT` identifies unsupported link/metadata semantics.
  Neither means insufficient caller authority or a missing name.

UID/GID, UNIX mode and setuid bits grant no CuBit authority. Service policy
authorizes the caller before filesystem object admission. Preserving standard
metadata does not import Linux's security model.

**This is not a hostile-volume alias proof.** A dishonest link count can hide
multiple directory references; distinct inodes can also share blocks on a
malicious image. Checking those properties requires a broader reference and
ownership scan. Global validation and recovery remain pending.
Hostile-volume alias/ownership validation is explicitly deferred as
[FS-006](development-backlog.md#fs-006--validate-hostile-volume-aliases-and-block-ownership),
not a blocker for ordinary interoperability or Config/Turso work. Existing
feature, link-count, bounds and I/O checks remain enabled.

## Metadata extensions: design direction, not implemented

Use standard extended attributes for future namespaced CuBit metadata, e.g.
`user.cubit.*` descriptive values or versioned policy references. Preserve POSIX
ACL formats rather than repurposing their entries as capabilities. Do not reuse
reserved inode fields or another system's security labels. The current driver
preserves opaque attributes on ordinary updates; it does not create/edit them,
and truncate rejects external attribute blocks it cannot safely handle.

Imported metadata is untrusted evidence, not authority. Policy references,
signatures and publisher claims require validation by authorized policy
machinery. A writable xattr cannot manufacture a grant; missing or stripped
metadata must not broaden access. Keep values small, versioned and portable,
not a replacement for Config or a database.

## Validation and follow-up

`Ext2_Support.Check_File` has a proved equivalence to the Ghost ordinary-file
admission predicate. Combined accounting/admission/path/inventory proof: 87 checks, zero
unproved, no `Assume` or SPARK-Off escape hatch. Caller placement and I/O are
regression-tested, not formally proved.

Hosted production-code tests cover 160 volume admission cases, 53 rejected
inode cases, directory traversal/listing, and the existing mutation fault suites.
Native four-vCPU TCG tests use Linux `debugfs` to create real short/long symlinks
and two names for one inode, then test 25 rejected opens over CuBit IPC. Ordinary
storage, scoped authority, navigation, rename and file-coherence checks pass too.

The initial [Linux interoperability matrix](../tests/filesystem-interop/README.md)
now passes 18 Linux-created images with post-driver `e2fsck -fn`, payload checks,
multiple block/inode sizes, and extended metadata/xattr preservation. This is a
hosted production-driver matrix; a separate native CuBit benchmark round-trip
also passes Linux inode-reuse, payload and `e2fsck` checks after real guest I/O.

Newly reserved inode slots are initialized in full; updates of existing inodes
preserve their extended area. Admission requires power-of-two inode strides.
930 injected initialization failures cover slots from 128 bytes to 4 KiB.
Nonzero resizing now supports the direct/single/double-indirect range. Shrink
validates the complete tree, then publishes detached mappings and the new size,
flushes and reclaims storage in leaf-sized batches. Extension remains sparse and clears mapped bytes newly
exposed past EOF. Positioned writes use that same gap-clearing path, so bytes
retained in a partial block after shrink cannot reappear. The standard on-disk
layout is unchanged; no journal or crash-atomic transaction is implied.

`Resize_Request` uses an existing owned, write-authorized file handle. Native
CuBit tests check shared-inode visibility, unchanged seek cursors, denied
read-only handles, malformed requests and unsupported lengths. The hosted
matrix also checks nonzero shrink/regrowth with Linux content extraction and
`e2fsck`; 4,839 fault cases cover direct/single resizing and positioned-write gaps,
and 13,356 cover double-tree resizing.
The new SPARK property is exact sector-count subtraction without underflow;
tree traversal, zero filling, publication ordering and IPC are tested, not proved.

Dirty/clean lifecycle and
partition-backed sessions remain follow-up work, along with broader metadata
and feature coverage beyond this initial matrix.

## Double-indirect mutation support

In-place overwrites of existing double-indirect mappings are now supported,
including batching across pointer-leaf and single/double boundaries. They
preserve the inode, allocation count and mapping blocks. The batching limit
still respects both provider and grant capacity; a failed speculative pointer
lookup submits no payload batch. A warm contiguous 4 KiB overwrite needs one
payload request, with no extra metadata write.

New double-indirect allocation, hole filling, extension, shrink and reclamation
are supported at 1/2/4 KiB block sizes. Triple-indirect access remains unsupported.
The limit is `(12 + P + P*P) * block_size`, where `P = block_size / 4`.
Growth above 2 GiB requires the standard LARGE_FILE feature already enabled;
CuBit does not silently change feature bits. Sparse gaps skip absent subtrees,
while mapped newly exposed bytes are zeroed before publishing a larger EOF.

`Block_Paths` expresses direct/single/double/unsupported paths as a discriminated
value type with bounded indices. SPARK proves the selected indices and admission
ranges, absence of runtime errors in decoding, and rejection of out-of-range
64-bit logical indices. The predicate is Ghost; no assumptions or native
runtime assertions were added. Tests exhaust all 1,378,087 supported/boundary
paths across the three block geometries and check large unsupported indices.
105 new injected faults exercise existing double-tree overwrites, cache retries
and cross-leaf batching. Linux image tests check complete payloads, unchanged
inode bytes and clean fsck results after these mutations.

Allocation initializes data, then its leaf, then the root before publishing the
inode candidate. `Double_Mappings` proves exact addition of one, two or three
blocks' sectors and preservation of unrelated inode fields; rejected candidates
are unchanged. Definite no-space permits checked reclamation of unpublished
reservations; uncertain I/O stops further commands and quarantines the volume.

Before resizing, `validateBlockTree` admits the claimed allocation count against
geometry and volume bounds, collects every data and pointer block, and rejects
out-of-range pointers, count mismatches and duplicate physical blocks across
tree levels. A heapsort replaces the old quadratic duplicate scan. Temporary
inventory space is four bytes per allocated block, at most 4,202,552 bytes,
released before mutation. No persistent per-file allocation table or giant
pointer-tree snapshot is added; the native service declares a 16 MiB stack.
Each detach/flush/reclaim batch uses at most one leaf plus its parent and a small
retirement list. The first batch can publish the new EOF before later outside-EOF
allocations are detached; an interrupted shrink is not an atomic operation.

SPARK proves inventory bounds safety and the final strict-order predicate,
**not** heapsort permutation preservation or the full disk traversal. Tests
independently check permutations, duplicates and the maximum-size inventory.
The 9,105 attachment fault cases cover new/reused roots/leaves, sector overflow,
no-space and cleanup failures. Reclamation tests inspect the durable tree before
every bitmap clear. These local checks do not prove ownership against other
inodes or filesystem metadata, nor safe mutation of concurrently changing media.

All 18 Linux-image round-trips pass complete payload checks and clean fsck after
CuBit-created sparse double trees and shrink/regrowth of Linux-created trees.
Native `storage-grants` also passes double-tree allocation, resize, alias
coherence, zero-tail checks, flush and truncate through real IPC/grants/NVMe
under QEMU TCG. This is correctness evidence, not a hardware performance result.
Exclusive database ownership and the persistent native Turso adapter remain next.
