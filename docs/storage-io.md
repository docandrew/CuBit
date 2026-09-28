# Storage Control Plane and Zero-Copy Data Path

Status: design accepted; writable RAM block-device bootstrap, generation-checked
acquired grants, and application/filesystem/block-driver staging paths
implemented; direct application-to-device grant derivation and IOMMU isolation
not yet implemented.

For current Files navigation, application-scoped ACL hardening, proof results,
and the remaining safe-save blockers, see
[Filesystem maturity](filesystem-maturity.md). Those control-plane improvements
do not eliminate the payload staging copy or add crash consistency.

The initial `Block.Device.V1` description and synchronous transfer contract is
implemented in the shared userspace runtime. RAM, ATA and NVMe now publish the same
validated geometry, feature, and transfer-bound representation, and ext2 uses
one block-session backend without testing controller kind. The filesystem's
staging grant is created through its authorized block endpoint and transported
as an explicit `(slot, generation)` reference. All three drivers acquire that
reference through the kernel, which validates its authenticated owner, complete
byte range, and required access and pins its physical frames until the driver
returns the acquisition. Fixed discovery slots and the staging copy remain
transitional control-plane and data-path wiring.

The application-to-filesystem protocols now use the same reference form.
`OPEN`, `READ`, `WRITE`, `READDIR`, `RENAME`, and ACL transfer requests carry
the generation beside the slot; filesystem.svc authoritatively acquires the
whole requested range with direction-appropriate access before touching it and
returns the acquisition after the transfer.
`READDIR` also carries the actual client-buffer capacity rather than assuming
that a grant occupies the maximum 16 MiB aperture slot. A live negative test
requires rejection of both a stale generation and a request one byte beyond a
one-page grant before exercising the valid read/write path.

File read and write completion is now explicit. ATA and NVMe return success
only for a full requested block transfer; ext2 verifies the reply shape and
exact byte count, and filesystem.svc reports no-space, read-only, out-of-range,
unsupported-file-range, and transport failures separately from successful
zero-byte completion. A failure may report a confirmed prefix, and the file
handle advances by exactly that prefix. Sparse holes remain successful zero
data and are therefore no longer confused with a failed device read.

The Ext2 volume-admission boundary validates the bounded implementation's block size,
inode layout, group geometry, bitmap capacity, and free-count assumptions once
before accepting a volume. Block and inode allocation now search all groups,
use group-relative bitmap indices, initialize newly allocated partial data
blocks before publishing their inode pointer. Uncertain metadata publication
quarantines writes instead of relying on unchecked rollback. The headless storage
test begins with a sparse file on an image whose first four block groups are
full, then exercises first-block allocation, explicit unsupported-range
failure, live file creation, write, seek, read, and close.

## RAM storage and the volume list

The writable `live-rw.ext2` seed is copied into heap storage owned by
`ramdisk.drv`, not filesystem.svc. Bootstrap starts the driver before FS and
grants FS an endpoint; there is no additional driver-registry ID or new syscall.
The driver uses the existing block protocol and kernel-checked grant acquisition.
Ext2's direct-memory backend, image pointer/length fields and `initMemory` API
have been removed. Hosted Ext2 fixtures also go through the block protocol.
The separate read-only CPIO bootstrap archive is unchanged.

`FEATURE_VOLATILE` explicitly states that the device has no persistence
obligation. Completed writes remain visible while it lives; its contents are
lost on driver restart or reboot. Volatile devices cannot also advertise
`FEATURE_FLUSH`. Ext2 permits volatile truncate without a persistence barrier,
but exposes Flush as durability-unsupported, not a successful no-op.

The internal **volume list** now associates service-lifetime volume identities
and names with authorized block-device endpoints. Per-volume contexts hold the
filesystem instance and staging grant. File handles, shared-inode keys,
directory children and recovery-driven handle retirement identify a volume,
not a hardware-driver enum. Names convey no authority, and
listing volumes must not imply permission to read their files. No global UNIX
directory attachment model or drive letters are required.

Bootstrap registers `mem:0` when the live seed exists, then `ata:0` and
`nvme:0`. Driver roles, fixed slots and transfer sizes occur only in these
bootstrap bindings; one generic session-admission path handles them.
File operations dispatch on filesystem format and volume index. The immutable
CPIO/ISO bootstrap readers remain separate; this is not yet a public enumeration
API or a conversion of those readers into list entries.

Explicit paths match the full registered name, e.g. `@nvme:0/file`. Implicit
device-zero shorthand is removed; no alternate spelling bypasses exact name
selection. Unqualified bootstrap lookup and the RAM-workspace creation default
remain, with policy checked against the original request before selection.
The readiness role is only a startup hint; the installed endpoint authorizes
grant creation. Lazy admission now distinguishes provider-not-ready, explicit
no-device, resource/grant failures, malformed descriptions, unsupported/invalid
filesystems and device I/O failures. Only the first two allow automatic search
to continue. Explicitly selected unavailable volumes return an error. Admitted-
volume lookup errors also stop automatic fallback before creation.

Block providers may answer Describe with canonical `REPLY_NO_DEVICE` (one zero
word, zero flags/reserved). ATA distinguishes absent/recognized non-ATA hardware
from IDENTIFY failure. Timeouts and malformed replies are not absence. Ext2
admission performs one checked superblock read, publishes no partial context on
failure, and no longer speculatively retries failed transfers. A failed admitted
attempt is sticky for the filesystem-service lifetime, including no-device;
hotplug/reprobe remains future work. Missing registered providers may be checked
again, as may allocation/grant creation failures before Ext2 admission begins.

See [bootstrap storage and Config](boot-storage-and-config.md) for the agreed
durable-declaration/boot-snapshot/live-list split; that startup design is pending.

Entries are append-only for a service lifetime (16 entries, 48-byte names).
Duplicate names and endpoint slots are rejected, and no entry can be rebound
under a live handle. Future removal/replacement needs explicit handle/cache
retirement, and durable identification remains separate work. Persistent media
identifiers are evidence, not trusted identity merely because an image claims
a UUID. Partitions sharing one endpoint are not implemented.

See [volume-list tests and proof scope](../tests/volume-list/README.md).

Ext2 now gates admission on a conservative standard feature profile and rejects
ordinary file access to symlinks, special inodes and multiply linked regular
files. Directory browsing is unaffected by normal directory link counts. See
[Ext2 interoperability](ext2-interoperability.md) for exact support, evidence and
the distinction between reported link counts and a disk-wide ownership proof.

Data-only overwrites now skip unchanged-inode publication. A typed inode comparison
removes one descriptor read and one inode-sector read/modify/write, while growth
and mapping changes still publish metadata. Measured production-code request counts
and fault-test scope are in [Turso I/O findings](../tests/config-turso/io-findings.md).
This adds neither a write-back cache nor a weaker flush contract. Sector accounting
now advances on successful attachment, not EOF growth: data blocks count once,
and a newly attached single-indirect root counts once too. Filling a sparse hole
below EOF updates the count; extending within an allocated block does not.
The production inode value transformation is SPARK-proved (exact increments,
overflow/rejection preservation, pointer update and unchanged unrelated fields).
The surrounding allocation/device/publication control flow remains fault-tested,
not formally proved; see [accounting proof scope](../tests/filesystem-truncate/README.md#allocated-sector-accounting).

Existing-data writes also coalesce contiguous full filesystem blocks, bounded by
the grant and provider transfer sizes, request, original EOF and supported mapping
range. Holes and fragmentation stop a run; failed speculative mapping lookup does
not submit the pending batch. Only completed batches contribute to the returned
prefix, and no atomic-write promise is added. Allocation/growth and partial-sector
paths remain separate. This reduces IPC/device submissions, not payload copies;
the service and NVMe submission path are still serialized.

See [RAM block-driver regression scope](../tests/ram-block/README.md).

`CuBit.Filesystems` is the shared SPARK protocol package for Ada clients and
the server. It centralizes operation labels, typed generation-bearing request
constructors, path bounds, seek origins, open options, and opaque file-handle
types. Its open-option decoder rejects unknown bits and the reserved access
mode. Read-write opens now request and retain both rights; this fixes the prior
wire-level ambiguity that silently produced a write-only handle.

Ordinary grant creation now rejects every range that overlaps the received-
grant aperture, closing the previous implicit regrant path.  The range and
permission attenuation rules have been extracted into the pure SPARK package
`Memory_Grants`; all of that package's checks prove with no assumptions.  This
is an interim fail-closed boundary, not derived-loan support: forwarding a
borrowed buffer intentionally fails until explicit parent/child lifetime state
is implemented in the kernel.

Future grant references are modeled as two explicit fields, global slot and
generation, rather than a packed integer with implicit bit layout. Generation
zero is never live, and exhaustion retires an identity instead of wrapping.
The kernel now persists and advances each slot generation across revocation and
PID reuse, offers owner-only generation lookup, and acquires a reference only
for its current grantee after checking authenticated owner, generation,
permission, byte offset, and byte length. Every published mapping owns pins on
its backing frames. Acquisitions defer revocation; after final return permits
retirement, mappings are removed and TLB invalidation is acknowledged before
their pins are released. Revocation becomes pending while an
acquisition exists, blocks new acquisitions, and completes after the final
return. Owner teardown retains the grant, its backing storage, and the process
identity until that point. Grantee teardown forcibly returns its acquisitions
before deleting its mappings. Generation-checked revocation and a typed
userspace `Grant_Reference` wrapper are implemented.

The live diagnostic checks direct and capability-directed acquisition,
read-only enforcement, range rejection, wrong-owner rejection, pending
revocation, final return, stale-reference rejection, and generation change on
slot reuse. This closes the single-hop acquire/use race. It does not yet provide
derived parent/child loans, per-request cancellation, quotas on retained pinned
memory, or device DMA isolation. See [shared IPC buffer lifetimes](ipc-buffer-lifetimes.md)
for the common model and current desktop migration.

The lifecycle is a private state machine in the pure SPARK `Memory_Grants`
package rather than three independently mutable kernel fields. Focused proof
discharges its runtime checks and contracts: acquisition is available-state
only, a revoke with borrowers becomes pending without changing their count,
and the last return from that state makes the grant inactive.

The application/filesystem and `Block.Device.V1` boundaries have been migrated
from single-integer grant IDs and direct aperture arithmetic. Other service
protocols still using that legacy convention are migration debt and must move
to explicit slot/generation fields plus authoritative resolution before
derived loans are exposed.

## Principle

Storage authority flows through successively narrower objects:

```text
hardware authority -> block-device session -> mounted-volume session
                   -> filesystem tree/file handle -> application
```

Payload bytes do not follow that chain. `storage.svc` is a discovery, policy,
partition, and mount control plane, not a data proxy. Once a filesystem has a
restricted block-device session and an application has a file handle, eligible
bulk I/O maps the application's pages directly to the device driver and, for
DMA devices, to the device's IOMMU domain.

```text
Control: app -> filesystem.svc -> storage.svc -> block driver
Data:    app pages <===========================> device
```

The filesystem remains the authority and translation point: it validates the
file handle and operation, clips the byte range, translates it to extents, and
submits only those extents. It need not copy or even map payload pages merely
to authorize them.

## Block-device sessions

ATA, NVMe, ATAPI, USB mass storage, and memory devices implement one typed
`Block.Device.V1` protocol. A session describes:

* logical and physical block sizes;
* block count and permitted LBA interval;
* read-only and flush support;
* alignment, scatter/gather, and maximum-transfer limits;
* removable-media identity and generation; and
* bounded outstanding-operation and queue limits.

The storage control plane gives a filesystem a session restricted to one
volume. Steady-state reads and writes go directly from the filesystem to that
session; routing them through `storage.svc` would add IPC latency without adding
authority enforcement.

## Derived memory loans

Current CuBit grants map owner pages into one grantee. The final storage path
needs explicit grant derivation rather than treating a mapped virtual address
as newly owned memory.

For a read:

1. The application lends page-aligned destination pages to `filesystem.svc` as
   `borrowed-rw` and submits its file handle, range, and completion token.
2. The filesystem validates and clips the operation, then derives a narrower
   `borrowed-rw` loan for the particular block-device session.
3. The kernel maps the same frames into the driver and pins them for the
   accepted operation. With an IOMMU, it maps those frames into that device's
   domain and returns an opaque I/O virtual address or scatter/gather token.
4. DMA writes directly into the destination pages. A PIO driver writes port
   data into that same final mapping.
5. Driver completion consumes its child loan. Filesystem completion returns
   the parent loan to the application exactly once.

A write follows the same path with `borrowed-ro`: the device may read the
application pages but no intermediary may widen that permission.

Derived loans form an ownership tree. A child range and permission must be a
subset of its parent. A parent cannot complete, revoke, unmap, or be reused
while an accepted child remains active. Cancellation is an explicit terminal
outcome; non-cancellable device work keeps the pages pinned until completion.
Generation checks prevent a late completion from resolving a reused loan.

The current generic grant implementation does not yet carry parentage. It can
translate a virtual address back to physical frames, so ordinary creation now
rejects any range overlapping the received-grant aperture. This prevents that
operation from implicitly re-granting a read-only mapping as read-write or
creating an untracked descendant. A typed derive operation with the proved
attenuation checks is a prerequisite for the direct storage path.

## Filesystem work versus payload work

The filesystem uses private bounded buffers for metadata such as superblocks,
inodes, allocation maps, and directory records. Fragmented files become bounded
scatter/gather submissions targeting successive regions of the same application
loan. Metadata traffic does not require copying file payloads.

Copying may still be necessary and must be visible when:

* the request has unaligned head or tail bytes requiring read-modify-write;
* the device cannot address the supplied pages or satisfy its alignment bound;
* encryption, compression, encoding, or another transformation is required;
* a cached-page or copy-on-write consistency rule requires distinct storage; or
* a very small inline IPC transfer is measurably cheaper than mapping pages.

These are explicit fallback paths, not the default bulk path. APIs should make
page- and block-aligned asynchronous I/O easy so normal large transfers remain
copy-free. Completion metadata travels through IPC; payload bytes do not.

The initial ext2 memory image is bootstrap scaffolding. It performs one copy at
mount because the kernel intentionally maps the initrd read-only, and ordinary
ext2 reads currently copy from filesystem block buffers into client grants. A
future native RAM store should own page objects directly so aligned reads can
share immutable pages and writes can use controlled copy-on-write.

The current disk path is also staged rather than zero-copy: application bytes
are copied into the filesystem's block-session grant, and NVMe copies again
into its controller DMA area. This is deliberately retained until derived
loans can preserve parent lifetime and permission attenuation; ordinary grant
creation rejects borrowed aperture pages instead of creating an unsound hidden
descendant.

## DMA isolation

A bus-mastering device can bypass CPU page-table capabilities. Secure direct
DMA therefore requires an IOMMU domain containing only controller queues,
descriptors, and pages pinned for currently accepted operations. The driver
receives opaque I/O virtual addresses, not unrestricted physical addresses.

The current frame-pin and ownership metadata is CPU-side lifetime enforcement;
it is not an IOMMU. HDA and NVMe still program physical DMA addresses, so each
device and the userspace driver controlling it remain in the memory-isolation
trusted computing base. CuBit may support this as an explicitly degraded mode,
but must report it visibly and must not describe that mode as full device
containment. PIO can preserve memory isolation at lower throughput.

## Required invariants

1. Control-plane authority can narrow but never widen at each delegation.
2. `storage.svc` is absent from the steady-state payload path.
3. A memory loan conveys no file, volume, or device authority.
4. A file handle conveys no authority over the supplied memory pages.
5. Derived loan permissions and ranges are subsets of their parent.
6. A read-only loan cannot be derived as read-write.
7. Parent completion or revocation cannot race an active child loan.
8. Each accepted loan is returned exactly once after all device access ends.
9. A device can DMA only to pages admitted for its current operations.
10. Zero-copy fallback and copied-byte counts are observable per operation and
    attributable through WHAT, WHO, WHEN, WHERE, and WHY diagnostics.
11. Resolving or acquiring a loan and using its mapping is atomic with respect
    to revocation, owner teardown, and identity reuse for the loan's lifetime.
12. A filesystem may claim crash consistency only when the block session
    distinguishes accepted writes from durable writes and its journal recovery
    has been validated across every relevant interruption boundary.

## Acceptance measurements

Storage diagnostics should count submitted bytes, directly transferred bytes,
fallback-copied bytes, mappings, IOMMU invalidations, queue depth, completions,
timeouts, and latency histograms. Tests should require zero fallback bytes for
aligned sequential reads and writes through ext2 on NVMe, including fragmented
scatter/gather cases, while checking data integrity and stale-loan rejection.
