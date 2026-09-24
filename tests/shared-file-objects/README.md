# Shared regular-file metadata

`Shared_Objects` is the production bounded table used by filesystem.svc for
open ext2 regular files. Client authority is **not** stored here: file handles
retain their PID owner, generation, open rights and seek cursor. The table joins
internal handle slots by a filesystem/inode key and stores one metadata value.

```sh
nix develop -c bash -c 'cd kernel && alr exec -- gprbuild -p -P ../tests/shared-file-objects/objects.gpr && ../tests/shared-file-objects/build/main'
nix develop -c bash -c 'cd kernel && alr exec -- gnatprove -P ../tests/shared-file-objects/objects.gpr -u model.ads --level=2 --report=all -j4'
```

The hosted model instantiates the same generic at the production capacity of
32 owners, with abstract metadata values. It checks sharing, differing volumes
with identical inode numbers, preservation of live aliases when another closes,
all slots occupied, busy-owner rejection, duplicate detach and 1000 last-close/
reuse cycles. No refcount arithmetic or heap allocator is involved.

GNATprove on 2026-09-23: **25 checks, zero unproved, zero justified**. Proved
contracts establish:

* Attach succeeds with the requested identity or leaves the whole state intact;
  it preserves other owners' attachment, identities and values.
* Replace updates every owner linked to that object and preserves all other
  values, identities and attachment state.
* Detach removes its link without changing another owner's metadata or identity.

This is a proof of those contracts and runtime safety in the hosted model
instantiation, not a proof of the native Ext2 instantiation, file-table coupling,
key uniqueness in all reachable states, policy enforcement, concurrent access,
driver behavior or persistence. Those integration boundaries have regression
evidence and review, not an end-to-end SPARK proof. No Assume/SPARK-Off escape
was added. Assertions are enabled for the hosted tests, not native services.

## Native regression

`tests/headless/run.sh --test storage-grants` requires
`FILE-COHERENCE-CHECK: PASS`. Before the fix, an independently opened reader
returned zero bytes after another handle successfully grew the file. The test
now covers both writers, new block mappings, independent seek cursors, read-only
aliases, zero-progress unsupported writes, truncate through a third handle,
closing one alias, and last-close/reopen readback.

## Cost and remaining boundaries

Lookup/replace on an attached handle is O(1), with an inode-sized metadata copy,
not a file-payload copy or disk reread. Close clears one bounded link. Open finds
a matching live object and, if necessary, a free slot with bounded scans; those
scans are not in the read/write hot path. There are at most 32 metadata objects.

The service remains a **single dispatcher**. The table is not a lock, request
pin, pending-I/O state machine or thread-safe registry. Pending operations must
gain explicit object-lifetime ownership before close can run concurrently.

The current mount set has one memory, ATA and NVMe volume; the native key uses
that backend identity plus inode number. Multiple mounts/remounts will require
a stable mount identity/generation. Directory handles retain their existing
enumeration snapshots and explicit rewind/refresh semantics.

Truncate now checks publication/reclamation errors and persists the detached
inode before freeing blocks. Ambiguous truncate or write-metadata publication
retires affected live handles and quarantines further volume writes. See the
[production Ext2 fault-injection tests](../filesystem-truncate/README.md).
These changes are regression-tested, not covered by the shared-object proof.
Block/inode reservations and regular-file creation now use checked publication
and stop on uncertain metadata writes, with ordinary exhaustion handled separately.
Indirect file-block lookup and cache replacement now propagate read failures,
with a separate production-code hosted regression executable.
Other metadata mutations, remaining inode/path error propagation, persistent recovery state and
crash ordering still need work before asynchronous mutation is enabled.
