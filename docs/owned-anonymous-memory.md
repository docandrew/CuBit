# Owned anonymous memory

The native kernel exposes a deliberately smaller operation than POSIX `mmap`:
zero-filled, private, read/write, non-executable pages owned by the calling
process. This is the reclamation foundation for Mesa's large allocations.
It is not GPU memory allocation, executable/JIT memory, or a general unmap API.

| Operation | Arguments | Result |
| --- | --- | --- |
| 115, allocate | byte count, 1 through 16 MiB | page-aligned base, or zero |
| 116, release | original base and byte count | zero, or unsigned 64-bit Last |
| 117, protect | page-aligned base, bytes, mode (0 NONE, 1 RO, 3 RW) | zero, or unsigned 64-bit Last |

Counts round upward to 4096 bytes. Release requires the exact base and rounded
size of a live allocation owned by the caller's current process incarnation.
Interior, partial, heap, device and foreign allocations cannot be released this
way. The kernel chooses an address in `[0x580000000000, 0x590000000000)`;
legacy mapping destinations cannot overlap that aperture. No caller-chosen
physical address, permissions or target PID is accepted.

Backing is allocated one page at a time, recorded in the process frame list,
and subject to its existing frame quota. A bounded global table holds 4096
allocation descriptors. Allocation failure rolls back the completed prefix;
failed cleanup quarantines its inventory. Address-space and registry locks
serialize allocation and release. There is no throughput or latency claim for
the linear descriptor search or per-page allocation path.

Release validates the frame inventory, removes mappings, waits for all online
CPUs' TLB acknowledgments, then detaches and frees the frame records. Existing
shared-memory grants keep their own physical pins: releasing the source mapping
does not invalidate a receiver's acquisition. The native self-grant retention
test covers this pin/alias path; cross-process concurrent release remains a
separate validation task. Process exit reclaims
unreleased pages and clears the corresponding descriptors after quiescence.

Legacy physical mapping requests are serialized against the inventory and
cannot create new aliases to its retained frames. Preexisting arbitrary physical
aliases are not revoked; holders of raw physical mapping authority remain
trusted. This is not an IOMMU/DMA isolation mechanism.

The native adapter and effectful syscall path are not SPARK-proved. Separate
pure layout contracts and hosted release-order tests cover only their stated
boundaries. The storage diagnostic exercises invalid requests, zero-fill,
interior/wrong-size/double-release rejection and 64 rounds of hole reuse with a
neighboring live allocation. It does not establish concurrency or fault-injected
cleanup correctness. Musl's shim now uses these operations, but its native
regression initially failed because guard mappings needed real protection
support. Syscall 117 now supplies that path; native revalidation is required.

Protection ranges must fit inside one live owned allocation. The kernel
validates all retained frame identities, marks affected leaves inaccessible,
waits for TLB acknowledgment, installs the requested non-executable access,
and synchronizes again. Existing grant aliases are unchanged: permissions apply
to this address-space mapping, not every alias of its backing. Frame inventory
inspection recognizes retained PFNs in nonpresent leaves so guards can still
be released. A failed transition quarantines the allocation instead of freeing
its backing. No executable mode or arbitrary heap/device protection is exposed.

The additional `OWNED-GRANT-RETENTION-CHECK` runs 64 rounds inside CuBit:
allocate and fill, create/acquire a read-only self-grant, release the original
allocation, reallocate its exact virtual address, verify zero-fill and write
different contents. The grant alias must still contain the original bytes.
Revocation stays pending while acquired; the alias remains readable until final
return, which must confirm retirement without changing the replacement pages.
Four-CPU QEMU serial evidence is in
`tests/mesa-software/target/owned-grant-serial.log`. This is regression evidence,
not a proof that every concurrent lifetime interleaving is safe.

The exit regression intentionally leaves a one-page and a three-page allocation
unreleased. `Forget_Exited` rejects a nonempty process frame inventory before
clearing descriptors; the native reaper reports two retired records after
freeing the tracked frames. The four-CPU `owned-exit-serial.log` records both
the producer marker and `Process.reclaimProcess: owned regions retired 2`.
This exercises ordinary no-grant exit cleanup, not allocator-wide leak freedom,
PID reuse stress, or owner exit while another process holds a grant.

Native protection validation now includes isolated processes which deliberately
read an inaccessible page and write a read-only page. The libc headless test
requires an armed PID/address, the matching kernel fault kind, and retirement
after that fault. Single-write `USER-MEMORY-FAULT` records avoid interleaving
with concurrent application output. The verifier handles PID reuse without
accepting an earlier retirement; negative controls reject missing, mismatched,
duplicate, wrong-kind and contradictory evidence. `owned-protect-faults-2`
passed with both expected faults plus LIBC/CXX PASS. Raw syscall tests also
reject unaligned, oversized/wrapping, empty and out-of-allocation ranges while
preserving data/access. This is native QEMU evidence, not physical NUC testing
or a proof of every concurrent protection transition.
