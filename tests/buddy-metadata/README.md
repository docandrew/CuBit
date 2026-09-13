# Buddy metadata layout and address arithmetic

`Buddy_Metadata` is the pure SPARK layout core now used by the kernel's
descriptor lookup and pin/owner/descriptor table reservation sizing. It is not
a separate allocator or an additional runtime metadata representation.

## Properties

The focused proof passes **20 checks, none unproved or justified**:

- An inclusive highest frame reserves exactly `highest + 1` entries.
- Every admitted slot, including its final byte, lies within that footprint.
- Distinct ordered frame numbers have disjoint slots; block-sized descriptor
  spans also remain within the footprint when the geometry core admits them.
- Page rounding provides sufficient storage without an unnecessary extra page.
- Given a table base whose complete footprint fits in the numeric address
  space, address addition does not wrap and distinct slots remain disjoint.

The descriptor bit size is named in `Buddy_Blocks` and used in both its record
representation and the boot-admission array component size. A compile-time
check rejects byte/stride mismatches. Pin and owner entries remain one byte;
block descriptors remain two bytes. No extra metadata is allocated.

The kernel takes the common highest-frame value from the already-admitted
`Buddy_Bitmap` layout. Boot requests are checked against `BootAllocator.AllocSize`
before conversion, since release kernel builds deliberately omit subtype
checks. This is one cold-path input-size admission check, not a new check on
every descriptor lookup. A too-large firmware-derived metadata extent is
rejected before any of these three table allocations are attempted.

`Fits_At` and all proof lemmas are Ghost. The actual `Address_Of` arithmetic is
inlined. With the pinned x86-64 toolchain, production `descriptorAddress` remains
433 bytes including its existing validation/error paths, and slot addressing
still uses an indexed `LEA`. No metadata helper calls or Ghost predicates enter
the kernel. The routine still contains the pre-existing geometry alignment
division; this round does not claim to remove it or improve allocator latency.

## Reproduce

Run sequentially from the repository root:

```sh
nix develop -c bash tests/buddy-metadata/prove.sh
nix develop -c bash -lc 'cd kernel && alr exec -- gprbuild -p -P ../tests/buddy-metadata/metadata_tests.gpr && ../tests/buddy-metadata/build/main'
nix develop -c bash tests/intrusive-list-splices/prove.sh
nix develop -c make -C kernel cubit_kernel
nix develop -c python3 tests/buddy-metadata/check-codegen.py
nix develop -c python3 tests/intrusive-list-splices/check-codegen.py
nix develop -c tests/headless/run.sh --test capability-security --disk kernel/capability_security_disk.img --accel kvm --cpus 4 --timeout 90 --serial /tmp/cubit-metadata-native-serial.log --keep-logs
nix develop -c env CUBIT_DOOM_MULTIAPP=1 tests/headless/run.sh --test desktop-doom --accel kvm --cpus 4 --timeout 90 --serial /tmp/cubit-metadata-desktop-serial.log --keep-logs
```

Host tests cover **1,574,454 slot cases**, including several address bases,
exact-fit and wrapping placements, full 40-bit frame limits, page granules from
one byte through 1 GiB (including non-power-of-two values), all supported buddy
orders, request-budget boundaries, and the actual Ada array stride. These are
numeric tests; they do not allocate multi-terabyte arrays or dereference the
synthetic address values. The earlier 213-check list/ledger proof also passes.

Final four-vCPU KVM regressions passed: `capability-security` verified eight
partial-load rollbacks and successful PID reuse; the multi-app `desktop-doom`
fixture verified game pixels and responsive Apps-menu input.

The proof reuses the existing CVC5 integer encoding from
`tests/buddy-bitmap/why3.conf`; it introduces no axioms, `Assume`, or proof-off
regions. The host oracle itself is tested, not a proved unit.

## Boundary still open

Nonwrapping numeric addresses do not establish mapped, owned physical memory.
The adapter must still establish that boot allocation returns suitably mapped,
aligned, disjoint reservations of the requested size; that these tables and
sentinels cannot be admitted as payload; and that all raw overlays implement
the indexed list/ledger model under the allocator lock. Cross-table separation
depends on those distinct reservations. This proof does not establish
arena-wide allocation non-overlap, firmware correctness, or SMP serialization.

The core currently uses 40-bit frame numbers and 64-bit numeric addresses. Those
are explicit target bounds, not a claim to cover every architecture unchanged.
