# Constant-time intrusive list splices

`Intrusive_List_Splices` is the production generic used by the buddy allocator
for insertion, removal, and count updates. It is generic over a private reference
type and uses only assignment/equality, with no pointer arithmetic, page size,
endianness or architecture-specific instruction assumptions.

Run sequentially from the repository root using Nix:

```sh
nix develop -c bash tests/intrusive-list-splices/prove.sh
nix develop -c bash -lc 'cd kernel && alr exec -- gprbuild -p -P ../tests/intrusive-list-splices/splice_tests.gpr && ../tests/intrusive-list-splices/build/main'
nix develop -c make -C kernel cubit_kernel
nix develop -c python3 tests/intrusive-list-splices/check-codegen.py
nix develop -c tests/headless/run.sh --test capability-security --disk kernel/capability_security_disk.img --accel kvm --cpus 4 --timeout 90 --serial /tmp/cubit-splice-final-native-serial.log --keep-logs
nix develop -c env CUBIT_DOOM_MULTIAPP=1 tests/headless/run.sh --test desktop-doom --accel kvm --cpus 4 --timeout 90 --serial /tmp/cubit-splice-desktop-serial.log --keep-logs
```

The original splice proof covered **44 checks, zero unproved/justified**, across
both `Natural` and `System.Address` instantiations. Contracts cover exact link writes and count
increments/decrements under their preconditions. Ghost checks prove insert/remove
restoration of the original neighboring links/count and singleton removal.
For a singleton, the two neighboring nodes coincide, but the actual output
fields (`sentinel.next` and `sentinel.previous`) are distinct. The host test
executes 200,000 deterministic operations and checks the whole represented list
for cycles, reciprocal links, expected membership, exact count, and correspondence
with the actual `Buddy_Blocks` ledger after each operation. Removal commits a
block to `Allocated`; reinsertion releases it through the production transitions.
Explicit empty/full/singleton cases are also covered.

## Ghost list/ledger refinement

The expanded proof command now covers **213 checks, zero unproved/justified**,
including the existing splice instantiations, the production block-state core,
and `Buddy_List_Refinement`. This is a combined scoped total, not 213 new kernel
properties. All proof units are SPARK; no `Assume` or `SPARK_Mode => Off` was added.

The refinement is entirely Ghost and lives under tests, not in the kernel's
runtime representation. An ordered sequence of node IDs plus its inverse rank
provides a witness for one order's intrusive list:

- The sentinel and each active node's actual next/previous links follow the sequence.
- The sequence and rank form a bijection between positions `1 .. Count` and
  descriptors marked `Listed` at the selected order. Other orders share the
  ledger but do not belong to this list.
- Consequently, membership is exact and unique; allocated, retiring, reserved
  and interior descriptors cannot be members. A traversal of actual next links
  returns to the sentinel after exactly `Count` non-sentinel nodes.

Initialization establishes the invariant by initializing only the sentinel's
links, leaving all unused payload links untouched. Insertion and arbitrary
removal preserve it, retain other descriptors and preserve the remaining list
order. Their bodies call the production `Buddy_Blocks.Move` and generic splice
operations in the physical adapter's order. Scalar link results are mapped
into indexed arrays; sequence/rank updates are proof-only. This establishes
logical composition, not the physical mapping itself.

The host build enables Ghost assertions and executes these refinement routines
for capacities 1, 2, 3, 7 and 32, every list length and removal position, and all
40 supported orders. An independent forward-walk oracle checks the results.
Unused initial links deliberately point outside the arena. Additional negative
tests reject detached chains, self-cycles, duplicate entries, incorrect counts,
orphaned ledger publications, and allocated/retiring nodes left in a free list.
The host harness itself is tested, not included in the proof target.

## Native implementation and code generation

Native four-vCPU KVM regressions also passed with the final implementation:
`capability-security` verified eight partial-load rollbacks and successful PID
reuse; `desktop-doom` with `CUBIT_DOOM_MULTIAPP=1` exercised multiple apps and
verified DOOM game pixels and responsive Apps-menu input. These are functional
regressions, not latency benchmarks or a concurrency proof.

The insertion helper is inlined into the actual kernel. With the pinned
toolchain it has the same 32-instruction count and same sole metadata-validation
call as before extraction. Count updates remain in-place machine increments;
Ghost routines and helper calls are absent. This is NOT a measured allocator
speedup or a promise of byte-identical code. The codegen test is intentionally
x86/toolchain-specific, unlike the generic splice proof.

## Proof boundary

The link primitives and their indexed list/ledger composition are proved, but
the physical free-list representation is not yet fully verified. The adapter
must map each admitted physical block head to a unique ID, preserve disjoint
metadata/link fields (including sentinel aliasing), and implement exactly these
operations under the allocator lock. Its valid initial physical representation
also needs establishing. Correctness of the address overlays, boot admission,
all-order split/coalesce/XOR composition, and SMP serialization remain open.
The whole-list host oracle is regression evidence, not a substitute for those proofs.

This refinement round changes no production code. No representation change,
runtime scan or extra metadata allocation was made here. The
free-set-tree experiment remains outside the kernel.
