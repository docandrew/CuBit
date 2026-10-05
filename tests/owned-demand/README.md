# Owned demand-page admission

`Owned_Demand_Policy` is a pure policy for an opt-in demand-backed owned
reservation. It is not yet called by the kernel fault handler and does not
reduce physical memory consumption by itself.

Run in the pinned Nix environment, with separate output for concurrent work:

```sh
nix develop -c bash -c 'out=$(mktemp -d /tmp/penny-demand-test-XXXXXXXX); cd kernel && alr exec -- gprbuild -p -P ../tests/owned-demand/demand.gpr -XDEMAND_OBJECT_DIR="$out" && "$out/demand_tests" && alr exec -- gnatprove -P ../tests/owned-demand/demand.gpr -XDEMAND_OBJECT_DIR="$out" -u owned_demand_policy.adb --level=2 --report=all -j2'
```

2026-10-02: 49,216 hosted cases passed. SPARK proved six checks, zero
unproved/justified checks. Output: `/tmp/penny-demand-test-I5wau1H5`.
The cross product covers address boundaries, every access/backing state,
read/write/execute/protection faults and quota/tracking exhaustion. Additional
cases cover malformed ranges, the canonical user boundary and Natural'Last.
These tests do not prove page-table concurrency or native isolation.

## Integration requirements

- Authenticate process generation and a live demand reservation while holding
  the owned registry and address-space locks, in that order. An address alone
  is insufficient authority. Existing eager/GPU/grant backing stays distinct.
- Pass access information from `Interrupts` into both `Process.pageFault` and
  `kernelUserFault`. Their current signatures lose read/write information.
  Keep instruction faults denied and protection faults outside demand repair.
- Store access permission independently of PTE presence: a guard page is not
  an absent readable/writable page. Resident retry requires a matching actual
  PTE and permission under the lock; the policy trusts caller-supplied state.
- Track sparse resident frames without changing the existing dense inventory
  assumptions in `Process.Owned_Memory.Inventory_Valid`, `Protect`, `Retire`
  and `Physical_Conflict`. Current First_Node/Last_Node ranges assume every
  virtual page has backing in reverse address order.
- Preserve quota accounting, zero initialization, rollback on allocation or
  mapping failure, and retained ownership on retirement failure. Unmap and
  synchronize TLBs before releasing physical frames. Pins from grants/GPU
  access remain authoritative and cannot be bypassed by discard advice.
- Validate native first-touch reads/writes, guard and read-only failures,
  simultaneous faults, partial residency/protection, process exit, full
  release and allocation failure before opting libc thread stacks into it.
  Measure physical use with the existing navigation/resize/close workload;
  reservation size is not stack high-water usage or a predicted saving.

## Sparse metadata

`pages.gpr` tests the actual packed `Owned_Demand_Pages` state. The 1544-byte
record holds permissions and residency for up to 4096 pages independently,
plus counts. It is intended to occupy one charged metadata frame per live
reservation. No allocation or syscall currently uses it.

2026-10-02: 233,504 checks passed, including forward/reverse/permuted commits,
all insertion ranks, empty/full bounds, permission changes with mixed resident
and absent pages, neighboring permissions, and reuse with a smaller range.
SPARK proved 31 checks with zero unproved or justified checks, including exact
permission updates and preservation of every residency bit. Native kernel
compilation passed in isolated objects. Evidence and source hashes are in
`.build-workspaces/penny-demand-nq9qtuvx/demand-metadata-evidence.json`.

Run `pages.gpr` as above in an isolated snapshot, using its
`build-pages/pages_tests` executable. The separate native compile uses
`gprbuild -P cubit.gpr --subdirs=demand-metadata-compile -u owned_demand_pages.adb`.
The unit uses Ada 2012 syntax to match the kernel.

The frame-list insertion prerequisite, `LinkedLists.moveFrontBefore`, is
covered by `tests/allocation-failures`: all 136 insertion positions in lists
of 1–16 nodes, foreign/null rejection, unchanged allocation counts, exact
node identity, and bidirectional structure. The existing 816 detach cases
and 20,000 model operations also passed, as did a private full kernel link.
Native fault integration and physical-memory measurements remain pending.

## Discard metadata (2026-10-03)

`Owned_Demand_Pages.Discard` now removes one resident page while preserving
all permissions and neighboring residency. The caller must complete unmapping,
TLB synchronization and safe frame-list retirement before publishing removal.
It does not itself reclaim memory and has no live kernel caller yet.

Hosted tests passed 100,958,243 checks, including full forward, reverse and
permuted removal, independent neighbor/permission and insertion-rank checks,
and repeated recommit/discard. SPARK proved the added count, preservation and
range contracts with no unproved checks. Evidence was generated with the pinned
Nix toolchain in `/tmp/penny-discard-xwcwldry`; this does not verify native faults,
concurrent page-table changes, pinned-frame retirement or physical savings.

## Fault access integration (2026-10-03)

The interrupt dispatcher now terminates user instruction-fetch faults before
entering the data allocator and rejects reserved-bit faults. Both user and
kernel-on-user demand handlers receive the original write flag. If a sibling
installed a page while the handler waited, retry requires effective user
read/write permission at every paging level, rather than presence alone.

`nix develop -c python3 tests/owned-demand/test-fault-routing.py "$PWD"`
executes the actual dispatcher body with modeled process calls across 64
error/handled combinations. Three negative controls reintroduce instruction
fallthrough, lost write intent and reserved-bit fallthrough and are rejected.
The wrapper removes the library-level SPARK aspect solely to compile the
extracted body as a nested procedure; this test is not a dispatcher proof.
`tests/user-memory` also checks every combination of inherited writable bits
and rejection of missing/supervisor entries using the production walker.

Native kernel compile/link and Penny's KVM load/click/resize/close regression
passed with the candidate kernel (`penny-input-dnayeodg`, session 77514).
Source hashes and build logs: `.build-workspaces/penny-evidence-20261003/fault-access`.
This normal browser run does not exercise deliberate protection violations or
prove concurrent sparse demand faults. Sparse owned mappings and physical
discard remain unimplemented. The boot ISO was not republished by this test.
