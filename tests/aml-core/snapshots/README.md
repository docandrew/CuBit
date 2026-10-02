# Sealed boot snapshots

`Firmware_Tables.Snapshots` owns a single boot-lifetime immutable inventory.
Its table capacity, total byte capacity and per-table quota are runtime
discriminants fixed before construction. Append validates copied bytes,
DSDT-first ordering and these explicit quotas. FACS is excluded. The kernel adapter measures the sealed catalog and selects exact count, byte
and largest-table capacities. The typed object must fit the maximum buddy block
(currently 32 MiB including metadata); catalog count is limited to 256.
Userspace startup allocation and grants remain unimplemented.
No count or table bytes are published before Seal has all expected tables.
Failure is permanent, including incomplete sealing or exhausted capacity.

The state is limited and has no reset. Ghost Contents captures the complete
state for frame contracts: Begin, Append, Seal and Reject cannot change a ready
snapshot. Copy reads only the owned bytes, checks the table index and buffer
capacity, and proves exact table bytes followed by zero padding. Failed copies
zero their complete output, including when the snapshot is unavailable.

Kernel `ACPI.setup` captures after inventory sealing. Raw reads are private to
capture; public `ACPI.Copy_Table` now reads the retained snapshot. The cache has boot lifetime and is allocated from a checked buddy reservation.
No scratch buffer is needed: Append validates directly from retained firmware
backing into owned storage. Allocation failure publishes nothing. Repeated discovery after snapshot
construction is rejected. Existing firmware backing remains reserved; this does
not authorize reclaiming it. The source overlays and boot adapter are trusted
native code, not part of this SPARK proof.

The packed store reserves Byte_Capacity bytes plus Table_Capacity descriptors,
with no fixed-size payload slot per table. Kernel capture reserves the smallest available buddy order that fits the exact
payload plus metadata, and zeros the entire block before typed construction.
It is not page-isolated or grantable user storage. Future transport must copy
into disjoint owned process pages and use the existing grant lifetime path;
never give a user process a mapping of this kernel object. Process.IPC.createGrant
requires process-owned pinned frames; this change does not bypass that check.

```sh
nix develop -c bash -c 'set -e; ulimit -S -s 65536; cd kernel; alr exec -- gprbuild -p -P ../tests/aml-core/snapshots/snapshots.gpr; ../tests/aml-core/build/snapshots/snapshot_tests; alr exec -- gnatprove -P ../tests/aml-core/snapshots/snapshots.gpr -u firmware_tables-snapshots.adb --mode=all --level=2 --prover=cvc5,z3 --checks-as-errors=on -j2'
```

Included in `tests/aml-core/run.sh` and its `--prove` path. 2026-10-01:
2,508 hosted checks; 91 SPARK analysis results (83 proved checks and eight
flow/termination results), zero unproved/justified checks. Tests include altered
source bytes after capture, publication freeze, invalid checksums/ranges/order,
count/size/total exhaustion, failure permanence, zero padding and unusual output
bounds. No live boot, grant transfer or physical-firmware correctness claim.

Logs: `/tmp/cubit-acpi-snapshot-freeze.log` and
`/tmp/cubit-acpi-snapshot-final.log`. The earlier first compile rejected a reserved
identifier; the first proof found an initialization invariant, fixed by explicitly
initializing counters when beginning the one permitted construction.

## Native boot regression

`native.py --kernel /absolute/path/to/cubit_kernel` creates private GRUB ISOs
and QEMU guests with no disk image or network. The default accelerator is TCG;
`--accel kvm` is optional. It stops the unmodified kernel through GDB immediately
after `ACPI.setup` returns, before userspace launch. The normal case verifies:

- ready phase and complete advertised count;
- every retained SDT's signature, length, revision, checksum and DSDT-first order;
- exact equality with the original QEMU firmware bytes, at a different address;
- contiguous packed payload and zero unused payload capacity;
- the aggregate payload bound and successful essential ACPI setup.

`--fault table-limit` changes the Begin_Snapshot count to 33 in the guest at the
function breakpoint. `--fault late-table` changes the second Append descriptor's
extent to 65,537 after one table was successfully captured. These are debugger
fault injections into private guest state, not production hooks or mutations of
host firmware. Both must return from essential ACPI setup successfully, retain
the Failed snapshot phase, and emit the handoff-disabled diagnostic. The late
case must retain exactly one private table. The reports distinguish captured
private data from publication; they do not invoke a userspace grant/read API.

Each invocation covers Multiboot 1 and Multiboot 2 unless `--protocol` selects one.
Reports include the tested kernel SHA-256. QEMU is always reaped on exit; shared
staging and boot images are untouched. Use an isolated kernel build or hold the
shared build lock when selecting a shared build artifact. GDB requires debug
symbols and the current kernel type layout.

```sh
nix develop -c python3 tests/aml-core/snapshots/native.py --kernel /absolute/path/to/cubit_kernel
nix develop -c python3 tests/aml-core/snapshots/native.py --kernel /absolute/path/to/cubit_kernel --fault table-limit
nix develop -c python3 tests/aml-core/snapshots/native.py --kernel /absolute/path/to/cubit_kernel --fault late-table
```

Historical fixed-slot baseline (before the runtime-capacity storage change):
the complete kernel was freshly compiled and linked from a kernel-only private
source snapshot at `/tmp/cubit-acpi-boot-czopc2v3`, including the normal kernel
stack gate. No shared binaries were seeded. The 345 input hashes matched the
working sources; only generated `kernel/src/build.ads` changed inside the build.
The initial native runs passed on both boot protocols: seven tables, 9,095 bytes.
Both injected failure modes also passed on both protocols. Logs:
`/tmp/cubit-acpi-boot-build.log`, `/tmp/cubit-acpi-boot-check.log`, and
`/tmp/cubit-acpi-boot-rejection.log`. The final hash-reporting script is rerun in
`/tmp/cubit-acpi-boot-final.log`.

This proves QEMU boot capture for those fixtures, not physical-laptop firmware,
service launch, IPC authentication, grant delivery, reclamation or complete AML.

Final run: all six cases passed with paused serial capture, protocol-path checks,
and kernel hash `769b4112917b4a74aceb5c80cae97a0d59aeca835678576512f93d0d6d67192f`.
Combined reports: `/tmp/cubit-acpi-boot-results.json`; per-case directories are
listed in `/tmp/cubit-acpi-boot-final.log`. At completion all substantive source
inputs still matched; only the main checkout's generated build metadata had
changed during peer builds. The repository fixture matches the tested script.

## Runtime-capacity snapshot verification

The packed implementation passes 2680 hosted snapshot checks and 75775 catalog
checks (session 33952, `/tmp/cubit-acpi-dynamic-snapshot-final.log`). One snapshot
contains 39 tables of unequal lengths, including 145770 bytes and 1048577 bytes,
with an allocation sized from their sum. Fixtures check exact-fit admission,
independent count/aggregate/per-table exhaustion, source mutation, complete
publication, non-one-based destinations ending at Positive'Last, and zero output
padding. These are synthetic table images, not a physical-laptop boot test.

All 95 snapshot proof checks passed, including exact copy/zero padding and
freeze contracts. The report has 207 results across snapshot/catalog/copy units,
zero unproved or justified results and no Assume statements; saved at
`/tmp/cubit-acpi-dynamic-snapshot-proof.out`. The copy uses a slice assignment:
the old per-byte loop invariant caused quadratic runtime work with assertions
enabled on megabyte tables. The first large-fixture run was deliberately stopped
and replaced by the successful final run after this fix.

Catalog lengths now use the byte-index representation limit, not a 1 MiB policy
limit. Physical-window page counts derive from that length bound, including an
unaligned start. This changes metadata capacity only; no mapping or hardware
authority is granted. Consumer session 58611 passed 61468 exposure, 1051624
service and 1257 bootstrap checks, plus all 63 exposure proof checks with no
unproved checks or assumptions. Report: `/tmp/cubit-acpi-wide-catalog-proof.out`.
The previous exposure run caught the stale 257-page subtype; the new derived
bound fixes it. Geometry fixtures cover the representational maximum at all
4096 page offsets. The range-coverage fixture uses an aligned 2 MiB table;
Positive'Last is not page-aligned and would force gigabytes of runtime ghost
enumeration in this assertion-enabled test.

The rebuilt production kernel passes normal capture and both injected rejection
cases under both Multiboot protocols (six cases, session 43286 exited 0). Normal
capture retains seven tables, 9095 bytes, with exact source equality, packed
offsets and zero unused payload. This validates the new layout with the existing
default boot budget; it does not prove dynamic kernel allocation or grant
delivery. Kernel SHA-256:
`5eb9087dabb573f016dd3714a56416378950de8d862820a48c10c3f37aa563a6`.
Private build: `/tmp/cubit-acpi-packed-boot-6llq44n8`; 345 recorded inputs match
the workspace, and only generated build.ads differs within the build. Reports
and paths are in `/tmp/cubit-acpi-packed-boot-validation.log`. The packed-layout
GDB fixture was promoted under the shared build/test lock and byte-compared
with the tested private fixture.

## Discovered-size kernel capture (2026-10-02)

Private kernel `/tmp/cubit-acpi-sized-boot-4gedz_zg/kernel/cubit_kernel` was
rebuilt from copied sources. The compiled input manifest is retained alongside
it. Normal capture, 40 tables totaling 1,329,227 bytes, count rejection,
late-table rejection and allocation failure all pass under Multiboot 1 and 2.
Every successful table is compared byte-for-byte with retained firmware;
capacities match actual count/total/maximum, and unused allocator tail is zero.
Failure tests publish no prefix; essential setup returns successfully without
kernel panic. Allocation failure is injected at the allocator ABI return before
any block is consumed. This is not a proof of the native allocator/overlay code.

Evidence: `/tmp/cubit-acpi-sized-boot-test2.log` (normal cases),
`/tmp/cubit-acpi-sized-boot-test3.log` (count/late rejection and earlier 24-table
case), `/tmp/cubit-acpi-sized-final-test.log` (allocation rejection), and
`/tmp/cubit-acpi-sized-large40-test.log` (final 40-table case). Some combined
commands failed later on QEMU fixture limits, after their stated passing cases.
QEMU limits each `-acpitable` to 65,535 bytes and also bounds its aggregate ACPI
ROM blob; the final fixture uses 33 added 40,000-byte tables. Single tables over
1 MiB remain covered by hosted tests, not this native fixture.

Run `native.py --kernel PATH --large-inventory` for the 40-table case, and
`--fault allocation`, `--fault table-limit`, or `--fault late-table` for rejection.
Native `acpi.setup` reports a 432-byte dynamic-bounded frame; that is not a
whole-call-chain stack proof. Source backing remains reserved, and the kernel
snapshot is not grantable process memory. Service startup/grant integration and
AML/ASLTS coverage remain separate work.
