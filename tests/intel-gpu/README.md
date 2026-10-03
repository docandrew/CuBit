# Intel probe foundation (Linux-hosted)

## Asynchronous table retirement dispatcher

The asynchronous `Table_Provenance.Retirement.Dispatcher` component is covered by
`table_dispatcher_tests.adb` (explicit main through `vm_growth.gpr`). It drives
the production ledger over 150 records/three tickets, delayed exact acknowledgments,
and submit/poll/ownership failures at each ticket. Each step invokes at most one
transport callback or one bounded ledger sweep. A wrong ledger is rejected even
with the same session/generation; acknowledged groups clear while unacknowledged
references remain. Failure cannot replay or reopen. Backend polling must provide
its own deadline and classify uncertain expiry as failure. This is hosted
regression evidence, not SPARK proof or native dispatcher integration.

## Growable CPU table mirrors

```sh
nix develop -c bash -c '
  mirror_test_dir=$(mktemp -d /tmp/cubit-vm-mirrors.XXXXXX)
  cd kernel
  alr exec -- gprbuild -p -P ../tests/intel-gpu/vm_growth.gpr \
    -XVM_GROWTH_OBJECT_DIR="$mirror_test_dir" \
    vm_table_store_tests.adb vm_metadata_growth_tests.adb
  "$mirror_test_dir/vm_table_store_tests"
  "$mirror_test_dir/vm_metadata_growth_tests"
'
```

`Intel_GPU_VM_Image` now stores table words through `VM_Table_Store`, with a
configurable bootstrap and a separate maximum table quota. `Extend_Metadata`
accepts a trusted, already committed, stable CPU reservation in increments up to
64 KiB. It changes neither GPU mappings nor image revision. Old table indices
and contents remain stable; failed capacity checks do not partially map a range.
The store test grows four to 132 mirrors, verifies untouched uncommitted suffixes,
quota/geometry rejection, and independent copying. The actual image test grows
four to 100 mirrors, uses 99 tables for 96 sparse mappings, and exercises cloning,
adoption, retirement and reuse without cross-image aliasing.

These are CPU metadata tests, not GPU backing allocation or hardware validation.
The caller must authenticate/disjointly reserve the metadata memory and retain
it for the image lifetime. This is an unproved trusted-memory boundary. Native
instantiation now starts with four CPU mirrors. Initial-context and replacement
readiness gates drive asynchronous extension to the existing 64-table quota,
committing at most 64 KiB per allocator turn (last increment 48 KiB).
`context_mirror_growth_tests`, `update_storage_tests`, and
`update_mirror_failure_tests` exercise the allocator compositions, including
commit/ownership failures without replay. These changes compile natively but
are not yet in the v47 image or hardware-tested. GPU physical table backing and
fixed-size DMA/level/receipt arrays remain separate integration work; the physical
64-table reservation has not been reduced or made dynamically growable.

## Growth provenance callback revocation

```sh
nix develop -c bash -c '
  growth_test_dir=$(mktemp -d /tmp/cubit-growth-ownership.XXXXXX)
  cd kernel
  alr exec -- gprbuild -p -P ../tests/intel-gpu/vm_growth.gpr \
    -XVM_GROWTH_OBJECT_DIR="$growth_test_dir" vm_growth_ownership_tests.adb
  "$growth_test_dir/vm_growth_ownership_tests"
'
```

The production incremental directory writer is exercised over host RAM. The
fixture first measures a successful transaction, then revokes exclusion inside
each provenance callback while returning a positive lookup result (9,286
boundaries in the stepped writer). It also revokes ownership between each step
and attempts premature commit at each step (6,174 additional rejection cases).
No read/write/flush may occur after revocation; failed publication cannot be
replayed or adopted even after authority is restored. This exposed a missing
post-callback exclusion check in the original synchronous writer (negative control: "write
after ownership revocation"). It does not prove hardware ordering, native
incremental-growth integration, or general thread safety; callers must still
serialize the transaction and retain all uncertain backing.

The writer now exposes `Start`, `Step`, and `Pending`, replacing the synchronous
publication API. `Start` preflights without memory IO; `Step` performs at most
one read/write/flush callback, checked by the tests. The receipt stores its plan
and cursor across service-loop turns. Source epoch/root and ownership are checked
again on each turn; failure consumes the attempt. Planning and final metadata
adoption still walk the configured capacity; this is bounded hardware IO, not a
claim of constant-time planning or native dispatcher integration.

## Whole-context allocation identity

The application-image integration fixture is also required after changes to
the retirement child: build `submission_buffer.gpr` and run
`build-submission-buffer/submission_buffer_tests` from this directory under Nix.
Its 29 publication/retirement paths assert exact address release and reject
stale image operations after a new claim reuses the same VA. It executes real
Application_Image code over host RAM/mock PTEs, not GPU hardware.

Under Nix, build `gprbuild -p -P tests/intel-gpu/context_tickets.gpr` and run
`tests/intel-gpu/build/context-tickets/context_tickets_tests`. It covers 128
context-parent owner/generation transitions, sixteen combinations of pending
allocation/quarantine/device loss/missing retirement evidence, revoked-session
cleanup, pinned/table-kind separation and stale/duplicate acknowledgment.
This is metadata-only: supervisor release, hardware reference retirement and
reusable native session admission are not established by this fixture.

## Retirement dispatcher ordering

`nix develop -c python3 tests/intel-gpu/test-retirement-dispatch.py` compiles
the actual driver request-poll guard and checks 48 input combinations. While a
supervisor retirement is pending, no client request may be consumed or stale
`Found` flag dispatched. Both metadata gates are covered too. A negative
control removes the pending-retirement guard and must fail. This is hosted
control-flow coverage, not proof of hardware completion or kernel revocation.

## GGTT address reclamation transaction

Run under Nix:

```sh
gprbuild -p -P tests/intel-gpu/ggtt_reclamation.gpr
tests/intel-gpu/build/ggtt-reclamation/ggtt_reclamation_tests
tests/intel-gpu/build/ggtt-reclamation/ggtt_retire_tests
gprbuild -p -P tests/intel-gpu/ggtt_reuse.gpr
tests/intel-gpu/build/ggtt-reuse/ggtt_reuse_tests
```

The new reservations child has no bookkeeping-only release entry point. Its
one-shot transaction scratch-remaps the exact claim, verifies PTE readback,
waits for invalidation, and checks ownership before removing that claim.
The 4,512 hosted cases cover all ledger sizes/removal positions, all ten I/O
failure points and twenty ownership gates at every full-ledger position,
malformed inputs, preservation of other claims/PTEs, and same-address replay.
The original lower-level retirement primitive still retains its claims.

These are mock-PTE regression tests, not hardware validation. The private
ledger transformation called after successful retirement is now separately
SPARK-proved: exact swap removal, preserved other extents/aperture, count
decrement and the complete ledger invariant. The ledger proof reports 65
analysis results with none unproved or justified; `Forget_Detached` has seven
proved checks. Evidence snapshot: `build/reclaim-proof.DDG5pI/gnatprove.out`.
The callback-driven transaction itself remains outside SPARK; this proof
does not establish hardware quiescence or that cleanup authority is valid.
The child is wired into native image retirement and compiles/links natively,
but has not been hardware-tested. The additional reuse fixture runs 1,024
cycles through the actual publisher/reclaimer over mock PTEs, with different
backing, neighboring claims, nonzero scratch entries and stale attempts.
Physical backing,
CPU grants, supervisor tickets and session identity are not released by it.
The exclusive serialized owner and truthful hardware callbacks remain trusted
requirements; tests do not establish those facts on a running machine.

## Native allocation and mapping checks

These separate fixtures boot CuBit under QEMU; they do not emulate Intel GPU
execution. With a current built `kernel/cubit_kernel`, run:

```sh
flock --exclusive coordination/build.lock nix develop -c bash tests/intel-gpu/native/run-demand.sh mappings
```

The `mappings` fixture uses production sharing, metadata reservation/commit and
record-growth code with real kernel self-grants. It fills the initial 64 records
while retiring temporary grants, keeps a reader alive during forced growth to
128 records, uses record 65, then grows to 256 with readers in both storage
tiers. Closing the BO must retain both grants until their readers return; stale
grant access and mapping the closed name must fail. Only two grants are live at
once: this tests stable metadata growth, not removal of the kernel's current
16-grant owner limit or automatic growth under many simultaneous mappings.
The privileged loopback fixture also does not establish cross-process isolation
or GPU retirement. Unique evidence directories contain the serial log and
kernel/fixture binary hashes. Existing `memory`, `ipc` and `views` modes cover
backing allocation, real allocation transport and forwarded-view retention.

## Hosted register and policy checks

Native pipe and primary-plane adapters (2026-09-28): build `native_pipe.gpr`
and `native_plane.gpr`, then run `build-native-pipe/native_pipe_tests` and
`build-native-plane/native_plane_tests` in Nix. These compile the actual native
adapters. The pipe fixture exercises rejected calls without host MMIO and
substitutes mapping readiness and the clock boundary; the plane fixture supplies
a retained-power callback plus anonymous host pages for all twenty plane
register sets. `native_cursor.gpr` similarly exercises all four cursor adapters.
Neither establishes successful physical acquisition.

Current native source requests display pages 0x45000/0x46000/0x44000 in slots
24/25/28, avoiding GGTT slot26 and log observer slot27. After successful reset,
A/B power acquisition uses retained PW1/PW2 references and initial-boot PCI IRQ
disable evidence. C/D additionally require the completed native DC transition.
The collector logs two samples of five planes and one cursor on each held pipe.
`scanout_inventory.gpr` combines all24 observations; missing/changing/unsupported
observations reject the inventory. Its non-overlap contract is SPARK-proved and
tested against65536 interval cases. This is exclusion evidence, not authority
to reclaim or publish GPU addresses. Native four-pipe validation is pending.

ADS storage layout: build `tests/intel-gpu/ads_layout.gpr` in Nix, then run
`tests/intel-gpu/build-ads-layout/ads_layout_tests`. Covers1089 section-size
combinations, exact/one-byte-short backing, unrepresentable sizes and rejection
of1MiB backing for the selected firmware's private area. No native ADS data
initialization or GPU publication is exercised.

Forcewake fallback: `nix develop -c bash -c 'gprbuild -P
tests/intel-gpu/forcewake_fallback.gpr &&
tests/intel-gpu/build-forcewake-fallback/forcewake_fallback_tests'`.
Twenty injected clear/set cases cover missing original ACK, stuck fallback
set/clear, invalid MMIO/time, stalled/regressing clock, late read and original
ACK lost during cleanup. A composed lease test recovers acquire and release.
These are regression tests, not hardware validation or SPARK proof.
Three lease-hook guards additionally reject recovery on bad MMIO/regressing
clock and verify that a failed recovery leaves the lease quarantined.

PCI IRQ snapshot regression: `nix develop -c bash -c 'gprbuild -P
tests/intel-gpu/probe.gpr pci_interrupt_tests.adb &&
tests/intel-gpu/build/pci_interrupt_tests'` (join command lines).
Tests sweep all 256 flag bytes and 256 capability pointers, recognized
record bounds for all MSI formats, duplicate/cyclic/overlapping chains, and
all-ones input. GNATprove level 2 on `intel_gpu_pci_interrupts.adb` proves
runtime checks, dependencies and termination, not PCI hardware quiescence
or full functional decoding correctness. Bootstrap v4 carries this observation
to the native Intel logstore publisher. Tests exhaust all 256 encodings,
round-trip valid ones, reject contradictory/reserved bits and reject old v3.
Both native services compile; no physical-hardware IRQ handoff claim is made.

Run from the repository root in the pinned Nix environment:

```sh
nix develop -c bash -c 'cd kernel && alr exec -- gprbuild -p -P ../tests/intel-gpu/probe.gpr && ../tests/intel-gpu/build/main'
nix develop -c bash -c 'cd kernel && alr exec -- gnatprove -P ../tests/intel-gpu/probe.gpr -u intel_gpu_probe.adb --level=1 --report=all --checks-as-errors=on -j2'
```

The test exhausts all 65,536 device IDs for Intel display, wrong vendor, and
wrong class, plus bounded mapping sizes/offsets and 64-bit boundary cases.
Assertions are enabled in this hosted fixture, not in a kernel/native build.

The package is not yet wired to native device discovery. Its checked address
construction must only be used with an authorized, live mapping and separately
validated register semantics. See [bring-up plan](../../docs/intel-gpu-bringup.md).

The BAR fixtures cover 32/64-bit memory, all flag combinations, I/O rejection,
unsupported encodings, zero/unassigned addresses, missing high words, and
addresses above 4 GiB. BAR size is deliberately not inferred.

2026-09-27 evidence: hosted regression passed; GNATprove level 1 reported
15 obligations discharged (9 flow/initialization/termination, 6 prover), none unproved or
justified, no warnings and no `pragma Assume`. This is only the pure helper,
not a verified GPU driver. Report: `build/gnatprove/gnatprove.out`.

Resource handoff: `resource_tests.adb` exercises `Intel_GPU_Resources`, including
4,896 page-range combinations, unknown extents/platforms, decode-disabled
devices, cache-class rejection, unaligned requests and top-of-address-space
boundaries. No memory mapping or hardware access occurs.

Proof command for this unit:
```sh
nix develop -c bash -c 'cd kernel && alr exec -- gnatprove -P ../tests/intel-gpu/probe.gpr -u intel_gpu_resources.adb --level=2 --report=all --checks-as-errors=on -j2'
```
All reported checks passed, including the containment/rejection postcondition
and three intermediate arithmetic assertions. No Assume or SPARK-Off escape.
This proves containment relative to the supplied resource extent, not that a
caller obtained that extent from trustworthy PCI resource discovery.

The ADLN-specific entry point also checks hardwired BAR encoding/address width
and tests every page in the 16 MiB aperture: only the initial 512 register pages
are admissible. Its functional postcondition passes the same level-2 proof.

`boot_tests.adb` checks the four-word bootstrap format, reserved bits, header
type, version and identity rejection. The decoder's conversions and termination
pass GNATprove level 1. Sender authentication occurs in the native adapter,
not in this pure decoder. Native `main.adb` accepts only the registered devmgr,
maps the admitted region with read-only mode, and remains idle without touching
registers or claiming display ownership.

`check-native.py SERIAL_LOG` checks the private RAM-backed fixture exercising
this real service and the subsequent desktop boot. The fixture is labelled
NOT hardware. It does not validate GPU registers, power domains, scanout,
command submission, or hardware acceleration.

`observation_tests.adb` uses a recording reader to check the staged two-register
snapshot: exact addresses/order, no reads for unknown platforms or unconfirmed
D0, truncated/unaligned/zero/wrapping mappings, and preservation of raw all-ones
and zero values. It does not access MMIO. Native bootstrap still leaves this
capture disconnected pending trusted PCI power-state evidence.

`pci_power_tests.adb` exercises the pure type-0 PCI configuration decoder:
all first-pointer byte values, all PMCSR low-byte values, self-cycle, duplicate
PM records, overlapping PM/header data, truncated PM at 0xFC, and a maximum
48-header chain with/without a cycle. Missing PM is unavailable, not D0.
GNATprove level 2 proves bounds, arithmetic, dependencies and termination of
`intel_gpu_pci_power.adb`; these are not proofs of PCI hardware behavior or of
the returned snapshot remaining current during subsequent MMIO access.

`firmware_tests.adb` covers CSS layout parsing, truncated mandatory payload,
optional absent modulus/exponent data, empty code/key, inconsistent/wrapped
header counts and maximum DWORD code-size arithmetic. The layout contract
passes GNATprove level 2 with `-u intel_gpu_firmware.adb`. These are layout
proofs, not firmware authenticity, compatibility, upload or execution tests.

Real-file hosted fixture (also packaged by the private bring-up image):

```sh
nix eval --raw --file tests/intel-gpu/firmware-source.nix blob
# After building probe.gpr, pass the printed store path:
nix develop -c tests/intel-gpu/build/firmware_file /nix/store/PRINTED-tgl_guc_70.bin
```

The pinned linux-firmware 20250917 TGL GuC file parses as 335360 bytes,
334976 code bytes, signature offset 335104 and 256 signature bytes. This tests
the same Ada decoder as the synthetic cases. The fixture's LICENSE.i915 is
also hash-pinned. The private image packages both separately from the driver.
A successful layout parse
does not authenticate Intel signatures or establish firmware ABI compatibility.

`firmware_reader_tests` exercises the caller-buffer firmware reader with a
1 MiB policy budget and 4 KiB reads: size rejection without I/O, complete and
short reads, failed reads (including after progress), EOF, oversized replies,
malformed CSS, nonzero array origins, and the maximum Natural array index.
`firmware_file` now reads the entire pinned binary through this same generic,
not just its header. Run it and `build/firmware_reader_tests` after building
`probe.gpr`. These are Linux-hosted regressions, not a proof of the reader or
a native filesystem integration test. The future native adapter must bound
IPC waits and finish/revoke buffer loans before returning; the reader cannot
cancel an outstanding IPC request or authenticate a changing file.

`ggtt_tests` exercises the pure GGTT window planner: 4 KiB page rounding,
cross-table-page ranges, last-entry admission, zero/unaligned/oversized inputs,
and exact capacity plus one-byte overflow for every table size from 1..2048
pages. GNATprove level 2 on `intel_gpu_ggtt.adb` proves the returned mapping
stays inside the supplied table size. No actual page-table access occurs.

The pure-reader instance in `observation_proof.ads` makes generic capture code
available for proof, including address preconditions and capture/rejection
postconditions. It does not model physical reads, power stability or faults:

```sh
nix develop -c bash -c 'cd kernel && alr exec -- gnatprove -P ../tests/intel-gpu/probe.gpr -u observation_proof.ads --level=2 --report=all --checks-as-errors=on -j2'
```
# GGTT publication transaction

The display-claim suite also exercises the `PW1_Write_Allowed` predicate used
by the native parent-power adapter: all524288 aligned register offsets with
both permitted masks,192 single-bit mutations, unaligned offsets and invalid
all-ones readbacks. These are hosted tests of write selection, not proof of
fresh hardware reads, serialization, MMIO ordering or physical power behavior.

`build/ggtt_publish_tests` is Linux-hosted callback fault injection, not a
hardware or native CuBit test. It checks occupied entries (including nonzero
non-present entries), preflight read failures, failed preparation, every write
position (including failure after a store), readback failure/corruption, and
invalidation failure. Only the fully successful sequence reports Published.
Out-of-range, unaligned, partial-page and over-budget inputs produce no I/O.
The exact upper DMA boundary is covered. Device ownership, concurrent writer
exclusion and platform visibility/invalidation are external obligations.

The generic `Maximum_Bytes` defaults to 1 MiB for upload staging. ADS callers
can explicitly select 16 MiB; an absolute 16 MiB limit still bounds callbacks.
Tests opt in to a full 4096-page ADS mapping, verify every PTE, and inject
failure at the final preflight read, final write and final readback. Preflight
failure makes no writes; either later failure retains the claim and quarantines
the attempt without invalidation. This is hosted regression evidence, not
native ADS publication or a proof of MMIO ordering.

Publication now consumes a limited, noncopyable `Attempt`. Its state records
whether no writes occurred, writes may have occurred, or publication completed.
Every scenario repeats with the same and a different GPU start address: both
must reject without any callback and preserve the prior phase. There is no
reset/free operation; callers must keep the attempt associated with its retained
allocation. Publication also requires a shared `GGTT_Reservations.Ledger`;
table geometry comes from its one-shot admission, not a second caller argument.
Before any callback, the publisher reserves the exact GPU range. All acquired
claims remain retained, including failures before the first write. Tests create
a fresh attempt with different DMA backing after each reservation-bearing
scenario and verify rejection without callbacks. A default ledger and a range
outside an admitted aperture likewise cannot reach hardware callbacks.
The caller must retain and share the same ledger; constructing a replacement
ledger is not a supported way to bypass quarantine. Firmware/display exclusions
must be established before admission, never inferred from empty PTEs.
This is API misuse resistance, not kernel enforcement or a concurrency proof.

The ledger now has a Ghost `Valid` predicate covering nonempty claims,
containment in the admitted aperture and pairwise nonoverlap. `Admit` and
`Reserve` require and preserve it; their contracts also prove that admission
does not change the claim count and only `Reserved` increments it by one.
Ghost snapshots additionally prove every existing claim is preserved on all
outcomes, and a successful reservation appends exactly the requested extent.
The limited runtime ledger remains noncopyable; snapshots exist only for proof.
GNATprove level 2 discharges both functional contracts, loop invariants and
run-time checks (no unproved checks or `Assume` pragmas). The hosted 1296-pair
regression also passes with contracts enabled. Reproduce the proof with:

```sh
nix develop -c bash -c 'cd kernel && alr exec -- gnatprove -P ../tests/intel-gpu/ggtt_reservations.gpr -u intel_gpu_ggtt_reservations.adb --level=2 --report=all --checks-as-errors=on -j2'
```

This proves a serialized software ledger property, conditional on a valid
incoming ledger. It does not prove platform aperture admission, MMIO/DMA
visibility, native driver concurrency or successful firmware execution.

`Find_Free` proposes a page-sized, power-of-two-aligned range inside an already
admitted aperture, scanning at most 65 passes over 64 unsorted claims. It does
not reserve the proposal: the same serialized owner must subsequently call
`Reserve`/`Publish`, which may still reject descriptor exhaustion. No search
result confers authority over firmware memory or empty GGTT entries.

`Allocate` composes search and reservation in one serialized-owner operation.
It returns an address only with `Reserved`; otherwise the address is zero,
the count is unchanged, and existing claims are preserved. It supplies no
internal lock and does not publish PTEs. Tests cover repeated aligned claims,
descriptor exhaustion, invalid size, a full aperture and a valid zero address
(callers must check status, not use zero as a success sentinel). SPARK checks
the preservation, count and exact successful-claim contracts.

The `Space_Free` return contract proves a successful proposal is nonempty,
contained and disjoint from existing claims; runtime safety and termination
are also proved. This quantified contract currently needs level 3 with
`--timeout=60` using the command above (level 2 did not discharge it).
An independent eight-page occupancy oracle checks 9216 queries covering every
occupancy pattern, four alignments, nine sizes and reverse-order insertion.
Alignment and lowest-fit selection are regression-tested, not part of the
proved return contract. No claim is made about GPU performance from this test.

`GGTT_Publish.Publish_Available` combines that proposal with the existing
reservation-and-publication sequence under caller-held exclusive ownership.
It consumes unsuccessful searches without device callbacks. Hosted tests
exercise aligned selection past retained claims, successful publication and
ambiguous first-store failure, then clear the simulated PTEs and verify a new
attempt still skips the retained claim. Reusing an attempt, including after
no-space failure, performs no device callbacks. This composition is tested,
not SPARK-proved, and does not admit a native firmware aperture by itself.

## Multi-domain forcewake coordination

`build/domain_lease_tests` exercises 128 combinations of three synthetic domain
selections and acquisition/release failures. It checks reverse cleanup order,
continued cleanup after release failure, exact uncertain-domain tracking,
rejection of nested acquisition and repeated release, and non-reuse after
failure. Callback failures are reported as Boolean results, not exceptions.
These Linux-hosted regressions do not validate actual ADL-N domain selection,
register handshakes, reset ordering, or concurrency. No native binding is enabled.

`inventory.gpr` builds the separate ADL-N inventory test in `build-inventory`.
It exhausts all 4,096 media fuse-field combinations and all 65,536 device IDs,
checks invalid vendor/all-ones MMIO rejection, and verifies distinct aligned
domain register pairs. It is a pure hosted decoder test, not hardware discovery.

`forcewake.gpr` runs the existing handshake/deadline/failure-cleanup tests for
each of the five ADL-N register pairs. Request/ack offsets are fixed at generic
instantiation, not selected from untrusted input during a lease. The GT-specific
procedure names were replaced by Acquire/Release, without compatibility aliases.
These are mocked MMIO tests; only GT has been tested on the NUC so far.

`adln_forcewake.gpr` combines the real fuse decoder, five fixed handshake
instances and coordinator. All 288 combinations of media selection and
acquire/release timeout positions are tested, including failed-acquire cleanup,
continued release after failure, exact uncertainty, and invalid identity/fuses.
The MMIO model acknowledges request bits or injects timeouts; it is not a
simulation of Intel silicon or evidence that reset is safe.

`reset_prepare.gpr` tests normal/already-ready/catastrophic preparation,
poll and clock deadlines, invalid MMIO, clock regression, and cancellation
write encoding. No native reset adapter is linked. The model does not establish
engine stopping, cancellation acknowledgment, or hardware-workaround compliance.

`engine_stop.gpr` checks stop/prefetch encodings, all 1,024 combinations of
pending forcewake requests and enables, acknowledgment rejection, settling
with a stalled clock, invalid MMIO, idle timeout and zero-budget no-write.
These callbacks are a register model, not native hardware or SPARK proof.

`gt_reset.gpr` checks two successful full-reset acknowledgments followed by
settling, failure at either cycle, all-ones responses, stalled/regressing
clocks, delayed-read deadline expiry, and rejection of repeated attempts.
It does not issue hardware reset writes or prove safe display preservation.

`handoff.gpr` covers 6,912 combinations of media engine selection, forcewake,
stop, preparation, reset and cleanup outcomes. Stage callbacks assert ordering;
tests reject reset after failed stop/preparation, require all-engine cleanup
after preparation begins, and reject attempt reuse. These abstract callbacks
do not yet exercise the register-level helpers together or native hardware.

## Linear scanout footprint

`scanout_range.gpr` exercises `Intel_GPU_Scanout_Range.Linear`: a pure
calculation from already-decoded GGTT surface address, row pitch, dimensions,
pixel size and source offsets. It retains complete rows including padding and
leading offset rows, rounds outward to pages, and rejects invalid/overflowing
geometry or ranges beyond the table aperture. It is not a register decoder or
an ownership/admission decision. It must not be used for tiled, compressed or
multi-plane formats. The native caller still needs a stable inventory of all
enabled planes/cursors and both live and pending surfaces.

```sh
nix develop -c gprbuild -P tests/intel-gpu/scanout_range.gpr
nix develop -c tests/intel-gpu/build-scanout-range/scanout_range_tests
nix develop -c gnatprove -P tests/intel-gpu/scanout_range.gpr -u intel_gpu_scanout_range.adb --level=2 --report=all
```

The hosted oracle enumerates touched pixel addresses for 5168 layouts and
checks page containment, full-row retention and rounding tightness. Additional
cases cover the final page of a 4GiB aperture, overflow and malformed inputs.
These tests do not establish actual hardware fetch/prefetch behavior or the
correctness of a future register decoder. Evidence belongs under
`tests/mesa-software/target/scanout-range-2.log`. The level-2 proof establishes
runtime checks, termination and the accepted extent's nonempty, page-aligned,
aperture-contained postcondition. Pixel coverage and tight rounding are tested,
not included in that proven postcondition. The first proof attempt is retained
in `scanout-range.log`; it did not prove nonemptiness before explicit rejection
of zero/under-rounded spans was added.

## ADL-N linear plane decoder

`plane_decode.gpr` tests the strict initial RGB8888 plane-register decoder.
It covers a 1920x1080 baseline, all 32 control-bit mutations, all-ones values
and changes in each of six sampled registers, pending/live mismatch, offsets,
stride flags, address flags and aperture overflow. Every rejected result has
an invalid extent. No test drives native MMIO or establishes snapshot atomicity.

```sh
nix develop -c gprbuild -P tests/intel-gpu/plane_decode.gpr
nix develop -c tests/intel-gpu/build-plane-decode/plane_decode_tests
nix develop -c gnatprove -P tests/intel-gpu/plane_decode.gpr -u intel_gpu_plane_decode.adb --level=2 --report=all
```

The contract covers ready/valid agreement and, on success, identical samples,
matching live/programmed addresses, the supported control values, matching
surface origin and a nonempty page-sized extent within the aperture. Hardware
register semantics and the caller's ownership assumptions remain outside that
proof. Evidence: `tests/mesa-software/target/plane-decode-2.log`.

## Read-only plane collection

`plane_collect.gpr` tests the composed collection/decoder boundary with fake
power-reference and register-read callbacks. Collection holds Begin/End access
over exactly two ordered six-field samples. It stops at the first read failure
or all-ones value, calls End once after every successful Begin, and does not
decode partial data or data collected with failed cleanup. A reused output is
cleared before acquisition; failure cannot retain a previously valid extent.

```sh
nix develop -c gprbuild -P tests/intel-gpu/plane_collect.gpr
nix develop -c tests/intel-gpu/build-plane-collect/plane_collect_tests
```

Fault injection covers every read in both passes, callback failure vs all-ones,
successful vs failed End, changes in each second-sample field, unavailable
power and a successful decode. These are hosted regression tests, not a proof
of callback behavior, actual power references, MMIO access or snapshot
atomicity. There is no native binding yet. Evidence:
`tests/mesa-software/target/plane-collect-2.log`.

## Display-power owner claim

`display_claim.gpr` covers the one-shot state used by native devmgr request
0x022F. Tests enumerate designated/caller IDs and badge/device validity, then
reject every retry or replacement after an owner is consumed. GNATprove
checks the exact success condition and unchanged owner on denial. It does
not prove the broker authenticates badges, reads PCI correctly or holds a
hardware power reference. No writable MMIO accompanies this designation.

```sh
nix develop -c gprbuild -P tests/intel-gpu/display_claim.gpr
nix develop -c tests/intel-gpu/build-display-claim/display_claim_tests
nix develop -c gnatprove -P tests/intel-gpu/display_claim.gpr -u intel_gpu_display_claim.adb --level=2 --report=all
```

Passing evidence: `tests/mesa-software/target/display-claim-2.log`.
# Display reference lifecycle

`display_power.gpr` builds `build-display-power/display_power_tests` in Nix.
It composes the real topology, six request-well transactions and lease against
shared simulated MMIO, with 512 full-pipe fault combinations and 32 cross-pipe
reuse transitions. DC-off and IRQ/VGA callbacks remain models, not native code.

`dc_write.gpr` builds `build-dc-write/dc_write_tests` for the low-level
DC_STATE_EN write verifier. It tests seven-consecutive-read stability,
sentinels, write errors, independent budgets and periodic glitches. Run both
build and executable in Nix. Success is not a complete DC-off transition:
DMC/PHY/clock/DBUF integration is still required before native use.

The per-well MMIO enable transaction is tested with `display_enable.gpr` and
`build-display-enable/display_enable_tests` in Nix. Its 156 simulated cases
cover six wells, inherited requests, write/read/clock failures, exact fuse
selection, late acknowledgments and failure quarantine. Another 84 release
cases cover inherited retention, safe request removal, cleanup failures and
successful reuse. Native DC-off/ownership/IRQ/VGA integration remains missing.
The same executable also composes the real transaction and coordinator for
36 acquire/release cycles. This guards against skipping inherited *software*
reference cleanup while still requiring no inherited-release hardware access.

Golden-context reservations use `adln_golden.gpr` and
`build-adln-golden/adln_golden_tests`: eight media inventories, one image per
class, exact addresses/state sizes, short backing and upper-bound/alignment
rejection. GNATprove accepts `-P tests/intel-gpu/adln_golden.gpr
-u intel_gpu_adln_golden.adb --level=2` for runtime checks and the capacity
postcondition. There is no captured context or native GPU mapping in this test.

Complete ADS system info uses `ads_system_info.gpr` and
`build-ads-system-info/ads_system_info_tests`:2048 media/count combinations,
all640 bytes, count256, ignored reserved bits, changed/all-ones observations
and invalid topology. GNATprove accepts `-P tests/intel-gpu/ads_system_info.gpr
-u intel_gpu_ads_system_info.adb --level=2` for runtime/termination checks.
Native sampling occurs under inventory forcewake and admission after release;
no GPU publication or hardware validation is implied by hosted tests.

ADS register-section serialization uses `ads_register_image.gpr` and
`build-ads-register-image/ads_register_image_tests`. Eight media inventories
check packed records, all4096 descriptor bytes, physical VCS2 indexing, zero
tails, exact address ceiling fit, overflow/misalignment and failed admission.
GNATprove accepts `-P tests/intel-gpu/ads_register_image.gpr
-u intel_gpu_ads_register_image.adb --level=2` for runtime-check analysis.
This is host-side byte construction, not native GPU mapping or publication.

ADL-N engine settings use `adln_engine_settings.gpr` and
`build-adln-engine-settings/adln_engine_settings_tests`: all320 engine/MOCS
combinations, exact render settings, masked-write encoding, preserved unrelated
RMW bits and invalid/disabled inventory. GNATprove accepts
`-P tests/intel-gpu/adln_engine_settings.gpr
-u intel_gpu_adln_engine_settings.adb --level=2` for runtime/termination checks.
Platform applicability and masks are audited/tested, not a hardware correctness
proof. This is a pure plan: no MOCS selection, MMIO application or GPU execution.

The native render initialization consumes this plan, including twelve
FORCE_TO_NONPRIV entries assembled from the Intel register-field record.
Four explicit read-only counter DWORDs avoid relying on the range alignment
interpretation; three tuning registers are read/write and the remaining entries
use RING_NOPID. These are the twelve slots managed by i915, not a claim that
all hardware permission mechanisms have been sanitized. Application admission
remains closed. The ADS merge preserves their existing unsteered save entries.
`engine_configure_tests` injects read, write and readback failures at each of
the twelve entries and checks that initialization stops and cannot be retried.
These are hosted mock-MMIO regressions; the native driver compiles and links,
but this permission initialization has not yet been validated on the NUC.

ADL-N common register sets use `adln_regset.gpr` and
`build-adln-regset/adln_regset_tests`: all five engine bases,54 exact entries,
mask/steering flags, sorted offsets, disabled engines, unavailable steering and
insufficient MMIO extent. Run GNATprove with `-P tests/intel-gpu/adln_regset.gpr
-u intel_gpu_adln_regset.adb --level=2` for runtime checks and the successful
entry-count contract. The combined builder also checks315 engine/DSS plans,
63render/55other counts, sorted uniqueness, common-entry preservation and every
workaround flag. Exact upstream equivalence is regression evidence, not proof;
native state initialization/ADS publication remain outstanding.

ADL-N steering selection is exercised with `adln_steering.gpr` and
`build-adln-steering/adln_steering_tests`: all1024 DSS/L3 mask pairs, all256
slice masks, range edges, all-ones reads and ignored reserved bits. Run
GNATprove with `-P tests/intel-gpu/adln_steering.gpr
-u intel_gpu_adln_steering.adb --level=2` for runtime checks, termination and
the returned-index bound. Lowest-enabled selection and ABI/platform agreement
are regression-tested, not a hardware proof. Native fuse reads/MCR writes are
not enabled by this helper. The native driver separately reads the three fuse
registers twice under GT forcewake, then uses `Decode_Stable` after successful
release/identity admission. Tests also reject a change in each sample field
and matching all-ones samples; SPARK proves differing samples cannot be valid.

ADS register-list construction is exercised with `ads_regset.gpr` and
`build-ads-regset/ads_regset_tests`. It checks1024 flag/steering encodings,
full-capacity sorted insertion, exact duplicates, conflicting flags, register
bounds and atomic failure. Run GNATprove with `-P tests/intel-gpu/ads_regset.gpr
-u intel_gpu_ads_regset.adb --level=2` for runtime checks and the nonmutation
failure postcondition. Sorting/ABI equivalence are regression-tested; complete
engine register lists and hardware steering selection are not provided here.

ADS engine serialization is exercised with `ads_engines.gpr` and
`build-ads-engines/ads_engines_tests`. Eight media fuse combinations compare
all576 bytes against an independent expected mapping/mask construction;
the observed NUC fuse and invalid/missing-core inventories are also checked.
Run GNATprove with `-P tests/intel-gpu/ads_engines.gpr
-u intel_gpu_ads_engines.adb --level=2` for runtime checks and the admission
postcondition. Byte-level ABI agreement is regression-tested, not formally
proved equivalent to Linux. The generic system-info tail remains unimplemented.

ADS scheduling policy serialization is exercised with `ads_policies.gpr` and
`build-ads-policies/ads_policies_tests`. Both engine-reset modes check all24
little-endian DWORDs, including zero queue-depth/reserved fields and unchanged
bytes outside the reset flag. Run GNATprove with
`-P tests/intel-gpu/ads_policies.gpr -u intel_gpu_ads_policies.adb --level=2`
for the exact byte postcondition. This is a serializer, not a native ADS
publication, firmware compatibility proof or working recovery implementation.

The ADL-N topology is exercised with `display_topology.gpr` and
`build-display-topology/display_topology_tests`. This checks all 256 low-byte
selections against an independent dependency oracle and composes all four
pipe selections with the display lease callbacks. Run GNATprove with
`-P tests/intel-gpu/display_topology.gpr -u intel_gpu_display_topology.ads --level=2`
for the pure topology contracts; hardware correctness remains outside that proof.

Run `nix develop -c gprbuild -P tests/intel-gpu/display_lease.gpr`, then
`nix develop -c tests/intel-gpu/build-display-lease/display_lease_tests`.
The hosted fault matrix checks ancestor ordering, inherited-request retention,
failure quarantine and successful reuse. This is regression evidence only:
the native power-register backend and its platform prerequisites are not yet
implemented, and no hardware reference is created by running these tests.

Capture-list encoding: build `capture_list.gpr` and run
`build-capture-list/capture_list_tests` under Nix. Prove with
`gnatprove -P tests/intel-gpu/capture_list.gpr -u intel_gpu_capture_list.adb
--level=2 --report=all --checks-as-errors=on -j2`.
Tests check all 255 nonempty supported lengths, empty-list backing bytes,
every descriptor word and padding byte, steering combinations, capacity and
invalid offsets. This is a single-page ADL-N encoder, not platform register
selection, firmware publication or evidence of working hardware capture.

ADL-N platform capture pages: build `adln_capture.gpr`, run
`build-adln-capture/adln_capture_tests`; prove the `intel_gpu_adln_capture.adb`
unit with GNATprove level2/checks-as-errors. The504-case matrix covers all
eight engine inventories and63 nonempty DSS masks, enabled/absent class
selection, relative instance offsets and per-DSS steering. These are hosted
tests and runtime-safety proofs, not native GuC capture validation.

Capture assembly: `ads_capture_image.gpr` builds
`build-ads-capture-image/ads_capture_image_tests`. GNATprove target is
`intel_gpu_ads_capture_image.adb` (level2, checks-as-errors). Tests resolve all
66 pointers across8 inventories, compare each referenced page to its source,
check zero-page/tail content, and exercise capacity/alignment/ceiling rejection.
The allocation contract reserves32KiB even when fewer pages are populated.

Read-only upstream ABI audit (supply the downloaded pinned v6.16 header):
`nix develop -c python3 tests/intel-gpu/check-ads-abi.py /path/to/intel_guc_fwif.h`.
This checks every packed ADS field offset/size and system-info size, rejecting
unknown declarations. It neither modifies shared build outputs nor replaces
native firmware compatibility testing.

ADS composition and CPU materialization:

- `ads_header.gpr`: 256 complete packed-header byte patterns.
- `ads_initialization.gpr`: 504 inventory/topology combinations, section
  pointers, allocation boundaries, malformed inputs and disabled recovery.
- `ads_materialize.gpr`: complete 16 MiB comparison, nonzero array origin,
  zero padding/reserved storage, and unchanged destination on rejection.

Each executable is `build-ads-NAME/ads_NAME_tests` for NAME `header`,
`initialization`, or `materialize`. Use the Nix environment and a 64 MiB
host test stack (`ulimit -s 65536`) for the materialization fixture; production
receives existing backing and does not allocate that host-test array.
Prove `intel_gpu_ads_header.adb`, `intel_gpu_ads_initialization.adb`, and
`intel_gpu_ads_materialize.adb` through their respective projects with
`--level=2 --checks-as-errors=on`.

The writer checks copy bounds, clears supplied CPU backing, and copies the five
initialized sections. Its caller must supply an authentic preparation result
and exclusive writable memory. Neither these checks nor a successful write
establish GGTT ownership, GPU cache visibility, valid golden-context contents,
or permission to publish the ADS to firmware. Native driver binding remains
separate work.
# Native ADS hardware observation

`dma_cache.gpr` / `build-dma-cache/dma_cache_tests` exercise the shared x86
CLFLUSH wrapper on a mapped aligned host page and reject invalid extents.
This requires host CLFLUSH support and does not prove GPU visibility. Native
ADS initialization now materializes into retained DMA backing and uses that
wrapper, but remains uncalled until an owned GGTT extent is available. Numeric
range checks do not replace WOPCM pin-bias, firmware or scanout admission.

The ADS/publication integration regression uses the real ADS composer and
materializer with modeled PTE callbacks. It checks that a dynamically selected,
retained GPU extent (skipping an existing claim) supplies ADS pointers before
any PTE writes. This does not validate native cache visibility or GGTT MMIO.
The 16MiB hosted fixtures need a larger stack:

```
nix develop -c bash -c 'gprbuild -P tests/intel-gpu/ads_materialize.gpr && ulimit -s 65536 && tests/intel-gpu/build-ads-materialize/ads_publish_tests && tests/intel-gpu/build-ads-materialize/ads_materialize_tests'
```

`nix develop -c gprbuild -P tests/intel-gpu/ads_observe.gpr` builds the
`build-ads-observe/ads_observe_tests` hosted regression. It checks admission
without MMIO, exactly two ordered samples of topology and doorbell registers,
all 256 encoded doorbell capacities, invalid reads, sample changes and invalid
topology. Native reset captures the same observation only after completion,
with its forcewake reference retained. Captured values feed the future ADS
initialization path; this is not ADS publication or evidence of working GuC.
Tests verify callback behavior, not hardware power, MMIO ordering or atomicity.
