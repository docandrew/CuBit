# Intel probe foundation (Linux-hosted)

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

`build/ggtt_publish_tests` is Linux-hosted callback fault injection, not a
hardware or native CuBit test. It checks occupied entries (including nonzero
non-present entries), preflight read failures, failed preparation, every write
position (including failure after a store), readback failure/corruption, and
invalidation failure. Only the fully successful sequence reports Published.
Out-of-range, unaligned, partial-page and over-budget inputs produce no I/O.
The exact upper DMA boundary is covered. Device ownership, concurrent writer
exclusion and platform visibility/invalidation are external obligations.

Publication now consumes a limited, noncopyable `Attempt`. Its state records
whether no writes occurred, writes may have occurred, or publication completed.
Every scenario repeats with the same and a different GPU start address: both
must reject without any callback and preserve the prior phase. There is no
reset/free operation; callers must keep the attempt associated with its retained
allocation and must not create a replacement attempt to bypass quarantine.
This is API misuse resistance, not kernel enforcement or a concurrency proof.

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
