# USB input interface discovery

`USB_Keyboards` decodes eight-byte boot keyboard reports into usage sets and
press/release changes. Hosted tests cover all 256 modifier combinations,
ordinary usages 4..223, duplicate/reordered keys, chords, rollover preserving
held state, malformed modifier placement, and explicit release-all. These are
now connected to a private native keyboard queue and desktop event bridge.
Run `build/keyboard_tests` after building; prove `usb_keyboards.adb` using the
Nix/gnatprove pattern below. Seven new initialization/postcondition obligations
pass (103 combined cached obligations). Transition semantics are regression
tested, not yet specified as full functional-correctness contracts.

Private keyboard integration: hub keyboard/mouse with PS/2 disabled passes
(`cubit-usb-live.2eh2926n`), including Shift/Ctrl and navigation transitions.
This is native delivery regression evidence; the adapter/rings are not proved.
See the reference-hardware note for remaining key coverage/recovery limitations.

Private native EP0 negotiation now validates the initial eight device-descriptor
bytes and speed-specific packet size before fetching the full descriptor.
Hub input with PS/2 disabled passes (`cubit-usb-live.y1_mgk1b`), as does direct
USB-flash/app boot (`cubit-usb-live.4pwmns8o`). Those QEMU fixtures do not change
the initial EP0 size, so the Evaluate Context resize branch is not covered by
these native tests and needs a suitable fixture or hardware validation.

`USB_Hubs` validates USB2 hub descriptors (including both port bitmaps),
decodes power switching/settling and TT think time, and classifies port status.
`hub_tests` exercises all 255 nonzero port counts and all 65,536 status words.
Run `tests/usb-input-discovery/build/hub_tests` after the build below; prove
the unit with the same command below substituting `usb_hubs.adb`.
The combined cached proof report now contains 96 discharged obligations,
none justified or unproved. This is bounded parser/status-model evidence,
not a proof of the native controller, DMA, or USB protocol implementation.

The private native implementation configures a hub, powers its ports, waits for
power settling, reads status and performs bounded port reset before acknowledging
reset completion. It now addresses children sequentially and reuses the native
mouse report path; keyboard report delivery remains unimplemented.
Its QEMU fixture uses four ports with individual power switching, matching the
NUC's port count, but not the VIA hub's high-speed transaction translator.
QEMU's default eight-port hub returns a ten-byte descriptor where this decoder
requires eleven (both bitmaps include bit zero); a regression rejects that
short descriptor rather than relaxing validation to suit the emulator.

Native private results: four-port power/reset probe and desktop boot PASS
(`cubit-usb-live.k4yiv225`); direct USB-flash/app regression PASS
(`cubit-usb-live.znu26hiu`). Neither proves physical VIA translator behavior.

Linux-hosted tests of the same SPARK descriptor decoder used by native xHCI.
No hardware access and no claim of working hub/keyboard delivery yet.

`XHCI_Topology` now models private, bounded routes and USB2 TT identity.
`topology_tests` covers NUC-style sibling ports 1/4, nested full-speed hubs,
all 255 downstream port values, the five-nibble route limit and unsupported
speed relationships. Ports above 14 use route nibble 15 while the translator
retains the actual downstream port number. Slot lifetime/ownership, multi-TT
mode, hub context setup and reset/change handling remain adapter obligations.

Reference: [Intel xHCI 1.2b](https://cdrdv2-public.intel.com/625472/625472_xHCI_Rev1_2b.pdf),
sections 4.3 and 6.2.2 (route string and slot context).

```sh
nix develop -c bash -c 'cd kernel && alr exec -- gprbuild -p -P ../tests/usb-input-discovery/discovery.gpr && ../tests/usb-input-discovery/build/main && ../tests/usb-input-discovery/build/configuration_tests'
nix develop -c bash -c 'cd kernel && alr exec -- gnatprove -P ../tests/usb-input-discovery/discovery.gpr -u usb_configurations.adb --level=1 --report=all --checks-as-errors=on -j2'
nix develop -c bash -c 'tests/usb-input-discovery/build/topology_tests && cd kernel && alr exec -- gnatprove -P ../tests/usb-input-discovery/discovery.gpr -u xhci_topology.adb --level=1 --report=all --checks-as-errors=on -j2'
```

Synthetic fixtures exercise keyboard discovery after an unrelated HID interface,
USB2 hub protocols 0/1/2, exclusion of SuperSpeed protocol 3, truncated frames,
too-small keyboard packets, zero interrupt intervals and endpoint aliasing.
The existing mouse/storage/composite fixtures run from their original source
using this isolated output directory. No physical device report descriptor has
been captured yet.

2026-09-27: all 59 GNATprove obligations discharged, none justified/unproved.
This proves decoder runtime checks and loop invariants, not USB protocol
completeness, transfer correctness or physical input latency. No Assume or
SPARK-Off sections added. Assertions in this project are hosted-test only.

Topology checks also pass at level 1 (13 additional discharged obligations).
The private native driver now constructs its root slot's route, root-port and
TT fields through this model; child paths use the same addressing routine.

Private native regression evidence: direct UEFI/4CPU USB-flash desktop/app
boot passes (`cubit-usb-live.xnxnlz6e`). The private harness's new
`--hub-discovery` fixture places mouse/keyboard at hub ports 1/4 and checks
recognized hub diagnostics plus desktop startup (`cubit-usb-live.zukc89zc`).
It deliberately exits before input tests. QEMU's emulated hub is not evidence
for the physical VIA high-speed hub's translator behavior.

New downstream integration supersedes the discovery-only fixture: mouse motion
and buttons reach Desktop with PS/2 disabled (`cubit-usb-live.to5dcdxn`), and
direct USB-flash/app regression still passes (`cubit-usb-live._g1auh7i`).
Keyboard discovery is checked, but no keyboard input is claimed. Commands in
the private workspace are `run-live.py --uefi --cpus 4 --usb-flash
--hub-discovery --without-ps2` and `run-live.py --uefi --cpus 4 --usb-flash`.
