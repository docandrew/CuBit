# Pre-landing checks

Run these before landing on micro. Each one is a permanent regression test
for a bug that was fixed: it failed before the fix and passes after. Add
your fix's test here when you land it (one entry: what it guards, then the
command). Most of `tests/` is ad hoc; this list is not.

Run from the repository root. First export
`TMPDIR=/home/doc/cubit-build-tmp`, then use `nix develop`. Native builds
and headless runs hold the shared build lock
(`flock /home/doc/git/cubit/coordination/build.lock …`) and use
`--accel kvm`. Build first with `make -C kernel world`, plus any target an
entry names.

Some entries land from other branches. An entry whose files are not in
your tree yet is skipped until they are.

## startup-health (CAP-003)

The full desktop boots with no refusal:
- every service registers;
- Files', the Calculator's and Desktop's records reach logstore, so their
  publishers are bound;
- Desktop reads its configured `gpu.power.waitboost`;
- netmgr reads its declared `net.` scope.

It fails on any refused startup entry, Config key, log badge or launch
(`tests/headless/check-refusals.py`). See
docs/declaration-checking.md.

```sh
make -C kernel world boot-logs
bash tests/headless/run.sh --test startup-health --accel kvm --timeout 90
```

## logs (CAP-003)

The Logs app shows logstore's records, and nothing is refused on the way.
The profile used to start timesync without network approval, so procmgr
refused its required "ntp" entry; now any refusal fails the test
(`tests/headless/check-refusals.py`).

```sh
bash tests/headless/run.sh --test logs --accel kvm
```

## log-retention (LOG-002)

One chatty source cannot erase another's log history. log-flood writes
4,000 records after log-retention-check (the quiet source) published 10
boot records; then a subscription filtered to the quiet source (what
`(logs.recent "acpi")` does) still replays all 10 in order, and a
full-history subscription replays the retained window in publish order
with no gap, then each source's tally (LOG-005): the flood's replayed
records are exactly its retained ones, its tally balances and, under the
`logs.retention` ceiling, nothing was given up. Failed before: logstore
kept one 512-record history shared by every source, so the flood evicted
the quiet records, and a new subscription began with a gap counting every
evicted record ("--- 269 records lost before delivery ---" in the Logs
app); later (LOG-002) each source kept only 32 + 32, and the full replay
had no account of the rest. The hosted core regression is
`tests/log-fanout/run.sh`.

```sh
make -C kernel world
bash tests/headless/run.sh --test log-retention --accel kvm
```

## log-burst (LOG-005)

A service's whole bring-up stays in logstore, and anything left out says
so. log-burst-probe writes 1,000 records at startup (as acpi.svc's few
hundred do), then 4,000 more; after each, `(logs.recent "log-burst-probe")`
at the console returns a `List<LogItem>`: its first records (line 1
first: the boot window is kept), typed `LogItem.Gap` entries where records
are missing (`GapReason.Not_Retained` for those logstore's history gave up
at its `logs.retention` ceiling, `GapReason.Not_Shown` for those one result
cannot hold), its newest records and its `LogItem.Tally`.
`tests/log-burst/check_serial.py` requires, for each result, that every
missing line between two shown records is exactly the gaps between them,
that shown records plus gap counts equal the tally's published count, that
the tally balances (published = retained + not retained; published plus
shed equals the lines written, the ring's shed count equal to the probe's),
nothing given up under the ceiling (phase 1) and exactly the ceiling (4096)
retained over it (phase 2). Failed before: logstore kept the first 32 and
newest 32 records of each source, so a 1,000-record bring-up lost 936 of
them, and `logs.recent` showed what was left with no mark for the hole
(the probe's middle looked silent); shed records were counted but never
reported. The hosted core (growth, ceilings, eviction from the middle,
gaps, tallies, a refusing heap; the counting invariant proved at level 2)
is `tests/log-fanout/run.sh`; the CCL types and their image builders are
`tests/ccl-type-discovery/run.sh` (log_view_tests); cubit-remote's rendering
is `python3 tools/cubit-remote/test_cubit_remote.py`.

```sh
make -C kernel world log-burst-probe
CCL_CONSOLE_DEMO=log-burst bash tests/headless/run.sh --test ccl-console --accel kvm --timeout 120
```

## log-exited (LOG-003)

An exited service's records keep its name. log-exit-probe publishes three
records whose text does not name it and exits at boot; afterwards
`(logs.recent "log-exit-probe")` at the console still returns them, and
boot-logs prints them under its name and says it exited. The names come
from the launchers (procmgr, devmgr) as registrar notices to logstore,
never from the publisher. Failed before: logstore keyed records by process
only and the name came from the live process list, so the console
answered "no running service or program is named", and boot-logs showed
the bare text. The hosted core regression (the notice codec, name matching
and the identity table's retention) is `tests/log-fanout/run.sh`.

```sh
make -C kernel world
CCL_CONSOLE_DEMO=log-exited bash tests/headless/run.sh --test ccl-console --accel kvm --timeout 90
```

## kernel-log (LOG-004)

Programs that end badly leave the reason in logstore, under their own
names, and the kernel's own lines from before logstore started are there
too. crash-probe logs one record and raises an exception nothing handles
(exit code 134): the runtime's last-chance handler publishes "unhandled
exception at main.adb:25" through the program's own log channel, so
`(logs.recent "crash-probe")` returns it after it is gone (the kernel holds
no program text). fault-check writes the null page and stack-probe
overflows its stack: the kernel stops them and files a typed fault report
(Fault_Reports) in two parts. Every reader of the program's logs gets the
summary, "com.cubit.fault-check: stopped by a page fault (write); details
restricted", with no address, offset or register; the detail (where:
null page, stack overflow; anchored offsets) is for diagnostics readers
only, and logstore has none, so it stays in the kernel ring (logstore
says on the console that it withheld each). `(first 4 (logs.recent "kernel"))` returns
kernel records older than logstore (tests/kernel-log-ring/check_serial.py).
Failed before: the handler's text went only to the serial console (none on
the laptop), a killed program left only "process killed" under logstore's
own name, and kernel lines never reached logstore. The overflow variant
runs a kernel whose ring holds 64 records and requires the exact "kernel
log: N records dropped (sequence 1..N)" marker. The hosted core (record
layout, the reader's exact account, fault decoding) is
`tests/kernel-log-ring/run.sh` (`--prove`: level 2).

```sh
make -C kernel world
CCL_CONSOLE_DEMO=kernel-log bash tests/headless/run.sh --test ccl-console --accel kvm --timeout 90
KERNEL_LOG_RECORDS=64 make -C kernel world
KERNEL_LOG_OVERFLOW=1 CCL_CONSOLE_DEMO=kernel-log bash tests/headless/run.sh --test ccl-console --accel kvm --timeout 90
make -C kernel world
```

## native-gprbuild-config

Native fixture scripts resolve the gprbuild configuration before their
first `cd`; resolved later from a relative `$0`, gprbuild ran with no
configuration and mixed GNAT 16 with the GNAT 15 runtime objects.

```sh
bash tests/native-gprbuild-config/check.sh
```

## fs-coherence (FS-022)

Filesystem data and metadata stay coherent across writers.

```sh
bash tests/fs-coherence/build-cubit.sh && bash tests/fs-coherence/run-cubit.sh 3
```

## filesystem-hosted

The filesystem's hosted (Linux) suite.

```sh
bash tests/filesystem-hosted/run.sh
```

## filesystem-stack-frames

The filesystem server's stack frames stay within their bound.

```sh
bash tests/filesystem-stack-frames/check.sh
```

## tls-roots

The TLS trust store's roots load and verify.

```sh
bash tests/tls-roots/check.sh
```

## config-storage (CFG-002)

Config persistence through config-storage: the startup table reaches its
Rust entry, and its requests carry the worker badge.

```sh
bash tests/headless/run.sh --test config-storage --accel kvm
```

## config-objects-reopen

Typed Config collections survive a restart of the store.

```sh
bash tests/headless/run.sh --test config-objects-reopen --accel kvm
```

## ccl-bindings (binding-number collision, 2026-10-30)

Every CCL interface a host grants takes its runtime binding numbers from
CCL.Binding_Blocks, and CCL.Catalog.Install refuses a number another
operation holds. wifi.scan once reused system.disks' number and the
console routed every (system.disks) to Wi-Fi; native config-values sat in
Log's block. The environment probe fails on any collision; the console
demos exercise each interface end to end. Run them for any change to CCL
bindings, catalogs or the console.

```sh
make -C kernel test-ccl-install
CCL_CONSOLE_DEMO=install tests/headless/run.sh --test ccl-console --accel kvm --timeout 600
CCL_CONSOLE_DEMO=wifi tests/headless/run.sh --test ccl-console --accel kvm --timeout 600
tests/headless/run.sh --test ccl-workbench --accel kvm --timeout 600
```

## secondary-stacks (per-thread secondary stacks, 2026-10-31)

Each thread's Ada secondary stack is its own owned-memory reservation
(CuBit.Secondary_Stack_Layout), found through an ELF thread-local slot
(CuBit.Thread_Areas). It used to sit at a fixed address 16 MiB under the
stack top, and acpi.svc's 64 MiB primary stack grew through it. The check
uses >16 MiB of primary and 9 MiB of secondary stack in three threads,
verifies regions and release, then exhausts a reservation and must get the
clean report. Run it for any change to the user runtime's secondary stack,
crt0, link scripts, TLS or thread start/exit (libc, Rust std, raw threads).

```sh
tests/secondary-stack-layout/run.sh
tests/headless/run.sh --test secondary-stacks --accel kvm --timeout 300
tests/headless/run.sh --test acpi-one-service --accel kvm --timeout 300
tests/headless/run.sh --test threads --accel kvm --timeout 300
tests/headless/run.sh --test rust-std --accel kvm --timeout 300
```

## block-requests and block-driver (HW-001, DEV-002)

The shared Block.Device.V1 front end (CuBit.Block_Requests) and the block
driver contract every provider instantiates: CuBit.Block_Driver (async
slots, deadlines, kept replies, write admission, OP_IDENTIFY_DEVICE) and
CuBit.Block_Sync_Driver over it (nvme, ata, ramdisk). tests/block-driver
drives the generic through a fake four-slot device; tests/ram-block and
tests/block-requests run the RAM disk and every driver configuration
(flush, FUA, read only, absent, faulted) through the production loop.
Proofs: the front end against the fixtures and the runtime's own specs
(the Block_Driver body itself is not yet proved; see DEV-002).

```sh
make -C kernel test-block-requests prove-block-requests
```

## scsi (HW-001)

Typed SCSI command blocks and response decoders shared by USB optical and
UFS: byte-exact CDB fixtures, status, INQUIRY, READ CAPACITY(10/16),
fixed and descriptor sense. USB optical's BOT codec encodes through it.

```sh
make -C kernel test-scsi prove-scsi test-usb-optical prove-usb-optical
```

## device-matching (HW-002)

Every driver is in every image and devmgr binds present devices by their
declared matches. The real drivers' index binds the IdeaPad's lspci
(intel-gpu to 8086:46D4, xhci to both controllers, hda to 54C8, ufs to
54FF, the rest unmatched) and QEMU q35; the index tool refuses two equally
specific drivers; the decoders survive every truncation and bit flip. The
generic launch never grants a device-memory window larger than the BAR
decodes (Devmgr_Windows; clamps are logged), and ufs.drv asks for no more
than 54FF's 4 KiB BAR 0 (it once asked 16 KiB).

```sh
make -C kernel ccl-manifest ccl-image
bash tests/device-matching/run.sh
```

The headless `devices` test fails when devmgr's match lines are missing.

```sh
bash tests/headless/run.sh --test devices --accel kvm
```

## generic-launch (HW-002)

A driver devmgr has no dedicated setup for, started from its manifest
alone: QEMU's edu device binds the fixture driver tests/generic-launch/
driver, which maps its window, finds its DMA region, gets one MSI after
OP_READY, and logs every stage through the logstore endpoint devmgr grants
it (boot-logs prints them).

```sh
make -C kernel world boot-logs
bash tests/headless/run.sh --test generic-launch --accel kvm
```

## multi-instance (HW-002 follow-up)

devmgr starts one xhci, nvme and hda driver per bound device, each with its
own device's registers, vectors, DMA and PCI location. Two qemu-xhci
controllers (a USB keyboard on one, a USB optical drive on the other) and
two NVMe disks: both xHCI instances start and each reports only its own
devices; the disks attach as @nvme:0 and @nvme:1; devmgr's lines reach
logstore. Fails on the code before it by construction (not re-run
there): devmgr started only the last xHCI and NVMe controller and logged
"not started: devmgr starts one xhci.drv", which check.py refuses; the
IdeaPad's USB-C controller never ran.

```sh
make -C kernel world boot-logs
bash tests/headless/run.sh --test multi-instance --accel kvm --timeout 120
```

## usb-hotplug and xhci-hotplug (xhci bring-up, IdeaPad mouse)

Two qemu-xhci controllers, the boot mouse on the SECOND (the IdeaPad's
layout: empty TCSS controller first, PCH with the mouse second). Its
reports reach Desktop; unplugging it detaches and disables the slot; a
mouse hot-plugged on the first controller enumerates and delivers, and
again after an unplug/replug (slot reuse); a hot-plugged keyboard on the
second controller delivers keys. Both instances log their bring-up stages
through logstore. Fails before the fix: the old driver had no Port Status
Change handling and exited when its controller had no device at boot.

The hosted tests: transfer/event rings against model controllers; the
EHB race (the old ERDP policy stalls in the model, the production policy
never does); the port machine (debounce, reset, recovery, detach); halted
endpoint recovery; composite receivers (boot keyboard + boot mouse) drive
both roles, not the keyboard alone.

```sh
bash tests/xhci-hotplug/run.sh
systemd-run --user --scope -p MemoryMax=24G -p MemorySwapMax=0 bash tests/xhci-hotplug/prove.sh
make -C kernel world boot-logs
bash tests/headless/run.sh --test usb-hotplug --accel kvm --timeout 150
```

## pci-capabilities (HW-002 follow-up)

devmgr's PCI capability walk (MSI / MSI-X facts on every match line, and
the offsets devmgr programs) on well-formed, truncated and looping lists,
reserved pointer bits, structures past configuration space and random
spaces; proved at level 2. Its mutations (no loop check, no header
check, no fit check, unmasked pointers, reserved MSI encodings) all fail.

```sh
bash tests/pci-capabilities/run.sh --prove
```

## intel-gpu ADL-N family (HW-002)

intel-gpu's device gates accept 8086:46D0..46D4 (the IdeaPad is 46D4), not
only the NUC's 46D2; neighbours and other vendors stay refused. Failed on
micro (46D0 device query refused), passes now.

```sh
(cd kernel && alr exec -- gprbuild -q -p -P ../tests/intel-gpu/adln_family.gpr && \
  ../tests/intel-gpu/build-adln-family/adln_family_tests)
```

## ccl-build embedded blobs (HW-002)

Firmware is baked into drivers: a vendored or fetched blob is pinned by its
SHA-256 (a mismatch is a hard error, a fetch is answered from the
Storehouse offline), linked into the program and exposed as package
Embedded. Part of the CCL build suite.

```sh
bash tests/ccl-build/run.sh
```
```
```

## ufs (HW-001)

The UFS driver's codecs (UFSHCI registers, UPIUs and request descriptors,
UFS descriptors, GPT) as byte-exact fixtures and proofs, including the
IdeaPad's UFSHCI 2.0 register set (the driver once declined it as too
old), and the driver on
QEMU's UFS model: every bring-up stage logs `ok`, the unit's capacity and
the GPT of a disk laid out like the IdeaPad's are read, nothing is written
(the disk is read only at the host too). QEMU is not the hardware test.

```sh
make -C kernel test-ufs prove-ufs
make -C kernel world boot-logs
bash tests/headless/run.sh --test ufs --accel kvm --timeout 150
```

## net-link-state (NET-LINK-001)

The driver contract's codecs and netstack's link state machine: a torn or
malformed link report is never taken as a report, and a flap the reader
missed (Up, Down, Up between two looks) is a bounce, never "unchanged".

```sh
bash tests/net-link-state/run.sh --prove
make -C kernel test-tcp-session   # IPv6 re-detection after the link comes back
```

## net-link (NET-LINK-001)

The host pulls the virtual cable for two seconds: netstack and netmgr log
Down then Up once each, a UDP send while down ends Unreachable, the TCP
stream survives and keeps transferring, the static settings are applied
again and IPv6 detection runs again.

```sh
make -C kernel world
bash tests/headless/run.sh --test net-link --accel kvm
```

## net-multi (NET-LINK-001)

Two virtio-net devices: both attach and are configured independently
(static, DHCP); while one cable is out, traffic keeps flowing over the
other and the default route fails over (and back, by metric); the DHCP
interface re-validates its lease with INIT-REBOOT when its link returns.

```sh
bash tests/headless/run.sh --test net-multi --accel kvm
```

## setting-codecs (SETMEM)

Reading a typed setting runs no CCL: the generated decoders accept exactly
what the CCL path accepts (every truncation and single-character mutation
of a corpus of every catalog type), line breaks in strings round-trip as
`\n`/`\r`, the decoders and token readers prove at level 2, and the
checked-in decoders are a fresh generation. tls-service fits its 128 MiB
again with a static interface setting (before: tls.svc found no netstack
buffers, because netmgr and devmgr each spent about 3 MiB compiling the
setting's CCL type).

```sh
bash tests/setting-codecs/run.sh --prove
python3 tests/ccl-manifests/test-manifests.py -k setting
bash tests/headless/run.sh --test tls-service --accel kvm
```

## wifi-crypto (W2)

The Wi-Fi driver's crypto units (WiFi_Crypto, userspace/lib/wifi/crypto;
docs/wifi.md, W2), Linux-hosted against the flake's pinned, unmodified
SPARKTLSCrypto and SPARKNaCl: HMAC_G (HMAC-SHA1 in Legacy_WPA2, and
HMAC_SHA256), PBKDF2-HMAC-SHA1 and the 802.11 PRF, AES key wrap, AES-CMAC,
AES-CCM, and the P-256 field helpers and SSWU hash-to-curve. Known answers
from RFC 2202, 4231, 6070, 3394, 4493, 3610 and 9380, NIST SP 800-38B, IEEE
802.11 Annex J (PSK, PRF, CCMP, SAE H2E PWE) and the 802.11i CCMP MPDUs.
`--prove` runs gnatprove (memory-capped) on every WiFi_Crypto unit.

```sh
bash tests/wifi-crypto/run.sh --prove
```

## ccl-build fetched library trees; WiFi_Crypto natively

ccl-build fetches SPARKNaCl and SPARKTLSCrypto as pinned trees
(build-catalog.ccl). tests/ccl-build/run.sh checks the fetched-tree
fixture: a wrong identity is refused and not stored, the library's profile
applies to its units only, the store answers with the server stopped, and
--import seeds a tree, a wrong seed being refused. rtw89 links WiFi_Crypto
and both crates. Its start-up self-test (WiFi_Crypto.Self_Test) runs
natively in wifi-crypto-check (needs network on the first build).

```sh
bash tests/ccl-build/run.sh
make -C kernel rtw89 wifi-crypto-check
bash tests/headless/run.sh --test wifi-crypto --accel kvm
```

## acpi-hardware (ACPI region boundary, slice 1)

`(Request.ACPI_Hardware "<binding>")` (coordination/acpi-region-boundary.md).
Catalog role 35 `acpi-registers` is retired with the ACPI pair
(ACPI-ONE-001): the one acpi.svc holds the hardware itself. Hosted: the
manifest compiler's caps/startup encoding, never-optional, at-most-once and binding-length
refusals, the generated `Startup.<Binding> return CuBit.ACPI_Windows.Grant`
(strict compile), role 35 refused by the catalog; the startup-plan approval
`(acpi-hardware approve-declared)` (`(acpi-registers ...)` refused), once and
combinable with `(firmware-snapshot approve-declared)` on one
entry; the FADT/ECDT window computation and devmgr's port-claim ledger
(proved at level 2 with `--prove`). Native (q35, KVM): the approved grant is
non-empty, sorted, disjoint, each handle exactly its range without grant,
equal to the FADT fixed blocks, the PM timer advances through its window;
the same image without the hardware approval is refused at launch.

Slice 2 (firmware memory): the kernel's firmware-memory roots (ACPI NVS
runs, the FACS page) and their authority are hosted-tested and proved in
tests/device-memory (authority.gpr, level 2; 13 firmware mutants killed by
the hosted tests); devmgr's ACPI memory claims and the windows' kernel-tag
classes in tests/acpi-hardware-windows. Native (q35 has no NVS; SeaBIOS
puts the FACS in its reserved span at the top of RAM): one write-back
control window, exactly the FACS page, its signature readable, the pages
around it refused, closed to device memory in devmgr; capability-security
checks that no other program has or can forge a window.

```sh
python3 tests/ccl-manifests/test-manifests.py -k acpi -k firmware -k strict
python3 tests/ccl-configurations/test-configurations.py
bash tests/acpi-hardware-windows/run.sh --prove
(cd kernel && alr exec -- gprbuild -p -P ../tests/device-memory/authority.gpr && ../tests/device-memory/build-authority/authority_tests)
make -C kernel world CUBIT_COMPOSITOR_TIMING=on
make -C kernel acpi-hardware-check
bash tests/headless/run.sh --test acpi-hardware --accel kvm
```

## SCI delivery (level-triggered legacy lines, no system call of its own)

The SCI comes with `(acpi-hardware approve-declared)`: devmgr resolves it
from its own FADT/MADT (CuBit.ACPI_SCI_Route), procmgr routes it to the
program and installs `<binding>-sci` (read-only line handle). The kernel
masks a level line when it delivers it and unmasks it when every owner
acknowledges; the acknowledgement is ENABLE_IRQ's owner form
(Interrupt_Line_ABI.Route_Acknowledge). Hosted and proved (level 2): the
ABI, the kernel's line policy, the SCI route and acpi.svc's register
layout; mutations of the policy and route must be caught. Native:
acpi-hardware's stand-in checks the protocol (no storm while unacknowledged,
an uncleared source re-delivered, a second press while masked delivered at
the acknowledgement); acpi-sci checks the one acpi.svc logs and acknowledges
two power-button SCIs.

```sh
bash tests/interrupt-lines/run.sh --prove
bash tests/interrupt-lines/mutations.sh
make -C kernel acpi-native acpi-metrics-check acpi-hardware-check
bash tests/headless/run.sh --test acpi-hardware --accel kvm
bash tests/headless/run.sh --test acpi-sci --accel kvm --timeout 300
```

## acpi-pci-config (ACPI-PCI-001, kernel/devmgr/procmgr half)

A kernel-only firmware-memory kind, `Capability_ABI.Firmware_PCI_Config_Tag`:
PCI segment 0 bus 0's ECAM slice from the MCFG (exactly 1 MiB, 256
functions), its kernel root READ+GRANT only, mapped uncached and only
read-only (`Device_Memory_Authority.Firmware_Mode`/`Write_Back`); devmgr
claims it beside the NVS/FACS roots (one of the eight firmware roots) and
closes it to device memory; procmgr installs it read-only, and only for an
entry that also approves `(acpi-pci-config approve-declared)` (refused
without `(acpi-hardware approve-declared)`). Off by default: no image or
other test approves it, and without the approval the grant is exactly the
slice-2 one (acpi-hardware checks that no program gets the window).
Hosted: tests/device-memory (exact slice, read-only even with a write
right, uncached, other tags refused, no derivation retags/widens/adds
write, eight-root budget, random-map oracle; authority.gpr proved at level
2), the startup-plan approval in tests/ccl-configurations. Native (q35,
KVM): the approved stand-in gets the window at its own MCFG's base, held
read-only without grant, a writable mapping and bus 1 refused, reads the
host bridge IDs devmgr enumerated at 00:00.0, and its write to the
read-only mapping is a write-protection fault; the same image without the
approval gets the grant without the window; capability-security checks no
forged handle maps the slice.

```sh
(cd kernel && alr exec -- gprbuild -p -P ../tests/device-memory/authority.gpr && ../tests/device-memory/build-authority/authority_tests)
python3 tests/ccl-configurations/test-configurations.py
make -C kernel world CUBIT_COMPOSITOR_TIMING=on
make -C kernel acpi-pci-check acpi-hardware-check
bash tests/headless/run.sh --test acpi-pci-config --accel kvm
bash tests/headless/run.sh --test acpi-hardware --accel kvm
bash tests/headless/run.sh --test capability-security --accel kvm
```

## Agent sizes, compaction and agent.app (AGENT-002, LIM-001)

The agent path has no fixed windows: llm.svc renders each request on the
heap and captures content sized from max_tokens, the client's window is
sized per ask and its content grows, a tool result is capped by a named
limit and labelled, and a conversation at its limit is compacted (a
marker, whole turns dropped, no stale thinking) instead of refused. Each
hosted case failed on the old fixed sizes (128 KiB content, 6 KiB tool
results, the 192 KiB ask that stopped a long session with Context_Full).
agent-app boots agent.app, types the fake key into its masked entry
(secrets.svc), and runs one mocked Diagnose mission with a tool round;
the key must never reach the serial log.

```sh
gprbuild -p -P tests/llm/llm_tests.gpr && tests/llm/build/llm_main
gprbuild -p -P tests/agent/agent_tests.gpr && tests/agent/build/agent_main
(cd kernel && alr exec -- gprbuild -p -P ../tests/llm/ccl/llm_ccl_tests.gpr && ../tests/llm/ccl/build/agent_ccl_main)
bash tests/headless/run.sh --test agent-app --accel kvm --timeout 150
bash tests/headless/run.sh --test agent-mock --accel kvm --timeout 150
bash tests/headless/run.sh --test llm-mock --accel kvm --timeout 150
```

## async-ipc: displaced reply ended (BUG-004, IPC-000 rule 3)

A server that receives a call and, without answering it, receives or
polls again ends that call: the caller wakes at once with the kernel's
`REPLY_ENDED` label instead of waiting for its deadline (with
Wait_Forever, a hang), and an unanswered async request displaced the same
way completes `COMPLETION_CANCELLED`. The same holds for a reply closed
unanswered, a thread that exits owing one, and a server that exits. A
server's own label-0 reply still arrives as that reply, never as an end. Failed before: `installReceivedWork`
overwrote the thread's reply authority and dropped it, so the call timed
out after 2 s (REPLY_TIMEOUT) and the async request never completed. The
kernel names such a server on serial (`REPLY-ENDED:`). The hosted proof
of the reply slot is `tests/reply-slots/run.sh --prove`.

```sh
make -C kernel world
bash tests/headless/run.sh --test async-ipc --accel kvm
bash tests/reply-slots/run.sh --prove

## nvme-timeout and nvme-stuck (BUG-003)

One slow NVMe read must not kill storage. Once nvme.drv serves, QMP
throttles the drive's reads (tests/nvme-timeout/fixture.py) so a read
outlives the driver's named 30 s I/O deadline. nvme-timeout lifts the
throttle once recovery starts: the driver aborts, resets the controller,
re-creates the queues and re-issues the read, and the boot completes.
nvme-stuck keeps the throttle until the request fails with the typed
outcome "timed-out" after its bounded passes (about a minute), then the
next reads succeed and the boot continues (tests/nvme-timeout/check.py).
Failed before: the driver waited 1.02 s, printed "NVMe: I/O completion
lost or foreign", latched, and failed every later read until reboot
("program file was not found"). The backlog's own repro
(`NVME_DRIVE_OPTIONS=,throttling.bps-read=50000` on boot-shell-nvme)
also failed before and passes. The hosted model tests and the SPARK proof
cover the identifier tracker, queue cursors and recovery state machine.

```sh
bash tests/nvme-recovery/run.sh
systemd-run --user --scope -p MemoryMax=24G -p MemorySwapMax=0 bash tests/nvme-recovery/prove.sh
make -C kernel world
bash tests/headless/run.sh --test nvme-timeout --accel kvm --timeout 180
bash tests/headless/run.sh --test nvme-stuck --accel kvm --timeout 180
NVME_DRIVE_OPTIONS=,throttling.bps-read=50000 bash tests/headless/run.sh --test boot-shell-nvme --accel kvm --timeout 150
```

## iommu and iommu fault probe (IOMMU-001)

DMA is confined by VT-d, and every DMA fault is a typed record. On QEMU's
VT-d model (`--iommu`: intel-iommu, intremap=on, aw-bits 39 as on the
IdeaPad's ADL-N; virtio-net modern-only with iommu_platform=on) the full
desktop boots: the kernel parses the DMAR, brings the unit up, and NVMe and
virtio-net each do DMA through their own domain with no DMA fault
(tests/headless/check-iommu.py active). With `--iommu` the generic launch's
edu driver does a DMA round trip through its domain, then reads from an
address outside its block: exactly one typed fault record names edu's
requester, the probe address and reason not-mapped (kernel console and
logstore), and the boot carries on (check-iommu.py fault). Without
`--iommu` every headless test checks the typed degraded record ("kernel:
IOMMU degraded: DMA unconfined (no DMAR table)"). Failed before: there was
no IOMMU support, any device could DMA anywhere, and nothing said so. The
hosted regressions (DMAR admission with the IdeaPad's 136-byte table,
truncations, every single-byte mutation, typed rejections; I/O page-table
arithmetic; the walker against the domain ledger) and the level 2 proofs
are `tests/iommu/run.sh --prove`.

```sh
systemd-run --user --scope -p MemoryMax=24G -p MemorySwapMax=0 bash tests/iommu/run.sh --prove
make -C kernel world boot-logs
bash tests/headless/run.sh --test startup-health --iommu --accel kvm --timeout 90
bash tests/headless/run.sh --test generic-launch --iommu --accel kvm
bash tests/headless/run.sh --test startup-health --accel kvm --timeout 90
```

## acpi-one-service (ACPI-ONE-001: one acpi.svc, laptop MBAR refusal)

The one acpi.svc holds the ACPI hardware grant, the platform-register pool
and the PCI configuration broker itself, and binds the ACPI agent's local
root (tables admitted before Fields; Hardware, memory bridge and Fields from
the copied FADT inside the module callback; Finish_Startup after device
_INI; hardware maintenance bounds the serving wait; the shutdown controller
retires the root before exit). q35 gets QEMU's edu device (devmgr's pool
test profile) and the regression SSDT tests/headless/acpi/acpi-one-service-pool.aml:
its module-level code reads a BAR-backed CHIPSET_SHARED pool member, which
firmware data cannot serve, during table load. Failed before: the acpi-backend.svc pair refused exactly this read (STARTUP_REFUSED;
laptop image 11, MBAR RBAR at 0xFEDC7110). Passes after: module AML
completes for both tables and all 8 tables install.

```sh
make -C kernel world CUBIT_COMPOSITOR_TIMING=on
make -C kernel acpi-native acpi-metrics-check
tests/headless/run.sh --test acpi-one-service --accel kvm --timeout 300
```

## acpi-live-replace (LIVE-001 for ACPI: graceful live replacement)

The one acpi.svc is (replace graceful) in the laptop's live and installed
plans, so cubit-remote's system generations switch it while it runs. The
test switches it over ccl-remote. On Stop, acpi.svc serves STOP_REQUESTED,
retires QUIESCENT and exits 0; the new program runs startup again and
registers. A crashing program and one that never registers are rolled
back. A 1 ms stop grace either aborts (Stop_Failed; the generation is kept,
and after a late stop procmgr starts it again) or is met; the run prints
which BRANCH it took. Over all instances, each got the first instance's
hardware grant exactly and mapped the platform pool as it did, and no grant
was refused as still held.

Failed before: acpi.svc's main returned without an exit status, and the
binder's main is a procedure, so it exited with code 1 (RAX). Every
graceful switch therefore ended "did not quiesce cleanly", and the old
program was started again.

Passes after: acpi.svc sets 0 only after a served Stop and a QUIESCENT
retirement.

```sh
make -C kernel world CUBIT_COMPOSITOR_TIMING=on
make -C kernel acpi-native
tests/headless/run.sh --test acpi-live-replace --accel kvm --timeout 400
```

## platform-quirks (ACPI-POOL-001 step 1b)

The IdeaPad's P2SB discovery is classified by the step that failed
(image 12: `UNHIDE_IGNORED`, step unhide), the quirk table selects the
sideband source and GPIO controller by typed platform identity, the GPIO
bootstrap payload round-trips and refuses every malformed field at its
byte, and the admission decision gives every refusal reason and admits
the IdeaPad's four GPI0 `_CRS` communities against its memory map. The
headless platform-pool test drives the admission over the firmware-data
badge end to end (grant copy, decode, gate, idempotent repeat).

The kernel's late insert (the approved post-seal rule) refuses a wrong
kind, an undeclared or used-up late room, and an overlap with a member or
a recorded claim (tests/platform-pool `Late_Room`).

```sh
bash tests/platform-quirks/run.sh
bash tests/platform-pool/run.sh
make -C kernel world platform-pool-check
bash tests/headless/run.sh --test platform-pool --accel kvm --timeout 120
```

## exit-status (LIVE-002: a returning main's exit status)

Every Ada program whose main returns exits with a defined status. The user
runtime's System.Exit_Status_Supported makes the binder's main a function
that returns Ada.Command_Line's status: Success unless the program set
another. exit-status-check runs three times from the plan: a plain return
exits 0, Set_Exit_Status (Failure) exits 1, and an unhandled exception
exits 134 (Unhandled_Exception_Status, unchanged). The run pairs each
program's PID with procmgr's exit line.

Failed before: the binder's main was a procedure, so a main that returned
exited with whatever RAX held (acpi.svc exited 1 that way, see
acpi-live-replace). A return without a status now exits 0.

```sh
make -C kernel world CUBIT_COMPOSITOR_TIMING=on
tests/headless/run.sh --test exit-status --accel kvm
```

## desktop-live-replace (LIVE-002: Desktop and timesync live replacement)

desktop.svc is (restart on-failure) (replace graceful) in the laptop's
live and installed plans. Switched over ccl-remote: on Stop, Desktop hands
every output lease and buffer back to Display and exits 0 ("desktop:
stopped; display released, exiting 0"); the new program takes the display
and presents a frame. A crashing program is rolled back and the previous
one presents again; system rollback returns to the image's program. Open
windows are not handed over (UI-037). The same run switches timesync.svc
(replace transparent): it reads Stop between polls and exits 0.

Failed before: Desktop ignored procmgr's Stop, so a graceful switch could
only end in Stop_Failed (the running instance never ended).

```sh
make -C kernel world CUBIT_COMPOSITOR_TIMING=on
tests/headless/run.sh --test desktop-live-replace --accel kvm --timeout 400
```

## gpio-launch (ACPI launch, docs/device-matching.md)

devmgr launches the driver of an ACPI device it verified for use, from
that driver's manifest and the admission record alone. The GPIO bootstrap
handler answers and records the controller; the launch waits for acpi.svc's
Consumer_Initialization_Complete (16#0253#: all startup AML is over), and
only status Completed queues it, and the service loop binds the driver by `(Device_Match.ACPI_ID "...")` and gives
its K-th `device-memory` request a capability narrowed to the admitted
span its index names (`_CRS` order, at most max_bytes). In QEMU (the edu
test profile with the CBTEST/GPIOBOOT firmware identity, whose quirk row
runs P2SB discovery and names the test controller CBIT1057), the fixture
requester waits while logstore starts and nothing is launched; after its
completion messages (four non-canonical frames carrying COMPLETED for
the right epoch: MALFORMED, unrecorded; epoch 0 and an unknown status:
MALFORMED; INCOMPLETE for epoch 2: recorded, nothing queued; COMPLETED
for epoch 2: CONFLICTING, no promotion; COMPLETED for epoch 3, where
nothing was observed: nothing released; COMPLETED for epoch 1, twice:
one receipt, one launch) the driver gets exactly its two
windows in index order, maps them (the
read-only one only read-only), and the kernel refuses the page past each
window and the other span. Hosted: a request for more spans than were
admitted, a read-write request for a read-only span and non-memory
resources refuse the whole launch; Devmgr_Phase_Declarations releases only
the sender's records of exactly the declared epoch and refuses epoch 0,
conflicting statuses and non-canonical frames; the compiler gives ACPI-only manifests
the ACPI basis (device memory only, indexes 0 .. 15); `Generic_ACPI`
round-trips and refuses bad words; `Bind_ACPI` binds exactly and finds
ambiguity; a P2SB_AVAILABLE proposal is verified for use only when its
spans are exactly the inserted P2SB communities (tests/platform-quirks) (tests/launch-configuration also checks `Generic_ACPI`; its
older "statistics size" check fails independently). Failed before: devmgr bound ACPI_ID entries to nothing ("not
enumerated by devmgr"), an ACPI-only manifest with device-memory did not
compile (MISSING_DEVICE_MATCH), and no driver was ever started for an
admitted controller.

HID offers (16#0252#, `CuBit.HID_Offer_Exchange`, wire docs/hid-offer-wire.md):
the same requester then offers a PNP0C50 touchpad whose I2C controller is
edu at 00:15.0 (bound to generic-check.drv) and whose GpioInt is on
CBIT1057. Four non-canonical frames, request id 0, a wrong version, length
field, compatible ID and epoch 0 are MALFORMED (with the payload byte) and
unrecorded; a grant that is not the sender's is COPY_FAILED; an unresolved
`_ADR` and an absent function are UNKNOWN_CONTROLLER, the driverless host
bridge CONTROLLER_MISMATCH, another GPIO controller and another epoch
GPIO_NOT_ADMITTED; the touchpad is QUEUED at position 1 with its I2C-target
and GPIO-input badges, its repeat gets the same answer, a second request
for the same device is DUPLICATE_DEVICE, and a recorded UNKNOWN_CONTROLLER
id keeps its answer for the canonical payload. The deferred step runs from
the service loop only after the GPIO driver reported ready (the harness
checks the order). Then (SUP-001) devmgr starts hid-launch-check.drv, the
image's PNP0C50 fixture, through procmgr's Start_Child: the plan's
(driver-class "PNP0C50" ...) policy merged with devmgr's descriptor
register fact (0x20) reaches the driver exactly, the driver reports ready
and its exit reaches devmgr; procmgr never refuses devmgr's start.
Hosted (tests/gpio-launch/windows, `Devmgr_HID_Offers`, proved at level 2):
every known-mask 0 .. 6, another segment, sender, epoch, GPIO `_HID`/`_UID`
kind/value/path, pin 0xFFFF, duplicates per (sender, epoch, path), golden
badge words, and the launch order. Failed before: devmgr had no 0252
handler (the label was "rejected unknown message" with REPLY_ERROR) and the
hosted suite did not compile without `Devmgr_HID_Offers`.

```sh
bash tests/gpio-launch/windows/run.sh
bash tests/platform-quirks/run.sh
make -C kernel ccl-manifest ccl-image
python3 tests/ccl-manifests/test-manifests.py -k acpi_spans -k typed_drives -k round_trip_through
bash tests/device-matching/run.sh
make -C kernel world CUBIT_COMPOSITOR_TIMING=on gpio-bootstrap-check
bash tests/headless/run.sh --test gpio-launch --accel kvm --timeout 120
bash tests/headless/run.sh --test platform-pool --accel kvm --timeout 120
```

## supervision (SUP-001: Start_Child, supervision edges, reaping)

procmgr is the only process creator; Start_Child records a typed
supervision edge. Headless: supervision-check (a supervisor whose
start-children scope is its may_launch list) starts supervision-child.app
with typed parameters, which logs exactly what it decoded; its exit event
reaches the supervisor (exited code 3); procmgr refuses, each typed, a
parameter of the wrong type (LAUNCH_REFUSED, parameters rejected), a
program outside the scope (NOT_GRANTED), a device-class start
(CLASS_NOT_GRANTED), badge bindings (BINDINGS_NOT_GRANTED) and an ended
sibling as a dependency (DEPENDENCY_UNKNOWN); when the supervisor ends
with a child still running, procmgr reaps (stops) the child. Hosted
(tests/supervision, proved at level 2): the spec codec (round trip, every
truncation, each field and rule broken), the fact/policy merge checked
by CCL's type checker against hid-i2c's own HID_Parameters (a plan
naming a fact field or missing a policy field is refused; the
supervisor may give only declared facts), (driver-class ...) blocks in
plans, the laptop plans' PNP0C50 block, and the supervision edges (one
supervisor per process, ranks ordered along supervisor chains so no
cycle, removal, reaping marks, restart decisions). Failed before:
procmgr answered 16#0114# with REPLY_ERR (unknown label), so every
check failed; the plan compiler refused (driver-class ...) as
UNKNOWN_DECLARATION; the hosted suite did not compile.

```sh
bash tests/supervision/run.sh --prove
make -C kernel world CUBIT_COMPOSITOR_TIMING=on supervision-test
bash tests/headless/run.sh --test supervision --accel kvm --timeout 90
```

## touchpad plumbing (SUP-001: held start, GPIO sink, controller-correct bindings, typed class launches)

Headless gpio-launch, with two I2C-controller stand-ins (edu at 00:15.0
and 00:15.1, a generic-check.drv on each; the touchpad offer names
00:15.0): devmgr starts hid-launch-check.drv Held through Start_Child,
naming each binding's provider (the generic driver launched for 00:15.0,
the GPIO controller's driver) and asking for the sink; procmgr holds the
child and its receipt carries an endpoint to it badged with the GPIO
input; devmgr offers it Write-only to the GPIO fixture with
OP_GPIO_ADMIT_SINK (16#02A4#), which checks the caller is its launcher,
zero words, one Write-only endpoint whose Describe badge is the offer's
GPIO input (0x475010000001004C), answers COMPLETED and sends one event
through it; only then does devmgr release the child (Release_Operation
16#0115#), and the harness checks held < admitted < released < first
line. The fixture's "i2c" call reaches the 00:15.0 driver with the I2C
target's badge, never 00:15.1, and the GPIO event arrives through the
admitted sink. Failed before: the bindings were badged copies of
whichever process held the role's registry entry (the last i2c-dw to
register; for the fixtures none, so "i2c absent, gpio absent"), no sink
existed (the GPIO driver could not admit the input; the HID driver ran
before any admission), and procmgr answered 16#0115# REPLY_ERR.

Hosted: tests/supervision (spec v2 codec: start mode, sink kind and
badge, binding providers, release words; the typed class launch
`(driver-class "PNP0C50" (Device_Class_Launch facts => [...] policy =>
...))` checked by CCL's type checker against hid-i2c's record less the
facts: a policy naming a fact or leaving a policy field out is a type
error, facts not exactly the supervisor's are refused, a fact outside its
range fails the full check, default strings with parentheses are one
field; every laptop plan's PNP0C50 launch merges to a HID_Parameters).
tests/gpio-launch/windows (Provider_At: each controller's own driver,
launch order irrelevant, a not-ready driver or two drivers for one
function give none). Failed before: the plan reader parsed a hand-written
(facts ...) (parameters ...) form; no Provider_At, no v2 fields.

```sh
bash tests/supervision/run.sh --prove
systemd-run --user --scope -p MemoryMax=24G -p MemorySwapMax=0 bash tests/gpio-launch/windows/run.sh --prove
make -C kernel world CUBIT_COMPOSITOR_TIMING=on gpio-bootstrap-check hid-launch-check supervision-test
bash tests/headless/run.sh --test gpio-launch --accel kvm --timeout 120
bash tests/headless/run.sh --test supervision --accel kvm --timeout 90
python3 tests/ccl-images/test-images.py
```

## shutdown and shutdown-console (PWR-003 slice 1: the root stop)

A shutdown request is a typed, ordered stop, not a kill. Headless (KVM):
QEMU's power button (system_powerdown) reaches power.svc through
shutdown-standin, a TEST STAND-IN for acpi.svc's side of
CuBit.Power_Event_Exchange (it polls PM1_STS and writes PM1a_CNT; no AML);
shutdown-console types (power-off) at the CCL console instead, the
console's own power-console badge. procmgr stops the apps first; then
acpi.svc (the stand-in) prepares to sleep (Prepare_To_Sleep S5) while
everything else still runs; then procmgr keeps power.svc and the firmware
step's protected tail and stops every other plan entry in reverse startup
order, each within its (stop-grace-ms N): shutdown-stubborn ignores Stop
and is ended after 500 ms (FORCED), shutdown-cooperative obeys
(STOPPED_CLEAN); then the filesystem commits and detaches the journaled
disk, devmgr stops its drivers, and the stand-in writes S5: QEMU must exit
by itself (status 0, not the timeout), the disk must have no
needs_recovery and pass e2fsck -fn. Hosted (tests/shutdown, proved at
level 2): the stop order (stage, then reverse start), the apps-only first
phase, kept units never stopped, ending only at a unit's grace, Forced /
Not_Ended / Ended_Early, storage, devmgr's drivers refused, polled and
timed out, Finished means every unit is kept or decided; power.svc's
answers and button policy; the wire codecs. Failed before: procmgr
answered 16#0274# and 16#0275# with REPLY_ERR, so nothing stopped and QEMU
ran to the timeout; the journaled disk kept needs_recovery (the
filesystem never detached); the plan compiler refused (stop-grace-ms N) as
UNKNOWN_DECLARATION; the hosted suite did not compile.

```sh
bash tests/shutdown/run.sh --prove
make -C kernel world CUBIT_COMPOSITOR_TIMING=on shutdown-test power
bash tests/headless/run.sh --test shutdown --accel kvm
bash tests/headless/run.sh --test shutdown-console --accel kvm
```

## shutdown-text (PWR-003: apps close safely)

procmgr's Stop reaches every UI app as a typed close request
(CuBit.UI.Input.Close_Reason System_Shutdown, delivered by CuBit.UI.App.Run
with no per-app plumbing); the framework first calls the app's
Save_Recovery hook. Headless (KVM): Text has an unsaved line when the power
button is pressed; it writes @system/Recovery/com.cubit.text/recovery-1.txt
(CuBit.UI.Recovery: the document's bytes, never over its own file) and
exits within its 3000 ms grace (STOPPED_CLEAN); the disk then holds the
line in the copy and notes.txt unchanged. Hosted (tests/text-editor): a
missing recovery copy opens nothing; a present one opens with the restore
note. Failed before: Text never read the Stop, was ended after its grace
(FORCED) and the unsaved line was lost.

```sh
bash tests/text-editor/run.sh
make -C kernel world CUBIT_COMPOSITOR_TIMING=on shutdown-test power
bash tests/headless/run.sh --test shutdown-text --accel kvm
```

## shutdown-desktop and shutdown-desktop-cancel (PWR-003: Shut Down... in the Apps menu)

Desktop's Apps menu ends, under a separator, with Restart... (shown,
greyed: no platform reset path yet) and Shut Down..., keyboard first.
Headless (KVM), by keyboard: Super, Up (wraps to Shut Down...), Enter opens
the confirmation; Enter shuts down (Desktop's power-desktop badge, power.svc
ACCEPTED, then the root stop and S5 through the stand-in: QEMU exits by
itself); Escape cancels and nothing is sent (no power request, no
procmgr stop). Hosted (tests/desktop-launch): Up wraps to Shut Down...
skipping the disabled Restart..., Enter chooses it, a click on Restart...
does nothing. Failed before: the menu's Power row did nothing.

```sh
bash tests/desktop-launch/run.sh
make -C kernel world CUBIT_COMPOSITOR_TIMING=on shutdown-test power
bash tests/headless/run.sh --test shutdown-desktop --accel kvm
bash tests/headless/run.sh --test shutdown-desktop-cancel --accel kvm
```

## ccl-manifest-capacity (CCL storage sized by its input; no fixed program limit)

The CCL front end, compiler and verifier size their storage by the program
(CCL.Language.Analysis_Of and With_Analysis, CCL.Compiler.Compilation_Of;
evaluation compiles in rooms that double until the program fits), and the
manifest compiler reads files of any size. Only named ceilings remain, each
refused with its typed error. The test:
- analyses, compiles and verifies tests/large-manifest-probe's typed
  manifest with its schema, past the old rooms (about 14 KiB of code, 531
  syntax nodes, 587 instructions, 93 constants);
- runs a plain program over twenty times past them (8191 nodes, 8192
  instructions) to its value;
- compiles that manifest and Desktop's typed one, and refuses a manifest
  over a named ceiling (the launch table, the 16 access scopes) with its
  typed error.

Failed before: the old ccl-manifest refused files over 4096 bytes; CCL
refused programs over 8192 bytes of source ("Source exceeds its size
limit"), 512 syntax nodes, 256 instructions or 32 constants (PROGRAM_FULL).
This manifest did not compile; Desktop's typed one and ccl-remote's and
ccl-console's (with their metrics grants) compiled only with the MET-002
stopgaps (512 instructions, 64 constants), which are gone.

Passes after: all of it compiles, verifies and runs.

```sh
bash tests/ccl-manifest-capacity/run.sh
```

## large-manifest (a large typed manifest loads and launches)

large-manifest-probe's manifest is past the old CCL limits (see
ccl-manifest-capacity). procmgr loads its sections and launches it.

Failed before: the manifest did not compile, so the program could not be
built.

Passes after: `procmgr: init launched: large-manifest-probe.app` and
`large-manifest-probe: started with its large manifest PASS`.

```sh
make -C kernel world CUBIT_COMPOSITOR_TIMING=on
tests/headless/run.sh --test large-manifest --accel kvm
```

## input-rings (IPC-009, DEV-002: input on source rings, not the event lane)

Input drivers feed `CuBit.Input_Source`; each device has its own ring to
Desktop, and the kernel only sets it up and wakes.
- Hosted (`tests/input-rings`): a full ring keeps every report in order and
  shows the producer's overflow; wakes come once per arming; two sources at
  once keep each one's order; a source added mid-session is read, one that
  leaves is drained first, one that comes back is a new source; a producer
  cannot name its source; Desktop admits only the input sources' badge.
- desktop-restart: the input drivers' rings open again to the restarted
  Desktop.
- key-repeat and desktop-display (drags) still work over the rings.

```sh
bash tests/input-rings/run.sh
make -C kernel world CUBIT_COMPOSITOR_TIMING=on
make -C kernel desktop CUBIT_DESKTOP_FAULT=exit-once CUBIT_COMPOSITOR_TIMING=on
cp kernel/isodir/boot/desktop.svc /home/doc/cubit-build-tmp/desktop-fault.svc
make -C kernel desktop CUBIT_COMPOSITOR_TIMING=on
CUBIT_DESKTOP_IMAGE=/home/doc/cubit-build-tmp/desktop-fault.svc \
  bash tests/headless/run.sh --test desktop-restart --accel kvm --timeout 120
bash tests/headless/run.sh --test key-repeat --accel kvm --timeout 90
bash tests/headless/run.sh --test desktop-display --accel kvm --timeout 90
```

## frame-rings (IPC-009: Desktop's frames to Display on a ring)

Desktop presents through one Request_Channels channel to Display, and
Display answers each frame through it; the kernel only sets it up and
rings a side that sleeps.
- Hosted (`tests/frame-rings`): every present is answered, or fails typed
  once when the channel ends; a superseded frame is answered and taken as
  a drop (redraw), not a fault; a forged answer (unknown tag, another
  frame's result, a malformed entry) is refused and counted while the
  present stays outstanding; a full ring refuses the next present without
  dropping one.
- desktop-restart: the failed Desktop's frame ring ends at Display and the
  restarted Desktop opens a new one (with the input rings, above).

```sh
bash tests/frame-rings/run.sh
```

## desktop-reap (IPC-009: a dead client's window goes though nothing redraws)

reap-check holds a window with no surface ring and no buffer and ends
without goodbye. Nothing it held signals its end and nothing redraws
afterwards; Desktop's look for windows like it (every
Ringless_Reap_Interval_Ms) still removes the window ("ended: its process
ended"). Failed before: the look ran only when a frame was composed.

```sh
make -C kernel world reap-check
bash tests/headless/run.sh --test desktop-reap --accel kvm --timeout 60
```

## tlb-tags (address-space tags, PCID: no stale translations)

No process sees translations another address space left under its tag:
twelve lives of tlb-probe on reused PIDs each find their fresh pages zero
(exit, then PID reuse), and every second life faults reading pages its
owner revoked while a waiting thread had them cached (revocation, unmap).
Only a TAGGED_TLB=1 kernel on Intel (PCID and INVPCID) can show a stale
translation here; on the AMD hosts and TCG it checks the untagged path.
The proofs and their mutants are tests/address-space-tags.

```sh
make -C kernel world tlb-tags
bash tests/headless/run.sh --test tlb-tags --accel kvm
make -C kernel test-address-space-tags prove-address-space-tags
```

## futex-share idle after notices (stale notice doorbell)

An activity loop sleeps when it is idle after kernel notices. A grant-return
notice is counted on the readiness page when it is posted, and its doorbell
rings once grantLock is free; a process that took it in between (its event
wait) used to be turned away by the kernel (no doorbell yet) and then left
the doorbell answering every activity wait with Work: devmgr spun 1.4 million
turns a second in gpio-launch. futex-share's client takes notices on a
second thread while the server gives back 2000 notified grants, then an idle
300 ms activity loop may turn at most 50 times. Failed before (51 turns, 3 of
3 runs), passes after (1 turn).

```sh
make -C kernel world futex-share
bash tests/headless/run.sh --test futex-share --accel kvm --timeout 300
```

## net-link idle stream (a stream read empty lets its reader sleep)

netstack counted bytes a client had already read as unread (clients read
without telling it), so every NET_WAIT on such a stream was answered at
once: ccl-remote sent about 22 million waits in one desktop-live-replace
run. net-link-check reads a 1460-byte stream that then goes quiet
(tests/net-link/fixture.py, port 18462) and waits 300 ms for more: at most
50 system calls. 163,132 before, 4 after.

```sh
make -C kernel world net-link-check
bash tests/headless/run.sh --test net-link --accel kvm
```

## alignment-flag (interrupt entry clears RFLAGS.AC, SEC-026)

User code may hold RFLAGS.AC set (POPFQ at CPL 3), an interrupt keeps it,
and under SMAP a set AC lets supervisor code touch user pages, so interrupt
handlers ran with SMAP off. alignment-flag-check sets AC and takes timer
interrupts and page faults; a probe kernel (SMAP_ENTRY_PROBE=1) reads the
process's own page from each and wants the SMAP fault. Needs a CPU model
with SMAP (Chungus KVM guests have it).

```sh
SMAP_ENTRY_PROBE=1 make -C kernel world
bash tests/headless/run.sh --test alignment-flag --accel kvm --timeout 60
```

## kernel-map-probe (what the kernel's own map reaches, KERN-014)

The kernel maps all of RAM at `LINEAR_BASE`, so a kernel read bug or a
speculative gadget reaches every process's pages (docs/kernel-memory-map.md).
A probe kernel (KERN014_PROBE=1) reads, on kernel-map-probe's request, one
word of its heap page through the linear map, and at boot frees a frame and
reports whether the buddy allocator wrote its free-list links into it. Since
KERN-014 slice 4 process frames come from regions outside the kernel's
linear map, so the probe's read faults ("unmapped"); before, it read the
marker. Since slice 1 a freed frame holds no free-list links (the probe
reported "yes" before it). Since slice 2 the kernel reaches a process's
memory only through the process's own mapping: kernel-map-probe asks for
kernel stores into a writable page (stored), a read-only page (refused,
unchanged), a reserved page with no frame and the kernel half (refused), and
wants at least two recovered user-access faults (none were counted before:
the kernel walked page tables and wrote through its linear map).

```sh
rm -f kernel/build/libcubit.a    # build.ads is regenerated with the flag
export KERN014_PROBE=1    # run.sh rebuilds the kernel: keep the flag
make -C kernel world
bash tests/headless/run.sh --test kernel-map-probe --accel kvm --timeout 40
```

## startup-health: the idle shell sleeps (no 1 ms poll)

shell.app (root init.ccl) idled by polling for an event and sleeping 1 ms:
about 1,980 WAIT calls a second, the largest idle consumer in a desktop boot
(census, Chungus KVM). It now waits for activity. Once, 1 s after its first
prompt with no key, it prints its own thread's system calls over that
second; startup-health allows at most 50.

```sh
make -C kernel world
bash tests/headless/run.sh --test startup-health --accel kvm --timeout 90
```

## bench-net: netstack's busy-poll window polls memory

While traffic flows netstack polls for 50 us before it sleeps. Each turn
made two kernel calls (a request poll and GETTIME): about 107,000 a second
of each while ccl-remote streamed (desktop-live-replace census). netstack
reports windows, turns and kernel calls at each power of two windows from
1,024; bench-net allows one call per ten turns.

```sh
make -C kernel netstack && tests/net-bench/build-cubit.sh
bash tests/headless/run.sh --test bench-net --accel kvm --timeout 240
```
