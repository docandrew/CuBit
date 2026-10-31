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

## block-requests (HW-001)

The shared Block.Device.V1 front end (CuBit.Block_Requests and the
CuBit.Block_Device_Server generic) that nvme, ata, ramdisk and the USB
optical service use: request checks, grant acquire and return, replies,
in every driver's configuration (flush, FUA, read only, absent, faulted).
The RAM disk's own regression (tests/ram-block) runs on it unchanged.
Proofs: the front end against the fixtures and the runtime's own specs.

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
