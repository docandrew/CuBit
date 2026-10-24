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
(intel-gpu to 8086:46D4, xhci to both controllers, hda to 54C8, the rest
unmatched) and QEMU q35; the index tool refuses two equally specific drivers;
the decoders survive every truncation and bit flip.

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
UFS descriptors, GPT) as byte-exact fixtures and proofs, and the driver on
QEMU's UFS model: every bring-up stage logs `ok`, the unit's capacity and
the GPT of a disk laid out like the IdeaPad's are read, nothing is written
(the disk is read only at the host too). QEMU is not the hardware test.

```sh
make -C kernel test-ufs prove-ufs
make -C kernel world boot-logs
bash tests/headless/run.sh --test ufs --accel kvm --timeout 150
```
