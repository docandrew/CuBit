# Internal volume list

All builds, tests and proofs use Nix:

```sh
nix develop -c bash -c 'cd kernel && alr exec -- gprbuild -p -P ../tests/volume-list/volumes.gpr && ../tests/volume-list/build/main'
nix develop -c bash -c 'cd kernel && alr exec -- gnatprove -P ../tests/volume-list/volumes.gpr -u volume_list.ads --level=1 --timeout=5 --checks-as-errors=on -j2'
```

The production `Volume_List` implementation holds up to 16 append-only bindings,
with names up to 48 bytes. Registration rejects invalid/duplicate names,
duplicate endpoint slots and exhausted capacity without changing the list.
Names match exactly: `@nvme:1/file` never means volume zero. Null strings and
non-1-based strings are included in the hosted parser tests.

Identities are internal to one FS-service lifetime, not persistent media UUIDs,
client handles or authority. Names are never removed. FS-034 adds `Detach`: a
removable disk's endpoint is given back and the name can be attached again
(a stick pulled out and plugged in again). The service invalidates every
handle, cached name and file version on the volume before it detaches it
(Fs_Server.Storage), so a live handle is never redirected to the new disk.
The system volume is never detached.

Hosted tests register two endpoints with the same driver-role hint, then use
their distinct volume indices in the production shared-object table with equal
inode numbers. Metadata updates, aliases and last-close operations stay local
to the proper volume. Failure atomicity, capacity and stable bindings are
regression-tested behavior, not additional functional contracts.

GNATprove discharged **27 checks, zero unproved**, for the volume-list package
under its API preconditions. No Assume, proof suppression or SPARK-Off section
was added. This is runtime-error/flow evidence for the bounded package, not a
proof of FS dispatch, kernel authority, device behavior or service restart.

Native CuBit integration:

```sh
nix develop -c make -C kernel filesystem storage-check
nix develop -c bash tests/headless/run.sh --test storage-grants --accel tcg,thread=multi --timeout 60 --serial /tmp/volume-storage.serial --keep-logs
```

The four-vCPU TCG run passed with RAM and NVMe file handles open together,
different contents, and RAM-alias truncation leaving NVMe contents intact
(`VOLUME-ISOLATION-CHECK: PASS`). Existing directory navigation, rename,
positioned I/O, grant/authority and same-volume coherence checks also passed.
Equal inode numbers and two same-role providers are tested on the host; this
native fixture does not provision two NVMe providers. TCG is not hardware
performance evidence.
