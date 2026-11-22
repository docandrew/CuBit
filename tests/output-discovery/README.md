# Typed native output discovery

`CuBit.Output_Discovery` is a pure SPARK, bounded wire codec/responder shared
by the native virtio driver and display broker. It is a control-path snapshot,
not a query performed for each frame or input event.

```
nix develop -c make -C kernel test-output-discovery prove-output-discovery
nix develop -c make -C kernel display virtio-gpu display-check
nix develop -c tests/headless/run.sh --test display-discovery-multi-output --accel kvm --keep-logs
nix develop -c tests/headless/run.sh --test display-grants-virtio-vga --accel kvm --keep-logs
nix develop -c tests/headless/run.sh --test display-discovery-boot-only --accel kvm --keep-logs
nix develop -c tests/headless/run-output-rebind.sh --accel kvm --keep-logs
```

## Meaning and authorization

An authorized caller first requests the catalog revision/count, then requests
each description using that revision and a one-based index. Descriptions echo
both, allowing the caller to reject mixed/stale walks. A changed revision is
`Bad_State`; invalid or noncanonical input is `Bad_Object`. No partial descriptor
is returned on failure. Count zero is a valid backend snapshot, not failure.

Each description distinguishes:

- `Detected_Only`: enabled output reported by the driver; no current resources.
- `Backend_Ready`: resources configured, but not selected for this desktop.
- `Selected_For_Desktop`: the output currently used by the display broker.

Advertised dimensions and current rendering dimensions are distinct. A detected
output has no current dimensions in the Ada type and must encode zero in that
wire word. The current virtio implementation creates resources only for head
zero at 1024x768; advertised sizes on other heads are **not** active modes.

Requests use existing kernel-authorized display/GPU endpoints. They do not
acquire leases, map pixels, create presentation sessions or grant output control.
This does **not** introduce separately delegated discovery-only authority; that
finer split remains policy/interface work. Names and indices are not handles.
The revision is scoped to the authenticated endpoint's lifetime; clients must
discard it on service restart/rebinding, not persist it as a monitor identity.
Registry references remain private to `display.svc`.

The broker gathers and validates the entire GPU snapshot at startup before
publishing it: matching revisions/indices, supported source, unique native head
numbers, supported roles, matching current mode for the selected GPU. It adds a
selected firmware output when the GPU is not primary. When virtio is primary,
the old boot mapping is not advertised as a second independent destination.
A malformed GPU catalog makes broker discovery unavailable without disabling an
otherwise working desktop. The broker's revision follows its output registry;
the metadata-rebind fixture invalidates outstanding discovery queries too.

These are startup snapshots. Hotplug/configuration events and backend restarts
are **not implemented**. Those paths must refresh the full validated catalog and
advance its revision atomically before publishing it, rather than serve stale
startup metadata. Only the selected output is in the native presentation
registry today; detected heads are inventory, not registry-ready work areas.

## Wire format

All successful requests/replies have length four and zero flags/reserved bits.
All unspecified payload bits/words must be zero. Dimensions pack width in low
16 bits and height in the next 16; each dimension is in 1..65535.

| Message | Words 0, 1, 2, 3 |
| --- | --- |
| Catalog request | 0, 0, 0, 0 |
| Catalog reply | success=0, nonzero revision, count (0..17), 0 |
| Description request | nonzero revision, index (1..17), 0, 0 |
| Description reply | revision, metadata, advertised dimensions, current dimensions or zero |

Metadata: index in bits 0..7, source in 8..15, native head (0..15) in 16..31,
role in 32..63. Source is boot framebuffer=1 or virtio GPU=2; role is detected=0,
backend ready=1 or desktop selected=2. The bound allows 16 GPU heads plus one
distinct selected firmware output. Labels are broker 0x090D/0x090E and backend
0x0A08/0x0A09 (catalog/description). Errors are canonical one-word desktop status
replies, never plausible successful descriptions.

## Evidence and limits

Validated on 2026-09-21 in Nix: hosted checks/proofs and QEMU/KVM
`display-discovery-multi-output`, `display-grants-virtio-vga`,
`display-discovery-boot-only`, the output-rebind `display-grants` fixture,
`desktop-display` and `desktop-virtio-vga` all pass. Production display/check
binaries are restored after the rebind fixture.

Hosted regression: **297,906 checks**, including every role/source/native head,
all indices, boundary revisions/dimensions, bit-mutated descriptors, malformed
headers, missing indices and stale revisions. GNATprove: **23 proof diagnostics**,
including runtime safety/termination, query/summary round-trip postconditions,
and canonical `Bad_State` rejection of every valid stale-revision query.
The proof audit rejects missing obligations, skipped proofs and assumptions.
Descriptor round trips and other responder status semantics are regression-tested;
they are not additional functional proof claims.

The native fixture walks the catalog before acquiring a presentation lease,
checks malformed/stale requests, then exercises existing grant/frame behavior.
The three-head fixture requires explicit markers for the selected boot output,
ready virtio head zero, and detected 1920x1080 and 1080x1920 heads. The rebind
fixture additionally requires an old discovery query to fail after revision
rotation. A no-virtio fixture checks the firmware-only path used on hardware
without a native GPU driver. These test real CuBit IPC, not a Linux-hosted
display demonstration.

No proof is claimed for hardware discovery, driver/MMIO/DMA correctness, broker
integration, concurrent hotplug, independent multi-head rendering or physical
presentation timing. Those remain separate work and validation boundaries.
