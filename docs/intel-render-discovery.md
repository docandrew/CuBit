# Native render discovery boundary

Status: full discovery contract proposed; read-only identity/topology query
implemented. Mesa's topology translation and runtime finalization are tested
separately, but there is no native Mesa physical-device discovery connection.
The main Intel service now answers the same bounded metadata query previously
present only in the private GPU build workspace. This is not a render session.
`DRIVER_GPU = 17` names the existing virtio display registration, not an Intel
or multi-adapter render registry. Do not repurpose it.

## Implemented metadata subset (2026-09-30)

`Intel_GPU_Device_Query` validates label `0x0a20`, exactly four words, zero
flags/reserved fields, and request `[1, selector, 0, 0]`. Selector 0 returns
Intel PCI identity/revision; selector 1 returns retained measured DSS/EU masks.
Replies are `[status, 1, value0, value1]`. Identity render-feature bits remain
zero: a successful query must not be interpreted as allocation/submission
support. Only the measured `8086:46d2` path is currently admitted.

The service responds through the existing caller/reply mechanism; this change
does not grant any application an endpoint or create a global adapter catalog.
The C/Ada query bridge still requires its caller to retain the same authorized
endpoint slot throughout both calls. Epoch-tagged discovery, slot-lifetime
management and a public render transport are not implemented by this subset.

The promoted codec's hosted tests exhaust EU masks, DSS masks, PCI IDs and
reserved fields, and exercise malformed envelopes/request words. Focused
SPARK checks prove its stated response postcondition and termination; they do
not prove hardware observation correctness, endpoint authorization or the
whole driver. The earlier private QEMU query test used synthetic hardware.
The main-service native rebuild remains pending the shared build lock.

## Authority split

Applications receive a query-only render-adapter endpoint through manifest
policy. Neither PCI inspection nor device control, DMA allocation, MMIO, GT
reset, display ownership or firmware upload authority is delegated to Mesa.
Query authority must not imply context creation/submission. Future context
authority is a separate grant, scoped to an adapter and its lifetime.

The owner of the adapter publishes its discovery endpoint; procmgr delegates
the authorized endpoint, not a caller-supplied PID. A multi-adapter catalog
must identify the backing adapter without making its physical address a handle.
Use the existing typed IPC/catalog mechanism; do not add Linux DRM ioctls or
smuggle an untyped pointer through a reply. Large descriptive data belongs in
a bounded shared object with validated lifetime, not arbitrary memory access.

## Minimum discovery semantics

- Identity: protocol/type revision, opaque adapter identity and lifetime epoch,
  admitted PCI vendor/device/revision, and recognized graphics/display IP.
- State: discovered, resetting, firmware-ready, render-ready or failed. A
  successful query does not imply a usable Vulkan physical device.
- Validity: measured topology and timestamp frequency have explicit validity,
  not zero-filled defaults that look like hardware measurements. Snapshot
  fields refer to the same adapter epoch. Partial discovery remains inspectable.
- Limits: only implemented/admitted address widths, allocation alignment,
  memory budgets and engine classes; do not advertise theoretical chip maxima
  that the native driver cannot provide.
- Capabilities: currently supported allocation, address-space, context,
  submission and synchronization operations are independent from display
  modesetting. No render-ready flag until its prerequisites actually succeed.

Mesa may translate a valid measured snapshot and run the upstream finalizer,
but must refuse device exposure while required operations are unavailable.
Device loss invalidates the epoch; stale descriptors cannot authorize reuse
of handles. Software Mesa remains a separate implementation, not a successful
Intel backend fallback disguised as acceleration.

## Integration gate

Shared edits are required in devmgr/procmgr, runtime service identifiers and
manifest/catalog policy. The coordination request is outstanding; no numeric
role/opcode/slot is allocated by this document. Once coordinated, implement
query denial, partial/failed state, unknown revision and stale-epoch tests
before using the endpoint in Mesa. Keep this work distinct from the pending
NUC GGTT takeover evidence.
