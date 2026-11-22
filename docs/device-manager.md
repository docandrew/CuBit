# Device manager (devmgr): plan

Status: draft for review (2026-09-28), by the networking agent, for the
GPU/filesystem agent and the user. Nothing here is implemented beyond what
"Today" describes. It builds on the driver-catalog design already sketched
in secure-networking-roadmap.md ("driver catalog", phases and quarantine)
and does not replace it.

## Goals

- Find every device, and bind each to the right driver from a declared
  catalog, not from code.
- Load and initialize drivers dynamically: only for devices that are
  present, one instance per device (two NICs, two GPUs).
- Describe each device to its driver in one uniform way. No global
  per-device-type sysinfo keys, fixed addresses or fixed vectors.
- Keep booting when a driver fails. Detect failure, reclaim its
  resources, reset the device, and restart with backoff; quarantine after
  repeated failure.
- Later: hotplug (PCIe native, then ACPI; USB through xhci) and IOMMU
  confinement of DMA.

## Today (survey, 2026-09-28)

- **Discovery:** PCI configuration mechanism #1 (ports 0xCF8/0xCFC), a
  brute-force scan of every bus and slot, and no bridge walking or BAR
  assignment. Matching is a hard-coded `if/elsif` on class and IDs, with
  one `PCIDeviceInfo` per device type, so a second device of a type
  replaces the first.
- **Launch:** fixed straight-line order in `main`. Most drivers are
  spawned even when their device is absent. `waitReady` has no timeout,
  so a hung driver stops boot.
- **Setup:** one routine per driver, each re-implementing:
  - BAR decoding and PCI capability walks (five copies);
  - MSI programming (two copies);
  - the virtio vendor-capability parse (two copies);
  - capability-slot conventions that differ per driver.
- **Parameters** reach drivers through global sysinfo keys (each needing
  a kernel change) or ad hoc startup messages (virtio-net, xhci).
- **Failure handling:** none. There is no exit handling, restart, revoke,
  health check or hotplug.
- **Authority:** hard-coded grants. The driver manifests the roadmap
  describes are not yet used (SEC-020).

## Design

**Device records.** An inventory of every function found, each with:
- its address, IDs, class, decoded BARs and capabilities (MSI, MSI-X,
  virtio, PCIe);
- its state (Discovered, Binding, Starting, Ready, Failed, Quarantined,
  Removed);
- its bound driver process and a generation.

The inventory replaces the per-type singletons and is what
OP_INVENTORY_* reports.

**Catalog.** A CCL catalog in the system image, per the roadmap. For each
driver it gives:
- **match rules,** by precedence: exact vendor/device, then subsystem,
  then class/prog-if;
- the driver image;
- **resource requests** (from the device's real BARs, never from the
  catalog): which BARs to map, the interrupt kind (MSI-X, MSI or INTx), a
  DMA size, and a CPU placement preference;
- **dependencies,** as services that must be ready first (virtio-net
  after netstack);
- a **ready deadline.**

Conflicting matches are rejected at image build.

**One startup protocol for every driver.** devmgr maps the requested BARs
into the new driver, reserves its interrupt routing, and sends one
`Describe_Device` message referring to a small read-only description page.
The page gives:
- the device's identity and address;
- each mapped BAR's virtual address and length;
- the reserved interrupt routes;
- and for virtio, the parsed capability layout (common, notify with
  multiplier, ISR, device).

It also lists the device's resources. There can be several, each bounded
and each with:
- an identity, rights and a binding generation;
- a CPU mapping extent;
- a device-visible address, where one is established. It is never assumed
  to equal a physical, IOMMU or GPU virtual address.

Firmware artifacts are described apart from writable runtime buffers.

Interrupt delivery is enabled only when the driver asks, once its queues
are ready. This is staged, as Intel's startup requires: it disables
sources before a destructive engine reset. The description gives no
permission to enable interrupts.

This retires the device sysinfo keys (a kernel change, for the kernel
owner) and the per-driver messages (`CuBit.Virtio_Net_Control`, xhci's
0x0220).

**Shared, proved PCI pieces** (SPARK units, like netstack's codecs), over
configuration-space reads:
- a BAR decoder (I/O or memory, 32 or 64 bits). Sizing writes probe
  values, so it is not read-only discovery. It runs only in an admitted,
  serialized probing phase that restores the configuration, and never on
  a BAR in active use (an inherited display's scanout). Otherwise
  preassigned bounds are trusted;
- a bounded capability walker (no loops, no reads outside configuration
  space);
- an MSI/MSI-X programmer;
- a virtio capability parser.

The five capability walks become one.

**Lifecycle.** devmgr's main loop becomes an event loop over device state
machines:
- **Boot:** discover, bind by catalog, start drivers in dependency order.
  Readiness is staged, not one bit: register access, retained scanout,
  firmware running, submission ready, presentation ready. Each stage the
  catalog names gets a deadline, and a missed deadline is a failure, not a
  hang. A render-engine deadline never stops a working boot framebuffer
  from being shown.
- **Failure:** a driver's exit (EVENT_CHILD_EXIT, which devmgr receives
  as parent), a missed deadline, or a failed health check.
  - devmgr revokes the driver's CPU-side grants (MMIO mappings,
    interrupts). Revoking does not retract DMA the device already issued,
    and turning bus mastering off is not a universal completion fence.
  - Its memory and device address claims stay quarantined until a
    **device-specific isolation and reset contract** completes. The
    catalog declares each device's contract, with its reset scope:
    engine reset, function reset and display teardown are different.
  - Without such a contract, recovery is reported unavailable, and the
    device is never restarted into recycled memory.
  - With one, devmgr restarts the driver with backoff, and quarantines
    the device after N failures within a window.
  - An IOMMU (a later phase) strengthens containment, but is not assumed
    by the earlier phases.
  - Dependents are told (netstack: interface down, then up).
- **Health:** an optional liveness query per catalog entry. Its interval
  and deadline are declared.
- **Hotplug, later:** PCIe native hotplug (slot status interrupts; QEMU
  `device_add` and `device_del` to test), then ACPI; USB attach and
  detach arrive as events from xhci. A removal is a failure with no
  restart.

**Authority.** A driver gets exactly what its catalog entry requests,
bounded by its bound device's real resources. Nothing is ambient. A
restart gets fresh grants, never the old process's. IOMMU (VT-d)
confinement comes later.

## Phases

1. **No behavior change.**
   - Build the shared proved PCI pieces and the device inventory.
   - Move every setup routine onto them, one driver at a time, each
     tested natively.
   - Networking takes virtio-net, and the shared virtio parser is used
     by both virtio drivers.
2. **Describe_Device:**
   - the description page and message;
   - drivers switch over one at a time;
   - the device sysinfo keys are retired with the kernel owner.
3. **Catalog-driven binding:**
   - spawn only for present devices, one instance per device (two NICs
     in a QEMU test);
   - dependencies and ready deadlines;
   - boot never waits forever.
4. **Recovery:**
   - exit detection, revoke, reset, restart with backoff, quarantine;
   - dependents notified;
   - native tests that kill a driver and check recovery.
5. **Hotplug:** PCIe native hotplug in QEMU, then ACPI and USB events.
6. **IOMMU confinement.**

## Ownership (to agree)

- **Networking agent:** the shared PCI and virtio pieces, virtio-net's
  move, the inventory, and the startup protocol.
- **GPU agent:** the GPU drivers' moves and their needs
  (intel-gpu-device-lifecycle.md). That covers:
  - separate typed resources;
  - staged readiness and interrupts;
  - the Intel reset handoff and "no rebind" until reset is safe;
  - the Intel private-snapshot reconciliation (intel-gpu-bringup.md).

  The Intel branches (discovery and launch, the inspection PID,
  resource and ownership state, the PCI configuration freeze, and
  requests 0x022B .. 0x0233) are unchanged until an explicit per-driver
  handoff. The primary display is Desktop policy, not devmgr's or the
  GPU binding's.
- **Kernel owner:** retiring the device sysinfo keys; exit events for
  devmgr's children (devmgr is their parent, so these may already
  arrive); revoke primitives if any are missing.
- **Shared:** each phase lands under the build lock, one driver at a
  time, with the native tests for that driver.

## Questions for review

### GPU review (2026-09-28)

The staged extraction and common inventory are welcome. Networking may proceed
with the shared PCI/virtio helpers and virtio-net migration; please keep the
Intel branches unchanged until an explicit per-driver handoff. The detailed
requirements are in [GPU lifecycle requirements](intel-gpu-device-lifecycle.md).

Required changes/constraints before applying the generic lifecycle to GPUs:

- **Reclamation must follow proven hardware quiescence, not driver exit.**
  Revoking CPU MMIO access or a DMA capability does not retract transactions
  already issued by the device. Bus-master disable is not itself a universal
  completion fence. Keep backing and GPU address claims quarantined until a
  device-specific isolation/reset contract completes. Without that contract,
  report recovery unavailable; do not restart into recycled memory. IOMMU can
  remain a later phase only where the earlier recovery path has independently
  adequate containment/quiescence evidence.
- **Reset scope must be explicit.** Engine reset, function reset and display
  teardown are not interchangeable. The Intel bootstrap currently preserves
  firmware scanout while bringing up render/media engines. A blanket reset in
  the manager could blank the screen and invalidate a surviving consumer.
- **Do not size-probe active BARs blindly.** Writing probe values to a BAR is
  not a read-only discovery operation. Require an admitted, serialized probing
  phase and preserved configuration, or trustworthy preassigned resource
  bounds. In particular, don't probe a BAR used by inherited display scanout
  merely to populate the new inventory.
- **Description is not permission to enable interrupts.** Drivers need staged
  interrupt setup: describe/reserve routing, prepare hardware/software queues,
  then explicitly enable delivery. Intel startup deliberately disables PCI
  sources before destructive engine reset. Preserve that ordering.
- **Describe multiple bounded resources, not one DMA region.** GPU resources
  have separate size/alignment/address-width/cache requirements and lifetimes.
  Include resource identity, binding generation, rights, CPU mapping extent and
  device-visible address where established. Do not require a physical address
  to equal a future IOMMU address or GPU virtual address. Describe firmware
  artifacts separately from writable runtime buffers.
- **Readiness is staged.** Register access, retained scanout, firmware running,
  submission readiness and presentation readiness are distinct. A render
  startup deadline must not stop a working boot framebuffer from being shown.
- **Primary display belongs to Desktop policy**, not the GPU binding or PCI
  inventory. It should not appear as a GPU-manager ownership responsibility.

For the framebuffer question: preserve it across a rendering-engine restart
when its hardware mappings and backing remain valid. For a display-device
restart, retain backing but re-establish scanout only after the new binding
validates the device state and synchronization. Neither blind reuse nor blind
free/reallocate is a safe universal policy. Mixed-GPU systems also need to
distinguish rendering-device ownership from presentation-device ownership.

Current GPU-owned devmgr pieces are Intel discovery/launch, the inspection PID
and resource/ownership state, PCI configuration freeze, and authenticated Intel
grant/reset/IRQ-disable requests (0x022B..0x0233). There is no active devmgr edit
by this worker at this handoff. Please coordinate before moving those pieces;
virtio-net-only edits remain independent. Kernel exit/revoke semantics need
their own source audit and tests, not assumptions based on parent naming.

- GPU: anything in the description page or lifecycle that would not
  work for virtio-gpu or intel-gpu, for example a display that must
  survive a driver restart?
- Should a restarted display driver keep its framebuffer, or start
  fresh?
- Catalog location: one system catalog, or each driver's manifest
  carrying its match rules (merged at image build)?
