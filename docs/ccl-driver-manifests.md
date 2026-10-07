# CCL manifests for drivers and services

**Status: design, 2026-09-30.** Nothing here is implemented. It closes
SEC-020 (docs/security-hardening.md): every driver and service states its
authority in a CCL manifest; devmgr and procmgr grant what a reviewed policy
approves and nothing else; logging and audit go through shared services.
Related: [device manager plan](device-manager.md), [launch
parameters](ccl-launch-parameters.md), [typed logging](typed-logging.md),
[security model](security-model.md) (§220 manifests, §1075 audit),
[authority policy roadmap](authority-policy-roadmap.md).

## Direction: typed CCL values, no special cases (decided 2026-09-30)

Startup profiles, manifests and service catalogs are **ordinary typed CCL
values**, checked by CCL's type checker, with no hand-written keyword
readers. Their types come from imported interfaces the type checker can
see, and a profile is an argument to operations such as `startup.load`.
The whole startup sequence can then be type-checked and emulated in a
Linux-hosted CCL evaluation (compiler and VM) against a simulated `startup` host.

```lisp
(type Priority     (range 1 10))
(type Microseconds (range 1 1073741824))
(type Milliseconds (range 1 60000))
(type Network_Approval (enum Deny Approve_Declared))
(type Startup_Role     (enum Application Config_Storage))
(type Device_Approval  (enum Manifest_Resources))
;; A Driver is per-device and carries its approval; a Service never gets
;; device resources. The bad combinations cannot be written.
(type Launch_Kind (variant (Service Startup_Role) (Driver Device_Approval)))
;; Built only by startup.realtime, which enforces budget <= period and the
;; kernel's 70% share (a relation between fields, not a range).
(type Realtime_Ceiling (record (budget Microseconds) (period Microseconds)))
(type Scheduling (variant (No_Realtime) (Realtime Realtime_Ceiling)))
(type Readiness  (variant (No_Deadline) (Ready_Within Milliseconds)))
(type Launch (record
  (executable Executable_Name) (priority Priority) (kind Launch_Kind)
  (network Network_Approval) (scheduling Scheduling) (readiness Readiness)
  (after (List Launch))))      ;; values, not names: acyclic by construction
(type Startup_Profile (record (launches (List Launch))))
```

**Rules:**
- **Illegal states are unrepresentable** (variants instead of
  "A requires B" rules).
- **Cross-field invariants** live in imported smart constructors.
- **Dependencies are values.** Immutable values can only refer to values
  that already exist, so startup order is acyclic without any lookup.
- **Range types** also make profiles candidates for formal verification
  later.

**Later: signed packages.** A `Launch` will also name the signed package
its executable must come from: the publisher key and version constraint,
checked before spawn. Signed binaries are central to supply-chain security.
This is not designed yet.

**Value model (decided 2026-09-30): one bounded cell arena per
evaluation.** All compound values (records, variants, lists) live there as
typed cells addressed by index, never by pointer. Object images become
the export/IPC form, produced by copying a subtree. Lists of records, list
fields, nested lists and recursive types all follow from this, and the full
startup profile (about 23 launches plus nested records) fits.

**Language gaps to close first** (probed 2026-09-30):

| Needed | Status |
|---|---|
| enum fields, variants with payloads, nested records | works |
| records as results (canonical literals) | done 2026-09-30 (value arena) |
| lists of records | done 2026-09-30 |
| list-typed record fields, `(list-of T)` | done 2026-09-30 |
| list literals over 16 elements | done 2026-09-30 |
| CCLB compiler/VM parity for all of the above | **not started**; next |
| range types | done 2026-09-30 (subtypes of Integer; interpreter only) |
| recursive types (`after (List Launch)`) | done 2026-09-30 (a list of itself only) |
| named-field record construction | positional only |
| imports that carry types (descriptors are scalar-only) | unsupported |

Each gap lands in the analyser and in the CCLB compiler, verifier and VM
together. (The table is the 2026-09-30 probe. The interpreter was removed
2026-10-05; manifests and profiles are now compiled and run on the VM, so
the "interpreter only" and "not started" entries above are historical.)

**Scaffolding to be replaced.** The keyword forms added earlier on
2026-09-30 are frozen:
- `(launch ...)`, `(approve-device)`, `(after ...)`,
  `(approve-scheduling ...)` and `(ready-deadline-ms ...)` in
  `CCL.Configurations`;
- the driver/resource forms in `CCL.Manifests`.

They will be replaced by "evaluate, then check against the imported type".
Two proved pieces sit below the language and stay: the `.cubit.resources`
decoder and the `Startup_Grants` decision. A typed `Launch` will feed them.

## Where things stand

- **Manifests:** `CCL.Manifests` compiles `(executable-manifest v1 ...)` into
  `.cubit.id/.caps/.access/.streams` sections. About 40 apps and six
  procmgr-started services have one. Only **procmgr** reads them
  (`parseAndGrantManifest`).
- **devmgr** starts all drivers and core services directly
  (`spawnFromBootStorage`), with about 80 literal grants (`mintCap`,
  `grantEndpoint`). Examples:
  - nvme: BAR s4, IRQ s5, DMA s6, notification s7, ready s15;
  - mixer: `CAP_SCHEDULING` 1500/5000 µs.

  A manifest on a devmgr-spawned program has no effect.
- **The language has no driver forms:** no I/O ports, IRQs, device memory,
  DMA, scheduling, process/CSPACE authority, stack size or dependencies.
- **Startup is two-stage.** A fixed Ada order in devmgr, then procmgr's
  `init.ccl` `(startup v1 (start ...))`.
- **Logging:** logstore with a typed client (`CuBit.Logging`) exists.
  - Three components publish to it.
  - The rest print to serial directly: about 650 `debugPrint` calls across
    all 23 services.
- **Audit:** there is no audit service.

## Principles

1. **A manifest is a request, never a grant** (security-model §220). The
   grant comes from policy: the platform's reviewed CCL policy approves each
   request, and the parent (devmgr or procmgr) mints only approved requests.
2. **Hardware facts come from discovery, not from the manifest.** A driver
   asks for "BAR 0 of the device I was matched to, at most 16 KiB", not for
   an address. devmgr binds the request to the real device it enumerated. A
   manifest can never name physical memory or an arbitrary IRQ.
3. **One parser, one proof.** devmgr and procmgr read manifests through the
   same proved decoder, factored out of procmgr into a runtime package.
4. **Named constants, typed values.** Slots, kinds and budgets are CCL
   types. The Ada literals go away (no magic numbers).
5. **Visibility.** Every grant, denial and startup step is a structured log
   record. Security-relevant ones are also audit records.

## Manifest additions

**Implemented in the compiler (2026-09-30):** hosted tests in
`tests/ccl-manifests/test-manifests.py`. Nothing reads the new section yet;
devmgr and startup still use their own grants.

Device and scheduling requests use the existing request machinery. Each has
a binding name, gets a slot from the same allocator as service requests, and
appears in the generated `Slot_<name>` constants. They are emitted into a new
ELF section, `.cubit.resources`, and never into `.cubit.caps`, because device
resources are authorized separately from endpoint delegation.

```lisp
(executable-manifest v1
  (identity "com.cubit.nvme") (version "1")
  (match-pci-class 1 8 2)                    ; class, subclass, prog-if
  (device-memory registers (bar 0) (max-bytes 16384) read-write)
  (interrupt completion msix (vectors 1))    ; or msi (vectors n) / line
  (dma queues (bytes 1048576))
  (request-service filesystem read-write fs))

(executable-manifest v1
  (identity "com.cubit.ps2") (version "1")
  (platform-device ps2-controller)           ; resources from devmgr's platform catalog
  (io-ports data (resource 0) (count 1))
  (io-ports command (resource 1) (count 1))
  (interrupt keyboard (resource 2))
  (interrupt mouse (resource 3)))

(executable-manifest v1
  (identity "com.cubit.mixer") (version "1")
  (request-scheduling realtime-cpu realtime (budget-us 1500) (period-us 5000)))
```

**Rules the compiler enforces:**
- **Match.** At most one per manifest: `match-pci-class`,
  `match-pci-id VENDOR DEVICE` (vendor 0xFFFF rejected), or
  `platform-device` (`ps2-controller`, `ata-primary`, `cmos-rtc`).
- **Resources need a match, and a match needs resources.** Device resources
  require a match, because they are relative to the matched device. A match
  with no resources is rejected as a mistake.
- **Resource indexes.** PCI devices use `(bar 0..5)`; platform devices use
  `(resource 0..7)`. Using the wrong one is rejected. A manifest never names
  a physical address, port number or vector.
- **Sizes and rights:**
  - device memory: page multiples, 4 KiB–256 MiB, `read` or `read-write`;
  - DMA: page multiples, 4 KiB–64 MiB;
  - I/O ports: 1–65,536;
  - MSI/MSI-X: 1–32 vectors.
- **Scheduling.** 1 ≤ budget ≤ period ≤ 2^30 µs, and budget/period ≤ 70%
  (the kernel's `Realtime_Admission.Realtime_Share`). A request the kernel
  could never admit is a compile error.

**Wire format (little-endian):**
- Header: `"CBRS"`, version 1, entry count.
- Match, 16 bytes: kind (0 none, 1 PCI class, 2 PCI ID, 3 platform), a
  reserved byte, three 16-bit values, 8 reserved bytes.
- Entries, 24 bytes each: kind (16 device memory, 17 I/O ports,
  18 interrupt, 19 DMA, 20 scheduling), rights, slot, index, amount, extra.
  - For an interrupt, extra is the mode: 1 MSI-X, 2 MSI, 3 line,
    4 platform line.
  - For scheduling, amount is the budget and extra is the period.

**Decoder:** `CCL.Resource_Sections.Decode` is what startup will use to
read the section.
- **Proved at level 1:** 85 checks, 0 unproved. That covers freedom from
  run-time errors, plus the postcondition that every decoded entry is valid
  for the match and that slots are distinct. Device entries are present
  exactly when there is a match.
- **Treats the bytes as untrusted.** It accepts only what the compiler can
  emit: reserved bytes must be zero, lengths must be exact, slots must be
  1–61 (never 0, 62 or 63), and every limit above is enforced.
- **Single source of truth.** The compiler takes its wire codes and limits
  from this package and decodes its own output before emitting it.
- **Tests:** hosted tests round-trip every draft through the decoder and
  flip random bits (1,400 corruptions) without a crash.
- **Proof command:**
  `gnatprove -P ../tests/ccl-manifests/resources.gpr -u ccl-resource_sections.adb --level=1`
  (run from `kernel/`).

**Not yet designed:**
- readiness deadlines and driver registration (both belong in the startup
  policy);
- bootstrap-only powers: procmgr's process and CSPACE authority, allowed
  only for identities the policy marks as bootstrap (SEC-020);
- launch parameters (ccl-launch-parameters.md).

## The startup supervisor

One small supervisor, `startup`, does spawning and granting. The kernel
starts it first, as the root task. Precedents: Genode's `init` and Fuchsia's
`component_manager` (manifests in CML, compiled to a `.cm` shipped beside the
binary; CuBit instead signs the request into the ELF itself).

**What it does:**
- compiles the CCL startup policy;
- reads each program's manifest with the shared proved reader;
- spawns in dependency order with readiness deadlines, restarts or reports
  exits;
- grants exactly manifest ∩ policy through the graphics agent's
  `Capability_Grants`/broker (the only place grants are made);
- **routes services:** a `(request-service log publish)` or
  `(request-service netmgr config)` is resolved against policy and the
  endpoint capability is placed in the slot the program declared. Programs
  never look services up by name, and there is no global registry to probe;
- records every grant, denial and exit to log and audit.

**What it does not do:** logging (logsvc/logstore), network configuration
(netmgr), device logic (devmgr), or any parsing beyond the one manifest and
policy reader. Kept that small, the goal is a SPARK proof at level 1 (not
yet implemented or proved).

**Agreed with the graphics agent (2026-09-30), after its delegation
primitives were promoted:**
- The GPU admission broker belongs in startup, not in a new devmgr grant
  handler. startup keeps a stable, grantable GPU source endpoint; copies
  given to applications omit GRANT.
- syscall 121 delegates endpoints only. Device resources (BAR/IRQ/DMA) are
  authorized by a separate, device-specific path.
- Admission captures the recipient before evaluating policy, preserves the
  selected source slot, and is asynchronous: startup keeps dispatching
  resource and lifecycle work while an admission is pending. This avoids the
  circular wait where startup blocks on the GPU while the GPU's dependency
  path needs startup.
- The existing Intel bootstrap/resource validation in devmgr stays until an
  equivalent startup migration exists.
- Driver capability slot 62 is reserved for a saved reply capability (the
  GPU's buffer-object reply). It is never allocated from a manifest or
  granted as an ambient resource; the driver catalog allocates only 4–14.

**devmgr becomes dynamic discovery, like udev.** It enumerates PCI and
platform devices, and later hotplug (USB via xhci, PCIe), and holds their
hardware capabilities. It spawns nothing and holds no process or CSPACE
authority (SEC-020's split between bootstrap and steady-state powers).

- **Typed device events:** it publishes `added`, `changed` and `removed`
  events. Each carries a typed device record: bus, vendor/device, class,
  location, and resources discovered.
- **startup matches events to drivers.** It compares each event with the
  drivers' `(match ...)` forms and the policy, then launches the matching
  driver. Hotplug and boot therefore use the same path: boot is just the
  initial burst of `added` events.
- **Resources on request.** When startup launches a driver for a device, it
  asks devmgr for that device's BAR/IRQ/DMA and places them in the driver's
  declared slots. A driver never receives another device's resources. On
  `removed`, startup stops the driver, and the resources are revoked.
- **Visible in CCL.** `devices()` in the REPL is a typed list of the same
  records, and `device-events` is a stream (with a capability-gated
  observe grant), per ccl-system-data.md.

**procmgr** is the likely seed for startup: it already reads `init.ccl` and
applies manifests. Either it grows into the supervisor, or it keeps process
lifetime and accounting only.

The policy extends the existing `(startup v1 (start ...))` profile. These
fields are implemented in `CCL.Configurations` (hosted tests in
`tests/ccl-configurations/test-configurations.py`); nothing acts on them yet.

```lisp
(startup v1
  (start "logstore.svc" (priority 5))
  (start "ps2.drv" (priority 5) (launch per-device) (approve-device)
    (after "logstore.svc") (ready-deadline-ms 2000))
  (start "hda.drv" (priority 5) (launch per-device) (approve-device))
  (start "mixer.svc" (priority 5) (after "logstore.svc" "hda.drv")
    (approve-scheduling realtime (budget-us 1500) (period-us 5000))))
```

**Rules:**
- **`after`** names up to 4 *earlier* entries, each started exactly once,
  so startup order is acyclic by construction.
- **Drivers:** `(launch per-device)` launches the driver for each
  discovered device its manifest matches. It requires `(approve-device)`,
  and `(approve-device)` requires it: only per-device drivers receive
  device resources, and they exist to receive them. A per-device entry
  cannot take the config-storage role.
- **Real-time ceiling:** `approve-scheduling` caps the manifest's
  real-time request; it is never a grant by itself. It is checked against
  the kernel's admission limits (`CCL.Scheduling_Limits`, shared with the
  manifest compiler).
- **`ready-deadline-ms`** is 1–60,000.

**The decision:** `CCL.Startup_Grants.Decide` intersects a decoded
manifest with the program's policy entry. It is proved at level 1
(18 checks) to grant an entry exactly when both allow it, and to deny
everything else:
- device resources only for `(launch per-device) (approve-device)`;
- real-time scheduling only when `approve-scheduling` is present and its
  ceiling covers the request, using the kernel's `Covers` rule.

Each denial carries a reason (`Device_Not_Approved`,
`Scheduling_Not_Approved`, `Scheduling_Exceeds_Ceiling`) for the log and
audit. Deciding grants nothing: startup mints only the granted entries,
through the graphics agent's broker.

Anything requested but not approved is denied, logged and audited; the
program still starts if it can.

## Logging everywhere

- **logstore starts early.** It runs first in devmgr's stage, before any
  driver, so every component can publish from its first line. Until
  logstore is up, the client buffers a bounded early record set and mirrors
  it to serial.
- **Every manifest requests `(log (publish))`.** The policy approves it by
  default, so the publisher endpoint arrives through the manifest, not the
  current special cases (devmgr messages 0x0228/0x0229, procmgr identity
  checks).
- **`debugPrint` becomes a thin shim over `CuBit.Logging.Emit`.** The shim
  has severity, component and a typed payload, keeps a serial mirror for
  boot diagnostics (as today), and is migrated service by service.
- **Records stay typed.** Visibility tools (the REPL, the Observatory) read
  logs as typed lists: `logs() | where(...)`, per ccl-system-data.md.

## Audit service (auditsvc)

Separate from logstore, which may drop records (typed-logging.md says
durable audit must not use its discard contract):

- **Append-only, durable, integrity-chained:** each record carries a hash
  of its predecessor, and gaps and overflow are visible security events
  (security-model §1075).
- **Authenticated sources:** the kernel stamps the publisher identity.
- **Separate authority** for write, read and delete (agent-security §218).
  Delete is not granted to anyone in the default policy.
- **First producers:** devmgr/procmgr grants and denials, capability
  revocations, filesystem ACL changes, config changes, and agent mission
  approvals.

## Enforcement path

1. **Shared decoder:** move procmgr's manifest decoding into
   `CuBit.Manifest_Reader`, to be proved at level 1. procmgr uses it unchanged.
2. **startup reads each manifest** before spawning, intersects it with
   policy, and grants through the broker; devmgr supplies device resources
   for matched drivers only.
3. **Service-to-service endpoints** use the graphics agent's constrained
   endpoint delegation (`CuBit.Capability_Grants`, syscall 120/121), once
   promoted, instead of devmgr minting both ends by hand.
4. **Tests:**
   - a hosted policy × manifest intersection suite;
   - a native `capability-security` extension: a driver whose manifest
     over-asks gets exactly the approved subset, and the denial appears in
     the log and audit;
   - the existing headless suites stay green at every migration step.

## Migration order

1. **Design review** (this doc) and coordination with the graphics agent,
   which is changing the same capability code.
2. **Decoder extraction and language forms** (`userspace/ccl/**`, hosted
   tests): driver forms, `request-service` routing, startup policy fields (done 2026-09-30, hosted). No
   behavior change.
3. **startup** (from procmgr), after the graphics agent's broker is
   promoted; devmgr loses spawn/grant authority.
4. **Pilot: mixer's real-time grant and the ps2 driver,** both through
   manifest ∩ policy, with native tests.
5. **The other drivers** (nvme, ata, xhci, hda, virtio-gpu, virtio-net,
   ramdisk, intel-gpu), one per step, each with its headless tests.
6. **Core services** (filesystem, config, netstack, netmgr, procmgr's
   bootstrap powers).
7. **Logging:** early logstore, then `debugPrint` to the shared client,
   component by component. This can run in parallel with steps 4–6.
8. **auditsvc** and the first producers.

## Decisions needed

- Is the policy file part of `system.ccl` or its own `startup.ccl`?
- Does procmgr grow into startup, or stay as process lifetime only?
- Should an unapproved request fail the start, or start with a subset
  (proposed: subset plus audit)?
- May audit records live on the same volume as logs, or on a separate one?
