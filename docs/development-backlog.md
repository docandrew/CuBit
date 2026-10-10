# CuBit Development Backlog

This is the short operational backlog for user-visible defects and engineering
work that does not belong in the security-hardening ledger. Design requirements
remain in their subsystem documents.

## Authority delegation

- [ ] Audit and simplify devmgr/procmgr bootstrap authority and delegation.
  Separate service use from grant-making powers, constrain steady-state brokers,
  and make each grant chain understandable and inspectable. Track acceptance
  criteria in [SEC-020](security-hardening.md#sec-020--simplify-and-constrain-bootstrap-authority-delegation).

### CTX-001 — Security contexts (user-requested, 2026-10-04)

The authority a program gets should depend on the context it runs in, not
only on its manifest and its launcher. The user's goal: open a console in a
directory, and nothing run from it can affect anything outside that
directory, except perhaps a temporary place. Delegated places
(`CuBit.Launch_Grants`, docs/self-hosting.md item 4) and procmgr's
attenuation give the mechanism: a child holds at most what its launcher
holds. What is missing is a first-class description of the context the
launcher itself holds authority in. Questions until now dodged:
- **Identity.** Which user (or agent) a session acts for, how a process
  carries that identity, and how services check it, beyond process IDs and
  capabilities.
- **Session kind.** Logged in locally, remotely (Observatory web REPL, CCL
  remote), or as an automated agent. The same program could warrant
  different authority in each.
- **Location and network.** On a VPN, an untrusted network or an isolated
  device, as an input to which grants a session may receive or delegate.
- **Confined consoles and workspaces.** A console started in a directory
  gets a context that names that directory (and a temporary place) as all
  it may write. Everything launched from it inherits the context, and its
  delegation and attenuation can never widen it.
- **Visibility.** The capability graph (declared, granted, exercised,
  denied) should show each context and why a grant was refused under it.
- **Revocation.** Ending a session or leaving a network revokes what was
  granted for it, including from running children.

Acceptance: a design document covering identity, session kinds, context
inheritance and attenuation, network and location inputs, revocation and
inspection, reviewed with the user before code; then a first slice in which
a console confined to a directory cannot write outside it (plus a temporary
place), shown by a guest test. Related: SEC-020 (bootstrap delegation),
docs/stream-wiring.md (approvals), and the capability graph plan.

## Builds

### BLD-002 — Kernel assembly to GNU as, Intel syntax (user, 2026-10-05)

Status: open, after the self-hosting tools (binutils, jj, gcc, GNAT): until
jj runs on CuBit there is no way to clone the tree there anyway.

The kernel and runtime use yasm for about 1,200 lines in eight files:
- `kernel/src/`: `boot.asm`, `boot_ap.asm`, `interrupt_handlers.asm`,
  `syscall_entry.asm`, `context_switch.asm`, and `init.asm`, which is a
  flat binary;
- `userspace/runtime/gnat/art0.asm`;
- `userspace/apps/capability-test/syscall_registers.asm`.

Move them to GNU `as` with `.intel_syntax noprefix`. This keeps Intel
syntax, needs no yasm port (yasm is unmaintained), and means `as`, already
ported, can build CuBit on CuBit.
- Translate the few NASM features: `%include`, three `%macro`, one `%rep`,
  `%assign`, `section`/`bits`, `times`/`equ`. Write explicit `qword ptr`
  and `offset`.
- Build `init.asm` with `as` and `objcopy -O binary`.
- Verify each file mechanically: assemble it both ways and compare the code
  bytes and relocations, which must be byte-identical.
- The SameBoy boot ROMs (Game Boy assembly) are a separate toolchain and out
  of scope.

### SELF-001 — Back to self-hosting: develop CuBit on CuBit (user, 2026-10-08)

The user: "add a task for us to get back to the original self-hosting goal."
The plan and its status are in docs/self-hosting.md.

Where it stopped (2026-10-05):
- binutils 2.46 runs on CuBit (`as` and `ld` with typed parameters).
- GCC 15.3 `cc1` and the driver compile C to byte-identical objects.
- Remaining: a musl `libgnat`, then `gnat1 -c`, `gnatbind` and the link
  through `gcc`.
- Also not done: gprbuild's replacement (BLD-001, the CCL build tool), git/jj,
  and the hardening items in docs/self-hosting.md (D4–D7: partial `munmap`,
  response files for long links).

Direction, in order:
1. **Re-run where it left off.** Run the `binutils` and `gcc` guest tests on
   today's tree. KERN-003 changed process identities and grant mappings
   under them.
2. **GNAT,** items 4–5 of the plan: `gnat1 -c hello.adb`, `gnatbind`, the
   link.
3. **The CCL build tool (BLD-001)** builds one real userspace component, then
   the kernel.
4. **Milestone:** edit, build and boot a changed CuBit service, on CuBit.

### BLD-001 — Content-addressable CCL builds, like Nix (user-requested, 2026-10-04)

The CCL build tool that replaces make (docs/self-hosting.md, item 6) should
be a full content-addressable build system in the spirit of Nix, not a
timestamp-driven rule runner:
- **Every input is named by its content.** Sources, tools (as, ld, gcc,
  GNAT and their manifests), program descriptions, parameters and the
  rendered arguments are hashed (SHA-256 through the verified SPARK crypto
  stack, SPARKTLSCrypto over SPARKNaCl). A build step's identity is the
  hash of all of it.
- **Outputs are stored by that identity** in the Storehouse (FS-020), of immutable,
  read-only entries. A step whose identity is already present is not run
  again, and the same inputs give the same output on any CuBit machine.
- **Typed steps.** A step is a typed tool call (`(ld.run (Ld_Parameters
  ...))`, docs/ccl-launch-parameters.md), so each step's inputs and outputs
  are exactly its file parameters and declared ports. The delegated places
  give the sandbox: a step can read only its inputs and write only its
  outputs, so undeclared dependencies cannot creep in.
- **Visible.** Every step and Storehouse entry appears in the capability and
  stream graphs: why it ran, what it read and wrote, and its ports' output
  as cards.
- **The store's place and views** are decided in FS-020 (Storehouse and
  views, 2026-10-05): build outputs live in the Storehouse,
  `@system/Storehouse/<hash>/`, written only by the software manager, browsed
  through generated `Applications/` views, with garbage collection by
  generation.
- **Open questions:** garbage collection details;
  sharing and substitution between machines (signed, through tls.svc);
  pinning the bootstrap toolchain; how the Storehouse relates to the filesystem
  journal and to Git and jj history; and whether CCL definitions
  themselves (packages, docs/ccl-packages.md) are Storehouse entries.

## Code organization

### Qualified CCL names and debugger outcomes

- [ ] Give qualified names (`Type.Alternative`, service operations) room for
  their separately bounded components. The parser currently applies the same
  32-character bound to the whole token as to a single type name. Add boundary
  and BASIC/Lisp round-trip tests; do not silently truncate discovered names.
- [x] Use shared readable VM/parser status labels in Workbench rather than
  enumeration images from the minimal Ada runtime. Hosted diagnostics pass;
  native rebuild/visual verification pending the shared build handoff.
- [ ] Expand aggregate result/locals inspection beyond `<native object>`.

### Retire `apps/shell` (user-approved, 2026-10-03)

- [ ] Delete `userspace/apps/shell/`. The CCL console replaces it, and git
  holds the history. One change, under the build lock, announced in
  `coordination/` first, because the compositor and graphics agents' tests
  depend on it:
  1. `kernel/Makefile`: remove the `shell` target, its `world` entry and
     `isodir/boot/shell.app` from `DISK_CONTENTS`.
  2. Init profiles: stop starting `shell.app` in
     `tests/headless/init-desktop-display.ccl`, `init-desktop-session.ccl`,
     `init-servo.ccl` and `tests/observatory-metrics/init-viewer.ccl`.
  3. `tests/headless/run.sh`:
     - Replace the `shell:` markers in `boot-shell-nvme` (procmgr/ps2
       readiness; rename the test), `desktop-display`, `desktop-virtio-vga`,
       `virtio-gpu`, `virtio-gpu-multi-output` and `virtio-vga-primary`.
     - Delete the shell install block (about lines 937–948).
  4. Mark it retired in `docs/ccl-repl.md` and `docs/ccl-system-data.md`.
  5. Verify with `make -C kernel world` and the six affected headless tests.

### CCL-005 — Named slots from CCL; no hand-numbered capability or reply slots (user, 2026-10-09)

The user: CCL should define names that the slot system refers to, so slot
numbers update themselves instead of being hand-assigned. Today services mix
generated bindings with literals. Examples in intel-gpu `main.adb`:
`capSubmit (15, ...)`, `Budget_Reply_Slot := 61`, `Activation_Reply_Slot := 60`,
`Application_Reply_Slot := 62`, and the new `Submission_Reply_Slot` (59),
whose uniqueness was checked by reading the code by hand. Some services
already use generated `CCL_Manifest_Bindings` (`Slot_Files_Filesystem`).
- **Declare by name.** A manifest names every capability slot (the endpoints
  it is granted) and every reply slot it holds (saved reply capabilities).
- **Generate, never write.** The manifest compiler assigns the numbers,
  checks they are unique and within range, and generates the bindings
  package each program uses (`CCL_Manifest_Bindings.Slot_GPU`,
  `Reply_Slot_Submission`). No numeric slot literal survives in program code.
- **Typed.** A slot is a typed CCL value checked by the type system (no
  keyword readers). A grant and the code that uses it agree because both
  come from one declaration.
- **Checked at build:** a static check fails the build on any remaining
  literal passed to `capCall`/`capSubmit`/`saveReplyCap`/`replyCap`.
- Migrate every service, driver and app. intel-gpu, display, desktop and
  devmgr first: they have the most slots.

### CODE-001 — Formatting and readability pass (user, 2026-10-09)

The user: many source files have no comments and no blank lines between
subprogram declarations. Make the code read well, without changing
behaviour:
- **Layout:** a blank line between subprograms; group declarations by
  concern with a short heading comment.
- **Comments:** each package and non-trivial subprogram says what it is for,
  in the surrounding code's voice. No restating the code.
- **Formatter:** pick a GNAT-native formatter configuration (gnatformat /
  gnatpp) matching the project's style, check it in, and run it per package.
  Formatting-only commits are kept separate from logic changes.
- **Safety:** no behaviour change. Proofs are rerun on SPARK units and every
  headless test affected by each batch must pass. One owner per area, under
  the build lock, coordinated through `coordination/` so it never collides
  with feature work. Biggest and most-edited files first: desktop, intel-gpu,
  display, filesystem, devmgr, procmgr. Pairs naturally with splitting the
  monolithic `main.adb` programs below.

### Break up monolithic `main.adb` programs

- [ ] Split large services and apps into a pure, testable core (SPARK where
  it holds invariants) plus a thin IPC adapter in `main.adb`, one module per
  concern. Keep behavior unchanged and run the headless tests at each step.
  Each owner refactors its own files.
  - **Largest:** desktop (8,647 lines), intel-gpu (5,673), filesystem
    (3,887), devmgr (3,811), procmgr (3,287), display (1,718).
  - **Order:** procmgr, filesystem, desktop/display, intel-gpu.
  - **Desktop:** the compositor and window manager stay in one process
    (user's choice, Windows style), as modules: window management,
    taskbar/menu, settings, scene, input routing. Decide whether
    `desktop-shell` still has a purpose.

### Separate the CuBit runtime library from GNAT internals

- [ ] Move public CuBit runtime APIs and reusable IPC machinery out of
  `userspace/runtime/gnat` into a dedicated common library (for example,
  `userspace/lib/cubit`; final location to be decided during the inventory).
  Keep GNAT implementation units and Ada runtime support in the GNAT tree.
- [ ] Inventory dependencies first: distinguish portable protocol/type/lifetime
  logic, native syscall adapters, service-specific clients, and compiler runtime
  internals. Build on existing subsystem libraries rather than duplicating them.
  Shared machinery such as `CuBit.Async_Requests` must remain usable outside CCL
  and Config, with no added payload copies or authority semantics.
- [ ] Give the common library explicit project/build boundaries; update native
  linking, source lists, hosted tests and SPARK projects so portable code can be
  tested and proved without pulling in GNAT implementation units.
- [ ] Validate the move with focused proofs/regressions, native application
  builds and QEMU smoke tests. Remove superseded paths rather than retaining
  compatibility copies. This is an organizational refactor, not an IPC redesign.

## Nonblocking GPU follow-up

### GPU-001 — Asynchronous GPU submission and completion (user, 2026-10-09)

The NUC freeze (IPC-004, IPC-005) traced to intel-gpu busy-waiting inside
request handlers: every submit waits for the GPU, every VM update parks
contexts with a GuC round trip. The user: start on async submit and
completion now, as the base for Vulkan games, 3D applications and video.
Design doc to follow (current path mapped first). Direction: per-session
submission queue in shared memory; a GPU-written per-context timeline page
the client reads without IPC; waits woken from the driver's event loop (then
MSI); batched VM updates; at least two frames in flight in Desktop; hang and
device-lost detection per context. Steps land one at a time, each tested.
Design: [gpu-async-submission.md](gpu-async-submission.md).

- [x] Step 1 (2026-10-09): driver-loop completion, no in-handler busy-wait.
- [x] Step 2 (2026-10-10, driver side): session queue over a Channel_Protocol
  open, up to 32 jobs in flight per context, 64-bit timelines, one kick per
  context per turn, the loop serving completions and wakes for every
  session; VM updates park only with nothing in flight (proved rule).
  Hosted and proved; NUC validation pending.
- [x] Step 3 (2026-10-10, Mesa): `anv_cubit_queue_exec_locked`/`_async` write
  descriptors and return; GPU timeline `vk_sync` (points resolved from the
  status lines, `OP_GPU_WAKE` waits, `WAIT_PENDING`); per-queue lock, no
  `lifetime_mutex` on submit/completion/VM update paths; synchronous
  `0x0A27` client binding removed. Hosted and proved; NUC validation pending.
- [ ] The read-only one-page timeline grant (PPHWSP `+0x200`) for Mesa's
  `get_value`, replacing the status-line copy.
- [ ] Wakes for every session: the 64-slot capability table leaves the
  driver one saved-reply slot for `OP_GPU_WAKE` (others get `Not_Held`).
- [ ] Steps 4-8 as in the design doc.

### GPU-002 — Vulkan games and 3D applications on the iGPU (user, 2026-10-09)

Applications, not just Desktop, get their own GPU sessions through Mesa ANV:
presentation to the compositor (WSI), timeline semaphores, multiple queues,
large buffers (the item below), conformance subsets (dEQP-VK) and a demo
title. Depends on GPU-001.

### GPU-003 — Hardware video acceleration (user, 2026-10-09)

Decode (and later encode) on the media engines (VCS/VECS) through a
VA-API-style interface for players and browsers (Penny/Servo). Reuses
GPU-001's per-engine queues and timelines; needs HuC firmware loading and
authentication on Gen12.
Design: [gpu-video-decode.md](gpu-video-decode.md) (2026-10-09): Vulkan Video
through ANV, not VA-API/iHD; ANV decode needs no HuC, so HuC moves off the
critical path. Standalone fuse, HuC and VCS segment units are built and proved.

- [ ] Support large individual GPU buffers, not just a growable total arena.
  The 2026-10-07 source audit still finds a 16 MiB per-buffer ceiling in
  `anv_cubit_memory.c`, native buffer adapters, submission/cache operations and
  GGTT helpers. A 3840x2160 RGBA8 surface alone needs 33,177,600 bytes before
  padding. Audit the inherited ANV `maxMemoryAllocationSize`, `maxBufferSize`
  and buffer-range properties against actual CuBit limits; allocation failure
  due to a permanent protocol ceiling is not transient budget exhaustion.
  Implement large logical buffers over separately accounted physical extents,
  with bounded allocation, map/bind/unbind and retirement steps. Preserve
  authenticated identities and checked 64-bit offsets across every chunk;
  do not merely increase a fixed bound or introduce unbounded page loops.
  Required acceptance includes 4K targets, allocations spanning many extents,
  near-boundary sizes/offset overflow, partial allocation/map failures,
  asynchronous cleanup and exact budget restoration after real reclamation.
  Advertised limits must be tested through the public Vulkan query path.

- [ ] Implement authenticated physical GPU-backing retirement, separately from
  logical slice reuse. Follow the [retirement design](intel-gpu-physical-retirement.md):
  stable allocation identities, per-extent view/publication drain, confirmed
  GPU/TLB/CPU retirement, hole-preserving residency and actual-reclamation refunds.
  Kernel-owner coordination and native/hardware acceptance remain pending.

- **2026-09-22 progress:** bounded pre-paint input dispatch passed an A/B/B/A
  native loaded comparison (repainting p99 <=0.277 ms versus <=1.381 ms; not
  photons). Mixed 1024x768 + 1280x720 output pixels, pointer confinement and
  cleanup are native-tested. Shared display models moved to `userspace/lib/display`.
  Preferred base EDID parsing/storage sizing and pointer containment are SPARK
  checked. Settings exposes a read-only live Displays page.
- Next: generation-bound surface logical/raster size and scale acknowledgement,
  toolkit relayout/rasterization, then native mixed-DPI crossing regressions.
  Rotation/scale controls must wait for an implemented renderer, not merely the
  already-proved geometry math. See the next-boundary section of the display doc.
- Broker startup handoff now permits different native/firmware dimensions and
  never falls back to firmware after GPU failure; native changed-resolution and
  two-output clear-failure regressions cover it. **Kernel console retirement is
  still required for full hardware takeover:** MAPFB suppresses normal mirroring
  but the last-chance handler can re-enable an obsolete framebuffer. Define an
  irreversible, authorized retirement with writer quiescence and panic policy;
  do not merely null a callback concurrently with an executing writer.
  GTK rewrites both display hints and its synthesized EDID; persist desired CCL
  mode policy separately from transient host observations.


### GPU-004 — Batched source uploads and reversible software fallback (2026-10-09)

Persistent client sources landed (docs/gpu-rendering-and-presentation.md,
"Persistent client sources"): steady-state publication no longer allocates,
glyph cells are allocated together at renderer start, the scene budget is
2048 layers, and the runtime switch is judged by time and progress.
Remaining: (1) every stale surface still costs its own transfer submission
and a discarded capture; batch all stale row bands into one transfer before
capture (one submission, no recapture per surface). (2) The runtime software
switch (now only for a real stall) is one-way; reopen the Vulkan device from
`Retired`, let `Compositor_Backend_Selection` return to GPU after
`Recovered`, and retry after an explicit deadline. (3) Split frames beyond
the layer budget into damage bands. All need NUC validation.

### VM-first accelerated desktop experience (user-requested, 2026-10-07)

Initial users should be able to try CuBit in a VM with responsive input,
accelerated composition and 3D applications, working audio, and usable display
configuration. This complements, rather than replaces, native Intel bring-up.
Current virtio-gpu support is a 2D resource/transfer/scanout path; enabling the
Vulkan compositor alone does not give that path host-GPU acceleration.

- [ ] **Virtio Vulkan backend:** evaluate and port Mesa Venus using the existing
  Mesa/runtime integration. Implement negotiated capability sets, contexts,
  command submission, blob/host-visible resources and synchronization in the
  CuBit virtio driver/adapter. Check pinned QEMU, host renderer and host Vulkan
  requirements before selecting a supported launch profile; do not assume
  Linux guest ioctls or services exist in CuBit. Keep 2D fallback explicit.
- [ ] **Resource safety:** authenticate context/resource ownership, use stable
  generation-bound identities and growable metadata, and account separately
  for guest RAM, host-visible apertures and device allocations. Bound service
  work; require completion and CPU/display reader retirement before reuse.
  Validate lengths, offsets, feature combinations and host responses.
- [ ] **Accelerated compositor and apps:** discover the virtio Vulkan device,
  exercise composition and the animated teapot gallery, and connect rendered
  resources to presentation with explicit acquire/release synchronization.
  Reduce avoidable readback/copies without bypassing buffer-lifetime checks.
  Report the selected renderer and distinguish host hardware from software
  Vulkan; API availability or a successful submission is not acceleration proof.
- [ ] **Virtual modesetting:** extend existing EDID/startup scanout selection
  with runtime resolution changes, safe resource replacement, output-generation
  changes, compositor relayout, Settings Apply/Revert, resize and multi-output
  tests. No dependency on 3D support; no claim of physical Intel link validation.
- [ ] **Audio and input acceptance:** choose a documented virtual audio device
  supported by CuBit (do not require virtio-snd merely because graphics uses
  virtio). Verify audible playback, volume/mute, underrun recovery and input
  responsiveness while 3D and window resizing run concurrently. Automated
  service/grant tests supplement, not replace, end-to-end playback checks.
- [ ] **Reproducible VM acceptance:** publish an exact tested launcher and
  host prerequisites, plus a usable nonaccelerated fallback. Test cold boot,
  application startup, sustained animation with frame-time distributions,
  audio/input under load, repeated resize, resource exhaustion and cleanup.
  Compare software and host-accelerated paths under controlled host load;
  record guest CPU virtualization and renderer separately. These results prove
  the VM experience, not CuBit's physical Intel command submission or modesetting.

Reference protocols: [Mesa Venus](https://docs.mesa3d.org/drivers/venus.html) and
[QEMU virtio-gpu](https://www.qemu.org/docs/master/system/devices/virtio/virtio-gpu.html).

### Native Intel modesetting and monitor discovery

These remain open hardware milestones, separate from working Mesa rendering
and composition into the firmware-selected display mode.

- [ ] **EDID discovery:** read monitor data over the connector's supported
  DDC/AUX path; validate checksums and lengths, bound extension parsing, and
  enumerate EDID/DisplayID timings. Handle missing/malformed data and explicit
  user overrides. Monitor identity is descriptive metadata, not authority.
- [ ] **Mode admission:** intersect advertised timings with the exact GPU,
  connector/link, clock and scanout-format limits. Distinguish advertised,
  selected, active and measured refresh rates; do not promise 144/240 Hz from
  EDID alone.
- [ ] **Native modesetting:** implement documented platform-specific power,
  clock/PLL, pipe/plane and link programming behind display.svc's authenticated
  output ownership. Use representation clauses for register subfields and
  validate those fields against upstream hardware documentation/Linux behavior.
- [ ] **Safe transitions:** quiesce old writers and presentations, retire the
  firmware/kernel-console ownership, and implement bounded transactional mode
  changes with Settings Apply/Revert. Preserve buffer/fence lifetimes; never
  revive a stale firmware framebuffer on failure.
- [ ] **Hotplug and multiple outputs:** re-probe capabilities on connection
  changes, invalidate stale output generations, and support independent modes
  on multiple connectors/adapters without conflating render and display owners.
- [ ] **Hardware acceptance:** on the NUC, verify mode changes, actual active
  timing, timeout/revert, unplug/replug and multiple outputs where available.
  Exercise malformed monitor data in hosted tests; QEMU coverage does not
  validate physical Intel link training or modesetting.

See [Intel bring-up milestones](intel-gpu-bringup.md#milestones-with-visible-acceptance-criteria)
and [display outputs and scaling](display-outputs-and-scaling.md).

Session presentation now uses asynchronous broker submissions and IRQ-driven,
fenced per-head GPU commands. The native delayed-head test checks independent
progress and busy-output lifetime protection. Before calling this a speed win:

- Measure and reduce async dispatch/wakeup overhead. The first loaded one-vCPU
  repaint run moved p99 from <=1.381 ms to <=1.933 ms; fewer >=1 ms samples do
  not cancel out the worse tail. Preserve fences and source-buffer ownership.
- Add native malformed-fence, missing-completion/timeout and backend-death
  injection tests. Quarantine is implemented; those failure paths are not yet
  covered by the new delayed-head test. Adapter reset/recovery remains separate.
- Migrate remaining synchronous broker configuration and shell presentation
  operations; mixed active modes/DPI remain the next display feature milestone.
- Make serial/debug records atomic or buffered across CPUs. A GTK smoke test
  reached the Desktop but missed its readiness marker because simultaneous
  startup strings interleaved. The redundant per-head announcement was reduced,
  but this is not a fix for the general logging problem.

See [the protocol boundary](display-outputs-and-scaling.md#nonblocking-session-presentation)
and [measurements](../tests/performance/graphics-results.md#nonblocking-brokergpu-session-presentation).

## Userspace allocation

### LOG-001 — Logging stalls render loops; publish through a shared ring (user, 2026-10-05)

Status: publisher rings done (2026-10-05). Guest test `log-authority`
passes on CuBit (TCG):
- 3,000 records written in 14 ms (about 4.7 µs each, encoding included);
  978 were shed when the 64 KiB ring outran logstore's drain, all counted,
  none blocking;
- delivery in order with identity, Detach and grant retirement, and a
  collector dying in Attach all pass.

Still open:
- the graphics agent removing its adapter's pacing (see my coordination
  note);
- removing the no-op `Pump`, `Collect`, `Set_Delivery` and `Complete` once
  their callers drop them;
- logstore learning about publisher exits from procmgr;
- a budget in bytes;
- the serial echo.

Original report: logging causes visible stutter in the graphics agent's
benchmark rendering. The likely causes are in the client path
(`userspace/runtime/gnat/cubit-logging.adb`):
- **Every record is an IPC.** `Announce` and `Publish_Now` call logstore
  synchronously, one round trip per record, so the caller waits for the
  reply. `Emit` is asynchronous but allows only one record in flight, and a
  second record is dropped.
- **Shared rings only on the read side.** logstore writes each subscriber's
  ring (`CuBit.Log_Streams`); publishers have none.
- **`debugPrint` is synchronous serial I/O** (see "Console output is
  synchronous serial I/O" below). A program that logs this way per frame
  stalls on the UART.

**Fix:** each publisher gets a single-producer ring (`CuBit.Datagram_Rings`),
lent to logstore once.
- Logging becomes a copy into the ring: no IPC, no wait.
- logstore drains publisher rings in batches, woken only when a ring goes
  from empty to non-empty, or on its own timer.
- When a ring is full, records are shed and counted, with the count
  reported in the stream (the log persistence design's shed-by-default
  back pressure).
- Severity filtering stays on the client (`Wanted`), from a published
  minimum.
- The synchronous `Announce` stays only for startup records, where waiting
  is harmless.

**First, measure on CuBit, before and after:**
- which path the benchmark uses (`Announce`, `Emit` or `debugPrint`);
- time per record on each path;
- frame-time jitter with logging on and off.

Coordinate with the graphics agent on the benchmark and its log rate. I
own logsvc, logstore and the log protocol.

This is step 1 of the roadmap in docs/logstore-architecture.md ("Publisher
rings", planned 2026-10-03 when the reader rings landed). It was never done
because the roadmap was not mirrored here.

### EXC-001 — Full Ada exceptions in the user runtime (user decision, 2026-10-05)

Status: next, before the growable secondary stack, which then takes upstream
GNAT's `System.Secondary_Stack` (heap-allocated chunks) with its
`Storage_Error` handler intact.

**Policy (user):**
- Fully proven SPARK code compiled with `-gnatp` never raises, so exceptions
  cost it nothing.
- Ada code without `SPARK_Mode => On` may use exceptions, and is handled
  gracefully like Ada on any other platform.
- An unhandled exception is a way to stop an app cleanly, or (a goal) to
  restart a service.

**Steps:**
1. **Zero-cost exceptions:**
   - the full exception units (`a-except`, `a-exexpr`, `s-except`,
     `s-exctab`, `s-traceb` and the rest);
   - GNAT's personality routine (`raise-gcc.c`);
   - libgcc's DWARF unwinder. It already runs in CuBit processes: the
     `cxx-check` guest test passes with C++ exceptions.

   The unwinder finds tables through `dl_iterate_phdr`, which the CuBit libc
   provides, so native Ada programs link `libc.a` too. The libc is mostly Ada
   and shares the heap already.
2. **Restrictions:** drop `No_Exception_Propagation`, `No_Exception_Handlers`
   and `No_Exception_Registration` from the user runtime's `system.ads`. Proven
   units keep `-gnatp`.
3. **Last-chance handler:**
   - the exception's name, message and traceback, sent to logstore;
   - a typed run outcome the console shows (`Run_Outcome` gains a variant
     such as `Failed (Unhandled_Exception ...)`).
4. **Supervision (follow-on design):** a supervisor (procmgr, or startup
   declarations in CCL) restarts a service that ended with an unhandled
   exception, with limits.
5. **Then the upstream secondary stack:** heap exhaustion becomes a
   catchable `Storage_Error`.
6. **Guest tests:**
   - raise and handle across subprograms;
   - `Constraint_Error` from a check;
   - secondary-stack exhaustion caught;
   - an unhandled exception arriving as a run's outcome in the console.

**Phase 2: more of the upstream GNAT runtime, selectively (user,
2026-10-05).** Import units from upstream (`adainclude` of the pinned GNAT),
changed only where CuBit differs, each with a guest test:
- **Now:** exceptions and the secondary stack (above); `Ada.Strings.*`
  (fixed, bounded, unbounded); `Interfaces.C.Strings`; numerics;
  `Ada.Characters.*`; `Ada.Containers` once `No_Finalization` is lifted
  (controlled types).
- **Over CuBit services:** `Ada.Text_IO` and streams over the libc (CuBit
  names, no stdin/stdout/stderr assumptions); `Ada.Calendar` and
  `Ada.Real_Time` over the clock service.
- **Not now:** tasking (TASK-001); `Ada.Directories` and `GNAT.OS_Lib`
  (Unix paths and processes); anything that forks or uses signals.

**Costs:** unwind tables (`.eh_frame`) in binaries, the libc in native Ada
links, and exception paths in unproven services. The kernel's own ZFP
runtime (`kernel/runtime/`) is unaffected.

### TASK-001 — Ada tasking in services (user, 2026-10-05)

Status: open, after ALLOC-001's thread caches and ALLOC-002's growable
secondary stack. Services are single-threaded today. The kernel has threads
(syscalls 90/91) and futexes, and C programs get POSIX threads over them
(musl), but the Ada user runtime is built `No_Tasking`.

Prerequisites:
- **A heap that scales across threads.** CuAlloc is already safe (one
  spinlock), but serialized; per-thread caches with batched remote frees
  (ALLOC-001, step 4) keep services from contending on it.
- **Per-thread secondary stacks**, growable (ALLOC-002, first step).
- **A tasking runtime profile over kernel threads and futexes:** Jorvik (the
  Ravenscar successor) first. Tasks and protected objects are declared at
  library level, with no dynamic task creation or abort. SPARK analyses
  Jorvik programs for data races and protected-object priority (ceiling)
  errors, which keeps the "prove instead of check" rule for services.
- **Scheduler fit:** task priorities and deadlines map onto the Kolivas
  virtual-deadline scheduler. Driver and service boosts stay grants, never
  self-assigned.

Candidates, measured before and after on the existing benchmarks:
- **Networking** (`tests/net-bench`): receive and transmit pipelines on
  separate cores; per-queue workers for multi-queue virtio-net; TCP timers
  apart from the data path. Batching to cut IPC (the netstack's rule) still
  applies within each task.
- **Filesystem** (`tests/fs-bench`): client queues served in parallel;
  block I/O kept in flight while metadata work goes on; the JBD2 commit and
  write-back as their own tasks (FS-005).
- Later: logstore ingestion versus persistence, and the compositor's
  per-output work.

### ALLOC-002 — Heap-backed sizes instead of fixed limits (user, 2026-10-05)

Status: open, after ALLOC-001's proof step. Native programs now have a heap
(CuAlloc), so many limits that exist only because there was none can go.
First, a census of fixed limits, each classified:
- **Workspace (grow on the heap, bounded by the process's memory quota):**
  CCL source text (8 KiB, which the manifest schema shares with each
  manifest), catalogs and name tables, VM text (1 to 8 KiB), decoded
  descriptions, console buffers.
- **Wire and protocol formats (stay bounded, as part of the format):** IPC
  messages, launch blocks (64 KiB), grant regions (16 places), ring
  layouts. A receiver parsing untrusted input must know its maximum; they
  grow where they pinch, as format versions.
- **Policy (become typed configuration, not constants):** for example
  console runs (8), forwarded stderr rings (4), path length.

**Where workspace goes, in order of preference (user, 2026-10-05):**
1. **The stack or the secondary stack** for anything scoped: parsing,
   building and returning a result, rendering. Scope lifetime, no free, no
   fragmentation, and no ownership pointers for SPARK.
2. **The heap (CuAlloc)** only for objects that outlive their scope:
   session-held values, catalogs kept across evaluations, history, caches.
3. **Fixed bounds** only for wire formats and policy.

**First step: a growable secondary stack.** Today it is 32 KiB, fixed
(`Runtime_Default_Sec_Stack_Size`, `s-parame.ads`), and overflow ends the
process (`Storage_Error`, no propagation). Instead, reserve a large range
per thread (owned-memory call 123) and commit it in steps as it grows
(124), as CuAlloc's arenas do. It then stays a bump pointer, bounded only by
the process's quota, with one explicit failure point (commit refused). The
CCL per-call mark/release plan is the same discipline for the VM's arenas.

Converting a workspace limit is restructuring, not search-and-replace:
- **Proof:** heap objects need SPARK's ownership rules (level 2).
- **Allocation failure:** services run without exceptions (`-gnatp`), so
  every allocation needs an explicit failure path, on CuAlloc's proved
  layer.

First conversions, where limits hurt now: CCL source and manifest text, the
console's buffers, VM text.

### ALLOC-001 — Prove and tune CuAlloc (user, 2026-10-05)

Status: open, after the self-hosting push. CuAlloc
([design](userspace-allocator.md), "CuAlloc: one allocator for everything")
is the one heap of every native program: Ada `System.Memory`, the libc malloc
family and Rust `GlobalAlloc` (Penny through libc). The SPARK cores
(`Heap_Slabs`, `Heap_Extents`, `Heap_Bitmap`, `Heap_Classes`) are proved. The
layer around them (`process/cualloc.adb`, about 440 lines: arena directory,
routing, commit growth, huge blocks) is regression-tested only
(`tests/cualloc`, the hosted Rust test, guest tests), yet runs on CuBit with
`-gnatp`. The earlier jemalloc comparisons measured the old single-arena core
in a single-threaded hot loop, not CuAlloc.

In order:
1. **Prove the CuAlloc layer** (level 1–2): no overflow in size, offset and
   commit arithmetic; the directory stays sorted and disjoint; every address
   routes to the arena that holds it. Also prove the adapters' arithmetic
   (calloc's multiplication, alignment checks). Keep the checked test builds.
2. **First periodic benchmark of CuAlloc as it is** (single- and
   multithreaded; `tests/userspace-allocator/benchmark.sh`,
   `thread-benchmark.sh`; results in `userspace-allocator-results.md`).
   Measure the lock, the binary-search free and the all-arena scan on a miss.
3. **Aligned arenas with mask routing** in place of the binary search, and
   per-class availability in place of scanning every arena.
4. **Thread caches**, remote frees batched to their owner, cleanup at thread
   exit; the single spinlock goes.
5. **Give backing back**: decommit idle slabs and page runs on a decay
   schedule (needs a kernel decommit call); release empty arenas.
6. **Smaller items:** in-place growth for medium and huge realloc; skip
   zeroing fresh memory in calloc; sized free; measure rounding waste and
   finer classes; heap statistics through the observability service
   (capability-gated).
7. **Kernel policy** (stage C): the 2 GiB per-reservation cap becomes a quota
   policy, for single blocks above 2 GiB.

Measure real traces (Penny, cc1, the CCL console, fonts) as well as the
synthetic ones, reporting throughput, tail latency, retained memory and
waste separately. Do not promote a microbenchmark win into a general
performance or memory-safety claim. Benchmark periodically, not every change.
See the [dated measurements](userspace-allocator-results.md).

## Package metadata

### Live CD local ROM selection omits expected cartridges

Status: reported on the laptop; investigate later.

Additional Game Boy ROMs expected from the local ROM directory were absent from
the ISO. Check the selected image profile, local-input filtering/staging, and
cartridge audit against the actual directory contents. Distinguish files absent
from the ISO from files present but not exposed by SameBoy's current launcher.
Keep ROMs local and explicitly opted in; do not commit or distribute them.

### Harden complete executable identity-section validation

Status: metadata corrected; loader hardening pending

The old Devices C manifest declared a 19-byte identity value for
`com.cubit.devices`, which is 17 bytes. CCL generation now fixes the length;
regression tests assert this exact one-byte correction independently of the
unchanged authority metadata. network-check also gains a complete identity and
ccl-control gains a version. The original C bytes are retained as test fixtures.

`procmgr.parseIdSection` returns as soon as it finds `id`, without validating
the remaining TLVs or rejecting duplicate keys. In the Devices case it could
accept the two following version-header bytes as part of the identity. Harden
this parser to validate the entire section before publishing identity; add
truncated, duplicate, malformed-length, empty-value, and trailing-data loader
tests. Compiler validation does not make arbitrary ELF inputs trustworthy.

All 28 remaining Ada manifests (plus the previously migrated ccl-vm) now use
CCL, with exact authority-section comparisons and reviewed identity updates.
Checked CCL profiles now control normal/fallback initrd contents and USB optical
image composition. Next: source build dependencies and development ext2 payload
plans, keeping binary requests separate from image contents and launch approval.
See [CCL package design](ccl-packages.md).

## Boot and driver discovery

### Scannable physical-boot diagnostic capsule

- [ ] Add an optional, fixed-size QR code to the bootstrap diagnostic panel.
  It must encode a compact, versioned diagnostic capsule—not raw scrolling log
  text—containing the image/build identity, boot stage, first fatal code (if
  any), architecture/firmware facts that are already displayed, and a checksum.
  Keep the payload bounded and privacy-safe: no secrets, full memory map,
  certificate material, network configuration, device serial numbers, or raw
  addresses. Render it without allocation or a general image library, and keep
  the existing text panel readable at 1024x768. The code is a convenience for
  photographing/scanning a headless machine, never the sole diagnostic record;
  serial and the future authority-gated retained boot log remain authoritative.
  Add pure encoding/error-correction tests, hostile payload fixtures, and a
  native screenshot/decode regression before enabling it by default.

### Boot-map admission and userspace ACPI

The Multiboot-v1 decoder now has a bounded pure SPARK core (72 checks discharged)
and 75,085 hosted cases. The kernel snapshots variable-sized records into static
workspace and normalizes once for both allocators. Initial boot-entry numeric
admission/sanitization now adds 11 discharged checks and 169,416 hosted cases,
including the actual assembly gate. Boot module metadata now uses a sealed
catalog; payload pages are validated, disjoint, padding-sanitized and retained
for the lifetime of the boot. Its pure core discharges 62 checks and passes
1,000,460 hosted cases; real-GRUB fixtures reject malformed declarations before
allocator startup. See [module lifetime evidence](../tests/boot-modules/README.md).
Boot framebuffer admission now adds a pure 44-check core and 17,427 hosted cases.
One descriptor feeds reservation, renderer setup, sysinfo and MAPFB. The adapter
normalizes GRUB's aligned RGB union, validates page-rounded exclusions, and maps
framebuffer pages separately from RAM. Hardware backing/cache correctness remains
a trusted/tested boundary, not a proof of firmware truth. See
[framebuffer evidence](../tests/boot-framebuffer/README.md).
Next display slice: generation-bound output objects and explicit native takeover,
then shared mixed-DPI/orientation geometry. Multi-adapter and future Vulkan/3D
boundaries are captured in [display architecture](display-outputs-and-scaling.md).
See [entry evidence](../tests/multiboot-entry/README.md), [proof boundaries](allocator-verification.md)
and [decoder evidence](../tests/multiboot-memory-map/README.md).

Keep future AML interpretation outside the kernel. Move ongoing ACPI discovery,
events and device/power policy to userspace with scoped native device authority;
retain only necessary early bootstrap data and hardware enforcement in-kernel.
Firmware declarations cannot mint access. Implement bounded table parsing before
AML; do not expand the current allocator work into an interpreter project.
See [ACPI userspace plan and remaining decisions](acpi-userspace.md).

### Panic diagnostics must not assume frame-pointer chains

Status: unsafe walk removed; native local-halt regression passes.

The former `Last_Chance_Handler.printCallStack` followed RBP as a linked frame
chain even though optimized kernel builds do not guarantee frame pointers. A
rejected duplicate module logged its admission error, then the diagnostic walk
interpreted non-frame data as an address and could fault again. The handler now disables local
interrupts before output, preserves the original diagnostic, explicitly reports
that no stack trace is available, and loops on HLT. It no longer calls `x86.panic`
(which raises another software interrupt/exception). Optimized builds remain
enabled without runtime assertions. The hardware/output adapter is explicitly
SPARK Off, not a purportedly proved unwinder.

The three real-GRUB rejection fixtures check the expected diagnostic before
allocator admission, a single panic banner, unchanged serial output, and two QMP
observations of CPU 0 halted with IF clear. Remaining work: a supported bounded
unwinder, coordinated SMP panic shutdown, and stronger emergency-output isolation.
This is only a local CPU stop; valid runtime message pointers, a usable stack,
and working diagnostic output remain trusted. It does not contain NMIs or prove
all possible panic causes safe.

### Do not start HDA without a usable controller grant

Status: observed during CCL image regression; native fix pending

A Q35 fallback boot without an HDA PCI device starts hda.drv anyway. The driver
reports a denied device mapping (no CAP_DEVICE_MEM), faults on its MMIO address,
and boot progression stalls. The CCL image migration preserves existing image
membership; this failure is in hardware discovery/startup, not CCL evaluation.
The fallback image test now supplies the HDA device used by run-laptop.

Gate driver startup on a successfully discovered and provisioned device, handle
failed mappings explicitly, and ensure a failed optional audio driver cannot
block the rest of boot. Add absent-controller and driver-failure regressions.
Do not fix this by granting an unprobed driver broader device-memory authority.

## Audio controls

SameBoy now plays through the native mixer and has app-local mute/volume keys.
Next: a separately authorized master-volume interface, desktop volume widget,
multimedia key routing, click-free gain ramps and Config persistence. Ordinary
audio playback authority must not allow changing another application's volume.
See [audio volume-control boundaries and follow-up](audio-volume-control.md).

## Desktop defects

### Desktop close lifecycle and rejected-request visibility

The close-NetSurf/launch-DOOM freeze exposed kernel receive starvation; the
shared request receive paths now rotate fairly between blocked senders and
queued work. See [incident and regression evidence](ipc-receive-fairness.md).

Follow-up: add typed close requests and handle terminal surface errors in
clients, replacing surface deletion followed by a best-effort process kill.
Keep force-termination authority separately scoped; do not grant blanket
process-write authority to the compositor. Count and attribute rejected IPC
alongside accepted traffic, with bounded reporting, and test endpoint budgets
against sustained abusive clients.

### UI-016..UI-021 — Retained-layer compositor (docs/compositor-scene-design.md; user, 2026-10-09)

The user asked to restructure the desktop scene along established practice.
The survey (macOS Core Animation, DWM/DirectComposition, SurfaceFlinger/HWC,
wlroots/KWin/Mutter, Chrome viz, WebRender, Skia) finds every system keeps a
retained texture per surface plus hardware planes; CuBit's gap is the
desktop's own UI (chrome, taskbar, menus, text), redrawn every frame as about
60–80 tiny draws per decorated window, each re-recorded per damage box with
a full pipeline/descriptor rebind. Ordered steps (each tested on its own):
- **UI-016 Bind on change, coalesced chrome primitives:** per-frame counters
  first (draws, binds, boxes); bind pipeline/descriptor/scissor only when
  they change; title gradient and borders as single primitives.
- **UI-017 Glyph atlas, instanced text:** one R8 atlas replaces the 128 cell
  images; one instanced draw per text run; cache key shared with the CPU
  path. Bitmap masks, no SDF at UI sizes.
- **UI-018 Layer table, damage and culling (SPARK):** one flat retained
  layer list (wallpaper; per window a chrome image and its client surface;
  taskbar, menus, popups; pointers and video as plane requests); damage per
  layer mapped onto the proved 8-box output set; opaque-cover culling proved
  sound; batch builder emits each layer/box pair once, in order.
- **UI-019 Retained shell images, one CPU/GPU layer walk:** desktop-owned UI
  rendered by the shared toolkit into retained per-layer images only when its
  state changes; the CPU renderer walks the same list. A frame becomes about
  20 quads for 8 windows; budget drops to 256 draws and GPU-004 item 3
  (frame splitting) is dropped.
- **UI-020 Render into scanout slots:** no GPU readback
  (`desktop_readback_output`); on the N100's single DDR5 channel a blended
  4K full-screen pass costs about 2.6 ms, so planes and damage limiting are
  required for the 1 ms target.
- **UI-021 Tiled large layers:** tiling only inside layers larger than the
  buffer cap (also lifts the 16 MiB limit that blocks 4K), with
  compositor-side scroll (UI-014).
Planes (UI-015) then carry the Intel cursor, fullscreen bypass and video.

### UI-015 — Display planes: hardware cursors, then video overlays and fullscreen bypass (2026-10-09)

Design and as-built record: [display-planes.md](display-planes.md).

The pointer is a display *plane request*, like a future video overlay or
fullscreen primary.
- **Planner.** One pure planner (`CuBit.Display_Planes`, proved at level 2:
  deterministic, exclusive, covered, complete, prioritized) assigns each
  output's hardware planes. A request without a plane is composited by the
  desktop.
- **Glitch-free swaps.** A request that moves between a plane and the
  composite commits with the first published frame tagged with its plan
  epoch.

**Done (2026-10-09):**
- generic plane protocols, Desktop to display (0x0920..0x092A) and display
  to driver (0x0A10..0x0A14);
- virtio-gpu cursor queue plane, tested in QEMU with host-side evidence;
- desktop pointer on the plane, unscaled layouts only;
- Intel cursor register encoder (proved) and native writer (hosted tests).

**Next:**
- **Intel wiring** (display-planes.md, "Hardware wiring pending"):
  - cursor register pages from the broker;
  - GGTT cursor surface;
  - plane protocol in intel-gpu;
  - planes on firmware outputs.

  Then test on the NUC.
- **Scaled outputs:** map desktop space to native pixels, so scaled outputs
  keep the hardware pointer.
- **Absolute pointer input** (USB HID tablet or virtio-input, plus a typed
  absolute event): until then, virtio-gpu's host-pointer cursor plane is
  never used by Desktop's relative mouse, so `run-desktop` keeps a
  composited pointer.
- **More pointers:** a second pointer source in Desktop, for a second mouse
  or an agent pointer. The API and planner already take 8.
- **Scaler budget:** a per-pipe scaler budget as a planner input.
- **Video overlays (GPU-003):** NV12/P010 on HDR planes 1–3, with the
  safety rules in display-planes.md: grant, producer timeline wait,
  release after the flip.
- **Fullscreen bypass:** primary requests.

### UI-014 — Compositor-side scrolling: "region moved by N rows" (2026-10-08)

The Files work measured that a 4K full frame cannot meet the 1 ms target on
one core: filling 3840×2160 pixels once costs 1.33 ms at memory bandwidth, and
an in-app scroll blit costs as much as redrawing. Scrolling is the common
case, so the compositor should move scrolled content itself.
- **Protocol:** a desktop-protocol request "this rectangle's content moved by
  (dx, dy)", sent with the frame whose damage is only the newly exposed rows.
- **Compositor:** Desktop shifts the client's retained pixels in its own
  buffer, or, once GPU composition works, with a GPU copy. Then it composites
  the exposed rows from the client's frame.
- **Correctness:** the move applies to exactly one published frame, in order,
  never to a stale buffer. A client that can't take part keeps sending full
  damage.
- **First user:** Files, then Logs, the Workbench and CCL console.
Full redraws use several cores later (banded rendering, after TASK-001).

### UI-013 — Submenus in the Apps menu (user-requested, 2026-10-04)

The desktop's Apps menu is one flat list (`desktop.launch.*` settings in
system.ccl, `Desktop_Launch`), and it grows with every program: Console,
Logs, Workbench, Files, DOOM, SameBoy, Penny, and soon the build tools.
- **Submenus:** nest entries (Development, Games, System, Media), declared
  in CCL like the entries themselves: a typed menu tree, not keyword
  strings, checked when system.ccl is checked.
- **Keyboard first:** arrows open and close submenus, type-to-find across
  the whole tree, Enter launches. The headless tests launch by label, so
  they keep working when entries move (tests/usb-optical/run-live.py
  already does).
- **Shared widget:** the submenu belongs to the shared UI toolkit's menu
  (native menubars use it too), held to UI-011's reliability gate.

2026-10-08: The entries are now typed. `(category ...)` is an `App_Category`
member (System, Development, Web, Games, Media, Tools; the default is
Tools), checked at config compile time (CFG-001, option A). The hand-written
`Desktop_Launch.Parse` and the `launch v1` string format are gone. The
submenu work in the desktop's Apps menu follows.

### UI-012 — Copy and paste (user-requested, 2026-10-04)

There is no clipboard. The CCL console (and every text field) has no way to
copy a card's value or an error, or to paste a snippet. Pasting a multi-form
CCL snippet is how people try examples. Needed:
- **A typed clipboard service, not ambient global state.** A copy is a typed
  value (`String` first; later CCL values with their type, images, places).
  Reading the clipboard is a capability: a paste comes from the focused
  window, by the user's own gesture, never a background read.
- **Keyboard first:** Ctrl+C, Ctrl+X and Ctrl+V in the console input, the
  Workbench and the shared text widgets, with a selection model in the text
  widgets.
- **Console specifics:** copy a card's value (as CCL source that reads back
  to the same value), its type, or an error; paste multi-line input as one
  entry, with clear rules for entries holding several forms.
- **Visible:** the capability graph shows who wrote and who read the
  clipboard. A paste from another security context (CTX-001) is marked.
- **Remote:** the Observatory web REPL maps to the browser clipboard
  through the same typed operation.

### UI-010 — Make input delivery recoverable under loss

Status: in progress

Persistent IRQ doorbells, stable per-surface queues, explicit snapshot recovery,
and deferred-capability input waits are implemented. Typed source reports now
carry capability-stamped identity, device generation, sequence, delivery class,
state snapshot, and explicit resynchronization. Desktop keeps independent
source state and deliberately merges pointer buttons. Event-driven toolkit apps
no longer poll or sleep between input deliveries, and one stalled surface cannot
evict another surface's transitions.

Driver-side pointer retention no longer loses reports under a desktop stall
(2026-10-09, coordination/input-loss.md, tests/input-pending/README.md).
Root cause of the NUC resize cancel: xhci.drv and ps2.drv kept one slot per
raw HID report (32, plus the kernel's 16-event publisher credit), so a stall
longer than 48 report periods (48 ms at 1 kHz) discarded the backlog and
desktop saw a sequence gap. `Pointer_Pending` (SPARK level 2) now coalesces
motion by agreement into the newest unpublished non-transition report:
buttons, flags and wheel are never merged away or moved, displacement is
conserved, no sequence is consumed. Overflow needs 32 unpublished
transitions and stays explicit. xhci stats add `coalesced=`, `overflow=`,
`busy=`; desktop's `event_drop=` is renamed `event_busy=` (credit refusals
the publisher kept), and desktop reports pointer source age at intake.
Still open here: the credit-returned notice (publishers retry busy reports
on a 1 ms timer) and keyboard retention (32 bytes, explicit overflow).

The remaining architectural boundary is driver-to-input-router publication,
which still uses the transitional bounded `sendEvent` lane directly to desktop
rather than a typed `input.svc` handle. Replace it with transport-bound device
class identity and a bounded lossless transition strategy, then enforce latency
admission in the scheduler. Test stuck-button, stuck-modifier,
lost-capture, multiple-device, slow-client, queue-saturation, device-reset, and
mailbox-saturation cases.

### UI-001 — Physical-laptop touchpad movement is severely degraded

Status: in progress

The laptop's internal touchpad responds through the PS/2 service. Removing
synchronous vblank waits from cursor presentation produced a massive physical
improvement and made it mostly usable. A USB mouse is recognized without a
reboot but improved only slightly and remains impractical, isolating a second
problem in the xHCI report-delivery path rather than cursor coordinate scaling.

The first correction gives software-cursor damage an explicit immediate-present
operation so it cannot synchronously wait for legacy VGA vertical blank on every
input packet. The desktop now blocks on its mixed IPC mailbox while idle instead
of polling with a two-millisecond sleep, and reports its kernel event-ring loss
as `event_drop=` in periodic diagnostics. The xHCI driver now keeps eight
distinct interrupt transfers queued, replenishes before publishing reports, and
uses a dedicated MSI or MSI-X vector instead of its old one-transfer-at-a-time
millisecond polling loop. Its bounded polling fallback retains the queued
transfers when neither message-signaled mode is available. QEMU completed more
than a full transfer-ring wrap with MSI-X active and desktop `event_drop=0`.
The physical retest still showed badly jerky USB motion and no obvious clicks,
while the touchpad remained good.

The first Devices UI exposed a separate client-side latency trap: it marked its
entire 900-by-580 surface dirty for every pointer-motion event. The shared
application loop now owns pointer hover, capture, pressed/released state, and
control damage by default. Motion that remains inside one control performs no
client paint or present; hover transitions invalidate only the old and new
controls, and a drag invalidates the captured control's declared damage. This
is now inherited by Devices and future native apps
without application-specific motion handlers. Pointer-position-sensitive
canvases must explicitly request repaint-on-motion. Consuming input must not
imply repainting a surface.

The CCL Workbench had a separate custom-adapter regression: it rendered and
copied its complete canvas on every uncaptured pointer report, then slept for
ten milliseconds before polling again. It now blocks on the toolkit's
deferred-reply input wait, performs no repaint while the pointer stays within
one semantic region, and submits only status/splitter damage on hover
transitions. The deterministic input-stream gate crosses the rich editor with
128 reports and rejects a return to per-motion client-surface presentation.

Software-cursor presentation is now coalesced to a bounded four-millisecond
cadence so a high-rate USB mouse cannot force one synchronous display IPC per
report. xhci.drv also exposes low-rate aggregate report/error/button/raw-byte
diagnostics through devmgr.svc. The authority-scoped Devices app presents the
bounded snapshot alongside the PCI inventory without granting raw MMIO, IRQ, or
DMA access. Repeat the physical test with this image, then use those counters to
distinguish report-layout errors from load before adding end-to-end latency
histograms.

Add bounded diagnostics for controller/device identity, negotiated packet
length, synchronization drops, overflow packets, bytes attributed to the
keyboard versus auxiliary device, and implausible deltas. Use those results to
identify standard PS/2, Synaptics, ALPS, or another extended protocol before
changing acceleration or silently discarding broad classes of input.

Replace the transitional global keyboard/mouse driver registry with an explicit
desktop-session input route. A legacy shell now declines raw input registration
when desktop.svc is present, but the check/register sequence is not a final
authority-safe ownership protocol.

The laptop's stretched 1024x768 framebuffer is a separate display-mode defect.
Native mode negotiation may affect apparent horizontal speed but cannot explain
intermittent motion or event loss.

### UI-002 — Only one desktop application can be opened

Status: open

After one application is launched, attempts to open another application do not
produce a second usable application window. Reproduce through both the launch
menu and process-manager path, then inspect spawn completion, surface creation,
window ownership, focus/z-order, task buttons, and any singleton state in the
desktop service.

## Device management

### DEV-001 — Grow Devices into unified device administration

Status: in progress

The first Devices slice is an inspection-only application backed by devmgr.svc.
It provides a reusable keyboard- and mouse-navigable tree view, bounded PCI
inventory, driver ownership/state, and live aggregate xHCI diagnostics. Extend
the inventory with USB topology and descriptors, interrupt and DMA resources,
driver provenance, failures, and bounded event history.

Administrative operations such as reset, disable, rebind, or policy changes
must not be added to the inspection endpoint. Define a separate typed authority
for each mutation category, mint it only to the approved device-management
role, require an explicit confirmation surface where appropriate, and record
WHAT, WHO, WHEN, WHERE, and WHY through the security event path.

## Kernel / userspace boundary

### IPC-001 — One control plane and data plane for every transfer (user, 2026-10-06)

Design: docs/data-plane.md. Capability IPC sets up channels (type, capacity,
policy, queue or arena) and learns when they end; shared-memory rings and
arenas carry the data. It replaces five separate handshakes (filesystem
queues, log publishers, outlets, netstack channels, and none at all for
block devices).

Decided:
- The kernel knows grants, not channels. The rings already treat the peer as
  hostile, so the kernel need not enforce the protocol.
- The producer owns the data and grants it read-only to consumers; a
  lossless consumer grants its index back. Bidirectional means two channels.
- Arenas are a channel type, paired with a queue that hands buffers over.
- Policies: lossless, drop oldest, shed newest.
- Brokering is parent only, by introduction: the broker never maps the
  data. Delegated ends are derived and narrowed; a lossless consumer end
  moves rather than copies.
- In-place reads follow four rules (copy then validate, in place for data
  without invariants, transfer for large parsed buffers, proved single-fetch
  parsers). No per-message page-lock system call.

Steps, in order:
1. Kernel: grant lifecycle events ("revoke requested" to the grantee,
   "returned" to the owner), then grant derivation with cascading
   revocation.
2. The control-plane protocol and a proved codec.
3. The block path (filesystem to nvme/ata/ramdisk) as a queue pair plus an
   arena: the first new user.
4. Migrate logging (dead-publisher detection), filesystem client queues,
   outlets (console brokering; delegation replaces gcc's stderr
   forwarding, D3), then netstack channels.
5. Borrow and release on lossless queues; single-fetch parsers where a copy
   shows up in profiles.

### IPC-002 — Guaranteed delivery: no event or request is ever lost (user, 2026-10-07)

Design: docs/ipc-delivery.md. The rule: nothing is lost unless both
parties agreed to a lossy channel. The shared 32-entry mailbox ring drops
whatever arrives when it is full, so any sender can flood it, and kernel
events (grant ends, child exits, control) are lost behind the flood. A
bigger ring only moves the cliff.

Decided (direction):
- Events are state on kernel objects (grants owned and held, children,
  control capabilities), linked into a per-process ready list through the
  objects themselves: capacity equals the number of objects, so it never
  fills. Both parties to a grant always learn of its end.
- Control (Stop, Interrupt, Reload) is pending bits per sender
  capability, so two senders' messages stay two facts.
- Requests use per-capability queues with credits that the receiver
  grants from its declared capacity. A sender out of credit is told
  "busy" and keeps its request; a flooder starves only itself.
- One port-style wait over ready objects, request queues and data-plane
  doorbells.

Steps, in order:
1. Events on objects and the port wait (grants, children, faults).
   Done 2026-10-07.
2. Control per capability. Done 2026-10-07 (per sender slot on the
   target's process record).
3. Per-capability request queues with credits; retire the ring.
4. Remove the drop recovery (`Control_Events.Lost`, the drop-count
   checks, the No_Room sweeps, the filesystem resync) and `OP_CLOSE`.

Open: "busy" versus blocking in the kernel (recommended: always busy),
credit defaults, folding synchronous call and reply into credit 1.

### IPC-003 — A proved fast path for call and reply (user, 2026-10-07)

The user: make control-plane IPC as fast as possible. IPC stays for the
control plane, with bulk data on the data plane (docs/data-plane.md).

Where it stands: bench-ipc (KVM, 09-26) does 20,000 synchronous round
trips in about 50 ms, roughly 2.5 µs each. That is in Linux's
pipe/futex ping-pong range and roughly 5–10× seL4's fast path (a few
hundred cycles one way). Async submissions run about 0.8 µs each,
batched.

Direction:
1. **Measure first.**
   - Cycle breakdown of a call/reply (tracing already exists).
   - Same CPU against cross CPU, KVM against bare metal.
   - Finer than bench-ipc's whole milliseconds.
2. **A dedicated path** for the common case: a call to a receiver already
   blocked in receive (or reply-and-receive), with the message in
   registers. It switches straight from caller to callee, with no queue,
   no lane selection and no general scheduler pass, then back on reply.
   Anything unusual falls back to the general path: a receiver not
   waiting, a long message, a capability transfer, a cross-CPU target, a
   pending notice.
3. **Safety, as in seL4:** the fast path is a proved refinement of the
   general path. The same authority checks, the same state afterwards and
   the same reply-capability rules, in SPARK at level 1–2. Tests run each
   case both ways, with mutation checks.
4. **Interactions:**
   - the scheduler (Kolivas-style virtual deadlines, and the scheduler
     work in coordination/sched-latency.md): a direct switch must keep its
     accounting and fairness;
   - IPC-002's credits: a direct handoff queues nothing, so it uses no
     credit.

After IPC-002 step 3, so it is measured against the final queue
structure.

**Status, 2026-10-07** (docs/ipc-fastpath.md):
- **Built:** the call handoff and reply-and-receive fast paths (sync round
  trip −43%), and call deadlines (no default: every call states its
  deadline or `Wait_Forever`; `REPLY_TIMEOUT` is a kernel-only label).
- **Open:**
  - the refinement proof (step 3 above);
  - async batching (PERF-002), and cross-CPU wakeup latency (with the
    scheduler);
  - **tightening the deadlines:** about 340 call sites were migrated with
    `Wait_Forever`, keeping old behaviour. Give each a real deadline where
    the caller can act on a timeout, UI and network-facing paths first.
    `grep -rn "Wait_Forever\|WAIT_FOREVER" userspace` lists them; most are
    in the runtime (`userspace/runtime/gnat`), lib/ui, ccl/native,
    filesystem, mesa/anv and desktop-check.

### PERF-001 — How it feels under load: a standing latency benchmark (user, 2026-10-06)

The user: traction needs CuBit at least somewhat competitive with Linux;
feeling faster and being more secure are where it can win, and how it feels
under load is the hard part. Throughput benchmarks (fs-bench, net-bench)
exist; so do the pieces of a latency one: `bench-latency`
(tests/sched-latency: interbench-style wake, interactive, frame and audio
deadlines, the same C program on Linux) and `bench-input` (`--load-workers`
busy peers). Their loads are CPU hogs. What makes a desktop feel slow is
real work beside it.

Build one suite that answers "what does a keypress cost while the machine
is busy?", on CuBit and on Linux in the same QEMU configuration:
- **Probes:** keypress to photon (the desktop's presented frame; target 1
  ms, docs/input-latency.md and the render-performance target), service
  wake (a channel kick to the consumer running), and the existing frame and
  audio deadlines.
- **Loads, alone and combined:** a compile (the gcc test's cc1 runs), file
  I/O (fs-bench's sequential write and create), network (net-bench), log
  bursts (the log-authority burst), and CPU hogs.
- **Report:** p50, p99, p99.9 and the maximum per probe and load, misses
  against the 1 ms target, and how much work the load got done, so a
  scheduler that simply starves the load does not look good.
- **Standing:** run before and after scheduler, IPC and driver changes
  (A/B), like fs-bench and net-bench.

### PERF-002 — Async batching: many requests and completions per system call (user, 2026-10-08)

The user: next after KERN-003. The largest remaining control-plane IPC win
(docs/ipc-fastpath.md, "Next").

Where it stands (bench-ipc, KVM, 2026-10-07): 8,192 async requests at
depth 16 take 16–20 ms, about 2 µs each. That is no better than a
synchronous round trip on the fast path (about 1.4 µs). Each request still
costs the server one receive and one reply system call, each with its own
mailbox lock round, and each completion wakes the client separately.

Direction:
1. **Receive many:** a server takes up to N queued requests in one call,
   into a caller-sized buffer (checked with `Process.User_Memory`).
   - The selection stays the same as N single receives: round-robin across
     senders (`Kernel_Credits.Take`), each sender's order kept, nothing
     taken that a single receive would not have taken.
   - Async requests carry their reply authority by request ID. Synchronous
     calls in a batch need one reply authority each, not the thread's one
     slot; decide between saving them automatically and taking only async
     requests in a batch.
2. **Reply many:** one call completes N requests; completions for one
   client are queued under one lock round, and its waiters are woken once.
3. **Client side:** submit many in one call, and `waitCompletion` already
   returns several. Check the runtime and libc use both.
4. **Users:** the netstack, filesystem and desktop service loops move to
   the batch calls where they serve several clients (see "Reduce IPC").

Interactions:
- Credits (IPC-002 step 3): a batch frees credit as it takes, so senders
  are told busy no more often than with single receives.
- Call deadlines: a synchronous caller that times out while its request
  sits in a taken batch is answered as today (the reply is refused).
- KERN-003: built on the process objects, after it.

Verification:
- **SPARK, level 1–2:** the batch selection equals N single `Take`s.
- **Guest tests:** batch receive and reply, mixed with single calls;
  fairness under a flooding sender; hostile buffer pointers.
- **Bench:** bench-ipc's async phase, A/B against single calls under KVM,
  plus the netstack and filesystem benches once they use it.

### Console output is synchronous serial I/O

Status (2026-10-10): asynchronous console landed. Prints copy into one
bounded kernel FIFO (`Console_Ring`, SPARK level 2) under the output lock;
idle CPUs write it in 16-byte `rep outsb` batches when the UART's holding
register is empty, CPU 0's timer after 10 ticks without progress. No loss: a
writer finding the FIFO full writes its oldest batch itself; fatal stops
flush and write directly; output before the first idle thread is direct.
Single global FIFO, not per-CPU: one owner keeps cross-CPU order. Measured
(QEMU KVM, 500 Hz pointer flood, timing Desktop): Desktop's once-per-second
14.5 KiB report cost 60-200 ms of blocked loop per second before (input
source age p99 57 ms), under 1 ms after; see
coordination/desktop-1hz-stall.md and tests/console-ring/. Remaining from the
direction below: logstore for applications, the PERF-003 verbosity level.

Every kernel print and user `debugPrint` writes the UART one byte at a time
(an `out` per byte: a VM exit under KVM, about 87 µs per byte at 115200 baud
on hardware). The debug-write syscall does this with interrupts masked, and
since 2026-09-24 a console lock (`TextIO`) serializes CPUs so lines stay
whole, which makes contention visible.

Direction:
- prints copy whole lines into per-CPU, lock-free memory rings;
- a low-priority drainer writes the rings to serial and counts drops instead
  of stalling callers;
- applications log through logstore rather than `debugPrint`;
- serial stays for early boot, panics (direct, unlocked) and test markers.

### Physical allocator functional verification

Status: bitmap, block-head transitions, local split/coalesce geometry and
constant-time link-update primitives proved and integrated.

The actual buddy allocator now uses the SPARK bitmap layout core; inclusive
maximum-frame sizing no longer aliases the next order's bits. Out-of-band
block-head states now validate exact-order releases and list removals, separate
boot admission from runtime free, and preserve pin-aware order-zero retirement.
The new physical-span core proves exact split/coalesce coverage and round trips;
production child/parent address construction uses it. The architecture-neutral
intrusive splice generic now proves exact link writes and count arithmetic
(44 checks across integer/address instantiations); the kernel retains its O(1)
lists with no extra metadata or out-of-line helper calls. The separate free-set
tree remains an experiment, not an allocator speedup. A Ghost sequence/rank
witness now proves single-order membership/count correspondence with the real
ledger/splice primitives (213 combined checks, none unproved), without runtime
bookkeeping. Next establish its physical address/field mapping and boot base
case, arena-wide partition preservation and allocation
non-overlap. Pins, ownership and SMP
integration remain separate end-to-end obligations. See the
[allocator verification plan](allocator-verification.md) and
[bitmap evidence](../tests/buddy-bitmap/README.md) and
[block-state evidence](../tests/buddy-blocks/README.md) and
[physical-span evidence](../tests/buddy-geometry/README.md) and
[intrusive-splice evidence and proof limits](../tests/intrusive-list-splices/README.md).

Boot-boundary review fixed two concrete errors: equality with the inclusive
boot high-water mark no longer admits a boot-owned frame, and the boot bitmap
limit is now its last represented PFN rather than its bit count. Sentinel counts
are explicitly initialized; a block spanning the boot range no longer queries
out-of-range bitmap entries. The shared arithmetic/admission core passes 13
checks, plus exhaustive small-arena and full-width edge tests. See
[boot-admission evidence](../tests/buddy-boot-admission/README.md). Firmware
alignment and conflicting-region handling still need proof. The subsequent
reservation ADT removes the redundant free counter (see below).

Metadata byte/page sizing and descriptor address arithmetic now use a shared
SPARK core: 20 checks prove slot bounds/separation, page coverage and numeric
address nonwrap under a valid span premise. Its descriptor lookup retains the
existing code size/indexed LEA. Physical reservation ownership and exclusion of
metadata/sentinels from payload remain open. The pre-existing division in the
geometry admission path is a future measured optimization candidate. See
[metadata evidence](../tests/buddy-metadata/README.md).

The actual boot bitmap/reservation/high-water state now lives in a private SPARK
ADT. Its focused target passes 27 checks, proving successful span ownership,
unchanged state on failure, disjoint successive reservations and coverage by the
bitmap-checked buddy handoff. Idempotent admission and on-demand diagnostic
counts replace unsafe redundant accounting; the unused boot release API is gone.
Hosted tests pass 206,022 requests, including independent first-fit/count checks.
The packed-map reservation code is a leaf with no runtime proof baggage, and
setup scans only its own arena. Firmware partial pages/conflicting regions and
physical mapping/metadata/sentinel exclusion remain open integration work. See
[boot reservation evidence](../tests/boot-frame-allocator/README.md).

Firmware admission now uses one shared pure policy in both allocators: inward
usable-page rounding, outward reserved-page exclusion, map-order-independent
reserved precedence and unique ownership across duplicate usable entries.
Its 51 checks all prove; byte-oracle tests cover 57,346 ranges, 2,008 maps and
401,354 candidate blocks. Buddy setup tiles edge blocks instead of dropping
aligned/trailing capacity. Empty firmware entries are initialized, numeric
intervals are validated before endpoint arithmetic, and framebuffer extent
arithmetic is bounded. The raw Multiboot buffer/count/variable-entry parser and
direct-map overlap/cache-mode handling still need review, as do the full physical
mapping/free-list refinement and boot-module reservation argument. See
[firmware admission evidence](../tests/firmware-frames/README.md).

### Heap growth must fail locally, not panic the kernel

Status: partial hardening; whole-request admission and fallible runtime page acquisition.

Heap growth now rejects wrapping, oversized and quota-exceeding requests before
allocation, with a proved pure planner and four-vCPU native regression. Runtime
heap/stack page acquisition now reports resource failures; slab exhaustion
releases its lock, and frame-list insertion/removal preserves accounting.
Initial stack/ELF/process construction now returns failures with unpublished
rollback; bounded ELF metadata is snapshotted before validation and use. Native
SPAWN now copies ELF/name sources through a checked page walker and retained
physical frames; pure copy/walk/name helpers have tests and focused proofs.
Native adversarial exhaustion/retirement and authorized bad-pointer tests,
other syscall source-buffer validation, stack-reservation
policy and cross-process/kernel-guard mapping serialization remain open.
The entire allocator is not yet contained.
See [evidence and admission/cleanup work](kernel-heap-admission-issue.md).

### Signed executable admission

Status: local SPARKTLSCrypto API inspection and design; not yet enforced.

Pin and test a minimal verifier, define the signed envelope and trusted-key
policy, then bind approval to immutable bytes actually passed to the loader.
Keep signatures separate from capability grants and cover boot-path bypasses,
tampering, substitution, development exceptions and rollback/key rotation.
See [signed executable admission](signed-executable-admission.md).

### KERN-004 — Kernel interfaces for the userspace ACPI service (user, 2026-10-09; low priority)

The userspace AML interpreter (coordination/acpi.md) is hosted and tested
but has no hardware authority. Before it touches hardware, design and build
the small set of capability-gated kernel interfaces it needs, and nothing
broader:
- **SCI forwarding:** the FADT's SCI interrupt is routed by the kernel
  (level-triggered, shared, active-low on most machines) and delivered to the
  ACPI service as an IRQ doorbell, masked until the service acknowledges
  (as NVMe/xHCI interrupts are).
- **OperationRegion access:** SystemMemory regions mapped read or write per
  exact range; SystemIO through a per-process I/O permission bitmap limited
  to the granted ports; PCI_Config through the existing PCI config path. Each
  grant names the range and access; ranges overlapping other drivers'
  devices are refused.
- **Sleep:** the service decides and runs `_PTS`/`_GTS`; the kernel parks the
  other CPUs, saves state, performs the final PM1 control writes and owns the
  real-mode wake trampoline and resume path. S3 first; S4/S5 power-off and
  reset through the same final-step call.
- **CPU power:** C-state hints from `_CST` used by the kernel idle loop;
  P-state and thermal MSR writes as a narrow capability-gated call.
- **Early tables stay in the kernel** (RSDP, MADT, HPET), as today; the
  service gets the rest (DSDT/SSDTs) read-only.

Write a design doc first (docs/acpi-kernel-interface.md), then the backlog
steps. Pick up after the current graphics work (user may hand the ACPI
service itself over too).

### KERN-001 — Move hardware service work out of the kernel

Status: planned — follow-up audit, not an immediate migration

Keep scheduling and low-level memory allocation/virtual-memory enforcement in
the kernel. Review the remaining hardware code by responsibility, rather than
moving entire packages merely to reduce the kernel's line count.

Initial candidates:

- Video: audit `kernel/src/video*`, framebuffer console rendering/scrolling,
  and boot-time display setup. Move ongoing rendering, device-specific display
  work, and mode-setting policy into the existing userspace display/driver
  architecture. Retain only the bootstrap/emergency-output mechanism actually
  needed before those services are available or after they fail.
- ACPI: separate the minimal boot-time topology/interrupt/timer information the
  kernel needs from ongoing discovery, firmware interpretation, and power/device
  management that can live in an explicitly authorized userspace service.
- PCI and bus mastering: separate enumeration, device configuration, driver
  assignment, and DMA lifecycle orchestration from the privileged enforcement
  of device ownership, MMIO/config-space access, interrupts, and DMA mappings.
  Move service policy/mechanism into userspace where safe; do not replace
  capability checks with unrestricted PCI configuration writes.

The existing authority/endpoint/handle model remains the security boundary.
Userspace placement alone does **not** isolate a bus-mastering device's DMA.
Document the trust assumptions on platforms without an IOMMU and define how
bus-master enablement, buffer pinning, device quiescence, driver death/restart,
and eventual IOMMU protection interact. DMA must be stopped or contained before
device-visible memory can be reclaimed or assigned to another process.

Deliver an ownership/dependency map with links to current call sites, an
explicit kernel trusted/proof boundary, and a staged migration order. Preserve
boot/recovery output, existing IPC authority checks, and low-latency input,
display, storage, and audio paths. Gate migrations with QEMU and physical-laptop
boot tests, unauthorized-device-access tests, and driver-failure/lifetime tests.
Do not stall the current CCL REPL/widget milestones on this audit.


### KERN-002 — Publications: read-only shared values (user-approved, 2026-10-04)

A fast IPC cache, filled through a system call. A service owns a value. It
publishes the value through the kernel, and processes read it from a page
mapped read-only into them: a memory load instead of a system call or an IPC
round trip. Linux's vDSO data page ("vvar") works this way for time. CuBit
generalizes it to any value that changes rarely and is read constantly.
Filesystem read delegations already use the pattern once: the service writes
the queue header page, and clients check its valid word before and after
reading.

Design direction:
- **Writer:** one per publication, set by capability. Updates go through a
  system call, so the kernel is the only writer and every update can be
  traced (visibility motto). A directly writable mapping could come later if
  update rates ever need it.
- **Reader protocol:** a seqlock. The writer makes the counter odd, writes,
  then makes it even. Readers retry if the counter was odd or changed. After
  a bounded number of retries they fall back to a system call or report
  "unavailable", so a stalled writer cannot make readers spin forever. x86
  needs only compiler barriers; write the reader with fences in mind for
  other architectures.
- **Data only, no code:** typed, versioned record layouts, declared once.
  The proved readers live in the runtimes (libc, the Ada runtime, the Rust
  std port). Linux ships code because its interface must stay stable across
  kernel versions; we build all of user space, so we don't have to.
- **Visibility:** pages per publication, so a manifest grant decides which
  ones a process gets mapped. Each costs one physical frame, plus page-table
  entries per process.
- **Proof:** a SPARK model of the protocol: a value read with an even,
  unchanged counter is one the writer finished (tests/ pattern).

First users:
- **Wall clock.** `SYSINFO_WALL_CLOCK_OFFSET` (1403) moves here, plus TSC
  calibration, so `CLOCK_REALTIME`/`CLOCK_MONOTONIC` become memory reads
  with nanosecond precision. The sysinfo call stays as the fallback.
- **Log minimum level.** Publishers drop below-minimum messages before
  sending them, instead of logstore dropping them after the IPC.
- **Service registry generations.** A process sees that a service such as
  logstore restarted or was replaced, and rebinds without polling.
- **CPU count and topology.** libc's `sched_getaffinity` currently
  hard-codes 4 CPUs.
- **Time zone rules, boot ID, host name, DNS servers, and settings services
  read often.**

Not for secrets or per-request state. Wall time is not worth gating:
attacker code can measure time in plenty of other ways.

Status: the first publication, the monotonic clock, is done (2026-10-09,
docs/fast-clock.md). The kernel maps a read-only page at
0x5B00_0000_0000 into every process. It holds the TSC-to-nanosecond
conversion behind a seqlock, and is published when the TSC is invariant.
`CuBit.Monotonic`, `Deadline_After`, the runtime's clock users and libc's
`CLOCK_MONOTONIC` read it without a system call. `msTicks` is now the same
conversion sampled at CPU 0's ticks, so page deadlines never expire early.
The conversion and the seqlock model are proved at level 2. The headless
`--test clock` runs under KVM. Under KVM a read costs 35 ns, against 6.9 µs
for the old HPET system call.

Next:
- the wall-clock offset into the page, so `CLOCK_REALTIME` needs no system
  call;
- moving services' direct `SYSCALL_GETTIME` calls (about 230) to
  `CuBit.Monotonic.Milliseconds`;
- refining the PIT-calibrated rate against the HPET through the proved
  rebase protocol;
- the other publications, as per-publication pages behind manifest grants.

### PERF-003 — Serial verbosity: no unconditional serial output on hot or benchmarked paths (user, 2026-10-08)

The user asked whether benchmarks skip serial output. They do not: no
build or startup flag exists. Bench loops do not print, but kernel
lifecycle messages print unconditionally to the slow serial port, and so
do procmgr's per-launch lines. Examples: `reclaimProcess: stopped PID`,
spawn and loader lines. Process-heavy benchmarks pay for these.

Direction:
- A typed verbosity level: a CCL startup value for services, and a
  `Build` constant for the kernel, compiled out at the lowest level.
- Lifecycle and diagnostic prints are gated by it.
- Bench builds and profiles run at the lowest level.
- Headless markers the runner waits for stay. They are test protocol,
  not diagnostics, and are counted separately.

2026-10-10: serial output no longer stalls the printer (asynchronous kernel
console, "Console output is synchronous serial I/O"). Desktop's periodic
report is housekeeping: captured at the period boundary, written a line at a
time only while no input, request or completion is pending; Desktop's
loop-turn, input-to-present and pointer source-age latencies are permanent
metrics (keys 13-15). Volume is still a cost on idle CPUs and on the UART,
so the verbosity level remains open.

### PERF-004 — No linear scans over processes or slots (user, 2026-10-08)

The user: linear scans are no longer acceptable as performance becomes a
priority. KERN-003 steps 3–4 remove the eight the inventory found
(docs/process-objects.md, "Steps 3–4 plan"):
- reaping
- mailbox retirement
- grant-notice wakes
- the DMA pending round-robin
- PROCLIST
- owned-memory conflict checks
- `Kernel_Reports.Take` and `Close`
- `Id_Ledger`'s first-free search (O(N²) with its page scans)

Also `Grant_Windows.Allocate`, which scans up to 256 words: add a summary
level so it finds a free window in a constant number of steps.

New kernel code keeps per-process sets as lists or summarized bitmaps,
never whole-table scans.

### IPC-004 — The synchronous call graph is a DAG (user, 2026-10-08)

The user: "we need to keep the IPC graph as a DAG to the max extent possible
to avoid deadlocks." The GPU live image froze Desktop on the NUC. Every Mesa
glue call to intel-gpu used `Wait_Forever` on Desktop's main thread
(`userspace/mesa/anv/native_gpu_buffers.adb`, `native_gpu_query.adb`,
`native_gpu_presentation.adb`), and intel-gpu also calls Desktop (viewer
target requests). That cycle can deadlock, and either way a stalled driver
froze the UI.

Direction:
1. **Layering:** synchronous calls go only down: clients → compositor →
   display/GPU drivers → kernel. Anything flowing back up is an async event,
   completion or published state.
2. **Bounded waits:** no `Wait_Forever` toward a peer that may call back.
   Desktop's GPU calls take explicit deadlines, and a timeout falls back to
   software with the timed-out operation logged.
3. **Static check:** declared endpoints in manifests give the call graph.
   Reject cycles at image build, as part of the capability graph.
4. **Kernel check:** a call that would close a wait-for cycle (the callee's
   thread is blocked calling the caller, directly or through a short chain) is
   refused with a distinct error and counted, instead of hanging.
5. **Audit existing two-way synchronous pairs:** Desktop ↔ intel-gpu,
   Desktop ↔ procmgr, display ↔ Desktop, netstack ↔ drivers.
6. **GPU → CPU failover at runtime (Desktop).** The NUC GPU image froze
   about 30 s after boot, with or without input. Mesa's completion wait has
   no deadline, so any GPU or driver stall hangs Desktop's thread.
   - Prepared, not applied: capping `anv_cubit_sync.c` completion waits with
     the process's call bound, marking the device lost on expiry. Held back
     until the failover exists, because without it a lost device makes
     Desktop exit rather than freeze.
   - The gap: a GPU frame with an unknown outcome takes the `Unsafe` branch
     in main.adb and calls `exitCompositor (1)`, so Desktop exits instead of
     recovering.
   - Needed: quarantine every target and source the GPU touched (it may
     still write them), allocate fresh targets, and continue on the CPU
     renderer (`Desktop_CPU_Software_Renderer`).
   - Correction (2026-10-08): fence exhaustion was not the cause. The fence
     stream has wrapped since 0988b8ab (40,000 submissions pass). The driver
     now keeps contexts resident and logs submission and gate stalls; the
     freeze's cause is still unknown.
7. **Done 2026-10-08 (bounds and visibility, not yet a fix):**
   - display → intel-gpu calls wait at most 10 s (`GPU_Call_Deadline_Ms`),
     and desktop → display calls at most 30 s (`Display_Call_Deadline_Ms`).
     A timeout fails like a refused request and is logged.
   - The kernel draws a stuck-call panel above the heartbeat every 2 s
     (`Process.IPC.reportStuckCalls`): each thread blocked in a call for
     3 s or more, as caller → server, the seconds waited, queued or taken,
     and the server's state. It is meant for the NUC, which has no serial;
     headless runs get the same lines on serial as `STUCK-CALL: ...` (a
     healthy boot prints none).
   - Open: a filesystem.svc build stalled 3/3 boots at devmgr's first
     OP_SET_ACL (2026-10-09, before the serial report existed); the same
     binary booted fine later in a rebuilt tree. Timing-dependent, cause
     unknown; the next STUCK-CALL line should say which side waits.

### IPC-005 — Every request terminates (user, 2026-10-09)

The NUC GPU freeze (2026-10-09) was not a deadlock: intel-gpu's main loop
spun at 98% CPU with a pending stage that never cleared, and Desktop retried
the VM update (label 0x0A28) on every "unavailable" answer without bound. The
user agreed that the fix is in how requests end, not in the multi-server
design:

1. **No busy bounce.** A request that cannot run now is queued and later
   completed, failed or timed out (async submit + completion, the default
   for all clients); "unavailable, try again" over a synchronous call is
   removed from driver protocols.
2. **Bounded stages.** Each multi-step driver operation has a deadline; a
   stage that makes no progress faults that operation (and its context),
   replies with an error and logs it. Prove the step bound (SPARK level 2).
3. **Recovery.** Clients have a failure path: Desktop falls back to the CPU
   renderer (IPC-004 item 6); services become restartable (separate
   follow-up).

First target: intel-gpu's VM-update path, once the stuck stage is found.

## Developer tools

### APP-001 — CuBuilder: a Visual Basic-like GUI builder for native apps (user, 2026-10-08)

The user: "I want to eventually give CuBit a Visual Basic-like tool that
supports writing easy GUI Ada, Rust and/or CCL apps using our native toolkit
with nice drag-and-drop interface. Since we support DPI scaling it should
look and feel great no matter what, and be fast - no Electron or any gross
slow stuff like that. And we'll bake in IPC endpoints that make it easy to
drive from an AI agent running in CuBit."

Shape:
- **A form designer on the native toolkit** (userspace/lib/ui): drag
  widgets from a palette onto a form, set properties in an inspector, and
  wire events to handlers. The same toolkit renders the designer and the app
  it builds, so what you see is what runs.
- **Resolution independence:** layouts use the toolkit's DPI-aware,
  responsive primitives (UI-003 density/typography, UI-004 layout
  primitives), not pixel positions. Forms look right at every scale.
- **Speed:** native code and no web runtime. The designer and generated apps
  meet the render target (1 ms keypress-to-photon, software rendering first).
- **Languages:** forms are a typed CCL description (no hand-written keyword
  readers). They generate or bind to Ada, Rust or CCL handlers. CCL apps can
  run directly in the CCL VM. Ada and Rust go through the toolchain
  (SELF-001, BLD-001).
- **Agent-drivable:** typed IPC endpoints for everything the designer does:
  create and arrange widgets, set properties, attach handlers, build, run,
  inspect. An AI agent in CuBit can then build an app through the same
  operations a person uses. Agent authority follows docs/agent-security.md:
  capability-scoped, approved and visible.
- **Output:** an app bundle (Applications/<name>/<version>/) with its
  manifest, declared inlets and outlets, and requested capabilities.

Depends on: the shared toolkit's layout and widget reliability work (UI-003,
UI-004, UI-011), the CCL VM, and for compiled languages SELF-001 and BLD-001.

## Shared UI toolkit

### KERN-003 — Process identities that are never reused, and more than 256 processes (user, 2026-10-07)

Design: docs/process-objects.md (2026-10-07). Order agreed with the user:
this design, then IPC-002 step 3 built against it, then this, then
PERF-002.

**Status, 2026-10-08:** step 1 (64-bit identities on the wire, a typed
`Process_ID` in userspace and a private `Identity` in the kernel) is
built; see the design doc, "Step 1, as built". Steps 2–4 (global grant
table, process objects, raising the limit) are next. Follow-up from step
1: retype the remaining process words (completion `from`, CCL `Run`,
`Channel_Arenas`, lib/display and intel-gpu incarnation records, mixer
streams), and the ports' hand-picked runtime unit lists (doom, sameboy)
should link the built runtime library instead.

Since IPC-002 step 1 (docs/ipc-delivery.md), a PID is not reused until
every exit and fault report about it has been read, like a Unix zombie.
The scarce resource is now the process table's 256 slots, not the
identities. The user suggested fresh or random PIDs.

The user, same day: "the 256-slot table was always kind of a primitive,
proof-of-concept way to do it ... We should have a search tree or
something that supports more processes/slots."

Direction (to design before building):
- Process objects allocated on demand and found through a search
  structure (a balanced or radix tree keyed by identity), replacing
  `proctab` and `mailtab` and every other array indexed by process number.
- The grant global slot is owner index × 4,096, and the received-grant
  address region is laid out from it, so grant slots and their address
  ranges must be allocated rather than computed from the owner's number.
- New per-process state belongs on the process object, not in global
  tables. `Kernel_Controls` is already per-target state. `Kernel_Reports`
  is still a table indexed by subject, and its `Take` scans every possible
  subject; it needs recipient-side lists.
- Expose a 64-bit process identity that is never reused: the slot plus
  its generation, or a monotonic counter. The slot becomes a
  kernel-internal index. Stale-PID bugs go away by construction.
- Optionally randomise the identity's low bits. A PID grants nothing
  (authority is capabilities), but sequential IDs show how many processes
  started and when, a small side channel.
- Allocate the least-recently-freed slot rather than the lowest.
- No fixed process limit: slots, not identities, run out under zombies.
- Make unread exit reports (zombies) visible per parent.

### CCL source-view proof and remaining surface integration

`CCL.Language.Views` supplies bounded Lisp/BASIC conversion and formatting for
the current expression language. Both surfaces pass the existing analyzer;
tests cover canonical round trips, formatted-source idempotence, node ranges,
and preservation of paused Workbench VM inspection. The new core contains no
`Assume` or SPARK-Off sections. A printer scratch-buffer alias reported by
SPARK was removed by separating the compact-output spans from the read-only
layout spans. GNATprove still aborts internally (`Assert_Failure
sem_util.adb:7554`, during expansion of the bounded append helper); no complete
proof result is available. Minimize/report that tool failure and resume proofs,
without suppressing checks or substituting assumptions. Reproducer:
`tests/ccl-views/README.md`.

Next surface tasks: attach comments to syntax nodes rather than gathering them
above the expression, integrate BASIC with REPL completion/history and other
CCL entry points, and design multi-binding syntax without changing
scope/evaluation semantics. Infix arithmetic/equality and expression-valued
`IF … THEN … ELSE … END` are now implemented and regression-tested for
precedence, unchanged grouping/overflow, short-circuit branches, host admission,
and exactly-once left-to-right invocation. The existing `let` reader still admits
one binding. The Workbench can save/reopen BASIC using its syntax header;
this does not mean other CCL consumers already accept that surface.

### CCL functions: remaining runtime work

The typed-stream prerequisite now has a portable delivery-policy model:
`CuBit.Protocols.Stream_Policies`, with hosted boundary/matrix tests and focused
SPARK compatibility proofs. Still needed: versioned descriptor/wire encoding,
handshake validation before grants, runtime backpressure/close enforcement,
ownership and cancellation integration, and typed CCL stream combinators.
Do not treat schema-only subscription as enforcing the new delivery policy.
See [typed IPC](typed-ipc.md) and `tests/stream-policies`.

Named typed functions now run in the shared interpreter/REPL/Watch and round-trip
between Lisp and BASIC. The initial scope is non-recursive and non-capturing,
with 16 functions, 8 parameters, exact parameter/result types and unchanged
authority admission. Tests cover hostile edits, isolated frames, bounded depth,
fuel, copied text results and function-driven labels. SPARK flow analysis passes
105 initialization/termination checks; this is not a complete runtime-error or
functional proof. No assumptions or SPARK-Off escape hatches were added.

Runtime-owned handler references now support the explicit `() -> Boolean`
profile, owned checked-code snapshots and current-grant identity checks.
The bounded callback queue has generation/lifetime handling, explicit discards
and a focused SPARK proof; the dispatcher is serialized and synchronous.
CCL-visible `(handler name)` values, typed registration, and one real shared
Workbench button are now implemented, with headless lifecycle/authority tests
and rendered Linux-preview tests. See `tests/ccl-callbacks` and the
`button-clock.ccl` sample. The owned source/name transport is checked once at
registration; each click executes retained checked code. Next: connect this UI
owner/client model to native Desktop widget IPC and independent surfaces.
Also outstanding: CCLB call frames and
debug metadata, full function-signature/intellisense support, overloads,
ownership-aware captures, and reclaiming call-local text while preserving
returned values. Current text is invocation-owned and budgeted, not reclaimed
on each return. Definitions do not persist between REPL submissions.

### CCL session diagnostic formatter proof boundary

GNATprove on `ccl-sessions.adb` reports three unproved bounds for diagnostic
string concatenations in `Result_Image`. The message functions return
unconstrained String, so their finite message sizes are not available at this
call boundary. Use a bounded representation or another demonstrably provable
design; do not suppress checks or add assumptions. New admitted REPL submissions
and scoped numeric label hooks have hosted regressions, but this formatter and
the host adapter are not an end-to-end proof of session safety.

### Shared image decoding and a read-only image viewer

Status: deferred; return to CCL work first.

Evaluate an existing Ada image-decoding library for reusable desktop image
support, then build a small native viewer using the shared toolkit and file
picker. Confirm licensing, supported formats, freestanding-runtime dependencies,
memory requirements, and actual SPARK coverage before selecting a library.

Use native filesystem messages and scoped read-only handles for selected files
or an approved pictures folder; no write authority or unrestricted filesystem
access. Keep byte acquisition separate from decoding so Linux-hosted tests can
use the same decoder without introducing host file APIs into CuBit applications.
Image parsing stays in userspace, outside the desktop compositor; assess a
separate restricted decoder process if the chosen implementation warrants it.

Treat images as untrusted: validate dimensions and size arithmetic, bound decoded
memory and work, and test truncated/malformed images and decompression bombs.
Reuse aspect-preserving Fill/Fit/Center rendering and clipped damage handling.
Later this can support a wallpaper file picker and CCL image widgets. Keep the
current embedded wallpaper path as the safe startup fallback.

Package bundled wallpapers as separate read-only files on the Live CD rather
than only embedding rasters in `desktop.svc`. Expose a narrowly authorized
wallpaper asset folder through the shared picker and, where explicitly granted,
Files. Files currently opens only its NVMe/live-memory roots, not the optical
volume; add authorized source selection rather than granting whole-volume access
just to choose a background. Selecting an image must not confer write authority.
Retain a built-in fallback when media is absent or decoding fails, and publish
a new background only after successful loading/validation so the current one
survives errors.

### UI-011 — Complete the Win32-grade shared-widget reliability gate

Status: in progress

Apply the invariants, open findings, interaction matrix, and release gate in
[`ui-toolkit-audit.md`](ui-toolkit-audit.md). Keep interaction state machines,
geometry, input ordering, and minimal damage in the shared toolkit; Files,
Devices, and CCL Workbench are integration clients, not alternate widget
implementations.

### UI-003 — Add density and integer-scale typography

Status: open

The shared toolkit and native desktop now rasterize bundled TrueType fonts in
Rust, with bounded caches; the 13-pixel em/17-pixel line default preserves current
geometry. Scale notifications, adjustable raster sizes and widget metrics still
need integration. Do not stretch the completed framebuffer to implement DPI.

The portable multi-output geometry core now supports rational scales, signed
placement and all four rotations, with SPARK checks and hosted pixel tests.
A native QEMU fixture discovers three differently sized scanouts; it does not
yet render a multi-monitor desktop. Next add generation-bound output/session
ownership, then route composition, damage and input through the shared model.
See [display architecture](display-outputs-and-scaling.md) and
[geometry tests](../tests/display-geometry/README.md).

Connected-edge layout admission is now implemented in `CuBit.Display_Layouts`:
37 proof diagnostics and 6572 hosted arrangements pass. This is not yet wired
into native display configuration. See [layout tests](../tests/display-layouts/README.md).
Pure window placement and the bounded recovery timer are now implemented:
47 proof diagnostics and 231,563 hosted placement/timer cases pass. Successful
proposals preserve full decorated extents inside a ready work area; automatic
fallback does not mutate desired homes. This is not yet live desktop recovery;
see [placement evidence](../tests/window-placement/README.md).
The owner-local output registry and opaque placement tickets now add revision
and incarnation checks with typed presence/power/readiness and fail-closed
counter exhaustion. See [registry evidence](../tests/output-registry/README.md).
The display service now registers the boot-selected output and binds its lease,
attachment and session state to that reference. A compile-time native QEMU
fixture exercises stale-generation rejection and explicit reattachment recovery.
Native adapters still need authority-filtered discovery/topology notifications,
cross-service lifetime identities and window-intent revisions, then serialized
checked placement apply. There is still only one presented output.
Before wiring persistent layout, implement named-display/profile resolution
and revision/generation-safe application of placement plans described in
[desktop layouts and workspaces](desktop-layout-and-workspaces.md). Keep desired
home positions separate from temporary fallback placement during boot/hotplug.
Include all four rotations and workspace membership from the outset; a virtual
desktop is not a physical output or a security boundary. Settings/CCL Config
must edit the same typed model, with explicit confirmation and save status.

### UI-004 — Add bounded responsive layout primitives

Status: open

Add row, column, grid, split-pane, padding, and minimum/maximum-size primitives
that compute checked rectangles without allocation. Workbench panes and native
applications should respond to surface size without duplicating coordinate
arithmetic or risking underflow at small dimensions.

Use the CCL Workbench as the acceptance case: its menu, semantic toolbar groups,
execution/source/bytecode panes, editor scrollbar, and status bar should be
declared as a bounded layout tree rather than a collection of absolute `x`, `y`,
`w`, and `h` literals. The layout result remains ordinary checked rectangles so
rendering, hit testing, damage tracking, and CuBit IPC never depend on hidden
native widget state.

### UI-005 — Define text overflow behavior

Status: open

Text-bearing widgets need explicit clip, ellipsis, horizontal-scroll, or wrap
policies. Each policy must be bounded and safe for proportional fonts. Editable
fields should keep the caret visible without allowing text to escape the field.

### UI-006 — Add chart primitives with units and scales

Status: open

Promote the Workbench's hand-drawn bars into bounded series, axes, units,
legends, and empty/error states. Data bounds and sampling policy must be
explicit so monitoring widgets cannot allocate or render without limit.

### UI-007 — Make CuBit Classic the native application default

Status: open

Apply the CuBit Classic theme by default to every native application using the
shared desktop widget toolkit, including Devices and future CCL-facing
applications. Migrate applications deliberately with screenshot and
interaction regressions so palette changes do not hide focus, authority, error,
or disabled states. Centralize the licensed window-control icon atlas rather
than duplicating hosted-preview masks. Choose and document a stable public name
for the toolkit itself; "CuBit Desktop Toolkit" is the provisional name.

### UI-008 — Add source-editor navigation and diagnostics

Status: open

Add an optional line-number gutter and a legible monospace editor font without
changing the proportional typography used by ordinary desktop controls. Add a
diagnostic marker bar beside the editor that maps the interpreter's bounded,
one-based source position to the affected line and highlights that line after a
parse or type-check failure. The marker must be derived from versioned
diagnostic data, remain aligned while scrolling, and disappear or become stale
when the document changes rather than implying that an old result still applies.

### UI-009 — Complete mouse-free desktop and widget navigation

Status: open

Make every essential desktop operation usable without a pointer. The desktop
now provides the first vertical slice: Super opens the Apps menu, Up/Down move
through launchable entries, Enter launches the selected application, and Escape
closes the menu. Extend this into a shared, consistent toolkit contract rather
than implementing application-specific key handling.

Define focus order and visible focus indicators for every interactive widget;
Tab and Shift+Tab traversal; arrow-key behavior within menus, lists, grids,
trees, tabs, sliders, and scrollbars; Enter/Space activation; Escape/cancel
semantics; window switching and window-control shortcuts; and keyboard access
to context actions. Modal surfaces must trap focus intentionally, disabled and
hidden controls must not receive focus, and client surfaces must not be able to
spoof or consume desktop-owned shortcuts. Add interaction tests that exercise
the entire Apps-to-application path with no mouse events.

## Configuration

### CFG-001 — Typed config values end to end, in a binary typed codec (parent, 2026-10-08)

Owner: the config owner (the main session). Follows UI-013.

Since UI-013, a setting whose key has a declared type (`CCL.Typed_Settings`;
first user `desktop.launch.*`, declared in `CCL.Interfaces.Desktop_Launch`)
is type-checked by the CCL analyser when the profile compiles (devmgr,
config activation, the host `ccl-config` preflight). It is stored and
carried as the value's canonical source text. The desktop reads it back
through the same typed path and accepts only that canonical spelling
(option A in the UI-013 decision).

The full form (option B) still needs:
- a compact binary typed codec carrying a schema key: not the 16 KiB
  native `CCL.Objects.Image`, which is not a disk or network codec;
- binary-safe values through devmgr's seed format, config.svc storage and
  its IPC, and the `CuBit.Config` client;
- typed rendering in Config Inspector and the Workbench;
- consumers decoding with `CCL.Objects.Views` against the bound contract
  instead of analysing canonical text.

See also: docs/ccl-boot-configuration.md ("Typed settings") and
docs/config-declarative-state.md (typed schema binding).

Known failing (noted 2026-10-08): `tests/ccl-configurations/
test-configurations.py` `test_all_existing_profiles` fails for system.conf,
system-live.conf, init-usb-live.conf and init-desktop-session.conf. These
failures predate UI-013: the fixtures in `tests/ccl-configurations/fixtures`
are stale, with no `desktop.launch.*` or `config-storage` entries. Refresh
the fixtures from `ccl-config --dump-plan` once their contents are agreed.

## CCL console

### CCL-001 — Launched programs' output as live, re-wirable buffers

Status: open (requested by the user, 2026-10-03)

When the CCL console starts a program, its stdout and stderr each go into a
console-owned buffer that CCL can name and manage, rather than straight to a
fixed destination. While the program runs, the person can redirect a stream
(into a file, another program's input, the log store, or a view) and then
un-redirect it, live, without restarting the program and without losing
output during the switch. A buffer keeps a bounded, inspectable history and
reports what it dropped.

Shape, building on docs/ccl-streams.md (phase 5 `launch`) and the self-hosting
plan (docs/self-hosting.md, item 5):

- `(launch "gcc" (arguments ...))` returns a process value whose `stdout` and
  `stderr` are typed `Stream<String>` buffers held by the console.
- Wiring is a CCL value that can be changed: attach a sink, detach it, attach
  another, tee to several. The buffer stays the source of truth, so a sink
  attached late can replay what is retained.
- Transport: the program writes its fds 1 and 2 into shared rings
  (CuBit.Channel_Rings, as the log streams now do), so redirection is a console
  decision and never needs the child's cooperation or a restart.
- Back pressure is per buffer: a full buffer either sheds (counting losses) or
  makes the writer wait. The choice is visible and can be changed.
- The buffers are visible in the console (and later the stream graph
  inspector): rate, size, drops and current wiring.

Depends on: posix_spawn file actions / descriptor inheritance (self-hosting
item 4) and the CCL launch built-in (item 5).

### CCL-004 — Per-file delegation from a manifest's argv grammar (user, 2026-10-05)

Status: open, after the gcc driver runs end to end with decision D2
(docs/self-hosting.md): the libc's `posix_spawn` passes on, whole, the places
the caller was delegated.

The narrower step is generic, with no per-tool code. A program's manifest
already declares how its typed parameters render into argv; `posix_spawn`
runs that mapping in reverse:
1. It asks procmgr for the child's description (`OP_PROGRAM_DESCRIPTION`).
2. It parses the argv the C program built with the child's own declared
   grammar.
3. It delegates exactly the files and directories that parsing names.

Only arguments that carry authority are declared (inputs, outputs, `-I`
directories); other options pass through as opaque text. The grammar covers
joined and `=` forms, `--` and response files once, generically.

- **Safety:** it only narrows D2. procmgr still checks every place against
  what the launcher holds, so a misparse can make a tool fail but never
  widen what it may touch.
- **Unknown flags:** refuse the launch. A manifest without a grammar gets
  nothing narrower than D2.
- **Files not named in argv** (cc1's system headers, the driver's temporary
  files): declared places, such as a toolchain parameter with a default and
  a temp place.
- **Scope:** only the handful of ported tools need this (binutils,
  gcc/GNAT). If it makes legacy Unix tools harder to bring over, that is
  intended (user, 2026-10-05): CuBit is an alternative, not a bridge to Unix.
- **Visibility:** the console can show which exact files a C launcher's
  child may touch.

### CCL-003 — Runs as affine resources (user decision, 2026-10-05)

Status: in progress (step 1).

A `Run` is plain data today, which causes three problems:
- **Accessors aren't tied to the right program.** `(as.outcome r)` with an
  `ld` run type-checks and only fails at run time.
- **A run can be forged:**
  - Any code in the session can write `(Run program => … pid => …)` and
    read the outlets of a run it was never given, a confused deputy.
  - Once runs take messages (CCL-002), it could also act on such a run.
- **Its lifetime is a guess:** the program bindings keep the 8 newest runs.

The fix uses only existing machinery (docs/ccl-type-system.md §6–7). No new
modes, contract forms or method system:
- Each program's run is its own **affine** resource type (`Ld_Run`,
  `As_Run`), from its approved policy. A run cannot be copied or written as
  a literal; it may be dropped.
- Operations on a run take it as a **borrowed receiver**, so `r` stays
  usable: `ld.outcome`, `ld.outlets`, and each outlet accessor. Their
  results (streams, tasks) stay plain handles.
- **Dropping a run releases it:** its rings, and the pins on its streams
  and outcome. This replaces the 8-run heuristic.

Steps:
1. **Session-held affine resources.**
   - `(define bad (as.run …))` makes the session the owner of the run, and
     rebinding the name drops it.
   - `:env` shows what the session holds.
   - The table is bounded, with exactly one owner at a time and exactly one
     drop. This is proved, as the owned locals are (tests/ccl-owned-locals),
     with tests.
2. **Runs as per-program resource types**, as above.
   - The console binds an unnamed run to a generated name instead of
     writing a run value.
3. **Discovery by receiver type:** completion and the Observatory list the
   operations that take the value in hand.
4. **CCL-002 messages** become operations on the same run type.

### CCL-002 — Program messages as typed IPC (user decision, 2026-10-05)

Status: open.

A program's manifest declares a `messages` list: single commands such as
"shutdown" or "reload", each with a qualified name and a typed payload. The
launcher sends one as an ordinary typed IPC call, and it answers with a
`Task` of a typed result, e.g. `(mixer.com.example.mixer.reload r) :
Task<Reload_Result>`. Messages are for infrequent commands where throughput
does not matter. Data flow stays on inlets and outlets (lent rings), and a
current state such as a volume is a `Level` inlet, not a message. Stopping a
run is a procmgr operation on any run, not a per-program message.

Why IPC rather than rings: a request/reply call completes the `Task`
directly. The capability model also checks authority per operation, so the
console can be granted `reload` without `shutdown`.

Most of the parts already exist:
- typed calls and the `Interface_Catalog`/`Granted_Bindings` split
  (docs/typed-ipc.md);
- the CCL host-import IPC adapter (`ccl-test-host`);
- `Task<T>`, `await`, and the console resuming an entry when its task
  completes;
- program interfaces generated from a description
  (`CCL.Interfaces.Programs`).

To build:
- **Discovery.**
  - The `messages` declaration goes in the executable manifest and the
    program description, so the launcher learns each message's name,
    payload and result types.
  - The launcher gets the program's endpoint from procmgr at launch, as
    authority it is given. Matching a protocol must never mint or find an
    endpoint (docs/typed-ipc.md, "Authority boundary"; the unfinished
    live-discovery boundary in docs/typed-commands-and-events.md).
- **Reflection.**
  - Generate each message's CCL operation and types from the declaration,
    as `ld.run` and `ld.outcome` are generated today. Check them with the
    type checker and publish them in the catalog. Grant each operation
    separately.
  - The reply completes the operation's `Task`: submit, then completion,
    never a blocking call from the console.
- **Tests:**
  - a hosted program with two messages, one of them not granted;
  - each message is typed in both engines;
  - the reply completes the task;
  - a guest run with a native program.

## Storage and files

### FS-020 — A deliberate directory layout (user-requested, 2026-10-04)

CuBit's own directory layout is flat: on the boot disk and the ISO,
programs, services and drivers (`*.app`, `*.svc`, `*.drv`), data (`tls/`,
`fonts/`, `doom1.wad`), startup profiles (`init.ccl`) and work places
(`work/`) all sit at or near the root, and `@cd:0` is an `apps/` tree. With
self-hosting, compilers, a sysroot, build outputs and a build store
(BLD-001) arrive. To consider:
- **Kinds of things:** programs and services by package (identity), system
  data, per-user and per-session places (CTX-001), temporary places,
  the build store, logs once logstore persists.
- **Names stay CuBit names** (`@nvme:0/...`, `@boot`, `@cd:0`), and launch
  tables keep naming programs exactly, with no search path; a layout change
  updates the tables, not a PATH.
- **Grants follow the layout:** manifests scope to subtrees (a tool gets its
  package's directory read-only, a session its own place), so the layout is
  also the authority map. Keep paths short (docs/self-hosting.md: 4096-byte
  limit, 256-byte scopes).
- **Images:** images/artifacts.ccl and the disk tools place files by the
  same layout. The repository tree may want the same review.

**Direction (user, 2026-10-05):** no Linux FHS. On the system volume:
`Applications/<name>/<version>/`, with siblings `Services/` and `Drivers/` of
the same shape. For example, `Applications/gcc/15.3.0/` holds the driver,
cc1, gcc's support files, specs, and the libc's headers and start files it
compiles against. Installing is unpacking a folder, and removing is deleting
it. Points to settle when this is implemented:
- **Self-contained and read-only:** a program gets its own version folder
  read-only (that is its grant), and needs nothing outside it.
  - GCC finds everything relative to its own name; `--prefix` only sets
    defaults.
  - Static linking means no shared library directory.
- **Mutable state outside the bundle:** in the user's or session's places
  (CTX-001), so removing a version never loses data and leaves nothing
  behind in the bundle. Deleting app data is a separate, visible choice.
- **Which version a name means:** a CCL catalog, not symlinks (CuBit has
  none). Launch tables keep naming exact paths.
- **Unpacking installs but grants nothing:** a manifest's requests are
  reviewed and granted visibly. Discovery can scan
  `Applications/*/*/` for manifests.
- **Config goes with its app (user, 2026-10-05).** State is tied to the
  app's identity, so removing the app removes its config. The bundle's
  manifest declares the collections it owns (namespaces owned by its
  identity, docs/config-contexts-and-inspection.md), each with a lifecycle:
  - *config:* removed with the app;
  - *cache:* removed with the app, and may be dropped any time;
  - *documents:* the user's, never removed with the app.

  How removal works:
  - Collections belong to the app, not a version: they survive upgrades and
    go with the last version. A collection declared per major version
    (`...v8`) goes with that version.
  - **Normal removal** is a CCL operation (`(apps.remove gcc)`): one visible
    step that removes the bundle and everything it owns.
  - **A plain delete** of the folder still works: the next scan finds
    collections whose owning identity has no installed bundle and collects
    them as orphans, shown in the transcript.
  - The config service enforces that only the owning identity (or the user,
    through an inspector) writes a collection, so ownership can be trusted.
    This and registration at install are still planned there.
- **`System/<name>/<version>/` (decided, user 2026-10-09):** the OS core
  (kernel, procmgr, filesystem, devmgr and the other stage-1 services) gets
  its own sibling of the same shape, so `Applications/` stays safely
  removable. No binary has moved yet: they still sit at the volume root and
  in the initrd.
- **`Assets/<name>/<version>/` (decided, user 2026-10-09):** read-only data
  packages (wallpapers, later icons, fonts and sounds) as a fourth sibling.
  The rules are the same: read-only, self-contained, views over the
  Storehouse later; plain directories on the disk images for now. The root
  is one setting, `system.assets.root`, so moving it under `System/Assets/`
  later changes configuration and image placement, not programs.
  - **As built (2026-10-09):** `Assets/cubit-wallpapers/1/{cubes,cubie}.qoi`
    on the development and desktop disks (`@nvme:0`), the headless test disks
    and the USB live images (`@cd:0`). Desktop reads the package through the
    filesystem queue with a read-only scope on exactly it, decodes with the
    proved streaming QOI decoder, and falls back to the flat theme colour
    with a logged warning. The wallpapers left `desktop.svc` (13.5 MiB of
    linked rasters; 4.6 MiB of QOI on disk). `docs/assets.md` covers layout,
    formats, adding an asset and theme selection.
  - **Later:** `Assets/cubit-icons/1/`, once the icon atlas is loaded at run
    time (it is about 50 KB, baked by the atlas generator, and stays linked
    for now). A package version catalog (which version a name means), as for
    applications. The all-in-initrd laptop fallback image has no asset
    volume.

**Storehouse and views, the Guix/Nix model (user, 2026-10-05).** CuBit takes the
Nix model, with clean names:
- **The system declaration** (CCL) lists applications, services and drivers
  with pinned versions and inputs, and each one's declared config.
  Activating it makes a **generation**. Launch tables and the catalog that
  maps a name to its exact program are generated per generation.
- **The Storehouse** (`@system/Storehouse/<hash>/`) holds immutable, read-only entries
  keyed by content hash (BLD-001). Two builds of one version can coexist
  there. Hashes appear only in the Storehouse.
- **`Applications/`, `Services/`, `Drivers/`** are views the filesystem
  service generates from the active generation: `Applications/gcc/15.3.0`
  names the Storehouse entry that generation selected.
  - Switching generations swaps the view at once, so upgrades are atomic.
  - Rolling back restores the previous view.
  - No symbolic links: the filesystem service resolves the names.
- **Identity is metadata:** the content hash is recorded on the Storehouse entry
  (an extended attribute, which ext2 keeps). It is pinned by the
  declaration, verified at activation, and shown by the CCL inspector.
- **Grants attach to the Storehouse entry**, not the view name: re-pointing the
  view cannot redirect a program's own read-only grant.
- **Install and remove are CCL operations.** Importing a folder hashes it,
  seals it read-only and records it. Removing drops it from the declaration;
  garbage collection deletes Storehouse entries no kept generation references.
  The Storehouse is read-only to everyone but the software manager.
- **Owned state outlives the edit, for rollback:** declared config is in the
  declaration and rolls back with it. Owned application state is deleted when
  the last generation referencing the app is collected.
- **The software manager (`softman.svc`) is the only writer (user,
  2026-10-05).** It is separate from `config.svc`, which serves config
  values. It seals entries into the Storehouse, activates generations and
  collects garbage, and nothing else.
  - **Small and proved:** it aims for full SPARK proof. Fetching substitutes
    over the network is a separate, unprivileged service that hands over
    archives, which this one verifies (hash and signature) before sealing.
  - **Enforced by capability, not by name:** the startup declaration grants
    it the Storehouse-write capability. The filesystem service honors only that
    capability, and nothing can obtain it later.
  - **Authorized requests only:** activations and imports come from the user
    through CCL, with the change shown first (added, removed, granted,
    config changes). Who approved what is recorded in the generation record.
  - **Signatures:** it verifies signatures on imported and substituted
    entries and signs the generation records it writes, so activation (and
    later the boot path) can check that the active generation is its own.
- **Read-only, without exception.** A Storehouse entry is immutable once
  sealed: only the software manager writes it, while building or importing it
  in a private staging area, and nobody (the app, the user, an
  administrator) modifies it afterwards. A change is a new entry with a new
  hash. The views are read-only and change only through activation.
  - **The filesystem service** refuses every other write to the Storehouse and
    the views.
  - **Grants:** a program holds its own entry read-only, and nothing ever
    holds write access to an installed program.
  - **Activation** re-verifies entries against their pinned hashes, which
    catches offline tampering. Per-launch checks belong with signed
    executable admission.

  Mutable state lives elsewhere: declared config in the declaration, app
  state in the config service, documents in the user's places, caches and
  temporary files in their own places. Ported programs that write into
  their install directory get a declared place instead.
- **Config is generational too (user, 2026-10-05).** A generation records
  both the program views and the config revision
  (docs/config-declarative-state.md). Activation switches both at once and
  rollback restores both. Realized config revisions are content-addressed
  like Storehouse entries: shared when identical, pinned by hash, and collected
  when no kept generation references them. Mutable application state uses
  versioned migrations at activation, with the old revision kept until its
  generation is collected; copy-on-write snapshots may follow once the
  filesystem journal exists.
- **Exports** are archives that carry their hash (copying a folder off CuBit
  drops the metadata); importing recomputes it.
- **Costs:** generated directories in the filesystem service (a mapping
  layer over ordinary ext2 directories), and the view persisted with each
  generation record.

The gcc port moves from its interim `@nvme:0/toolchain` place to
`Applications/gcc/<version>/` first, as an ordinary directory until the
Storehouse and views exist.


### FS-001 — Replace hardware-specific filesystem backends

Status: in progress

Define one typed, versioned block-device interface for ATA, NVMe, ATAPI, USB
mass storage, and memory devices. It reports logical block size, block count,
read-only state, alignment, transfer limits, and supported operations. Keep
storage discovery and mounting in a control plane, then delegate a restricted
direct endpoint to the filesystem data plane so bulk I/O adds no broker hop.

Add kernel-tracked derived memory loans before using client pages directly with
drivers. The current grant primitive has no parent/child lifetime tracking and
must not implicitly re-grant a borrowed mapping. Derived ranges and permissions
must attenuate their parent, and DMA loans remain pinned through terminal
completion. Add an IOMMU mapping object so drivers receive bounded I/O virtual
addresses rather than unrestricted physical addresses.

Ordinary grants now fail closed on received-grant ranges, and the kernel
implements persistent generation-tagged references plus explicit
acquire/use/return lifetime. The first acquisition pins backing frames;
revocation and owner teardown become pending until the final return; grantee
teardown forcibly returns its inbound acquisitions. `Block.Device.V1` and the
application-facing filesystem operations acquire and return typed references
with direction-specific range and access validation. The live diagnostic checks
direct and capability-directed acquisition, pending revocation, final return,
stale generations, wrong owners, access attenuation, and bounds. Continue
migrating remaining single-word grant protocols.

The new SPARK `CuBit.Filesystems` package is the Ada-side protocol definition
site and constructs generation-bearing requests. Filesystem.svc, config.svc,
procmgr.svc, and storage-check now build against it. Migrate the shell's
remaining hand-built requests and keep the C ABI definitions generated or
cross-checked from the same schema rather than maintaining parallel constants.

Filesystem and config ACL administration no longer trusts the first caller.
Both resolve the currently registered devmgr/procmgr roles on each policy
operation, avoiding stale cached-PID authority, and storage-check requires a
normal filesystem endpoint's self-grant attempt to be denied. Replace this
interim role lookup with distinct policy-operation capabilities so the receiver
does not need to infer authority from process identity.

Acquired single-hop grants are now lifetime pins. Next add typed parent/child
loan derivation, cancellation semantics, and pinned-memory quotas before direct
application-to-device I/O. Cover acquire/revoke/return and owner/grantee death
with concurrent negative tests rather than relying only on synchronous callers.

Remove `@ata:` and `@nvme:` from the application-facing namespace. Mount
logical volumes under policy-selected names such as boot, system, and work;
backend type must never change an authorization result.

### FS-002 — Add the Files explorer and resource chooser

Status: in progress

Build a native `Files` application using the shared tree, grid, editor, and
dialog widgets. It should browse only explicitly supplied roots and support
bounded directory paging, sorting, selection, change streams, and asynchronous
file operations.

The same application provides desktop-owned Open, Save, Export, and Select
Folder interactions. It must display verified requester identity and requested
rights, then return an attenuated typed handle—not merely a pathname—to the
requesting process. Keep volume administration, preview parsers, thumbnails,
and indexing in separately authorized components.

`Directory.Page.V1` now replaces the newline-list prototype with distinct,
generation-tagged directory handles and fixed one-page typed replies. Cursors
are owned by filesystem.svc, each ext2 record is validated before its name is
viewed, and malformed media has a focused negative QEMU test. The first native
read-only Files window consumes the interface, follows pagination, validates
reply layout, and is available from Apps. It currently lists an explicitly
granted NVMe or live-memory root. Child-handle traversal, in-folder refresh,
and Back through retained handles are implemented, with nested-folder QEMU
tests. Sorting, change streams, rich metadata, and chooser delegation remain.

Navigation retains at most 16 handles, displays at most 128 entries, and
publishes a new listing only after validation. The first controls are Open/Enter
and Back/Backspace; double-click activation is still pending. Escape closes Files.

2026-10-08: The rewrite is designed in [files-app.md](files-app.md). It is a
two-pane Commander-style view over the filesystem request queue, with
streaming listing, incremental sort and filter, and a 1 ms latency budget.
The shared SPARK policy units, the view and a Linux-hosted harness
(tests/files-app) are done. The harness has a mock service over the real
queue layout, an SDL2 window and benchmarks. Remaining work:
- switch the native main to the shared view;
- copy, move, mkdir, delete and rename through the queue, with progress and cancel;
- the protocol gaps listed in files-app.md (queue rename, server-side copy,
  change subscriptions, packed directory pages, a list-places query, and a
  completion wake the UI loop can wait on);
- the roadmap there (preview, viewer, thumbnails, tabs, bookmarks, search,
  archives, multi-rename, compare/sync, and CCL hooks).

### FS-007 — Filesystem protocol, second round (what Files needs)

Status: done (steps 1-3 2026-10-08, 4-7 2026-10-09); open follow-ups below

Fill the request-queue gaps the Files browser found, each end to end
(protocol, service, client runtime, tests). Design:
[filesystem-protocol-v2.md](filesystem-protocol-v2.md).
1. Done: completion wake a client's own event loop can wait on
   (`OP_FS_WAKE` submitted asynchronously; `CuBit.Filesystem_Sessions`;
   proved `Queue_Wakes`; storage-check `QUEUE-WAKE-CHECK`).
2. Done: rename and move as a queued request (`Queue_Rename`; libc
   `rename()` uses it; storage-check `QUEUE-RENAME-CHECK`).
3. Done: `Directory.Page.V2` (`CuBit.Directory_Pages`), metadata per
   request, a checked resume token (`Queue_Seek_Directory`); V1 removed.
4. Done: change notifications (`Queue_Watch`/`Queue_Unwatch`, event ring,
   `CuBit.Filesystem_Events`, proved `Watch_Reserve`: overflow becomes
   `Rescan_Needed`, every watch ends with `Watch_Ended`; `QUEUE-EVENTS-CHECK`).
5. Done: server-side copy (`Queue_Copy`/`Queue_Cancel`, progress words,
   deadline, busy past four; proved `Copy_Slices`; `QUEUE-COPY-CHECK`).
6. Done: granted-scope query (`Queue_List_Scopes`, `File_Access.Encode`;
   `QUEUE-SCOPES-CHECK`).
7. Done: free space per volume (`Queue_Describe_Volume`,
   `CuBit.Volume_Descriptions`; `QUEUE-VOLUME-CHECK`).
Follow-ups: `Attributes` events (mode/time changes) are not reported; libc
`statvfs` could use `Queue_Describe_Volume`; the service removes no
directory that uses indirect blocks (rmdir answers unsupported after a
folder grew past 12 blocks).

### FS-003 — Add logical per-application storage roots

Status: planned

Construct each process's filesystem view from launch-time handles with friendly
purpose names such as documents, pictures, project, application-data, cache,
and temporary. Required private storage and optional user-gated collection
access must be distinct manifest requests. Recent files and bookmarks retain
object identity and provenance and are revalidated before reuse.

### FS-004 — Complete ext2 allocation and file growth

Status: in progress

The first-block defect is fixed. Block and inode allocation now walk every
group, use group-relative bitmap indices, validate mounted geometry, and reject
duplicate frees that would inflate counters. Fresh partial data blocks are
zero-initialized before their inode pointer becomes visible. Checked block I/O
and explicit read/write outcomes prevent short ATA/NVMe transfers, no-space,
unsupported file extents, and transport failures from masquerading as success.
Allocation metadata updates attempt rollback when a later write fails.

The focused storage test now uses an intentionally sparse file on an image
whose first four block groups are full. It requires successful first-block
allocation and data verification, rejects an unsupported extent with its exact
typed reply, and creates, writes, reads, and closes a second file through the
live filesystem service.

Remaining work is to make every directory, truncate, free, and metadata-read
API return an explicit outcome; add direct-to-single-indirect boundary,
no-space, injected-device-failure, and remount-persistence tests; and define
the recovery story for a failure during rollback. Ext2 has no journal, so
power-loss consistency and online transport failure cannot be claimed merely
from best-effort reversal of completed metadata writes.

The latest audit found a concrete safe-save blocker: `Ext2.renameEntry` removes
the source name before adding the destination, and directory add/remove still
discard metadata-write status. Harden these around a shared validated record
iterator and an explicit commit/failure model before adding Save/replace UX.
See [filesystem maturity](filesystem-maturity.md) for the findings and tests.

### FS-005 — Add crash-consistent filesystem journaling

Status: planned (2026-09-28). Decision: mimic Linux. The journal is
ext3/ext4 JBD2 in data=ordered mode, compatible with Linux on disk
(volumes made with `mke2fs -j` mount on both). The earlier preference for
a smaller CuBit-specific scheme is superseded: JBD2 is proven and
reliable, and its performance is good. Order of work:
1. A block cache and per-request allocation batching, still write-through.
2. The device durability contract below.
3. Replay (proved codecs).
4. Transactions, with a write-back cache.
5. Crash injection.

The paragraphs below still apply as requirements.

Add metadata journaling before claiming crash consistency for writable
persistent filesystems. First define and implement durable completion in
`Block.Device.V1`: ordinary write completion, cache flush, and force-unit-access
must have distinct semantics supported by ATA and NVMe. A journal cannot make
correct ordering guarantees on top of an ambiguous device-completion contract.

Prefer a small, bounded transaction and recovery model that can be analyzed in
SPARK before attempting full ext4/JBD2 compatibility. It must cover descriptor
and commit records, sequence and wraparound handling, checksums, revoke
semantics, ordered data-before-metadata publication, checkpointing, and
idempotent replay. Validate it with deterministic crash injection after every
durability boundary. If the on-disk format is CuBit-specific, describe it as a
transactional ext2-derived filesystem rather than ext4.

### FS-006 — Validate hostile-volume aliases and block ownership

Status: deferred; not a blocker for ordinary Ext2 interoperability or current
Config/Turso file-I/O work.

Treat imported disk metadata as untrusted input. Eventually check directory
references against inode link counts, detect distinct inodes sharing data or
indirect blocks, and reject data mappings into reserved filesystem metadata.
Use bounded validation with explicit resource limits and adversarial fixtures;
keep the on-disk format standard.

Current regular-file admission rejects reported zero/multiple links and does
not follow symlinks. Retain those checks, but do not describe them as proving
the absence of aliases on a malicious image. The existing bounds, feature and
I/O checks remain in force while this broader ownership validation is deferred.
See [Ext2 interoperability](ext2-interoperability.md).

## Native clock/runtime follow-up

### Speed up build-time timezone generation

Status: deferred; current generator is local-only but CPU-heavy.

`tools/generate_time_zones.py` samples every bundled IANA timezone at six-hour
intervals across 2000–2099, then validates explicit transition boundaries.
Replace normal-build sampling with direct TZif transition extraction and
expansion of future recurring rules, preserving the supported date range,
typed zone identities, and alias-table sharing. Keep independent sampling and
boundary comparisons as regression tests rather than routine build work.

Narrow the generated-table prerequisites in `kernel/Makefile`: unrelated
`flake.nix` edits (such as font dependencies) should not trigger regeneration.
Track the actual pinned tzdata and generator/toolchain inputs instead. Add
progress reporting and measure cold-generation time; verify incremental builds
skip unchanged inputs and rule updates still rebuild correctly. No network
service is needed for generation.

### Remaining runtime integration

- [ ] Implement and test standard GNAT `Ada.Calendar`, `Time_Zones` and
  `Ada.Real_Time` adapters over CuBit clock authority, keeping wall time separate
  from monotonic deadlines. Cover full standard ranges and DST ambiguities;
  the current native `CuBit.Clocks` client is not a substitute for these packages.
- [ ] Separate NTP/NTS synchronization service and delegated clock-adjustment
  authority; validate source/freshness/uncertainty and step/slew policy. See
  [clock and time services](clock-and-time-services.md).
- [ ] Extract taskbar audio controls into shared toolkit widgets, add keyboard
  slider navigation, themed icons, accessibility and Config persistence.
