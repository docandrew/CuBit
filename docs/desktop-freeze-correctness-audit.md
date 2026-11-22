# Desktop freeze correctness audit

Date: 2026-09-06

This is a focused incident investigation and assurance inventory, not a
completed whole-system audit or proof of crash freedom.

Follow-up: [interrupt/lock handoff specification and proof boundary](kernel-interrupt-handoff.md)
records the implemented state ADT, corrected SPARK annotations, two proved
properties, and newer regression evidence. The incident evidence below remains
the historical basis for that work.

## Observed failure

The retained `kernel/serial_output.log` contains a double fault before an
application launch completed. Against the matching kernel ELF, the saved RIP
`0xFFFFFFFF80122EAA` resolves to `syscallReturn`, specifically `sysretq` in
`kernel/src/syscall_entry.asm`. The saved CS is kernel code (`0x8`), RSP is a
userspace address (`0x00007FFFFFFFFDB8`), and RFLAGS is `0x10296`, with IF set.
RCX contains a canonical userspace instruction address (`0x401051`).

This establishes a kernel fault, rather than just a stuck application. The
saved state is consistent with interrupt delivery during the unsafe interval
between installing user RSP/GS and completing SYSRET. The log does not include
the first fault's vector or a complete execution trace, so that exact sequence
is an inference. The retained log shows a Files launch; it is not evidence that
both reported Workbench and DOOM freezes have independently been reproduced.

## Confirmed context-state defect and repair

`pushCLI` stores the outer critical section's prior IF in the per-CPU
`intsEnabled` field. `popCLI` uses it to decide whether to enable interrupts.
The scheduler enables interrupts before acquiring `Process.lock`, setting
that field to true. Previously, the register-only context switch transferred
control into a suspended syscall without restoring its original false value.
Releasing the handed-off lock could consequently enable interrupts inside the
syscall. Direct IPC switches could also inherit another context's policy.

`Process.switch` now saves this policy with the suspended execution context
and restores it after the assembly switch. It obtains the per-CPU address
again after resumption to accommodate migration. All scheduler and direct IPC
switch sites pass through this wrapper. New contexts begin with interrupt
restoration disabled until their initial IRET. The final syscall and interrupt
return sequences explicitly disable maskable interrupts before restoring the
destination stack/GS; SYSRET/IRET restore the destination interrupt state.

These changes do not establish safety for NMI/SWAPGS windows or invalid SYSRET
targets; those require separate architectural review.

## Regression coverage gap

The headless runner checked for triple faults but omitted double faults. A
partially functioning SMP guest could therefore satisfy startup markers even
after one CPU faulted. The failure matcher now also rejects double faults,
machine-check exceptions, and the kernel's explicit exception diagnostic.

The existing Workbench test used a linear framebuffer. A new
`ccl-workbench-virtio-vga` profile exercises the same virtio scanout and page
flipping path as the interactive desktop. Both Workbench variants now stage
the current desktop/display service binaries instead of relying on the copies
already present in the base disk.

The DOOM regression now injects Escape, Enter, movement, and fire key events
after graphics initialization. This exercises input delivery while rendering
and audio run; serial markers alone do not certify which game/menu screen is
visible.

## Validation

The kernel was rebuilt in `nix develop`; the kernel stack-usage build gate
passed. Inspection of the generated `Process.switch` code confirms the saved
Boolean survives in a callee-saved register and GS is read again after the
assembly context switch.

* `ccl-workbench-virtio-vga --accel kvm --timeout 25`: passed; native window,
  first frame, and virtio page flipping observed.
* `desktop-doom --accel kvm --timeout 40`: passed with injected menu/gameplay
  keys and continued compositor/display activity. No fatal signature found.
* `files --accel kvm --timeout 25`: passed column resizing, scrollbar arrow and
  thumb, wheel input, Refresh, keyboard refresh, and retained window dragging.

The pre-change 35-second DOOM smoke run also passed. Consequently these runs
provide regression evidence, not a deterministic before/after reproducer for
the intermittent double fault. No new GNATprove claim is made for the context
switch or assembly transition. The original interactive sequence still needs
retesting, and a full system correctness audit remains open.

## Actual assurance boundaries

* Kernel capability and grant code has focused proof targets and recorded
  results. The available report has successful checks for selected units, but
  also explicitly skips other units after SPARK checking errors. This is not
  a whole-kernel proof. `SPARK_Mode => On` alone does not establish proof.
* The kernel build passes `-gnatp`, globally suppressing runtime checks.
  `kernel/gnat.adc` also declares check suppression. Unproved kernel paths
  therefore cannot rely on dynamic range/index checks for containment.
* Desktop, display, and virtio-GPU service main bodies have no SPARK proof
  boundary configured in their project files. Their current optimized Ada
  builds retain runtime checks, but an exception can still terminate a
  critical service.
* Context switching, interrupt entry/return, address overlays, MMIO, DMA, and
  page-table effects are trusted implementation boundaries. Proving scalar
  arithmetic in a caller does not prove those effects or their concurrency.
* The CI workflow proves selected capability properties and boots a capability
  security guest. It does not currently prove or exercise the full desktop
  stack.

## Next audit work, in order

1. Specify and test the interrupt/lock handoff invariant, including interrupt
   entry, scheduler resumption, direct IPC, and migration. Review NMI/SWAPGS
   windows and invalid SYSRET targets with architecture-level diagnostics.
2. Extract bounded IPC decoding and rectangle/buffer validation into a shared
   SPARK core. Display attachment currently accepts a pitch without an upper
   bound before converting it to `Natural`; several display/GPU request paths
   convert raw message words or calculate `x + w` before validating their
   representability. Reject malformed requests before conversion and prove
   the resulting offsets remain within the acquired buffer extent.
3. Audit synchronous desktop -> display -> GPU calls for peer failure and
   recovery. Memory safety alone does not ensure progress when a dependency
   dies or stops replying. Audit timeout handling before DMA command-buffer
   reuse; a timeout is not proof that the device released the buffer.
4. Audit fatal-fault reporting and system-wide containment. The double-fault
   handler prints through ordinary logging and halts locally; other CPUs
   continued printing afterward. A fatal path must not depend on a lock held
   by the faulting CPU or leave the desktop presenting an apparently live UI.
5. Publish proof coverage per component and gate new proof obligations in CI.
   Separately test scheduling/liveness, malformed IPC, allocation failure,
   service death, and sustained rendering with input and audio.

Related ledger: [SEC-002 and SEC-011](security-hardening.md).
