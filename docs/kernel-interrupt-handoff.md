# Kernel interrupt exclusion and context handoff

Status: implemented bounded state core, with focused SPARK proofs; hardware and
concurrent scheduler correctness remain trusted/review obligations.
Date: 2026-09-06.

## Invariant

**Every context switch hands over exactly one held `Process.lock`, owned by the
current CPU, with hardware IF clear and interrupt-exclusion depth exactly one.
On resumption, the saved interrupt-restoration policy belongs to the resumed
context, not to the CPU or the context which ran before it.**

This applies to scheduler-to-process, process-to-scheduler, and direct IPC
switches. A first-run context has no saved switch frame: its initial policy
keeps interrupts masked until IRET restores its initial flags and stack.

The depth counts outstanding exclusion entries, not CLI instructions. Other
locks may nest during execution, but must be released before handoff. The
single remaining lock is CPU-owned across the switch; logical task ownership
changes. A resumed task may run on a different CPU, so its saved policy must be
installed into the *current* CPU's state after re-reading GS.

## Representation and implementation

`Interrupt_State` owns a private `State` containing a nonnegative nesting depth
and an enumerated restoration policy. `Context` contains only the policy that
travels with the suspended context. There are no allocator, access-type, MMIO,
assembly, `pragma Assume`, or SPARK-off sections in this core. Its representation
preserves the two existing 32-bit fields in the assembly-visible per-CPU layout.

| Operation | State transition / obligation |
| --- | --- |
| `Enter` | At depth zero, capture prior IF; otherwise preserve the outer policy. Increase depth. Reject exhausted depth or unexpectedly enabled IF while nested without changing state. |
| `Leave` | Reject enabled IF or depth zero without changing state. Decrease depth; request STI only at depth zero with an enabled restoration policy. |
| `Capture` | At depth one, save the suspended context's policy. |
| `Resume` | At depth one, replace only the policy with the resumed context's saved policy. |
| `Can_Handoff` | Require depth one, masked hardware IF, and CPU ownership of `Process.lock`. |

`PerCPUData` is the Ada/hardware adapter. It snapshots the volatile per-CPU
state, invokes the pure transition, and writes back the result. It performs
explicit boundary validation, since the release kernel suppresses assertion
and language runtime checks. An invalid transition is fatal via the kernel's
last-chance handler; it is not silently repaired or treated as recoverable.

`pushCLI` masks interrupts **before acquiring the per-CPU address**. Otherwise
a timer could preempt and migrate the task between the GS lookup and CLI,
leaving a pointer to the previous CPU's exclusion state. `Process.switch`
captures the policy before `asm_switch_to`, and re-reads GS through the adapter
after return. Both capture and resume explicitly validate the handoff invariant.

The spinlock release boundary checks current-CPU ownership as well as locked
state. CLI, STI, and the atomic-exchange wrappers have compiler memory clobbers
so shared-memory operations cannot be moved across those boundaries. These are
compiler barriers; the proof does not model their machine-code semantics.

Final syscall/interrupt return paths mask interrupts before restoring the
destination stack and GS. This complements correct restoration bookkeeping;
it does not substitute for proving that bookkeeping.

## What is proved

The production `Enter`, `Leave`, `Capture`, and `Resume` implementations prove
their functional postconditions and absence of runtime errors. Ghost theorem
procedures use those actual implementations, not a separate mock:

- **Handoff independence:** for arbitrary depth-one suspended and incoming
  states, restoring the saved context and releasing the final exclusion enables
  interrupts exactly when the suspended context's policy requires it.
- **Nested exclusion:** for either initial hardware IF value, enter twice and
  leave twice. The inner release never enables interrupts; the final release
  restores the original IF policy.

The first property was proved before adding the second. No assumptions,
unproved postconditions, or disabled function sandboxing were added to this
proof. Ghost theorem procedures are omitted from the release executable.

The focused report contains 35 checks: 18 discharged by flow analysis and 17
by provers, with zero unproved checks, warnings, or `Assume` statements.

Reproduce inside Nix:

```sh
nix develop -c make -C kernel prove-interrupt-state
nix develop -c make -C kernel check-spark-boundaries
```

The focused proof has its own output directory so its totals cannot be confused
with cached results from other kernel units. The second command checks SPARK
**legality**, not functional correctness or absence of runtime errors for all
kernel code. An annotated specification whose implementation is outside SPARK
is a trusted interface, not a proved implementation.

The kernel-wide legality pass still reports imprecisely modeled address-overlay
and custom-storage-pool warnings (including page tables, slab storage, firmware
data, and fatal-path stack walking). These are additional trusted assumptions
to audit, **not** evidence that the pointed-to memory is valid or race-free.

## Corrected SPARK boundaries

The previous blanket annotations included local volatile address overlays,
custom-pool access types, effectful functions, and assembly context changes.
These prevented meaningful whole-project legality checking.

- Process/IPC/queues, scheduler, per-CPU adapters, ACPI, custom-pool linked lists,
  linker-symbol-based memory mapping, and boot orchestration use ordinary Ada
  rather than blanket SPARK-on claims.
  Explicitly annotated supported helpers can still be analyzed; for example,
  FPU reset-image construction remains SPARK-on.
- Spinlock, module-loader, keyboard-service, LAPIC, and PCI hardware bodies are
  explicit SPARK-off boundaries. Effectful PCI/serial functions and syscall
  dispatch and the live system-info registry are excluded at their interfaces
  where required. Interrupt dispatch/end-of-interrupt are hardware adapters;
  IDT entry construction remains SPARK-on. Syscall-number
  decoding remains SPARK-on.
- Timer routines using live tick/process state, allocator routines manipulating
  physical-memory free-list overlays, ELF loading/diagnostics, and EGA callback
  construction have explicit implementation exclusions. Allocator arithmetic
  helpers, supported EGA operations, and other supported code remain SPARK-on.
- Lock names now have a named access-to-constant type; the allocator's debug
  label is actually constant. System-info's volatile getter is declared as
  such and kept outside SPARK, and boot-allocation contracts no longer claim
  to modify interrupt state.
- The unfinished, unreferenced `vectors.ads` API sketch is excluded from the
  buildable-source project set; it is retained on disk, not represented as a
  checked or verified kernel component.

Existing capability-policy, attenuation, and memory-grant proof targets are
retained. Clearing legality errors must not be reported as proving their Ada
callers or the system-wide capability implementation.

## Regression evidence

The optimized Nix kernel build and its stack-usage gate pass. Inspection of the
generated switch wrapper confirms that the policy survives in RBX across the
assembly switch and that resumption calls the fresh-GS adapter. Four-CPU KVM
runs with live handoff/lock-owner checks enabled passed:

- `bench-ipc`, 20 seconds: 2,000 synchronous calls and 512/512 asynchronous
  completions, with the benchmark's own PASS marker.
- `async-ipc`, 20 seconds: asynchronous IPC functional regression.
- `ccl-workbench-virtio-vga`, 25 seconds: native Workbench startup/render path.
- `desktop-doom`, 40 seconds: rendering/audio plus injected input.
- `files`, 25 seconds: grid/scrollbar/refresh and retained dragging regression.

These are regression tests, not a proof of liveness or a deterministic
reproduction of the earlier intermittent fault. The original interactive
freeze sequence and longer-running real-hardware workloads still need testing.
CI now includes the focused handoff-state proof and buildable-kernel SPARK
legality gate alongside the existing capability proofs and adversarial guest.

## Remaining correctness obligations

1. **Architecture boundary:** verify register-save layout, ABI preservation,
   stack/GS/CR3 transitions, canonical SYSRET targets, and NMI/SWAPGS windows.
   Masking IF does not mask NMIs or synchronous exceptions.
2. **Concurrent lock ownership:** prove atomic acquisition/release and memory
   ordering against the machine model. `Can_Handoff` consumes an observed
   ownership Boolean; it does not prove mutual exclusion. The subsequent
   [locking audit](kernel-locking.md) replaces split lock/CPU metadata with one
   atomic owner word and proves its sequential ownership policy. The hardware
   compare/exchange adapter and the legacy ghost `x86.interruptsEnabled` remain
   outside an SMP hardware-IF proof.
3. **Scheduler state:** establish unique runnable-queue membership, no process
   running on two CPUs, safe migration, and lifetime of saved contexts/stacks.
   The current handoff checks validate the boundary, not arbitrary intervening
   process-table or memory corruption. The subsequent
   [retirement implementation](kernel-process-retirement.md) adds an explicitly
   acknowledged execution-presence state and worker-stack reclamation, with a
   separately proved lifetime core. Scheduler/assembly refinement remains open.
4. **Progress:** test and specify lost wakeups, deadlock, starvation, interrupt
   latency, and bounded critical sections. Memory safety alone proves none of
   these. The state model does not promise that a lock acquisition terminates.
5. **Containment:** replace lock-dependent fatal diagnostics where necessary;
   audit the release check-suppression policy for unproved code. Do not infer
   crash freedom from SPARK annotations while `-gnatp` suppresses runtime checks.

Service restart/recovery follows kernel scheduling correctness; it cannot
repair a broken lock handoff or a corrupted kernel stack.

Related: [freeze investigation](desktop-freeze-correctness-audit.md),
[security hardening ledger](security-hardening.md).
