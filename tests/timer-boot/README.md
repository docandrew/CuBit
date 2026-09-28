# Boot timer diagnostics

CuBit initially calibrates its TSC and LAPIC timer using PIT interrupts. The
old waits were unbounded. A missing PIT or undelivered IRQ0 could therefore
leave the boot panel at `Setting up PIT and enabling timer interrupts` without
ever entering the last-chance handler.

`Boot_Timer_Diagnostics.Wait_For_Ticks` now bounds this boot-only wait by both a
poll budget and an uncalibrated TSC-cycle budget. These are diagnostic work
limits, **not** a wall-clock timeout or a real-time guarantee. It samples the
channel-zero count latch and, on failure, records interrupt state before
disabling interrupts and stopping. It does not change the routing or choose a
different calibration source. The hardware adapter is explicitly SPARK Off;
these are native regression tests, not a proof of interrupt delivery.

The panel retains:

- `moved`: whether sampled PIT counts differed; absence of observed movement
  alone is not proof that a physical PIT is absent.
- `ticks`: ticks received during this calibration wait.
- `mask`, `irr`, `isr`: master PIC mask, pending requests, in-service requests;
  bit zero describes IRQ0.
- `IF`: CPU interrupt-enable flag sampled before the diagnostic disables it.

More precise completed checkpoints distinguish PIC setup, PIT programming,
and successful TSC calibration. If the machine stops before the bounded wait
is reached, the diagnostic cannot report that as a calibration failure.

Six retained evidence rows supplement the original summary:

- Timing: TSC offsets of the first/last **observed** tick change, total wait
  span, and which work budget expired. These are polling observations, not
  timestamps taken inside the interrupt handler. Zero means no observed tick.
- PIT: initial/final status byte, minimum/maximum sampled counter, and raw
  CPUID leaf 15 denominator/numerator/crystal-Hz fields. A normal programmed
  divisor is 1193 (`04A9`); sampled counts substantially above that identify a
  different effective reload. Status output-bit changes are normal.
- APIC: xAPIC/x2APIC mode, SVR, LINT0, timer LVT, highest in-service and pending
  vectors (`FFFFFFFF` means none), and processor priority. The snapshot uses
  read-only registers/MSRs; the xAPIC page is mapped as device memory only on
  the failure path. It does not send an APIC EOI or reprogram timer routing.
- Firmware takeover: saved HPET general configuration before/after quiescing,
  comparator-zero configuration, and local APIC timer LVT before/after masking.
  This fourth row comes from `Boot_Timer_Setup`, which deliberately disables
  inherited HPET generation/replacement and masks/stops the LAPIC timer before
  PIC/PIT setup. The later diagnostic snapshot remains read-only.
- IRQ timing: minimum/maximum entry-to-entry gap and maximum Ada-handler
  duration, in ordered TSC cycles. The duration includes the probe's PIC reads
  and address-space switches but excludes assembly entry/exit and IRET.
  A gap near 314ms with a short handler points outside the measured handler;
  a similarly long handler points inside it (including possible firmware/SMM
  or VM descheduling during that interval), not necessarily expensive Ada code.
- IRQ source: entered/returned handler counts, IRQ0-in-service count, and the
  OR of pre-EOI master PIC ISR values. All entries should normally have bit 0
  set. A vector 32 with PIC ISR bit 0 clear is not a confirmed PIC IRQ0.
  The probe is armed only during the BSP's initial calibration wait, before
  AP startup; it is stopped with interrupts disabled on both return and failure.
  There are no IRQ-path prints, allocations, locks, extra EOIs, or route changes.
  These counts can differ from the polling tick total if an interrupt arrived
  between taking the initial tick snapshot and arming the probe.

## Reproduce

Build `kernel/cubit_live_uefi.img` first. Run under the Nix environment and the
shared build lock, from the checkout root (or use a private workspace's lock
as documented in coordination/README.md). These fixtures boot only a read-only
ISO; they do not attach the user's development disk or a host block device.

```sh
flock --exclusive --nonblock coordination/build.lock nix develop -c \
  python3 tests/timer-boot/probe.py --without-pit --settle 20 --expect missing
flock --exclusive --nonblock coordination/build.lock nix develop -c \
  python3 tests/timer-boot/probe.py --mask-pic --settle 20 --expect masked
flock --exclusive --nonblock coordination/build.lock nix develop -c \
  python3 tests/timer-boot/probe.py --slow-pit --settle 15 --expect slow
flock --exclusive --nonblock coordination/build.lock nix develop -c \
  python3 tests/timer-boot/probe.py --drop-eoi --settle 15 --expect stopped
flock --exclusive --nonblock coordination/build.lock nix develop -c \
  python3 tests/timer-boot/probe.py --hpet-replacement --skip-hpet-takeover --settle 15 --expect slow
flock --exclusive --nonblock coordination/build.lock nix develop -c \
  python3 tests/timer-boot/probe.py --hpet-replacement --settle 8 --expect restored
flock --exclusive --nonblock coordination/build.lock nix develop -c \
  python3 tests/usb-optical/run-live.py --uefi --cpus 4
```

Outputs go to a unique directory under `TMPDIR`: serial, QEMU/GDB logs,
registers, PIC/LAPIC state, screenshot, and the exact QEMU command. `--bios`
selects the BIOS live image; an optional positional `command.json` instead
reuses a saved live-test setup with private monitor and serial outputs.

The PIC fault changes the argument to the guest's actual `OUT` instruction
which unmasks IRQ0. Direct HMP port writes were not effective against KVM's
in-kernel PIC and must not be treated as successful injection merely because
a later PIC snapshot happens to show masked interrupts (normal boot masks
the PIC after switching to the LAPIC).

An attempted LINT0-mask fixture was rejected: debugger physical-memory writes
did not change the observed LAPIC register, even under TCG. It is not retained
as a passing test. PIC-unmasked/LAPIC-masked delivery still needs a reliable
guest-executed injection mechanism or physical-hardware evidence.

These fixtures distinguish failure classes; none establishes the cause of
the GMKtec N95 hardware stall. Its measured HPET configuration was 1 before
takeover, not 3: legacy replacement was already off. Disabling HPET and masking
the firmware LAPIC timer did not change its seven-tick failure.
Photograph the full panel including the two IRQ rows for the next test.

The HPET fixture stops at the non-inlined native quiesce boundary after mapping.
GDB writes actual HPET device registers: a 314ms periodic comparator and legacy
replacement mode. Readback must confirm configuration 3. The bypass variant
returns from that one procedure without executing its observations/repair,
modeling the previous kernel's omission (its HPET evidence therefore reads
absent). It must then observe a normal PIT range, late ticks and clean PIC.
The repair variant executes the real handoff and must report configuration
3 -> 0 and successful LAPIC calibration. Neither variant changes software ticks.

The slow fixture changes the two actual PIT reload OUT arguments to zero
(65536 clocks). The dropped-EOI fixture replaces the seventh PIC EOI OUT with
an OCW3 read-selection command. Assertions require both the kernel failure
and timing evidence: slow ticks continue late into the wait, while dropped
EOI leaves an early burst with PIC ISR bit zero set. Neither test patches the
tick counter or merely substitutes a diagnostic message.

## Validation (2026-09-26)

- Original kernel, UEFI/KVM with PIT absent: captured RIP resolved to
  `Time.calibrateTSC`'s unbounded loop; IF set, PIC IRQ0 unmasked, no pending IRQ0.
- Diagnostic kernel, absent PIT: `moved=N`, zero ticks, mask FA, IRR/ISR zero,
  IF set; asserted missing-counter failure.
- Diagnostic kernel, guest OUT modified to mask IRQ0: `moved=Y`, zero ticks,
  mask FF, IRR 01, ISR zero, IF set; asserted masked-IRQ failure.
- Normal UEFI/KVM four-CPU and BIOS/KVM one-CPU live boot tests passed through
  desktop, DOOM, Workbench and Files (both before and after timer takeover).
- Failure-panel screenshot visually checked: latest detail and first failure
  both remain visible after the last-chance handler stops the CPU.
- Extended evidence: slow PIT produced 34 ticks, counter max `FFE3`, last
  observed tick near the end of the wait, clean PIC IRR/ISR. Dropping the
  seventh acknowledgement produced seven ticks, max `04A5`, an early final
  tick, and PIC IRR/ISR both `01`. Both asserted fixture outcomes passed.
- Extended pure panel model: 299592 transition tests and all 18 SPARK checks
  passed. Hosted renderer checks cover 1x1, 640x480, 1024x768, 1920x1080,
  busy-panic and retirement. This does not prove hardware timing correctness.
- HPET replacement bypass: six ticks, divisor max `04A9`, mask FA, IRR/ISR zero;
  ticks continue late into the wait. Identical injection with takeover enabled
  restores PIT/LAPIC calibration. Missing-PIT and masked-PIC failures still pass.
- Checked HPET decoder/register policy: see `tests/boot-hpet/README.md` for
  exact proof scope and the confirmed N95 observations.

## N95 investigation: firmware clocks versus IRQ handler time

Visibility revision: the panel heading now says `IRQ PROBE 2` and the final
Latest Diagnostic line duplicates the key values as
`IRQ2 n=... ret=... pic=... h=... gap=...` (all hexadecimal). Here n/ret/pic
are entry, returned, and confirmed-PIC-IRQ0 counts; h is maximum Ada-handler
duration and gap is maximum entry spacing, both in TSC cycles.
This avoids relying on visibility of the bottom rows. It does not fix the
underlying timer failure. The previous image was extracted and its embedded
kernel matched the tested kernel exactly; the cause of the user not seeing
those rows on physical hardware remains unconfirmed.

The N95's reported CPUID.15 values are D=2, N=88, crystal=38,400,000Hz,
implying a TSC frequency of 1,689,600,000Hz. The previous observations span
about 2.409s, first tick about 313ms, last about 2.196s; six observed intervals
average about 314ms. That is not yet a measurement of each IRQ interval.
The PIT count range stays within the programmed 1193-clock divisor.

Primary-source clues, inspected 2026-09-26:

- [Intel Alder Lake-N FSP definitions](https://github.com/intel/FSP/blob/master/AlderLakeFspBinPkg/IoT/AlderLakeN/Include/FspsUpd.h)
  expose `Enable8254ClockGating` and warn that enabling it during POST can
  prevent PIT-dependent legacy operating systems from booting. This establishes
  that the platform can gate PIT clocks, not that this GMKtec firmware did so.
- [Linux i8253 selection](https://github.com/torvalds/linux/blob/master/arch/x86/kernel/i8253.c)
  avoids dependence on chipset-specific clock-ungating registers when other
  usable timers are available.
- [Linux TSC calibration](https://github.com/torvalds/linux/blob/master/arch/x86/kernel/tsc.c),
  `native_calibrate_tsc`, uses CPUID.15 crystal and ratio data when available,
  and also derives an LAPIC timer period from the crystal frequency.

Likely architectural follow-up: use validated CPUID frequency information and
an independently checked LAPIC clock or TSC deadline timer rather than making
PIT interrupts mandatory. This image does not implement that change or silently
accept a failed calibration. Nor does it write undocumented chipset registers.
The IRQ measurements first distinguish slow delivery from long handler work.
They cannot see inside SMM or diagnose a handler that never returns.
