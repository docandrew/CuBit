# PIT-independent boot clocks

The BSP first tries Intel CPUID.15 when an invariant TSC is advertised.
Nonzero denominator, numerator and crystal frequency produce
`TSC Hz = crystal Hz * numerator / denominator`. Rates outside 1MHz..100GHz
are unsupported by this admission policy and select the bounded PIT fallback.
Missing/invalid data never becomes a guessed model-specific frequency.
CPUID's former `tscFreqHz` variable was renamed `crystalClockHz`: ECX describes
the crystal, not the scaled TSC.

After either reference path succeeds, IRQ0 is masked and interrupts remain
disabled through local-APIC calibration and controller handoff. LAPIC
calibration measures a masked one-shot countdown against 10ms of TSC progress;
it does not wait for PIT interrupts. A finite polling budget handles a stuck
TSC. An exhausted, stopped or backwards countdown, a sample longer than 100ms,
and a zero/unrepresentable computed rate fail closed. Writing initial-count
zero stops the countdown; merely masking its interrupt does not.

The existing LINT masking code also used AND with the mask bit, which could
leave an unmasked input unmasked and destroy its delivery-mode bits. It now
uses OR to implement the intended masking while preserving the other fields.
This is a separate handoff correction, not an explanation of the earlier
N95 stall (which happened before LAPIC setup).

Scheduler elapsed-time accounting uses the retained Hz value divided by 1000,
instead of multiplying the truncated integer cycles/us value. The existing
microsecond busy-wait and alarm interfaces still use integer cycles/us; this
change does not redesign their fractional-cycle rounding.

## Hosted tests and proof

Run from the checkout in Nix, or in an isolated build workspace:

```sh
nix develop -c bash -c 'cd kernel && alr exec -- gprbuild -p -P ../tests/boot-timer-rates/rates.gpr && ../tests/boot-timer-rates/build/main'
nix develop -c bash -c 'cd kernel && alr exec -- gnatprove -P ../tests/boot-timer-rates/rates.gpr -u boot_timer_rates.adb --level=1 --report=all --checks-as-errors=on -j2'
```

Tests cover 1,000 CPUID input combinations including zero fields, maximum
32-bit values, the N95 ratio, and frequency boundaries; LAPIC fixtures cover
zero/stalled intervals and narrowing bounds. All ten SPARK checks pass for the
pure rate module: five runtime checks, two initialization checks, two
termination checks and the valid-frequency-or-zero postcondition. This is not
a proof that physical counters are truthful, invariant or mutually synchronized.
Hardware access, sampling quality and interrupt handoff remain trusted and
regression-tested boundaries. Kernel assertions remain disabled.

## Native QEMU regression

Use the shared build lock for the main checkout, or the private workspace's
lock. Build the UEFI live image first, then:

```sh
nix develop -c python3 tests/usb-optical/run-live.py --uefi --cpus 4 --pit-free-fixture
nix develop -c python3 tests/usb-optical/run-live.py --uefi --cpus 1 --invalid-clock-fixture
nix develop -c python3 tests/usb-optical/run-live.py --uefi --cpus 4
```

The PIT-free fixture actually removes the virtual PIT (`q35,pit=off`).
QEMU/KVM scales the virtual TSC to 1,689,600,000Hz; GDB seeds the three cached
CPUID fields with D=2, N=88, crystal=38,400,000Hz at `Try_CPU_TSC`. This
setup does not expose usable CPUID.15 naturally. Vendor, maximum leaf and
invariant-TSC advertising are configured through QEMU CPU properties.
No tick count, calibration result or control-flow outcome is patched.
The production kernel then derives the rate, measures the real emulated
LAPIC countdown, starts timer delivery, and boots the desktop/apps.
The initial successful run measured 62,498 LAPIC ticks/ms (divide-by-16).
The negative fixture instead supplies denominator zero with the PIT present
and requires the ordinary PIT fallback to boot.

The existing `tests/timer-boot/probe.py --without-pit --expect missing`
still checks a bounded failure when neither a usable CPU frequency nor PIT
delivery is available (on this host's ordinary virtual CPU configuration).
These tests do not reproduce the GMKtec chipset's 313ms IRQ spacing.

## Physical evidence and next test

The N95's IRQ PROBE 2 result was seven entries, seven returns and seven confirmed
PIC IRQ0 sources, maximum handler 0x1089C cycles (40.092us) and maximum entry gap
0x1F90F5D2 cycles (313.443ms). That excludes a similarly long measured Ada
handler, but does not establish the exact firmware/chipset cause.

The new panel marker is `PIT-FREE 3`. A successful direct-frequency path prints
`CPU-reported TSC frequency accepted; PIT bypassed`, followed by local-APIC
calibration. No physical success is claimed until the N95 test.

Primary references: [Linux CPUID-based TSC calibration](https://github.com/torvalds/linux/blob/master/arch/x86/kernel/tsc.c),
[Linux PIT selection](https://github.com/torvalds/linux/blob/master/arch/x86/kernel/i8253.c),
[Intel Alder Lake-N clock-gating definitions](https://github.com/intel/FSP/blob/master/AlderLakeFspBinPkg/IoT/AlderLakeN/Include/FspsUpd.h).
CuBit measures the LAPIC countdown; it does not assume that its input frequency
equals the reported CPU crystal.
