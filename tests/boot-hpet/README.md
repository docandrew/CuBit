# Checked HPET boot description

Linux-hosted tests for `Firmware_Tables.HPET`, not a hardware timer test.
Run from the repository root in Nix:

```sh
nix develop -c bash -c 'cd kernel && alr exec -- gprbuild -p -P ../tests/boot-hpet/hpet.gpr && ../tests/boot-hpet/build/hpet_tests'
nix develop -c bash -c 'cd kernel && alr exec -- gnatprove -P ../tests/boot-hpet/hpet.gpr -u firmware_tables-hpet.adb --mode=all --level=2 --prover=cvc5,z3 --checks-as-errors=on -j2'
```

The decoder admits checksummed ACPI HPET descriptors with a supported memory
GAS, a nonzero 1KiB-aligned address, and the whole register window within a
caller-supplied address limit. It does not establish that physical memory is
actually an HPET: readable backing, MMIO mapping and hardware identity must be
checked by the native adapter. Rejection must never silently leave an active
replacement timer while calibration assumes PIT-origin interrupts.

Tests cover truncation, each single-byte corruption, shifted/high array bounds,
GAS field values, address limits and resealed malformed descriptors. The register
transform clears only general-configuration bits 0 and 1, preserving all other
bits. Its preservation and disabled-state properties are proved in SPARK, along
with decoder bounds/termination. No assumptions or SPARK-off sections here.

## Physical N95 evidence, 2026-09-26

Confirmed by the user with the previous diagnostic image:

```
span=F296B810 first=1F7B31C4 last=DD26F61C budget=polls
status=B4/B4 min=0001 max=04A9 CPUID15 D=2 N=58 Hz=0249F000
x1 SVR=0000010F LINT0=00000700 LVTT=00020005
ISR=FFFFFFFF IRR=FFFFFFFF PPR=00000000
PIT moved=Y ticks=7 mask=FA irr=00 isr=00 IF=1
```

CPUID gives 1,689,600,000 TSC ticks/sec: span 2.409s, first observation 0.313s,
last 2.196s. Seven ticks are consistent with approximately 314ms intervals;
individual inter-arrival times were not recorded. PIT divisor/mode look correct.
The APIC snapshot has no pending/in-service vector; the inherited local timer is
unmasked, periodic, vector 5. This is not evidence that vector 5 was delivered.

The current kernel ignores HPET and initializes its local APIC only after PIT
calibration. HPET legacy replacement is a hypothesis, NOT a confirmed cause.
`Boot_Timer_Setup` now takes ownership before enabling interrupts. It retains
HPET general configuration before/after and comparator-zero configuration,
then masks/stops the inherited local APIC timer without changing LINT routing
or issuing speculative EOIs. Firmware table backing and MMIO/MSR access remain
trusted adapters, not SPARK proofs of hardware behavior.

Validation: 146444 hosted checks; all 23 SPARK checks discharged. QEMU with an
injected 314ms HPET replacement and takeover bypassed reproduces slow delivery,
correct PIT count range and clean PIC state. With takeover enabled, the same
injected device state changes configuration 3 to 0 and calibration completes.
Normal UEFI four-CPU and BIOS one-CPU live desktop regressions pass. Physical
N95 confirmation is still required; this is a tested failure class, not proof
that the N95 inherited the same HPET configuration.

Reference: [Intel HPET 1.0a specification](https://www.intel.com/content/dam/www/public/us/en/documents/technical-specifications/software-developers-hpet-spec-1-0a.pdf),
sections 2.3.5, 2.4.2.1 and 3.2.4.
