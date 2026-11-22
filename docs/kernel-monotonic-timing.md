# Common kernel timing

Status: native high-resolution read integrated; physical NUC availability is
under investigation (v26 returned unavailable). No hardware accuracy proof.

### Startup diagnostics

Read-only sysinfo query1402 carries retained startup evidence, never addresses
or write controls. Detail0 is the startup code, detail1 HPET capabilities,
detail2 period in femtoseconds; unknown details return all ones. No extra MMIO
is performed by the query. Codes: 0 not initialized, 1 invalid mapping base,
2 adapter not attempted, 3 rejected identity/64-bit capability/period,
4 unreadable configuration, 5 disable not confirmed, 6 unreadable comparator,
7 comparator interrupt masking not confirmed, 8 unreadable initial counter,
9 enable not confirmed, 10 invalid/regressing startup counter, 11 stalled
counter, 12 running. Running records initialization, not continuing health;
subsequent time reads may still return unavailable. The Intel driver publishes
these fields through logstore so hardware testing does not require serial.

Native evidence: four-CPU UEFI Intel RAM fixture `fbvkjyxt` delivered
`clock startup=12 HPET id=8086A201 period-fs=10000000` to the desktop log
viewer. With HPET disabled, `00wpg32q` delivered startup0/id0/period0 and the
unavailable microsecond result; Desktop still started. Both fixtures passed.
These validate query dispatch and log delivery, not the NUC clock or GPU reset.

Hosted startup coverage now includes 699 cases: all comparator counts and
ignored writes plus 43 invalid-input/read cases. These include every comparator
position returning all ones, rejected 32-bit-only counter hardware, zero/invalid
periods, startup regression, and runtime reads before the captured epoch.
The tests check failure-stage retention, absence of premature MMIO writes,
counter-disable attempts on failed progress, and no repeated initialization
writes. They do not establish that a failed disable write took effect, detect
every possible hardware fault, or prove physical time accuracy.

The immediate consumer is Intel GPU initialization, whose reset sequence needs
minimum 1us and 50us settling intervals. The interface must not expose HPET to
that driver or turn UTC/NTP service policy into a dependency of device startup.

## Boundaries

N95 v27 reports startup7/id8086A701/period52083333fs. The v28 mask-only
check exposed timer offset0x180 before/after0xC000. This is timer4: Intel
documents FSB routing bit14 as read-only one on timers4..7. It is not a
separate interrupt gate. Counter-only admission now verifies INT_ENB bit2
is clear, still rejecting all-ones reads. Quiet_Timer also requests clearing
bit14 when writable, but admission does not require its readback to clear.
This follows the interrupt gate in section2.3.8 of the
[HPET specification](https://www.intel.com/content/dam/www/public/us/en/documents/technical-specifications/software-developers-hpet-spec-1-0a.pdf)
and Intel's timer4..7 description in its
[PCH datasheet](https://cdrdv2-public.intel.com/615170/615170-001.pdf).
There is no NUC model special case. Read-only diagnostic details3/4/5 retain
last comparator offset/before/after. Tests accept read-only FSB routing,
including exact C000 readback, but reject a stuck bit2 with either route.
The 891-case hosted suite additionally verifies exact retained comparator
diagnostics, unchanged evidence on repeated initialization, and all 32 possible
comparator positions becoming unreadable only after the mask write. Those
failures must return before counter reads or enabling counter operation.

* A platform counter backend supplies ordered, migration-safe monotonic reads,
  its epoch, conversion parameters and a justified error bound. HPET is one x86
  candidate; validated TSC is a prospective faster implementation. Invariant
  frequency alone does not establish cross-CPU synchronization.
* Common kernel timing owns elapsed-time arithmetic, bounded delays, deadlines
  and timer scheduling. Counter reading and interrupt programming are separate
  operations: a usable clock does not imply a usable event interrupt source.
* The userspace clock service owns UTC synchronization and presentation. Wall
  clock changes must not reset device timeouts or permit an early settling wait.

Existing `Time` remains the kernel integration point. Do not create a second
independent system epoch or replace scheduler interrupts as part of the first
GPU timing change. The existing millisecond GETTIME behavior must not silently
change units. A future high-resolution read needs explicit success/failure and
documented suspend/epoch behavior; a later read-only clock page can avoid a
syscall only after its consistency and migration guarantees are established.

## Minimum delay is not a timeout

`Monotonic_Wait.At_Least` is the first common policy helper. Its backend supplies
microsecond readings, and the caller supplies a justified upper bound E on how
much a measured difference can overstate physical elapsed time during the
operation. Completion requires measured difference >= requested duration + E.
Addition overflow, unavailable reads and counter regression fail explicitly;
a separate poll limit prevents a stalled counter from hanging the caller.
This is bounded busy waiting for short hardware transitions, not general sleep.

E includes counter phase, rounding and accumulated frequency error over the
operation, not merely the number of decimal places in the timestamp. The helper
does not establish E. HPET's reported period and the existing floor conversion
are insufficient by themselves to justify a <=1us bound. An admitted backend
must document its physical-clock assumptions and supported duration horizon.

Timeouts have different conservative rounding needs and do not inherit this
minimum-delay formula blindly. Hardware callbacks must themselves be bounded;
a poll limit cannot rescue an MMIO transaction that never returns.

## Verification and remaining integration

Hosted `tests/boot-hpet/wait.gpr` exercises 1,100 duration/error boundary pairs,
stopped clocks, intermediate regression, wrap, unavailable samples, overflow,
and zero-delay behavior. These are regression tests, not formal proof of the
helper or a hardware timing guarantee. Existing HPET conversion arithmetic has
separate SPARK checks; these do not prove oscillator accuracy.

Native progress: `Platform_Monotonic` binds the HPET adapter to x86 MMIO;
`Boot_Timer_Setup` enables counter-only operation after firmware takeover,
including a second register page where ACPI alignment requires it.
`Time.Read_Monotonic` provides a common kernel read with explicit availability.
Four-CPU UEFI QEMU boot `3fw63ri_` reached Desktop and the boot-log viewer with
`monotonic: HPET counter-only ready (scheduler unchanged)`. This tests emulated
hardware, not the NUC, and does not yet test reads across CPU migration.

Syscall114, `READ_MONOTONIC_MICROSECONDS`, now exposes the separate high-resolution
epoch; all-ones denotes unavailable. `CuBit.Monotonic.Read` converts this into a
discriminated `Reading`, so callers check availability before accessing the
timestamp. GETTIME remains unchanged. Kernel, runtime and Intel driver compile
natively. Four-CPU UEFI fixture boot `l8_e1ef7` executed that syscall from the
Intel driver and reported `monotonic microsecond progress=TRUE`; the RAM-device
forcewake timeout/cleanup, retained firmware buffer and boot-log viewer checks
also passed. This verifies native dispatch and conversion, not physical accuracy
or CPU migration coverage. The earlier QEMU run above predates this syscall.

An absent-HPET run (`3bg82da6`, q35 `hpet=off`) also passed the full fixture:
kernel reported high-resolution backend unavailable, the Intel caller reported
progress FALSE, and Desktop/log replay still completed. No fabricated timestamp
fallback was substituted. Both runs use native CuBit in QEMU, not Linux-hosted
unit tests. They do not emulate Intel GPU hardware.

## Short settling interval bound

HPET 1.0a section 2.4.1 specifies 500ppm accuracy over 1ms and up to two
ticks of short-interval error within 100us, with a maximum 100ns tick.
For the GPU's 1us and 50us minimum waits we conservatively budget 2us:
short-interval counter error (0.2us), an additional tick-phase allowance
(0.1us), and the difference of two floor conversions (<1us) fit below it.
The extra phase allowance is deliberately conservative, not an extra accuracy
promise by the specification. This assumes compliant hardware, correct period
reporting, ordered reads and no other owner modifying the counter.

The sufficiency argument is by contradiction: if the physical wait were below
the requested 1us or 50us, it would lie inside the short-interval window and
could not produce a timestamp difference of 3us or 52us respectively. A longer
descheduling delay already satisfies the minimum; no upper wakeup guarantee
is implied. Do not generalize this fixed allowance to long-duration deadlines.

Intel stop/reset helpers now use these thresholds and reject all-ones clock
readings explicitly. Hosted regressions cover the old premature boundaries and
the new inclusive boundary. This is not a proof that the NUC oscillator complies.
Source: [HPET 1.0a, section 2.4.1](https://www.intel.com/content/dam/www/public/us/en/documents/technical-specifications/software-developers-hpet-spec-1-0a.pdf).

Next: wire the
Intel minimum-delay callbacks through that interface. Only after native timer
validation may the GPU stop/prepare/reset helpers be enabled. Current published
v25 remains inspection/forcewake only; the new private test image is not yet a
published NUC test release.

References: [Linux timing separation](https://docs.kernel.org/timers/timekeeping.html)
and [timekeeping accessors](https://docs.kernel.org/core-api/timekeeping.html).
