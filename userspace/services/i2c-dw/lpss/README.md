# Intel LPSS I2C reset and timing milestone (hosted milestone)

The next native I2C owner needs this sequence before the pending DesignWare
configuration library. It is not a new native entry point or registered driver.

The profile explicitly recognizes Intel 8086:54e8 and 54e9. Linux v6.18 maps
both to bxt_i2c_info: fixed root133MHz, SDA fall171ns, SCL fall208ns and TX
hold42ns. Its intel_lpss_register_clock skips the fractional divider for I2C.
Accordingly no UART/SPI fractional clock programming is performed here.
The official pinned intel-lpss.c and intel-lpss-pci.c are retained privately.

Intel_LPSS_I2C_Reset uses static named selectors for capability0x2fc, reset0x204,
remap-low0x240 and remap-high0x244. An internal owner supplies a launcher-validated
physical BAR/grant pair; PCI identity selects this profile but does not grant
hardware authority. Zero, unaligned, undersized or wrapping minimum windows are
refused. No remote client can select these physical/remap facts. The sequence
checks I2C type, asserts/resets and reads back the functional bits, deasserts and
confirms them, writes admitted BAR low/high and checks both readbacks. It ignores
the nonexistent IDMA reset bit when confirming functional reset on a no-IDMA part.
Each Step performs at most one primitive access; deadline and explicit quota
bound waits even if the clock does not advance. Terminal failure/ready cannot
restart through Start. Unknown effects are terminal; successful writes and
possible reset-release effects survive post-access clock failures for cleanup.
No DMA setup, clock-divider write, recovery replay or input publication occurs.

Intel_LPSS_I2C_Profile derives positive16-bit HCNT/LCNT and hold fields from an
explicit speed ceiling, supported up to1MHz. It selects the standard/fast/fast+
timing minima, rounds cycles upwards, and extends low time if necessary to
meet the requested period. It uses bounded non-wrapping arithmetic. Slow speeds
whose low count exceeds16bits are refused, not truncated. 100kHz ->552/774,
400kHz ->100/229, 1MHz ->55/94, TX hold6 and RX hold1. These intentionally
conservative counts differ from Linux's nearest-rounded minima: our contract
also enforces the requested ceiling in the counter-period model. There is no
promise the physical bus runs at exactly the requested frequency.

Evidence: latest hosted run-qTN5gcLY passed3,995,445checks/profile checked/release.
All1,000,000integer speed inputs1Hz..1MHz were exercised, with an independent
counter-period ceiling check, explicit device/speed refusal and golden vectors.
Reset tests cover all9normal access positions with denied/unavailable/unknown
effects, every pre/post clock position (loss/expiry/reversal), reset stalls,
no-IDMA variant,64-bit BAR preservation, remap mismatches, wrong controller type,
invalid windows, terminal no-replay and possible release after clock failure.

Level2 proof for the PURE TIMING PROFILE ONLY:28checks discharged:13runtime,
2assertions,4functional contracts,6initialization and3termination. Zero unproved
or justified, no imported callbacks or pragma Assume. It proves arithmetic safety,
positive returned counts and the returned ceiling <=requested ceiling when
Calculated. It does not prove policy completeness, all timing-minimum semantics,
reset callbacks, grants, exclusivity, native scheduling or electrical behavior.
Initial unsigned-arithmetic proof missed the ceiling obligation; adding period
assertions left2unproved checks. Replacing modular arithmetic with bounded
integers proved both assertions and the contract. The intermediate evidence is
retained in proof-initial and proof-modular-lemmas. Limits were not increased.
One initial strict compilation failed on a redundant use clause; flags remained
strict and the corrected build/tests passed. No compiler-crash investigation.

Trusted/assumed: native BAR facts belong to the bound authorized controller;
exclusive reset ownership includes its DMA engine; callbacks are bounded,
ordered, non-reentrant/non-raising and report actual/unknown effects honestly;
reset/remap readback means the documented hardware transition; the fixed-clock
profile applies to the actual revision; the counter-period/electrical model and
fall/hold properties apply. Actual hardware validation must establish these.
Validated firmware timing overrides need owner policy before native integration.


Portable tests/proof: tests/i2c-dw-stack. No native component, controller access,
or hardware acceptance follows from these hosted results. Native startup/IPC,
typed BAR facts, electrical validation and recovery remain required work.
