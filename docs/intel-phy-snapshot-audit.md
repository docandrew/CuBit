# PHY snapshot comparison audit

## Hardware evidence

The NUC's `cubit_live_phy_probe.img` reports DC transition failure at
PHY-restore, PHY A, read-failed, nine attempted writes. In that executor path,
all nine write callbacks, posted-write check, and the following power check
returned success. The post-write snapshot callback then returned false before
the semantic Already_Ready check. PHY B restoration had not started.

The old adapter collapsed Rejected, Read_Failed and Changing into that false
result. Thus this evidence does not distinguish power loss, an all-ones MMIO
read, or differing samples. The prepared phy_evidence image retains those
categories and the first affected register/value pair without new MMIO reads.

## Documentation inspected

Intel IHD-OS-TGL-Vol 2c-12.21, printed pages 899-901, PORT_COMP_DW3 and
PORT_COMP_DW8, visually inspected in `kernel/build/tmp/tgl-registers-m-z.pdf`.

DW3 is read-only and contains:

| Bits | Meaning |
| --- | --- |
| 28:26 | Process information |
| 25:24 | Voltage information |
| 23 | PLL DDI power acknowledgement |
| 22 | First compensation complete |
| 21 | Process monitor complete |
| 20:19 | Compensation-code limit flags |
| 14:8 | Compensation code |
| 7:6 | Low-power-down code limit flags |
| 5:0 | MIPI low-power-down code |

DW8 bit 14 disables periodic compensation; zero enables it. This establishes
that the snapshot includes calibration/status data, not merely configuration.
It supports investigating whole-register equality as too strict, but does not
prove the actual failing field or a required settling interval.

The observed C0605E21 and C0606421 select process/voltage zero under the Linux
mask 1F000000. Their upper bits are nonzero where the TGL manual calls bits
31:29 reserved/MBZ. Do not reject ADLN observations using that TGL reserved-bit
description or silently assume identical semantics across models.

Linux v6.16 `intel_combo_phy.c` uses masked readiness checks and uses only
process/voltage bits from COMP_DW3 to select reference values. Its initialization
does not perform our full-snapshot equality check or immediate post-write
verification. Header offsets/masks and the selected reference values match our
current planner. Sources:

- https://github.com/torvalds/linux/blob/v6.16/drivers/gpu/drm/i915/display/intel_combo_phy.c
- https://github.com/torvalds/linux/blob/v6.16/drivers/gpu/drm/i915/display/intel_combo_phy_regs.h

## Subsequent N95 evidence and field-level conversion

The later run identifies PHY B, sample=changing, reads=20, pass=2,
register 0x6C100, first=0x80005F24, second=0x80005F23. This is PORT_COMP_DW0,
not DW3. COMP_INIT (bit 31) remains set in both observations. The reader's
whole-word comparison is the immediate reason it rejects this sample.

Intel's Tiger Lake manual, printed page 896, names the programmable periodic
counter at bits 19:8. Bits 7:0 are reserved/RO/MBZ in that document. Therefore
the changing low byte must NOT be called the documented counter. It is an
ADLN documentation discrepancy whose exact semantics remain unconfirmed.
Linux's PHY-initialized test uses COMP_INIT rather than whole DW0 equality.

The private register conversion compares only restore-owned configuration
fields and the process/voltage selector on which the plan depends. It retains
all-ones MMIO rejection and power checks. The second complete sample is returned;
the executor checks semantic consistency against the baseline and replans from
the fresh sample to preserve unowned fields. No retries are introduced.
Ignoring a field for this comparison does not assert that it is volatile or
that it is safe for an arbitrary caller to write.

The downloaded authoritative layout reference is:
`~/Downloads/intel-gfx-prm-osrc-tgl-vol-02-c-command-reference-registers-part-2.pdf`.
PHY layouts were visually checked on printed pages 664, 885-886, 896-903,
906-907 and 953. Linux v6.16 remains the generation-specific implementation
cross-check. Unknown N95 fields remain explicitly marked; this is not a claim
that every Tiger Lake reserved-bit rule applies to Alder Lake-N.

Status: private implementation and regression changes in progress, not yet
compiled or hardware-validated. This does not establish GuC startup success.
