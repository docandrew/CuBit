# Intel GPU capability selection

## Evidence and current gap

Linux v6.16 identifies ADL-N PCI devices46D0,46D1,46D2,46D3,46D4 as an
alderlake_p subplatform with xe_lpd_display defaults (display IP13).
Defaults include pipes A-D; runtime initialization removes fused-off pipes.
It exposes four sprite planes plus the primary for display IP13.

Sources:
- https://github.com/torvalds/linux/blob/v6.16/include/drm/intel/pciids.h
- https://github.com/torvalds/linux/blob/v6.16/drivers/gpu/drm/i915/display/intel_display_device.c
- https://github.com/torvalds/linux/blob/v6.16/drivers/gpu/drm/i915/i915_reg.h

The relevant runtime register is SKL_DFSM at0x51000, NOT the power-well
fuse-readiness register at0x42000. Pipe-disable fields are A30, B21, C28,
D22 (D applies display version12+). Linux also interprets DMC-disable bit23
on version11+. Other meanings are generation-dependent: e.g. older headers
also name bits24:23 as a CDCLK limit. Do not build one universal field record
from every symbol in a multi-generation header.

CuBit currently recognizes the exact ADL-N IDs in Intel_GPU_Probe but native
write guards remain specific to46D2. Its fixed scanout inventory requires
20 plane observations and4 cursor observations. Power-held does not establish
that all those objects physically exist. This is a gap, not a confirmed cause
of the last NUC log ending at C plane3.

## Required selection layers

1. Authenticated PCI identity selects the register-layout family and revision
   rules. Unknown identity cannot select a nearby generation by prefix.
2. Model defaults define candidate pipes/engines; documented fuse/capability
   observations refine physical presence. Failed/unstable/sentinel reads mean
   unknown, never absent. Power availability is a separate property.
3. Driver implementation support remains distinct: hardware can support tiled
   scanout while the current footprint decoder cannot safely describe it.
4. Activation requires existing authority, power and ownership preconditions;
   model recognition alone never permits MMIO writes or memory reclamation.

## Inventory integration requirements

- Represent presence as unknown/present/absent, not a Boolean default false.
- Skip only objects proven absent by the selected layout's capability rules.
- Require every present plane/cursor to be collected and decoded safely.
- Unknown presence makes inventory incomplete and forbids allocation admission.
- An unsupported enabled surface remains a driver-decoder limitation, not an
  assertion that the hardware lacks the format.
- Do not fabricate a Disabled register sample to represent absent hardware.
- Bind the capability snapshot to the same admitted device/owner as the MMIO
  collector. Test every pipe-disable combination and unknown/stale evidence.

Next implementation should add generation-scoped DFSM representation and
thread the resulting inventory into collection. The exact Intel PRM layout
still needs verification for unnamed fields; do not label them reserved based
only on Linux's partial header. The handed-off intel-fields.QV2u4T image does
not yet implement this selection.

Intel TGL Vol2c Part1 (IHD-OS-TGL-Vol2c-12.21), printed pp432-434,
PDF pages458-460, independently confirms all four pipe-disable bit positions.
Official source: https://cdrdv2.intel.com/v1/dl/getcontent/703046
Downloaded reference: kernel/build/tmp/tgl-registers-a-l.pdf.
All three pages visually inspected. Important discrepancy: this TGL table
labels bit23 reserved, while Linux's runtime code interprets a DMC-disable
field there on applicable platforms. This audit only establishes shared
pipe-disable positions, not a universal full-register semantic layout.
