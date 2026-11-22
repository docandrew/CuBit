# HID-over-I2C transport descriptor

The pure `HID_I2C_Descriptors` package decodes one exact 30-byte v1.00 HID
transport descriptor. It handles arbitrary array bounds, keeps the full
little-endian register/length/identity values, and preserves reserved bytes.
It rejects short/trailing transfers and unsupported length/version headers.
This is intentionally separate from the ACPI `_DSM` descriptor-register query.

`Check_Capacities` is a separate host policy: the report descriptor must be
nonempty, input must accommodate the two-byte length prefix, output may be
absent, and supplied capacities must cover the advertised sizes. It does not
allocate, truncate, validate the HID report language, or authorize registers.
A decoded descriptor is insufficient to establish operational readiness.

Primary layout references:
- https://learn.microsoft.com/en-us/windows-hardware/design/component-guidelines/required-hid-descriptors
- https://software-dl.ti.com/msp430/msp430_public_sw/mcu/msp430/HIDI2C/latest/exports/ti_hidi2c_msp430_latest/doc/html/struct_hid_descriptor.html

In the pinned Nix shell, with TMPDIR=/home/doc/cubit-build-tmp:
`bash tests/hid-i2c-descriptor/run.sh`

Hosted checks exhaust all 16-bit header lengths, versions and report-register
values, all nontrivial capacity boundaries, truncated transfers, reserved bytes,
and a descriptor ending at Natural'Last. Both profiles execute explicit checks;
release does not depend on assertion evaluation. Current count: 458785/profile.
No ACPICA comparison applies to this non-AML transport structure. Existing
ACPI discovery fixtures have separate ACPICA comparisons. No new SPARK proof,
native compilation, controller IO, reset, IRQ, report-parser, pointer-event or
laptop result is claimed. Actual controller/GPIO identity remains unknown.
