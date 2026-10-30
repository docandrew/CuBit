# Hosted ACPI-to-HID transport integration

Run `bash tests/acpi-hid-transport-integration/run.sh` inside the pinned Nix
shell, with TMPDIR=/home/doc/cubit-build-tmp. `--validate-only` checks the exact
source/recipe hashes and directory inventory without invoking tools.

This joins actual Core AML execution of a synthetic device's _CID, _STA, _CRS
and HID-I2C _DSM to callback-driven HID_I2C_Bring_Up.Run. The hosted owner checks
present/enabled/functioning status and separately binds a modeled controller.
It consumes slave address, speed, address mode, controller path and GPIO pin
from the same copied resource template. It passes descriptor-register values
0x0020, zero and 0xFFFF without narrowing them to eight bits. The transport header
then selects its independent command/report registers. No source address or
firmware description constitutes authority to perform IO.

The 428 checks include retained-result ownership, sizing retry without repeating
AML, foreign-context refusal and budget cases from the discovery regression.
The integration cases cover exact transport bytes, early IRQ during RESET,
report prefix/suffix and scratch handling. Missing/ambiguous/alternative
resources, disabled/absent/nonfunctioning devices and unavailable controller
binding perform no lower IO. No implicit retry occurs.

Checked (-gnata -gnato) and release (-gnatp) profiles must agree, with 86 local
source ALIs per profile and actual compiler switches verified. ACPICA20260408,
repair disabled, must match six expected integer values. Fixture diagnostic3150
is its intentional empty-resource case;2095 is its real empty fourth _DSM
argument package. The exact diagnostics are checked. ACPICA reports four cache
allocations at exit0 during shutdown; this is retained, not a clean-memory claim.
Each command uses the existing resource caps: j1, nice19, compile180s, runtime30s,
ACPICA5s, stack64MiB, core0, build/runtime log1MiB. Results use unique directories.

Assumed: initialization/dependencies are complete, the owner has a valid device
binding, and no concurrent firmware-state change invalidates observations. The
transport and interrupt callbacks are models. This is not a native driver test,
Lenovo firmware dump, logging-delivery test, HID report semantic/input test or
proof of the complete service. Real controller/GPIO resolution, power timing,
EC/region IO, snapshot export/launch and laptop validation remain open. No new
SPARK proof is claimed for this regression; hosted GPRs are test recipes only.
