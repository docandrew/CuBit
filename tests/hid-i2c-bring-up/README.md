# Hosted HID-I2C startup sequence

Run in Nix with
`TMPDIR=/home/doc/cubit-build-tmp bash tests/hid-i2c-bring-up/run.sh`.
`--validate-only` checks inventory/pins without compilation. Both profiles test
1992 checks against modeled controller/IRQ callbacks. The sequence acquires the
HID descriptor, powers on, arms before reset, waits/reads reset completion,
releases the IRQ generation and retrieves the report descriptor.

Callbacks bind one authorized device, honor one explicit absolute deadline and
never reenter or synchronously call back into ACPI. Clock/IO/IRQ/cleanup failures
stop without retry and clear partial outputs. A failed IRQ-context release
remains explicit for the owner. The owner supplies bounded logstore/serial stage
lines and summary from detached trace facts, using the existing logger as-is.

This is not native controller/interrupt validation, a HID semantic parser or
input readiness. Real hardware may require reviewed timing policy. No Lenovo
controller identity is guessed; no native build or proof is claimed. The ACPI
service supplies firmware facts; the I2C/HID driver owns hardware/input behavior.
