# Hosted HID-I2C wire codecs

Run in Nix with `TMPDIR=/home/doc/cubit-build-tmp bash tests/hid-i2c-wire/run.sh`.
`--validate-only` checks the closed inventory and pins without compilation.
Checked and release profiles test typed control messages, every register/length
word, frame bounds, reset indications and exclusion of transfer padding.

This validates transport encoding/framing only. It does not implement controller
IO, IRQ delivery, HID report semantics or touchpad input. Scoped framing/capacity
contracts and run-time checks have an opt-in proof in `tests/acpi-leaf-policy-proof`;
no native hardware claim. Input Data describes a completed transfer, not an
unfilled buffer's capacity. A zero prefix is never delivered as input.
