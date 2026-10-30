# Scoped pure-policy milestone proof

Opt in from Nix with `TMPDIR=/home/doc/cubit-build-tmp bash tests/acpi-leaf-policy-proof/run.sh`.
`--validate-only` checks source/recipe pins without running a prover. Level2,
5second solver limit, j1,180second command cap,64MiB stack; no resource escalation.
This milestone selects only ACPI_Battery_Status and HID_I2C_Wire. It proves their
run-time checks and existing contracts/flow, including bounded input payload and
capacity. It does not prove all battery semantics, transport callbacks, Core AML,
startup ownership or native hardware. Hosted tests remain separate evidence.

The initial milestone exposed ordinary uninitialized-array flow obligations and
an array-length conversion range check. A four-element local array is now fully
initialized. Wire framing and startup sizing compare array bounds, avoiding a
conversion of Natural'Last+1 elements to Natural. The complete array domain is
covered by proof without allocating a multi-gigabyte hosted test array.
