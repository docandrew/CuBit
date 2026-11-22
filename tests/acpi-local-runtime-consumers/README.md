# Runtime consumer integration

Run in the pinned Nix development environment with GNAT, GPRbuild, Node and ACPICA on PATH:

    export TMPDIR=/home/doc/cubit-build-tmp
    nix develop -c bash tests/acpi-local-runtime-consumers/run.sh

The actual hosted Core and actual lid, power-source, battery and HID-over-I2C discovery generics share one runtime facade. Both AML widths exercise three event windows: initial observations, an AML method changing samples and issuing three Notify operations, then updated observations after bounded dequeue. Touchpad _CID/_STA/_CRS/_DSM decoding finishes and releases its retained resource before event completion; idle evaluation is refused. The same Core preserves the changed lid, source and battery values across windows. No transport or controller authority is inferred from these firmware descriptions.

ACPICA with repairs disabled independently checks before/after lid, power and four battery values, plus touchpad status and the descriptor register. Its extra no-argument oracle wrapper supplies the typed _DSM arguments. Logs retain compiler/ACPICA diagnostics. Runtime tests use -gnatp; these are hosted functional checks, not a proof, native event loop, GPIO/I2C transport execution or laptop validation. Owner and clock callbacks are modeled, serialized and assumed to return normally. The battery values are raw observations; units need _BIX/_BIF. Native same-root consumer binding is checked separately with the SDK fixture.
