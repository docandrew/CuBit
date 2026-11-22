# Native AML delay checks

Run `bash tests/acpi-native-delays/run.sh` in Nix, exporting
TMPDIR=/home/doc/cubit-build-tmp before entering it. `--validate-only` checks
sources without compiling or executing. Workers use checked/release profiles,
one compiler job, nice19,180-second compilation and30-second process limits.

The policy matrix checks elapsed-time metadata, units, early wakes, stopped and
regressing clocks, overflow, global deadlines and finite work. The actual native
facade runs against modeled clock/kernel boundaries, including zero Sleep,
non-yielding Stall, one-shot arming, closure and refusal without replenishment.
The actual native ACPI_Service binding/Core then executes iASL fixtures in both
AML widths through retained/scalar paths. Normal return values are compared with
ACPICA. This does not measure native timing or establish electrical behavior.

Expected functional evidence is3882checks/48runtime processes and2ACPICA processes.
The separate startup suite checks closure on88owner lifecycle scenarios. The
separate API suite uses semantic compilation only. Pure policy proof is under
`tests/acpi-native-delay-proof`; it does not prove the stateful native facade,
actual scheduler/clock accuracy, enclosing caller or full interpreter/service.

The provider is startup-only and uses the owner's existing absolute deadline;
no wire request or AML operand can reopen it. Future event execution needs an
explicit owner-scoped timing domain. Existing Stall normalization accepts up to
255microseconds for ACPICA compatibility; the specification limit is100.
