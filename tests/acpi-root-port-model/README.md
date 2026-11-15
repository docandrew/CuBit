# Captured root-port initializer model

Run in the pinned Nix development shell, with TMPDIR=/home/doc/cubit-build-tmp:

    bash tests/acpi-root-port-model/run.sh

Use --validate-only to check the selected source/tool inventory without compiling.
All outputs go to a unique temporary directory; no native build is performed.

The exact disassembled RP04/RP07 _INI bodies come from the Lenovo captured DSDT.
Surrounding fields are named scalar models. PRES and PXSX helpers are constant
models. PINI External declarations are needed for ASL compilation and do not
establish that those declarations exist in the captured AML. This does not
reproduce hardware field access or prove the cause of the laptop startup failure.

Six variants at both ACPI integer widths exercise both initializers, optional
VM enable flags, absent PCI vendor/device state, alternate and false helper
paths, and zero sleep policy. Ten sequential observations per variant verify
stored effects, device/method identity, unresolved methods, and missing names.
Core and ACPICA must each match independent expected results, including the
width-dependent all-ones boolean value. Core also checks zero retained results
after every observation. Assertion-suppressed Core functional testing only;
no new formal proof or native/hardware execution is claimed.
