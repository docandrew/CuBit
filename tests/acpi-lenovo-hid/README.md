# Captured Lenovo HID and native eligibility regressions

Preserves selected TPD0 resource buffers/fields, INI/STA/CRS/DSM, parent INI, HIDD/TP7D/I2CM and PCI BBN/ADR fragments. Synthetic wrapper, GPIO identity/admission and GNUM/INUM/SRXO; no hardware operations. Captured provenance includes original DSDT hash and exact fragment byte/line spans. TPRD/TPWR omitted; IM01=0 only. Controlled STA matrix and added inactive alternatives are marked separately.

Actual Core, native startup enumeration, HID probe/resolver, PCI observer and offer collector are exercised. Native Root evaluation delegates to Core and logging is modeled. Inactive alternatives do not consume the active candidate limit; fresh unavailable devices never become offers. ACPICA independently compares full CRS bytes, status and vendor/modeled values at both integer widths; cache shutdown notices are retained.

Export TMPDIR=/home/doc/cubit-build-tmp before Nix, then run `bash tests/acpi-lenovo-hid/run.sh` or `--validate-only`. One hosted Core build O0/gnatp, nice19/j1, 180s/24GiB compile; each Core case30s; each ACPICA5s/128MiB/2MiB stack. No native execution, firmware/hardware validation or new SPARK proof. Temporary GPR is a hosted artifact only.
