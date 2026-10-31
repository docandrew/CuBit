# Immutable table Field loading

Run `TMPDIR=/home/doc/cubit-build-tmp nix develop -c bash tests/aml-table-field-load/run.sh` from the repository root. `--validate-only` checks pinned inputs without compiling.

Both checked and release builds exercise both AML integer widths: aligned, shifted, partial-byte, trailing-bit, alias, incomplete body, incomplete header and invalid declared length. Expected: 1,464 checks in 32 processes. Successful loads retain owned source bytes and declaration identities; failed loads leave counters, targets and child declarations unchanged. Fixtures are recompiled with pinned IASL and checked against recorded reference AML hashes. Wrapper bytes are pinned text hex. This gate does not execute ACPICA; the fixture provenance records the separate independent reference runs. No hardware provider, native run or new SPARK proof is claimed.
