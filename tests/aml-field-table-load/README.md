# Owned Buffer Field table loading

From the repository root, run:

```
TMPDIR=/home/doc/cubit-build-tmp nix develop -c bash tests/aml-field-table-load/run.sh
```

`--validate-only` checks the source manifest without compiling.

This hosted gate exercises aligned, unaligned, partial-byte, trailing-bit,
alias and short Field sources through the actual namespace and Core Load path.
It uses pinned ACPICA IASL for both AML integer widths, then runs assertion-enabled
and release builds: 1,308 checks in 24 processes. Compiled AML must match the
preserved reference hashes.

This gate does not execute ACPICA, access hardware, prove Field geometry, or
establish complete interpreter correctness. It tests loading an owned copy of
bytes read from a Buffer Field; hardware-backed Field sources remain separate.
