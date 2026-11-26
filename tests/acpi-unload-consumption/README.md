# Unload operand consumption

Run from the pinned development shell:

```sh
export TMPDIR=/home/doc/cubit-build-tmp
nix develop -c bash tests/acpi-unload-consumption/run.sh
```

`--output DIR` retains the build, generated AML, logs and `RESULT.json` at a chosen private location.
Sources and tool bytes are pinned before building. Both checked and release profiles run all 14 controls,
with the original finite memory/I/O model, two-table/256-byte copied owner, five-million AML fuel,
12-second case limit, 24 GiB address-space ceiling and 128 MiB stack ceiling. No device is accessed.

The controls distinguish a missing or truncated operand, non-handle values, Debug, an uninitialized local,
named/reference operands, method and Index side effects, and failure after an Index side effect.
The three side-effecting operands must run exactly once; the following statement must not run.
Every case must finish with zero retained results and zero model I/O.

The synthetic AML bytes were compared with normal ACPICA 20260408 without slack or error suppression.
The expected CuBit statuses retain its typed error vocabulary: malformed framing is `TRUNCATED`, and
Index bounds failures currently propagate the ordinary evaluator's broad `UNSUPPORTED_VALUE`.
This test does not claim exact ACPICA error-number parity.

These are new diagnostic regressions, not additional ASLTS passes. They do not provide general `_ERR`
continuation, authenticated table teardown, native execution, or proof of the full interpreter.
