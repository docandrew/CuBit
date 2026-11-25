# Module missing-name cursor tests

Run inside the pinned Nix development shell, from the repository root:

```sh
python3 tests/acpi-module-name-terms/run.py --output /tmp/name-terms-shipping --profile shipping --reference
python3 tests/acpi-module-name-terms/run.py --output /tmp/name-terms-checked --profile checked --fixtures /tmp/name-terms-shipping
```

Each output directory must be new. `--fixtures` reuses the two own compiled
ASL seeds, validating their source and AML hashes. The seeds and in-process
memory owner come from `tests/acpi-module-name-recovery`; this suite does not
copy firmware or change upstream tests. `generate.py` creates thirteen small
cases from public AML encodings with rebuilt table lengths and checksums.

The `External(\\MISS)` declaration sits under `If(Zero)`. An unconditional
External would create an ACPICA placeholder and test an uninitialized object
instead of a missing name. `--reference` runs the exact generated inputs through
the pinned ACPICA tool and checks `_LID`, `CONS`, `SIDE`, and `SINK` against
`cases.json`. Each reference and CuBit process has a twelve-second timeout,
24 GiB address-space limit, and 128 MiB stack limit. Builds use two jobs and
warnings as errors; shipping and checked compiler profiles remain distinct.

The cases cover a discarded rooted name, a nested scope, Store sources,
nested Stores, method arguments, an effectful remaining Store target, and an
effectful remaining arithmetic operand. They verify continuation from the
consumed name, with remaining effects occurring once in their original order.
The failed lookup must not substitute a value or replay an earlier effect.

Three propagation guards distinguish missing names inside called method bodies
or in the final probe. The two called-method cases check table abort and prefix
effects; they do not establish full catalog or namespace rollback parity with
ACPICA. A method failure must not become a module-level recovery diagnostic.

Results include exact commands, tool/source/generated-table/binary hashes,
logs, typed recovery counts, and running pass counts. These are hosted semantic
tests with a synthetic memory owner, not full ASLTS, native hardware acceptance,
or SPARK proof closure.
