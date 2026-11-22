# ACPI query admission

Run in the Nix development environment:

```sh
export TMPDIR=/home/doc/cubit-build-tmp
nix develop --command bash tests/acpi-query-admission/run.sh
# At the native admission milestone:
nix develop --command bash tests/acpi-query-admission/run.sh --proof
```

The hosted suite checks role/decoding precedence, full-width opaque sender
preservation, canonical generation/slot encoding, and private-buffer extent
bounds in checked and release builds. No mapping or authority is inferred from
a reference. The native input adapter reads the received sender/authorityTag
and leaves layout/selector validation to the generated typed decoder callback.
Its body and a concrete instantiation compile against the actual runtime specs;
the compile model supplies no real wire layout and never executes native IPC.

Optional level-2 proof selects only ACPI_Query_Admission, with a five-second
solver limit and one job. Its dependency contracts remain modular assumptions.
It does not prove the decoder, native receive wrapper, grant acquisition,
saved replies, AML, complete service or hardware behavior. No new selector,
message word layout, capability or syscall is allocated by these units.

Inputs/tool hashes, logs and disjoint snapshots are retained under TMPDIR
or `--output-root PATH`. Builds/proof have 180-second bounds, runtime30seconds,
logs1MiB, stack64MiB and core dumps disabled. This suite contains no AML fixture;
the larger owner/pump integration runs fresh ACPICA separately.
