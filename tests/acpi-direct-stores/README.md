# Direct integer and byte stores

Run in the pinned Nix development environment after exporting
`TMPDIR=/home/doc/cubit-build-tmp`:

```
python3 tests/acpi-direct-stores/run.py --mode checked
python3 tests/acpi-direct-stores/run.py --mode release
```

Each profile checks 180 assertions against the real namespace owner and object
store. Cases include byte truncation in both AML widths, integer origin and
package payload preservation, string-source indexing, foreign/stale references,
aliases and object-capacity failure. Byte stores must not allocate objects.
The initial owner cases derive from `tests/acpi-integration/namespace_owner_tests.adb`;
the catalog records the exact base and derivation hashes.

Sources, compiler, build profile, binary and logs are guarded in each receipt.
`--validate-only` checks source/tool pins without building. These are hosted
regressions; they do not claim hardware execution or a complete service proof.
Previously accepted proofs are reused only while their source is unchanged.
