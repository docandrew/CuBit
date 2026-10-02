# Discovered ACPI storage requirements

`Firmware_Tables.Provisioning` measures a sealed catalog before allocation.
Its contracts prove the exact sum of lengths (using a terminating ghost prefix
sum), exact maximum length, count, and equivalence between approval and all
three caller-selected quotas. Totals use a wide integer so rejected inventories
can still report their requirements. Unsealed/failed catalogs expose no prefix.
This does not allocate memory, validate physical backing, or grant authority.

The hosted fixture checks all 1–256 catalog sizes, independently computes sums,
checks exact-fit and one-byte-short quotas, rejects incomplete/failed inventories,
and measures 256 maximum-length descriptors without allocating their payloads.

Validation: 1795 hosted checks; GNATprove 29 proof checks plus 4 flow checks,
zero unproved, justified checks or `Assume` statements. Evidence is saved in
`/tmp/cubit-acpi-provisioning-exact.log`, `-proof.out`, and `-source-hashes.json`.
The kernel now uses these requirements for capture. The native allocator and
raw-memory adapter remain outside this proof; userspace startup and grant
delivery remain separate integration gates.

The fixture is registered in `tests/aml-core/run.sh`; `--prove` includes its
SPARK proof. To run just this fixture from the repository root:

```sh
nix develop -c gprbuild -p -P tests/aml-core/provisioning/provisioning.gpr
tests/aml-core/build/provisioning/provisioning_tests
nix develop -c gnatprove -P tests/aml-core/provisioning/provisioning.gpr \
  -u firmware_tables-provisioning.adb --mode=all --level=2 \
  --timeout=30 --memlimit=2000 --prover=cvc5,z3 --checks-as-errors=on -j2
```
