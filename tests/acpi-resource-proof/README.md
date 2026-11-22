# Copied resource framing proof recipe

Run `bash tests/acpi-resource-proof/run.sh prove` inside the pinned Nix development
environment at nice19, with TMPDIR=/home/doc/cubit-build-tmp set before and inside
Nix. `--validate-only` checks source/recipe hashes without compiling. The selected
unit is the actual ACPI_Resource_Data body. Limits: level1, cvc5/z3,5s per VC,
180s phase,64MiB stack,1MiB log,j1. Process0 alone does not prove all goals.

Scope: runtime safety, frame progress/geometry, scan quota and reported extents.
Reserved descriptor names remain unvalidated metadata. Descriptor-specific payload
semantics, authorization, copied-buffer provenance and full interpreter correctness
are outside these contracts. Standard Ada arithmetic and the proof toolchain remain
trusted. Run `check-result.sh` on a fresh result directory to reject unresolved,
skipped or missing checks. The inspected milestone discharged 51 checks across
93 valid goals, plus 10 flow checks; all three entities were analyzed in SPARK,
with no Assume pragmas or skipped checks. This is a scoped unit proof.
