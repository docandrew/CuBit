# Copied-table store proof

Run `run.sh` with the pinned Nix GNATprove/CVC5 environment and
`TMPDIR=/home/doc/cubit-build-tmp`. The recipe proves only
`aml_copied_tables.adb` at levels 1 and 2, with a five-second solver timeout,
one worker and a 180-second limit per proof process. `--validate-only` checks
the cached exact-source proof without running a prover.

Each level must discharge 138 checks, with no unproved, justified or skipped
checks and no `pragma Assume`. Sixteen negative audit controls check that the
auditor refuses invalid reports.

Proved scope: store safety and selected contracts, including exact output bytes
for successful windows, zero output on refusal, and complete store-model equality
when Retain does not add a table. Full successful-import byte-prefix semantics,
parser bodies and the interpreter are outside this proof. The parser has a
separate proof target. Interfaces intrinsics remain toolchain assumptions.

Cached evidence must match the current source manifest and the exact original
runner and checker hashes. No native execution or hardware validation is claimed.
