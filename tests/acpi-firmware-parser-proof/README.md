# Firmware table parser proof

Run `run.sh` with the pinned Nix GNATprove/CVC5 environment and
`TMPDIR=/home/doc/cubit-build-tmp`. The recipe proves only `firmware_tables.adb`
at levels 1 and 2, with a five-second solver timeout, one worker and a 180-second
limit per proof process. `--validate-only` checks the cached exact-source proof
without running a prover.

Each level must discharge 61 checks, with no unproved, justified or skipped
checks and no `pragma Assume`. Sixteen negative audit controls check that the
auditor refuses invalid reports.

Proved scope: parser safety, flow, termination and accepted extent contracts.
This does not prove complete signature/checksum semantics, hardware mapping
authority, or the interpreter. Interfaces intrinsics remain toolchain assumptions.

Cached evidence must match the current source manifest and the exact original
runner and checker hashes. No native execution or hardware validation is claimed.
