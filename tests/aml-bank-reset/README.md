# BankField catalog reset and completion

Run `python3 tests/aml-bank-reset/run.py --mode release` or `--mode checked`
in the pinned Nix environment with the required TMPDIR. The standalone namespace
and collecting Field_IO adapter pass 33 checks per profile. This exercises the
library reset interface; it does not add a reset API to the immutable service
catalog or restart a native service.

An unknown banked data operation survives metadata reset as an exact historical
receipt. Matching confirmed completion reconciles without dispatch. Reloading
the same physical region does not revive old descriptors, field keys or catalog
entries. New explicit authority is required. After Clear, adding only a fresh
selector association still cannot access data without a new catalog entry.

The fixture is the normal table embedded by ../aml-bank-access. Run that suite's
check_reference.sh for cached ACPICA provenance. Reset/capability assertions are
hosted API policy evidence, not ACPICA or native bus observations. Full proofs,
concurrency and native transport remain unfinished.
