# In-place AML value copies

`nix develop -c tests/acpi-package-copies/run.sh --output /absolute/private/copy-tests`
runs checked and optimized hosted profiles, both with runtime checks enabled.
The suite performs 337 copy checks and one malformed-integer rejection per
profile (676 checks total). It covers independent copies of repeated package
members, nested packages, event reset, allocation quotas, unsupported mutex
children and bounded cycle refusal. The private test child deliberately injects
an invalid integer size; no public constructor is claimed to emit that state.

The copier allocates each value occurrence directly into the current arena,
then publishes the completed result. A refused copy returns no target while
leaving a valid partial arena. Earlier namespace effects remain on a later
failure. Source objects and authority-bearing identities remain intact.
Unpublished temporary reclamation belongs to the forthcoming reference-count
integration; this packet does not implement zero-count deletion or a cycle
collector. `PROOF_STATUS.md` records the actual closure and open obligations.

`package-store.asl` is a local differential fixture, not an upstream corpus
exclusion or denominator addition. Its unmodified compiled AML has
MN00=0, ERRS=0, RMRC=3 in pinned ACPICA and CuBit checked/release observations.
The packet receipts preserve its original source/AML hashes and exact runs.
