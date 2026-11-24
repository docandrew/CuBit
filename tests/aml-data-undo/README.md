# AML data allocation undo checks

The composite data parser constructs in its supplied value arena, saving one
limited before-image for each new allocation. Failure restores records,
incarnations, cursor counts and overwritten byte/element tails in reverse.
The parser's local owned list never accepts another store. Ordinary and
journaled integer/buffer/package paths share one inlined mutation body.
This does not replace namespace value snapshots or method/table cleanup.

In the pinned Nix shell, set TMPDIR=/home/doc/cubit-build-tmp first. Build
`gprbuild -p -j2 -P undo_checked.gpr`, then run
`build-checked/bin/parser_tests` (504 explicit checks) and
`build-checked/bin/receipt_tests` (515). The release project keeps explicit
checks active and uses O3/gnatp. Use disjoint ACPI_UNDO_BUILD directories;
cap builds at120s and runs at30s with24GiB address-space/128MiB stack limits.

Cases cover both AML integer widths, old borrowed references, newly allocated
identity rejection, holes below the old slot bound, context rollback, dirty
unused tails, nested reverse restoration, partial object/byte/element quota
failure, zero-length objects, and64/65 package nesting. Storage_Error is mapped
to refusal before mutation, but allocator failure is not fault-injected here.

These are Linux-hosted checks. This packet is not a proof of the complete
parser or interpreter. The actual changed parser proof reachedPhase3 and its
90s cap. New receipt functional contracts remain open; earlier isolated owner
storage/body proofs cover only their recorded inputs. Proof-only alternative
models are private experiments and are not substituted for this code. Service
and the opt-in collector remain OFF. See the guarded packet receipts for CPU
results, including the retained allocation regression.
