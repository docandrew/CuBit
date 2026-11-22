# Local shutdown state-transition proof

Run in the pinned Nix shell with TMPDIR=/home/doc/cubit-build-tmp:

    bash tests/acpi-local-shutdown-proof/run.sh

The actual generic shutdown body is instantiated with SPARK callback models
whose public configuration supplies arbitrary Boolean/deadline inputs. The
models have bodies and contracts; there are no imported proof axioms or
pragma Assume statements. They do not directly mutate the controller state
and return normally. Native callbacks must separately satisfy that assumption.

Levels1/2 prove three controller postconditions: exact retirement predicate,
latched stop transitions, and advancement/cleanup monotonicity plus finite
waiting/indefinite retention classification. The controller has9 analyzed
entities,3 proof obligations and7 flow results per level. The models separately
have5 proof obligations and14 flow results per level. State latching is proved;
actual callback invocation counts, exceptions, reentry, physical cleanup,
minimum external deadline provenance and native behavior remain tested/assumed
as applicable. This is not whole-service or AML interpreter proof.

The runner pins actual sources, models, projects and real CVC5/Z3 binaries,
requires PROGRESS_PROOF and STOP_REASON_NONE/all-SPARK for both reports at
both levels, and rejects unresolved checks, skip entries and pragma Assume.
Each level has independent outputs. One worker,j1,180-second proof commands,
5-second solver bounds,24-GiB address space,64-MiB stack,1-MiB output.
Logs and RESULT.json stay in a unique TMPDIR directory.
