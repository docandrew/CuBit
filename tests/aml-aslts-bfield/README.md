# Original ASLTS bfield revision 2 (opt-in hosted gate)

Configuration correction: the historical `bfield-n64` CLI/filename is a legacy evidence identifier. The actual compiler command was plain normal iasl, using the original revision-2 source with default optimizations. This is **not** upstream ASLTS `n64` (`-oa -r 2`), and is not claimed identical to upstream `o64` either: upstream common flags also include `-of -cr -vs`. Full original corpus and measured pass remain valid for the exact recorded AML; no new run is implied by this correction.

From the repository root, within the pinned development environment:

```sh
TMPDIR=/home/doc/cubit-build-tmp python3 tests/aml-aslts-bfield/run.py --profile release --selection bfield-n64
```

Export TMPDIR before entering Nix as well. This is hosted-only, explicit opt-in; it is not part of the default regression selection and invokes no kernel/native build. Preserve a results directory before rerunning: the runner rejects an existing result directory. Do not run with optimized Python.

The exact original upstream bfield revision-2 AML is checked in with its nine-file ASL include closure, normal compiler/disassembly output and raw ACPICA results. No wrapper or reduced matrix: the arbitrary-field test retains 8 start positions by 80 lengths. Reference provenance and exact historical receipt hashes are in `reference/evidence.json`; raw logs preserve historical diagnostic paths and cache-allocation warnings, not runtime dependencies. Regeneration is deliberately not part of this gate.

The main and protected runner are byte-identical to the tested private predecessor. Two same-arena MN00 invocations must return zero; existing ERRS is read through public Evaluate_Named and must be zero each time. The predecessor passed with charges 1,088,769 and 1,088,859. This portable relocation passed release with the recorded revision-2 AML: MN00 and ERRS returned zero twice, with the same charges. Two isolated guard negatives (optimized Python and altered input) were rejected before compilation. Evidence is recorded in `reference/evidence.json`. Only release execution of the recorded default-optimized revision-2 AML is selected; no exact upstream n64/n32/o32/o64, checked, or whole ASLTS pass is claimed.

This gate explicitly reproduces the approved test-only 2,000,000 method-fuel / 180-second whole-process exception. Build cap remains 180 seconds, module fuel 100,000, explicit continuation capacity 128, stack 64 MiB, each log 1 MiB, sequential compiler j1/nice19. Default production and other portable limits are unchanged. Earlier 1,000,000-fuel failures remain recorded. On failure, the bounded public stored-data snapshot reports the subtest and completed status strings without invoking AML methods; all retained observation handles are released. No automatic retry, fuel increase or timeout escalation.

Input/inventory hashes, actual compiler path and ALI source selection are audited. Source changes require an explicit reviewed manifest refresh, not silently accepting whatever is installed. Existing historical generated ASLTS reports are not rewritten. The inventory remains 342 original entrypoints: bfield adds one selection to arithmetic/logic/control (four selected, 338 unselected), not four passes. WHL derivatives/repeats do not add configurations or entrypoints; no percentage is inferred.
