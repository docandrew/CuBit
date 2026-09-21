# CCL type foundation

Run from the repository root:

```sh
nix develop -c bash tests/ccl-types/run.sh
nix develop -c bash -c 'cd kernel && alr exec -- gnatprove -P ../tests/ccl-types/types_tests.gpr --subdirs=types-proof -u ccl-types.adb --level=2 --report=all --checks-as-errors=on -j2'
```

`registry_tests` exercises nominal identities, product/sum layouts, empty
products, enum recognition, bad/duplicate names, invalid/forward references,
layout overflow, registry exhaustion, and unchanged state on rejection.
`enum_tests` exercises the shared interpreter, typed parameters and returns,
enum equality and formatting, declaration visibility, malformed declarations,
cross-type rejection, Lisp/BASIC canonical round-trips, and enum compilation.
`variant_tests` exercises every scalar alternative, payload bindings/shadowing,
nested and reordered arms, lazy evaluation, nominal results, source diagnostics,
Lisp/BASIC roundtrips, compiled execution, and canonical CCLB v4 roundtrips.
`variant_rejection_tests` covers hostile schemas, truncation at every byte,
nominal stack joins, dispatch target completeness/bounds, payload type confusion,
ownership joins and laundering attempts, and full/wrapped/empty stack access.
These are Linux-hosted tests of the same core used in CuBit.

The registry proof covers runtime checks, initialization, termination, and
atomic publication/rejection. It is not a proof of type-system soundness or
compiler refinement. Registry identities are snapshot-local, not IPC type IDs.

## Variant proof boundary

```sh
nix develop -c bash -c 'cd kernel && alr exec -- gnatprove -P ../tests/ccl-types/types_tests.gpr --subdirs=variant-final-proof -u ccl-types.adb ccl-types-encoding.adb ccl-vm.adb ccl-ownership-bytecode.adb --level=2 --report=all --checks-as-errors=on -j2'
```

The variant slice passes 252/252 checks: 131 initialization, 3 non-aliasing,
67 runtime, 14 assertions, 12 functional contracts, and 25 termination checks.
Both VM instantiations of the bounded stack, including `Peek_At`, are covered.
No proof assumptions, disabled SPARK sections, or warning suppressions were added.
These results establish the checked properties, not a metatheoretic proof of
type-system soundness or compiler refinement.

The **complete module codec** (`CCL.Format`) is not included in that clean
result. Its broader run was stopped after proof transformation consumed over
26 GiB in the encoding worker. Refactor its growing serialization routines into
bounded section codecs before retrying a complete proof; the fixed-size nominal
schema codec is already separately proved. Module roundtrip/corruption and
native execution regressions pass, but they are not substitutes for that proof.

## Frontend proof follow-up (pre-variant baseline)

The full `ccl-language.adb` level-2 run discharged 1,255/1,262 obligations
(1,132/1,139 runtime checks, all 7 functional contracts, 92 initialization
and 24 termination checks). Seven runtime obligations remain in the shared
frontend; do not describe the whole interpreter as proved:

- `Parse_Program`: function count increment after recursive expression parsing.
- `Check_Node`: definition function reference and visible-function increment.
- `Evaluate_Node`: function-call reference and retained-handler reference.
- `Process_Source_With_Host`: root-node indexing for the handler-result diagnostic.
- `Deny_Host`: assigning an Integer result to a potentially constrained
  discriminated host-value output parameter.

Prefer structurally valid checked-tree and function-reference types to
redundant runtime guards or assumed contracts.
Flow analysis also reports unused initialization/state-restoration warnings;
strict proof/flow commands therefore do not exit cleanly for the full frontend.
No `Assume`, `SPARK_Mode => Off`, or warning suppression was added for this work.

Native regression: `tests/headless/run.sh --test ccl-vm --accel kvm` requires
`enum source PASS`, `enum isolation PASS`, and `variant bytecode PASS`, in addition
to the existing bytecode, IPC, scheduler and ownership markers. The variant
test compiles source, serializes CCLB v4, decodes/verifies it, and executes the
match in native CuBit. This is not a Linux-hosted simulation.
