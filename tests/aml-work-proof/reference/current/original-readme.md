# Scoped graph-work charge proof

The production router calls Charged_Field_Work. This standalone project selects
only AML_Execution_Limits, excluding the namespace and executor. The intended
properties are saturation at zero, exact decrement otherwise, and range safety.
No recursive namespace/compiler reduction is used. Check/flow/prove are separate
bounded phases; actual coverage and proof trees must be inspected before success
is claimed. Limits: 180s/phase,5s/VC,one worker,64MiB stack,1MiB log; level1.
This does not prove the calling router, its frames, effects, namespace, AML
semantics, native hardware or the trusted compiler/prover/runtime models.
