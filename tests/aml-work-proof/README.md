# Proven production work-charge function

`AML_Execution_Limits.Charged_Field_Work` is called by the production field router.
For every well-typed Field_Work_Count (0..1,048,576), it returns zero for zero
input, otherwise exactly input-1, without a range failure. It reads no globals.

Actual standalone check/flow/prove phases at field-work-proof-wMIRNym5 passed.
Two entities are wholly in SPARK, two flow results establish dependencies and
termination, and both range/postcondition obligations are proved (all transformed
subgoals Valid). No skipped proof/flow or additional callee assumptions appear in
the retained trees. Compiler, prover, arithmetic/runtime models remain trusted.
The proof used level1,CVC5/Z3,5s/VC,j1,180s/phase,64MiB stack,1MiB log.

`bash tests/aml-work-proof/check_cached.sh` validates exact production code/recipe,
retained source/tool/artifact origin records and proof trees. CURRENT-PROOF.json
pins four semantic/tool/recipe inputs; the original five-input narrative is
preserved under reference/current/original-readme.md. Remaining intermediate
build/prover artifacts stay at the original phase directories named in origin.
No new prover execution is implied by cached validation.

The namespace caller computes Accepted before charging and sets Value_Limit on
zero input. The caller contract, graph frame management, permission/order/effect
semantics, namespace, AML interpreter and native hardware ARE NOT proved by this
small result. Their existing hosted tests and proof gaps remain separate.

`bash tests/aml-work-proof/run.sh check|flow|prove` executes a new bounded phase
in the pinned Nix shell with required TMPDIR. Inspect each phase before advancing;
no process-status-only acceptance. It imports no namespace/executor and does not
retry or investigate the previously failing namespace constructor proof.
