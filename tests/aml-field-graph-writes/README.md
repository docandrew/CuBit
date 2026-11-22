# Deep field writes and update rules (hosted)

Normal561-byte ACPICA20260408 fixture compiles without errors, warnings or remarks.
Seventy-two ordered Integer observations cover12methods: full, Preserve-partial,
WriteAsOnes and WriteAsZeros stores through banked index/data, a banked selector,
and a mixed Bank->Index->Bank chain. See reference/cases.tsv for independent
expected snapshots/traces and reference/expected.tsv for the ordered values.
`bash tests/aml-field-graph-writes/check_reference.sh` checks cached observations,
embedded AML bytes, table extent/checksum and original evidence hashes.

The full matrix at field-graph-update-rules-MlesoiHY passes3474 release checks.
Assertion-enabled status and independent audits are in the checkpoint HANDOFF.
The earlier343-byte six-method fixture at D3CitBD9 passed2022checks in each build;
those retained results are distinct from this expanded12method run.

Combined request traces require fresh selector setup before each data read/write.
Whole/Ones/Zeros writes succeed without data-read permission; partial Preserve
requires that permission before any selector effect. All stores preflight target
and selector write authority and recheck after effects. Unknown completion is
injected before/after every one of60request positions (120cases), validating exact
leaf/direction/value receipt, all completed register values, no replay, denial of
wrong/unconfirmed receipts, and only explicit re-invocation after reconciliation.

Run pinnedNix/requiredTMPDIR, `python3 tests/aml-field-graph-writes/run.py --mode
release` (or `checked`). Only4independent named leaves and3explicit region catalog
entries grant authority; logical fields gain no direct binding/raw-address rights.
The model is not physical bank storage/hardware. Production library is unchanged
from the prior proved-charge checkpoint; this broadens tested semantics, not
new interpreter/native proof. Wider/deeper alias/lifetime/concurrency and fullAML
semantics remain open. Raw ACPICA allocation diagnostics remain without a clean
teardown claim; no forced/suppressed interpreter options.
