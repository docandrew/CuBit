# Hosted BankField declarations

Run `python3 tests/aml-bank-declarations/run.py --mode release` or `--mode checked`
in the pinned Nix environment with the required TMPDIR. The 77 checks cover full
Integer-valued BankValue expressions, method-local field metadata, selector
side effects before missing region/bank errors, repeat execution inside a loop,
selector failure and field cleanup. No hardware authority comes from declaring
fields. Existing incarnation/owner checks remain part of namespace admission.

`bash tests/aml-bank-declarations/check_reference.sh` checks the saved normal
393-byte ACPICA fixture, exact embedded AML and table checksum. It preserves the
ordered eleven Integer returns and three expected runtime errors: two missing
names and divide by zero. A zero process exit is not treated as all evaluations
succeeding. Compiler remarks and allocation diagnostics are retained.

These cases do not establish every collision, forward-reference, visibility or
reference-lifetime rule. The executor/namespace changes are regression-tested;
they are not covered by a fresh whole-interpreter proof. See ../aml-bank-module
for the explicitly different module initialization policy.
