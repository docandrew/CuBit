# BankField selecting through IndexField (hosted)

Normal ACPICA 20260408 reference: 192-byte DSDT, zero compile errors, warnings
or remarks; thirteen successful integer observations. Raw teardown diagnostics
are retained; no clean teardown claim. No forced interpreter options.

The model installs only IDX0/DAT0/MDAT leaf bindings and an explicit MDAT region
catalog entry. BNK0 is a logical indexed field with no direct target binding.
One combined request trace checks index->bank->data order, repeated selection
for Preserve read/write, permission preflight/rechecks, and before/after-effect
unknown completion at each of the six partial-write requests. Each uncertain
receipt identifies the actual leaf request and blocks replay until reconciliation.

Run `python3 tests/aml-bank-index-selector/run.py --mode release` (or `checked`)
in the pinned Nix environment, TMPDIR=/home/doc/cubit-build-tmp.

This is a single composition edge: index/data sources remain direct fields.
It is neither full nested field coverage nor a native physical bus/hardware test
or a proof of namespace/I/O correctness. Request-limit coverage is also exercised
by the independent aml-bank-access suite. Full AML and native service work remain.

The same test main also instantiates a five-request limit. A partial write stops
with Value_Limit after index/bank/read/index/bank and before data write, retaining
the known prefix without uncertainty. Subsequent explicit field operations start
fresh counters. Indeterminate selector requests keep the actual leaf receipt and
block new work. Release evidence: 385 checks at bank-index-tests-B9W3P4Py;
assertion-enabled results are recorded in the enclosing checkpoint handoff.
`bash tests/aml-bank-index-selector/check_reference.sh` checks the cached normal
reference, all thirteen typed observations, embedded table bytes and checksum.
