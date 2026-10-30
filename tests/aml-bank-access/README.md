# Hosted BankField access and register catalog

Run `python3 tests/aml-bank-access/run.py --mode release` or `--mode checked`
in the pinned Nix environment, with `TMPDIR=/home/doc/cubit-build-tmp` set before
entering Nix. Run `bash tests/aml-bank-access/check_reference.sh` to verify the
saved normal ACPICA observations without invoking ACPICA or building anything.

The actual service core passes 516 checks in each profile. Banked fields use an
explicit trusted `Field_IO.Add_Region_Register` catalog associated with a live
OperationRegion descriptor, copied named target and geometry, and separate read
and write permissions. Ordinary Field bindings do not imply catalog entries.
A catalog-only set is already tied to its namespace arena and provider.

The bank-selection field has its own independently authorized binding. Every
data read/write selects the bank first, including each Preserve read and write.
All required targets and directions are checked before effects and again before
dispatch. A completed selector write remains completed if a later access fails.
Unknown completion retains the exact selector or data request outside namespace
cleanup; reconciliation requires the matching confirmed operation and never
replays it. Method-local field retirement cannot erase that receipt.

Coverage includes whole and partial writes, update rules, a spanning write,
permission and geometry changes, named-register capacity, registration failure
without consuming any slot, provider/arena identity, malformed completed replies,
unknown selector/data operations, cleanup and repeated declarations. A matching
same-owner repeated declaration captures the newly evaluated selector while
preserving the descriptor identity. Other collision compatibility is unfinished.

The cached 429-byte DSDT supplies 17 successful Integer observations. Its normal
compiler remarks and outstanding-allocation diagnostics remain in the raw logs.
The fake provider checks named-target ordering and values; it is not physical
banked hardware or a serialization model. API policy tests are not ACPICA's
capability policy. Current tests use byte accesses; broader width/result and
failure-prefix partitions need more coverage. Global Lock, nested bank selectors,
concurrency, native provisioning and the whole-interpreter proof remain open.
See the adjacent reset, capacity, declaration and module lifecycle suites.

The completed-prefix cases model an uncertain response both before and after
the provider performs an operation. They cover a partial Preserve write, a later
selector write and the final data write after the first datum has completed.
The exact merged byte and named leaf request survive method-local cleanup.
Reconciliation neither rolls back completed writes nor finishes remaining
datums. Only an explicit new invocation performs a fresh sequence. Additional
cases cover preflight denial of the last target, late permission/geometry changes,
known failures, malformed completed acknowledgements and clearing bindings while
completion is unresolved. These are hosted provider-model cases; they are not
hardware fault measurements or a proof of every interruption point.
