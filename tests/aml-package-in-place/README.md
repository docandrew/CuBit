# In-place package completion

Export TMPDIR=/home/doc/cubit-build-tmp before entering the pinned Nix environment,
then run `bash tests/aml-package-in-place/run.sh`. `--validate-only` checks source
and tool admission without execution. Fresh outputs and hashes are preserved.

The actual Core case performs 40 checks over two same-arena evaluations: a missing
package name remains uninitialized, an earlier buffer-field value is captured,
initializer effects occur once, Core state stays valid and retained roots balance.
Normal iASL regenerates exact cached AML; repair-disabled ACPICA must return1/1,
package[65,null] and its missing-name diagnostic. Cache teardown diagnostics are
retained and do not establish clean-memory behavior. The unchanged baseline fails
the missing-member expectation; the candidate passes.

The synthetic quota suite covers24 cases across32/64bits, named/unnamed packages,
fixed/generic fields and missing/first/second-capture quota cases. It performs192
behavior assertions and32728 setup admissions (32920 total). Its clock callback
mutates the arena solely to inject quota: this is outside the production passive
clock contract, so it proves no callback/reentry safety. It checks valid state,
typed failure, no mistaken successful replay, monotonic incarnations and temporary
name cleanup; it does not impose namespace rollback or garbage-retention rules.

Use `--mode checked` or `--mode release` (default release). The named-base
checked-contract lowering repair is retained; fresh checked execution is now a
separate gate. The prior completion proof timed out at180s and the SPARK generic
Global frontend assertion has a separate minimal reproducer. This broader
functional/AoRTE proof is incomplete, not one of the minor follow-up equalities.
These tests establish no native execution, full AML or whole ASLTS conformance.
Exact current receipts and limitations accompany the guarded packet.
