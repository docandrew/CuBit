# Module-level BankField lifecycle

Run `python3 tests/aml-bank-module/run.py --mode release` or `--mode checked`
in the pinned Nix environment with the required TMPDIR. Eighty-seven hosted
service checks pass per profile. Run `bash tests/aml-bank-module/check_reference.sh`
to verify the thirteen retained ACPICA Integer observations and exact AML.

CuBit evaluates BankValue when the declaration executes and retains that value
for all fields in the group. Sealing and access do not replay the expression.
This is a documented interpretation and compatibility choice, not a proof of
all ACPI lifecycle rules. See LIFECYCLE.txt for the sources and remaining gaps.

The normal 236-byte fixture declares fields at selector 3, records the selector
counter in SEEN, then changes the source to 7. Its observations differ:

| Observation | ACPICA retained oracle | CuBit tested policy |
| --- | --- | --- |
| SEEN after declaration | 1 | 1 |
| Counter after initialization | 3 | 1 |
| Bank selected by field access | 7 | 3 |
| Bank after later source change to 9 | 7 | 3 |
| Data reads and write/readback | A5, 5A, C3 | A5, 5A, C3 |

ACPICA's deferred initialization reexecutes the expression; its own retained
source marks that BankField initialization as unnecessary. The checker preserves
those original results. It does not count the counter/bank differences as
compatibility successes. No fallback silently repeats AML effects.

Tests also check no hardware dispatch on declaration/initialization/scalar
queries, shared selection across both fields, exact named-target traces, no
extra retained roots, and rejection of initialization/module replay after Ready.
Field visibility, forward references, broader collision behavior, firmware that
depends on the extra ACPICA effects, full proofs and native integration remain open.
