# Module startup Field composition

    export TMPDIR=/home/doc/cubit-build-tmp
    nix develop --command bash tests/acpi-module-startup/composed/run.sh

The optional suite accepts `--selection basic|nested|reciprocal|wide|dependencies|budgets|all`,
`--profile checked|release|both`, `--source-root DIR` and `--validate-only`.
The default runs both profiles/all selections:15596checks in716test processes.
Selecting a group avoids compiling unrelated test mains. This does not add work
to the existing1030checks/profile in the parent module-startup runner.

Sources, models and fixtures are hash-pinned and copied to a private TMPDIR
directory. Seven hosted mains compile separately with one worker and a180-second
bound per invocation; runtime cases have30-second bounds. Strict ALI flag checks
distinguish assertions/overflow checking from suppressed-check release runs.
No native component, image, kernel build or hardware access is performed.

The actual Core/module/Field executor runs with modeled named-cell providers:

- Scalar BankField/IndexField startup, complete/unique associations, stale
  epochs, rights, geometry changes, unknown completion and default refusal.
- Both nesting directions, exact selector/data order, partial writes, distinct
  required providers refusing a mixed plan, unused providers, request exhaustion
  and uncertainty before/after every leaf of a six-request partial write.
- Unaligned72-bit direct/bank/index roots crossing ten cells, Buffer versus
  integer input, nine-byte results, preserved edge nibbles and last-cell refusal
  or uncertainty at both AML integer widths.
-80-bit selector/data dependencies, zero-extended scalar writes, write-only
  dependencies and a request counter shared with parent operations. Wide
  DataName reads beyond scalar width remain an explicit shared-executor refusal.
- Planning work exhaustion and the remaining-work handoff into preflight;
  authorization observations show the latter test reaches execution before
  exhausting its allowance without IO.

Every rejected/uncertain startup remains one-shot. A nested selector may finish
its still-admitted inner writes before the outer data check detects changed
geometry. Tests preserve that attributable prefix; they do not promise instant
cancellation of valid inner operations. Session/arena/provider/backing lifetimes,
independent native admission, reservation/held Global Lock and recovery remain
the controller's obligations.

Twenty ACPICA controls compare explicit initialized values. Nineteen value
controls match. The revision2 wide bank-value control records ACPICA's32-bit
truncation, rather than counting it as equivalence; see BANK-VALUE-COMPARISON.md.
Known cache-cleanup diagnostics are retained separately. The controls do not
establish automatic startup order, physical/indexed-bus protocol, request counts,
fault/lifetime policy or native correctness. These are hosted tests, not new
SPARK proofs. Full verified service/interpreter and native/hardware acceptance
remain unfinished.
