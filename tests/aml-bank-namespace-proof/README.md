# Constructor proof setup: not accepted

This library-level namespace instance uses the current service capacity (512)
and selects Define_Operation_Fields_For_Context at aml_namespace.adb:1287.
The project closure derives from actual hosted compiler dependencies, including
matching specs/bodies. It does not import native OS code or execute firmware.

The first bounded `run.sh check` failed during Global contract generation before
constructor analysis. The front end reported a zero-location range issue at
AML_Execute.Capture_Operand, followed by an internal errout.adb exception. The
actual output is retained at the original bank-namespace-proof-7xpWuzqh snapshot,
result-check-6GdPmsWo. No flow or prove phase ran; no constructor proof is accepted.

A limited source review found that Scalar_Location explicitly includes zero and
Capture_Operand handles Identity=0 before calls requiring a positive transport
slot. This is not evidence of a runtime failure and does not justify replacing
the sentinel or weakening its types. No production change was made from the
front-end diagnostic. No compiler reduction, special switches, repeated crash
investigation or unchanged-source retry is part of this work.

The failed diagnostic's original artifact manifest accidentally included itself;
its self-entry is unusable. Other original artifact hashes were independently
checked. The current runner excludes artifacts.json when capturing hashes. This
small logging fix is not a claim that the front-end issue is fixed, and the
failing proof was not rerun to test it.

This is a diagnostic setup for future work, not a passing proof registration.
Do not advance to flow/prove until a successful check on the intended source has
been inspected. Any later phase must still inspect actual selected-target SPARK,
flow and proof coverage; process exit alone is never acceptance. Limits remain
180 seconds per phase, 64 MiB stack, 1 MiB log, one worker and five seconds per
prover attempt. The unrelated selected field-plan helper has its own accepted
proof record and does not discharge this namespace obligation.
