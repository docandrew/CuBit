# Source-tagged battery information selection

The actual same-Core selector tries canonical BIX first and calls BIF only for
Missing_BIX. One total budget is retained; malformed/evaluation/budget and
cleanup/contract/uncertain failures do not cause fallback or replay. This is an
owner library operation under an existing admitted evaluation context, not an
observer's arbitrary method endpoint.

Information is a source-discriminated record: no information, extended BIX,
or legacy BIF. Each available variant carries only its own format's fields.
The explicit Common function copies common units/technology/quantities/text and
retains source provenance; it cannot synthesize BIX revision/cycle/accuracy data
for BIF. Text capacity is the maximum of both configured format capacities, with
no truncation; unknown quantities have zero payload in the common projection.

Run `node tests/acpi-battery-selection/run.mjs` in the pinned Nix hosted
environment with TMPDIR=/home/doc/cubit-build-tmp. ACPI_CORE_ROOT may name a
coherent private Core tree; ACPI_RESULT_DIR may select an existing private output
directory. Actual interpreter code generation is release-only. Normal iASL and
acpiexec run both AML widths, repairs disabled; `node reference.mjs ABS_RESULT_DIR`
checks both complete typed BIX/BIF packages. Commands are bounded/private.

Tests cover source preference, missing-only selection, missing/foreign context,
malformed data without fallback, all legacy budget boundaries, one evaluation,
separate source variants, common projection and unequal text capacities. Explicit
callbacks model overcharge and field uncertainty; actual Core performs the other
evaluations and retained-result cleanup. ACPICA is only an oracle for the valid
literal packages, not for selector policy, callbacks or ownership. Raw cache
allocation diagnostics are preserved.

No new SPARK proof, native owner/cache/CCL migration, Main activation, kernel/runtime
change or hardware validation is established here. The agreed next integration
must version the raw observer pages without a compatibility reader and expose
the typed information-source field publicly, keeping other CCL fields unchanged.
