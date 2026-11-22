# LoadTable source conversions

Inside pinned Nix, export TMPDIR=/home/doc/cubit-build-tmp before entering. Run:

```
bash tests/aml-loadtable-coercions/run.sh
bash tests/aml-loadtable-coercions/run.sh --mode checked
bash tests/aml-loadtable-coercions/reference.sh /absolute/output/from/run.sh
```

The default release profile uses -gnatp for functional testing. Checked uses
-gnata/-gnato. Each runs431checks in26fresh processes at both AML widths,
including zero-budget retry, pre-effect refusal, definition consumption, module
values, empty Buffer wildcard/path operands, Integer OEM-table-ID conversion,
and conversion expansion boundaries. Build180s/runtime30s, stack64MiB, nice19/j1,
core0, logs1MiB. Source/tool hashes are checked around every command; ALI flags
and executable hashes are recorded. --validate-only performs no build or oracle.

The independent normal ACPICA runner asserts16common admission/value/effect
controls. It also records four short OEM-ID String/Buffer observations separately.
Our unchanged deterministic zero-padded matching can select a table for which
ACPICA reports no match. These observations are not counted as conversion
conformance. The error status taxonomy is not claimed identical.

The new conversion views do not allocate namespace objects, modify their sources,
or change ParameterData, table reservation, hardware authority or transport.
This is hosted execution, not native/hardware validation or a new SPARK proof.
Full AML/LoadTable lifetime, readiness/provider semantics, joined interpreter
proofs and native device integration remain unfinished.
