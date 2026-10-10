# ToBuffer cached ACPICA reference

`reference/` contains copied ASL, normal iasl AML output, raw compiler/runtime
logs, observations and provenance from four frozen ACPICA 20260408 probe runs.
`reference/manifest.json` hashes every bundled file and identifies each original
checkpoint. Historical absolute paths inside provenance are records, not runtime
dependencies. No network access or installed ACPICA executable is needed to replay.

After compiling `release.gpr` or `checked.gpr` in the repository's Nix shell:

```sh
python3 tests/aml-tobuffer/compare.py --mode release
python3 tests/aml-tobuffer/compare.py --mode checked
```

The driver checks 88 executable classifications per profile: 84 complete values
and four precise reference-source `Unsupported_Value` outcomes corresponding to
ACPICA `AE_AML_OPERAND_TYPE`. Two ordinary Package-source cases rejected by normal
iasl are recorded separately and never executed. Table hashes, lengths and
checksums are checked before invoking the runner; status and payload cardinality
must match, including the entire returned Buffer and its length.

The original observations retain two kinds of historical notes that must not be
silently turned into expectations: preliminary source predictions can disagree
with ACPICA (notably named Buffer target truncation), and ACPICA cache-allocation
messages also appear in the control runs. Expected values are the recorded
observations. Raw execute/control logs remain available for inspection.

These are focused cached comparisons, not a full ASLTS run, a proof of AML
semantics, or native ACPI-service validation. Runtime logs are written to a fresh
`results/` directory by default; `--runner` and a new `--output` directory may be
supplied explicitly for isolated hosted validation.
