# Hosted explicit Decimal/Hex conversion checks

Run from the repository Nix shell:

```
python3 tests/acpi-hosted/run.py --group explicit-format --mode release
python3 tests/acpi-hosted/run.py --group explicit-format --mode checked
```

This explicit group uses one coherent project for pure formatting, owner, capture and collecting fixtures, plus shared ToString/ToBuffer owner and collecting regressions and ToInteger regressions. The project also builds format_runner for 70 cached comparisons: 64 returned values and six exact operand-type errors, with no compiler rejections. The original all-group selection is unchanged.

Canonical dispatcher worker 95331 passed release and checked profiles: 54,656 checks across the nine fixtures and 70 exact cached comparisons per profile. The confirmed project mains are format_tests, format_owner_tests, format_capture_tests, format_collecting_tests, format_runner, to_string_owner_tests, to_string_collecting_tests, to_buffer_owner_tests, to_buffer_collecting_tests and tointeger_target_tests. There were no compiler-rejected oracle inputs in this inventory. Release uses -gnatp; checked uses -gnata and -gnato. Each run writes a fresh disjoint result directory. The portable comparison verifies full byte observations, status and MARK; REFERENCE.md documents controlling DSDT revisions, width witnesses and per-mode opcode evidence. Manifest-declared opaque artifacts must all be retained during import.

The pure formatter has hosted tests but no completed proof in this checkpoint. Hosted tests do not establish owner, evaluator or collector proof coverage. This registration makes no native deployment or whole-interpreter proof claim.
