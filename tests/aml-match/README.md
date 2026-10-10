# Hosted Match checks

From the repository Nix shell:

```
python3 tests/acpi-hosted/run.py --group match --mode release
python3 tests/acpi-hosted/run.py --group match --mode checked
```

The explicit match group uses tests/aml-match/{release,checked}.gpr. Its five executed fixtures are match_owner_tests, match_collecting_tests, match_core_tests, format_collecting_tests and format_capture_tests; match_runner serves the cached comparator. Existing all-group selection is unchanged. Integration passed 29,266 checks and 48 cached comparisons per strict profile; the relocated canonical group passed both strict profiles.

The comparator checks 48 complete executable observations and retains six normal compiler rejections separately. Exact package-limit, empty-buffer and operand-type errors, integer results and same-session MARK effects are compared. See REFERENCE.md for raw ACPICA evidence, controlling DSDT revisions, runtime width witnesses and method-specific opcode verification. Import every declared reference artifact unchanged.

Release uses -gnatp; checked uses -gnata and -gnato. Every dispatcher invocation creates disjoint logs. This checkpoint makes no completed Match proof, whole-interpreter correctness or native deployment claim. Stale authenticated Name_Member behavior is a lifetime-policy boundary, not established ACPICA parity.
