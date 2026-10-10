# Hosted ToString checks

Run from the repository Nix shell:

```
python3 tests/acpi-hosted/run.py --group to-string --mode release
python3 tests/acpi-hosted/run.py --group to-string --mode checked
```

This explicit group runs the owner and collecting ToString fixtures plus shared ToBuffer and ToInteger regressions: 21,557 checks per profile in the frozen parent. The same project builds the opaque-service runner; the portable comparator verifies 72 complete cached observations and retains four normal compiler rejections separately. Original `all` selection is unchanged. Canonical worker 9335 passed this group in both strict profiles: 21,557 focused checks and 72 cached comparisons per profile, with four normal compiler rejections retained separately.

Release uses -gnatp; checked uses -gnata and -gnato. Each invocation creates disjoint logs. See REFERENCE.md for corrected DSDT width evidence, byte-safe observation wrappers, exact error mapping and source provenance. Reference entries are opaque evidence: the manifest-declared nested Python cache must be preserved, although it is never executed. Generated build outputs and top-level caches are not inputs.

The parent passed both strict profiles and ten generic-caller compile gates. Registration changes only test dispatch and this documentation; production and fixture bytes are unchanged. No proof, native deployment or full AML conformance is claimed.
