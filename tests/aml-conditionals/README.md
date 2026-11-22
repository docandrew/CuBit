# Tail conditional regression checks

Run from the repository root:

```sh
tests/acpi-hosted/run.sh --group tail-conditionals --mode release
tests/acpi-hosted/run.sh --group tail-conditionals --mode checked
```

The portable group verifies cached evidence, then builds/runs Tail_Tests (232 checks) and Tail_Boundaries (50 checks) in the selected strict profile. It preserves existing group/all behavior. Tests exercise long tail If/Else chains, needed non-tail continuation frames, While restart/Break/Continue, predicate effects, empty selected bodies, fuel failure, exact charged counts and method-created Name read/cleanup.

Reference verification checks 22 normally compiled ACPICA tables and scalar outcomes against the exact embedded AML payload/argument/width/value metadata. The other18 generated boundary cases are synthetic tests, not ACPICA parity. Both Ada fixture source hashes are preserved, including the Name read witness. Raw compile/oracle logs and historical outcomes are retained: the final original fixture compilation failure is historical evidence, not one of the22 successful reference commands. The later corrected fixture is the tested source.

No ACPICA process, network, control runner, diagnostic trace, fuel override or source regeneration is used by this group. Existing test bodies use their original bounded budgets; global interpreter limits are unchanged. All verifier/build/test failures make the entrypoint nonzero. The canonical harness uses nice19, -j1, 64MiB stack and disjoint logs; its default per-main runtime timeout is180seconds. These are hosted regressions, not a full ASLTS pass, native validation or new proof.
