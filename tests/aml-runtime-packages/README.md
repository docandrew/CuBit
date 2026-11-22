# Runtime package and continuation regression registration

Portable Linux-hosted regression registration. Run in the repository Nix shell:

    TMPDIR=/home/doc/cubit-build-tmp nice -n 19 nix develop -c python3 tests/aml-runtime-packages/run.py --validate-only
    TMPDIR=/home/doc/cubit-build-tmp nice -n 19 nix develop -c python3 tests/aml-runtime-packages/run.py --mode release
    TMPDIR=/home/doc/cubit-build-tmp nice -n 19 nix develop -c python3 tests/aml-runtime-packages/run.py --mode checked

The default selects all 25 closures: seven runtime package mains, continuation capacity, 14 runtime adapter mains and three compile-only adapters. An explicit `--selection` accepts unique exact labels and records the subset. Capacity runs all four capacity/width partitions in each profile; no case is omitted. Bounds remain 180 seconds per build, 30 seconds per invocation, 64 MiB stack, one compiler job and 1 MiB captured log. No native code or oracle regeneration.

The private package implementation passed 2,182 checks per profile across minimal, ordering, actual GC, replacement-name and excess suites, plus 584 boundary/failure checks per profile. These precede this portable registration and use separately pinned source closures. The separate broad adapter gate passed 28 runtime executions and six compile-only stages across both profiles (including guarded carried jobs); its final source includes the two existing Test_Namespace dependency specs. The current service smoke passed 106 checks/profile. These private gates do not constitute a fresh run of all 25 portable closures. The prior continuation fixture passed 528 per profile on its earlier capacity implementation; this is historical evidence, not a current composed rerun. Current full ASLTS control progressed past nested package construction and exhausted its 1,000,000 method-fuel limit; it has not passed. The full AML corpus remains unrun.

Normal ACPICA inputs/logs are retained in each fixture's reference directory, including warnings. Constructed malformed boundary bytes are not oracle results. Counted collection and retained-byte tests do not prove total heap safety. Builder protocol proof, complete runtime package semantics, native stack suitability and full interpreter proof remain open. Default continuation capacity remains 64; capacity tests explicitly configure 128 independently of lexical scope depth.

## Portable entrypoint validation

The portable release smoke passed the nested-buffer runtime main (48 checks) and the compile-only reference adapter. The current composed release capacity run passed all four partitions (129 + 129 + 135 + 135 = 528 checks). The checked default32/default64 partitions passed 129 checks each; checked large32 reached the unchanged 30-second host limit, and large64 was not run. This is retained as an incomplete checked result, not a pass or a request to increase limits. Eighteen lightweight source/selection/dispatcher guard checks passed. The complete 25-closure portable selection has not been rerun as one gate.

A later controlled 10,000,000-fuel full control run reached its 64-second host limit without an MN00 result. This remains incomplete ASLTS evidence and does not change the portable regression limits.
