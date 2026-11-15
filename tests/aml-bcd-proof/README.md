# Integrated BCD conversion regressions

Run in the pinned Nix development environment with TMPDIR exported before Nix:

    export TMPDIR=/home/doc/cubit-build-tmp
    nix develop -c node tests/aml-bcd-proof/run.mjs

Uses actual AML_Execute and namespace/owned-arena quota/target callers, then the actual Core table/member initialization and evaluator against fresh ACPICA for both table integer widths. The shared Bytes operator visibility clause is the only adaptation to the existing executor/quota helpers. Inputs, compiler/reference tools and private artifacts are pinned or recorded. Builds are single-job, nice19, bounded, with separate outputs and no shared native build.

Expected current behavior:54 executor checks,49,368 quota/target checks;20 Core method observations matching16 ACPICA integer returns and4 numeric-overflow results. Each Core observation also requires zero retained-result debt. Core execution uses checks suppressed; the conversion unit has separate checked functional tests and SPARK proof evidence. This suite does not prove the whole Core/service or validate native IPC/hardware.

The six exact conversion/lemma source files were proved at SPARK levels1/2 (193 proof checks and15 flow checks per level, zero unproved or justified), with standard Interfaces shift models trusted. Native compiler/caller validation is separate from that source-level proof.
