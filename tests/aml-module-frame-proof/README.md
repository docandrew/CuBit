# Bounded module-frame stack proof

Run bash tests/aml-module-frame-proof/run.sh in pinned Nix with TMPDIR set to
/home/doc/cubit-build-tmp before and inside the shell. --validate-only checks
cached reports and actual source/tool/script linkage without a prover. A normal
run reproduces levels1/2 in fresh private outputs with180s wall,5s solver,j1,
nice19,64MiB stack and core0 limits. No Core or native build is selected.

Four actual pure source files are used,without substitutes. Each level selects
12all-SPARK entities,proves21checks(including Empty,Push and Pop functional
postconditions)and10flow checks,with54Valid prover attempts. Skipped,non-SPARK,
unproved,stopped,empty or explicit-Assume evidence is refused. Sixteen report
audit cases are recorded. Full original reports and source snapshots are kept.

The three imported actual Frame_Handles helper interfaces remain modular
dependencies. The stack requires valid input and a separately authenticated
live/fresh owner frame. It does not issue identities or establish live lease,
namespace,AML,Core,service,hardware or table-load ownership correctness. No claim
of whole-library or native verification follows from this bounded proof.
