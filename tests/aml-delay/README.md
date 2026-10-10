# Hosted delay policy and evaluator checks

Run from repository root inside its Nix shell:

    python3 tests/acpi-hosted/run.py --group delay --mode release
    python3 tests/acpi-hosted/run.py --group delay --mode checked

Exact relocated delay_tests and delay_core_tests exercise bounded normalization, callbacks, errors and Core propagation. Deterministic providers do not establish elapsed-time correctness; native provider is explicitly unavailable. Historical embedded ACPICA cases require the separately recorded provenance; no new reference execution is implied. Canonical release and checked registration passed (worker82505).

## Pure delay policy milestone

    python3 tests/acpi-hosted/run.py --group delay-policy --mode release
    python3 tests/acpi-hosted/run.py --group delay-policy --mode checked

From the repository Nix shell, selecting only the actual policy unit:

    gnatprove -P tests/aml-delay/proof.gpr -u aml_delays.adb --mode=all --level=1 --timeout=5 --prover=cvc5,z3 -j1 --checks-as-errors=on --report=all

Set TMPDIR=/home/doc/cubit-build-tmp before entering nix develop; run one nice19 hosted worker with64MiB stack. The frozen policy source passed140 boundary checks per strictprofile and17/17 level1 obligations (13proof+4flow entries) before relocation. Canonical relocated release/checked each passed140checks; the documented portable proof project passed17/17 level1 obligations (worker24010). This proves exact normalization and unavailable-provider policy only, not actual timing, executor, scheduler, decoder body or wholeinterpreter correctness. No executable arithmetic change: new independent arithmetic postcondition and two redundant use-clause deletions.
