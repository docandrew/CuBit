# Hosted Field reset and reconciliation

This separate suite instantiates the actual AML_Namespace collecting Field_IO interface directly. It passed 42 checks in each strict release and assertion-enabled profile on the current Field implementation. It checks receipt persistence across Reset, stale-reference rejection and exact reconciliation without replay. The fixture is byte-identical to the accepted test; only its registration directory has moved.

Run `python3 tests/aml-field-reset/run.py --mode release` or `--mode checked` in the pinned Nix environment with the coordinated TMPDIR. The external Core.Values generic-instantiation compiler crash remains unresolved. These tests do not claim native hardware completion or full SPARK verification.
