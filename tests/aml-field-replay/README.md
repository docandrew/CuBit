# Cached ACPICA whole-register replay

Run `python3 tests/aml-field-replay/run.py --mode release` or `--mode checked` in the pinned Nix environment with the coordinated TMPDIR.

This suite runs six immutable ACPICA tables for 8-, 16- and 32-bit fields at both AML integer widths through the actual Core, hosted provider and named-register backend. It checks the 60 captured reference observations and additional interpreter/provider assertions, totaling 522 checks per profile. Reference bytes and provenance are retained locally; the runner does not depend on a private work directory.

ACPICA simulator results establish the observed AML behavior. Provider-call counts are local assertions and are not independent hardware bus traces. These tests do not establish native deployment, hardware compatibility or full SPARK verification.
