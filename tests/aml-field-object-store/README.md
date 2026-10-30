# Hosted aml-field-object-store

Raw Integer, Buffer, String and empty-source stores through named Field bindings, compared against retained ACPICA observations.

Run `python3 tests/aml-field-object-store/run.py --mode release` or `--mode checked`
in the pinned Nix environment, with the required TMPDIR. Both profiles passed
816 checks on the current production sources. `--validate-only` checks
registration and hashes without compilation.

The combined Field composition is recorded in
`../aml-field-io/MULTIDATUM-EVIDENCE.json`; receipt source scopes remain distinct.
Bounds remain 180 seconds/build, 30 seconds/runtime, 64 MiB stack and 1 MiB/log.
No physical hardware, native deployment or whole-interpreter proof is claimed.
