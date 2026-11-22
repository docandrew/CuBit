# Hosted aml-field-bindings

Explicit binding-set ownership, identity, geometry and capacity. This fixture and its support files exactly duplicate the 118 cases inside the multiple suite; the aggregate does not count them twice.

Run `python3 tests/aml-field-bindings/run.py --mode release` or `--mode checked`
in the pinned Nix environment, with the required TMPDIR. Both profiles passed
118 checks on the current production sources. `--validate-only` checks
registration and hashes without compilation.

The combined Field composition is recorded in
`../aml-field-io/MULTIDATUM-EVIDENCE.json`; receipt source scopes remain distinct.
Bounds remain 180 seconds/build, 30 seconds/runtime, 64 MiB stack and 1 MiB/log.
No physical hardware, native deployment or whole-interpreter proof is claimed.
