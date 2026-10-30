# Hosted aml-field-multiple

Named provider catalog, confirmation history, binding sets and multiple Core replay. Checks split 71/11/118/1194.

Run `python3 tests/aml-field-multiple/run.py --mode release` or `--mode checked`
in the pinned Nix environment, with the required TMPDIR. Both profiles passed
1394 checks on the current production sources. `--validate-only` checks
registration and hashes without compilation.

The combined Field composition is recorded in
`../aml-field-io/MULTIDATUM-EVIDENCE.json`; receipt source scopes remain distinct.
Bounds remain 180 seconds/build, 30 seconds/runtime, 64 MiB stack and 1 MiB/log.
No physical hardware, native deployment or whole-interpreter proof is claimed.
