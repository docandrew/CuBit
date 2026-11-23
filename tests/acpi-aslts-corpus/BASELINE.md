# Hosted ACPICA runtime baseline

Named base `d6b80c3f178c249e7d01d03687196cfefff518c3`, 2026-11-23T02:28:22.192388-07:00.

All **1,368/1,368 configurations completed (100%)**: **348 PASS (25.44%)**,32FAIL,130UNSUPPORTED,794BLOCKED,64TIMEOUT. These are342 runtime MAIN entrypoints ×four modes, not compiler-negative tests or all ACPI functionality.

| Mode | PASS | FAIL | UNSUPPORTED | BLOCKED | TIMEOUT |
|---|---:|---:|---:|---:|---:|
|n32|89|8|32|197|16|
|n64|85|8|34|199|16|
|o32|89|8|31|198|16|
|o64|85|8|33|200|16|

84/342 entrypoints pass all four modes. Blocks:700compiler,72upstream skipped/blocked,18incomplete original report records,4nonstandard entry/report contracts. Typed execution refusals retain last opcode context, not a proven causal-operator diagnosis; some argument mismatches need harness entry adapters.

Synthetic classification controls6/6PASS, excluded from these percentages.25transient launch errors were archived and repaired;1343other rows reused byte for byte. Full work list: `/home/doc/git/.acpi-proof-work/aslts-corpus-20261123-v1/CORPUS-RESULTS.csv`; aggregate `/home/doc/git/.acpi-proof-work/aslts-corpus-20261123-v1/census3/RESULT.json`; frozen receipts `/home/doc/git/.acpi-proof-work/aslts-corpus-20261123-v1/VALIDATION.json`. No production code edits, proof reruns, native or hardware execution.
