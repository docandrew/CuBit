# Cached normal ACPICA Field stores

These are unchanged ASL inputs, compiled AML, disassembly, compiler/debugger
logs and selected observations from normal ACPICA 20260408 runs. Each group
contains 24 tables: Byte/Word/Dword or nine-byte AnyAcc geometry, three update
rules and both AML integer widths. Scalar tables execute three stores; object
tables execute Buffer and String stores. All twelve backing bytes are observed
after every store. The resulting 120 cases contain 1,440 byte observations.

The check script verifies raw-log byte observations, AML/disassembly hashes,
and the exact generated Ada golden array. Reference-manifest hashes pin every
retained file. It needs Bash, jq, awk and coreutils, with no private directories
or external executable fallback. Original tool paths identify historical tools;
the script never executes them. This is cached simulation evidence, not a fresh
ACPICA run, physical SystemIO testing, access-count or atomicity evidence.
