# ACPICA runtime corpus baseline

All **1,368 configurations completed in each lane (100%)** on the pinned
342 runtime entrypoints across four modes. Original sources and assertions
remain unchanged; no compiler-negative exclusions were made.

| Lane | PASS | FAIL | UNSUPPORTED | BLOCKED | TIMEOUT | Passing |
|---|---:|---:|---:|---:|---:|---:|
| Strict upstream configuration |408|32|184|688|56|**29.825%**|
| Guarded 6141 expected diagnostic |536|56|352|336|88|**39.181%**|

Direct integer and byte stores remove full-arena candidates from the primitive
reference-store path. Each lane gains six passes from former timeouts: all four
0062_ASL_RUNTIME configurations and logic n32/n64. Every previously passing
configuration still passes. The immediately preceding baseline was 402/1,368
(29.386%) strict and 530/1,368 (38.743%) compatibility.

99/342 entrypoints pass every mode strictly; 131/342 do so in compatibility.
Strict compiler blocks remain 576; compatibility compiler blocks remain 196.
The compiler policy is unchanged: upstream install flags, original collection
settings, pinned aggregate include evidence, and a strict preflight before the
specific lone Device-without-HID/ADR diagnostic compatibility retry.

The candidate binary freshly executes all 1,172 runnable source/mode/AML
configurations. An identical fresh runtime receipt can serve both lanes.
Compiler-only blocks and compiler outputs reuse their exact guarded prior
receipts. No old-binary runtime result is called a fresh candidate execution.
Full case/log hashes, baseline comparisons and zero passing-case regressions
are recorded in the packet's CORPUS-VALIDATION.json receipt.

0062 n64 now passes in 1.0163 seconds, compared with the prior 12-second timeout
and acpiexec's 0.2191 seconds on the identical AML. The speedup is at least
11.83 times; performance parity is not established. CPU pinning and host load
are recorded, without a host-wide quiet guarantee.

Both checked and release direct-store regression profiles pass 180 assertions.
Unchanged accepted proofs remain reused. Edited namespace proof attempts stop
in SPARK Global-contract generation with recorded compiler errors; no new
namespace or whole-service AoRTE proof is claimed. Native SDK checks are semantic
checks only, with no native objects or hardware execution.

Typed refusals and blocks include missing providers and harness adapters; they
are not all interpreter defects. Last opcode is passive context, not proven
causality. Timeouts remain work items without raised limits. Older retained
baselines include 348/1,368 (25.439%) before upstream install flags and
354 strict/478 compatibility before aggregate settings.
