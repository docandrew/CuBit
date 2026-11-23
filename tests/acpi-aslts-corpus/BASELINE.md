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

The following compound-store cleanup preserves both complete corpus lines above:
named string/buffer payloads and BufferField reads/writes operate in the current
arena, without full-arena clone/copyback on those paths. All 1,172 runnable
configurations were freshly executed again; every one of the 2,736 lane statuses
is identical to the preceding direct-integer/byte-store baseline.

The field regression group passes 11/11 mains per profile (22/22,100%), with
60,167 assertions/profile. Test upkeep repairs five unconstrained-type
declarations in three mains and forces real quota retries in legacy liveness fixtures.
The historically named rollback test explicitly changes semantics under the
user's 032000 decision: it checks a valid partial state and a visible earlier
field write, rather than whole-store rollback. Four pinned ACPICA references
record the corresponding missing-member behavior. Synthetic CuBit quota failure
is separate from ACPICA malloc-failure behavior. All original failures and
control comparisons are retained in packet receipts.

A busy-host diagnostic pair keeps 0062 passing in 1.1674s versus acpiexec0.2166s;
bfield still times out at12s, and indexed decrement takes11.6387s versus0.1663s.
These samples do not establish parity. The remaining bfield sample is dominated
by runtime buffer completion/conversion/definition arena copies; package
replacement/refcount integration and broader copy retirement remain pending.
Unchanged proofs are reused; the known namespace Global frontend crash still
prevents an accepted new namespace proof. Native SDK checks remain semantic only.
