# ACPICA runtime corpus baseline

All **1,368 configurations completed in each lane (100%)** on the pinned342
runtime entrypoints ×four modes. No compiler-negative exclusions were made.

| Lane | PASS | FAIL | UNSUPPORTED | BLOCKED | TIMEOUT | Passing |
|---|---:|---:|---:|---:|---:|---:|
| Strict upstream configuration |402|32|184|688|62|**29.386%**|
| Guarded6141expected diagnostic |530|56|352|336|94|**38.743%**|

Strict uses upstream install common flags and original collection settings.
The pinned include graph maps225unconfigured leaves into existing aggregates;
152configuration compiles required emitted upstream expected diagnostics.
The6141lane performs a strict preflight and retries only the specific lone
Device-without_HID/ADR lint, preserving every other diagnostic. Its384guarded
compile retries include auxiliary modules. Every original source/assertion
is unchanged. The strict line is never replaced by the compatibility line.

97/342entrypoints pass every mode strictly;129/342do so in compatibility.
Strict compiler blocks remain576; compatibility compiler blocks196. Other
blocks are original skipped/blocked reports or missing harness report/entry
adapters. Typed execution refusals are not all interpreter defects: providers
and exception/async/entry adapters are still absent. Last opcode is passive
context, not proven causality. Timeouts are work items, not justified limit
increases. No ACPICA performance parity is claimed.

Strict runtime results:680reused by identical source/AML/runner hashes and
wall limit,112freshly executed. Compatibility:1,132reused,40fresh. Explicit
reuse pins the original case receipt and log; it is never called a fresh run.
Older retained baselines:348/1,368(25.439%) before upstream install flags;
354strict/478compatibility before aggregate settings. No proof reruns,
production edits or native/hardware execution occurred for this harness.

Full paired work list and immutable receipt hashes are in the packet's
CORPUS-LANES.csv and LANES-VALIDATION.json receipts. Runtime source README
places compiler-negative cases under src/compilation and states runtime
collections should compile and execute; names alone never remove a case.
