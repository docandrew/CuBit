# Original ASLTS logical-operator collection

This is the full unmodified functional/logic collection from the pinned ACPICA
ASLTS archive (SHA256 in sources.json), compiled normally in n32/revision1 and
n64/revision2 modes. Both original include closures have nine files, retained
under reference/source with their copyright/license headers. AML is not reduced,
rewritten, forced through compiler errors or compiled with diagnostic suppression.

```sh
export TMPDIR=/home/doc/cubit-build-tmp
nix develop --command bash tests/aml-aslts-logic/run.sh
```

The gate is explicitly release-only (-O1/-gnatp), with ALI switch audits. Each
original AML runs MN00 twice in the same CuBit arena. MN00 and stored ERRS must
return zero twice; the runner validates all nine then eighteen accumulated PASS
records by complete type/length/bytes and restores retained-result counts. Record
names and original expectations match fresh repair-disabled ACPICA20260408 runs.
A zero process exit or a skipped-test zero alone is insufficient.

Current original tables: n64 24,007bytes, n32 23,908bytes. Expected MN00 charges:
n64 51,929/52,064; n32 44,930/45,065 on the current source. Charges are recorded,
not used to weaken or replace the original functional assertions. Nine stored
records are groups LAN0/LN00/LOR0/LEQ0/LGR0/LGE0/LL00/LLE0/LNE0, not nine unique
opcodes or a full-suite coverage percentage.

Limits remain nice19/j1,180sbuild,30sruntime per table,64MiBstack,core0,1MiBlogs,
1,000,000method fuel,100,000module fuel and128continuations. Explicit hosted
namespace/method reserves and input capacity are inherited from the arithmetic
runner; these are test quotas, not ACPI limits. No service defaults are changed.
The runner still executes normal module admission and member initialization.

`--validate-only` checks172 source inputs/closed inventories and24 reference
files without execution. `--source-root DIR` selects a checkout. Sources and
cached oracle artifacts are pinned; no private workspace fallback. Outputs,
command statuses and pins are retained in unique TMPDIR directories. Reference
plans/commands/logs retain historical absolute paths as provenance, not runtime
dependencies. Default execution uses the cached immutable AML/reference data;
oracle regeneration requires normal original compilation, not edited tests.

ACPICA returns the expected zeros and PASS records in both widths. Its logs also
retain warnings/remarks and four outstanding cache allocations at shutdown;
process exit0 does not claim clean memory teardown. The original nine-file
source closure and raw compiler/disassembly/runtime logs are included.

No interpreter production change was needed. This is hosted independent
conformance for two configurations of one collection; it does not establish
checked behavior, other modes/collections, complete ASLTS/AML conformance, new
SPARK proof, native service execution or hardware validation.
