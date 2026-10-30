# IndexField results and stores beyond 64 bits

Run in the pinned development shell with TMPDIR=/home/doc/cubit-build-tmp:

    python3 tests/aml-index-wide/run.py --validate-only
    bash tests/aml-index-wide/check_reference.sh
    python3 tests/aml-index-wide/run.py --mode release
    python3 tests/aml-index-wide/run.py --mode checked

Both profiles pass 9,864 checks through the actual service, in five separately
bounded cases: baseline922, read32/read64 each2510, write32/write64 each1961.
The normally compiled
534-byte DSDTs supply 22 typed observations per AML revision, including five
ten-byte Buffer results. The AML bodies are identical. The reference checker
verifies query order, types, values, every Buffer byte, table extents/checksums
and exact embedded inputs.

The composed router now streams larger root IndexFields into an owned result
Buffer. It selects source bits across the full Buffer/String input, or zero-pads
an Integer/short source, instead of retaining only the first eight bytes. Scalar
child transfers stay 64-bit, and existing named-target authority remains required.
The result bound derives from AML_Objects.Max_Bytes (the existing 65,536-byte
object-store capacity); provider-request and work limits still apply independently.
The declared bound is not acceptance evidence for every payload size.

The fixture uses two explicitly bound Byte registers: index and data. It covers
73/80-bit fields, aligned/unaligned Byte, Word and QWord access through the smaller
data field, tail masking, two partial Preserve edges, Ones/Zeros, ten-byte Buffer
sources, Integer and short-Buffer zero-padding. Complete selector/data request
traces check every store, including source bytes beyond offset seven.

Policy tests reject an over-capacity result before I/O. A fresh serialized method
constructs a full-size named Buffer without copying it to a Local, checks its size,
and writes one explicit selector marker. Its following Field result allocation
must fail without further register requests. The method returns no retained result;
a later wide read succeeds after cleanup. Normal compiles have zero errors and
warnings, plus one retained remark2173 about the deliberate method-local named
constructor. No force or diagnostic suppression option is used. Original ACPICA
allocation diagnostics are retained; no clean-teardown claim.

The four fault cases inject denial before effect, known failed completion after
effect, and unknown completion before/after effect at every request in R080 and
WP80, including the final scalar read after Store. Across both AML revisions this
tests376 injected failures. Exact request prefixes and completed register values
are checked. No partial result handle may escape. Unknown completion blocks a
second invocation; unconfirmed/wrong receipts are rejected, exact reconciliation
does no I/O, and an explicit later invocation must succeed. The provider models
known effects and authoritative confirmation; these are trusted hosted assumptions.

An executable occupancy-map loop invariant in the private proof refactor caused
the checked baseline to grow to22seconds and the combined read case to time out.
An equivalent array aggregate removes its quadratic prefix rescans while retaining
the exact correspondence postcondition. The final checked baseline took1.46s;
all original cases pass without splitting or changing their30-second limits.
These are local regression timings, not controlled performance comparisons.

This is selected hosted functionality evidence, not a whole-interpreter SPARK proof, complete
Field support or native validation. Wide direct/banked fields, larger child data/
index fields, all size/width partitions, larger-payload fault prefixes, concurrency,
protocol attributes/GlobalLock, table lifetimes and native integration remain open.
The model uses a fixed data cell; it does not emulate physical indexed bank state.
See aml-index-large for bounded larger-payload checks and the prior bulk-copy
optimization. Full payload/width/transport coverage remains unfinished.

Build180s, runtime30s, ACPICA5s, j1/nice19, stack64MiB and captured log1MiB.
