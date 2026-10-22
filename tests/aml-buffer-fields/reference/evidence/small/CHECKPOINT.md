# Small CreateField oracle

Worker39974 terminal0;38 normal attempts,26 returned,8 exact runtime errors,4 normal compiler syntax rejections (GEN1/QTYPE SizeOf(ToBuffer(...)) is invalid grammar). No forced compilation, timeout or missing result.34compiled cases verify DSDT revision1/2, runtimeWIDT and actual declaration opcode (ARG through HELP). Source41 hashes unchanged. Per subprocess5s/128MiBAS/64MiBstack/1MiBoutput; pinned tool paths/hashes/version in results/tools.json. No unsafe highword/HUGE/stoppeddiagnostic.

Both widths: Integer0x41/StringAB source coercion yields65/66. Generic1read raw01; QWordread raw8bytes4142434445464748. Type wrappers rejected, so these byte outputs alone do not prove read result kind. Method-returned PackageIndex source accepted65; Local-held RefOf rejected AE_AML_OPERAND_TYPE. Empty source gives AE_AML_BUFFER_LIMIT. Count emptyBuffer wins over invalid index/source with MARK123; invalid index/source gives AE_AML_OPERAND_TYPE with MARK12, which does not independently distinguish index from source when both same status.

Named source CopyObject rebind during index retains old65, MARK1. Local/Arg/CountLocal produce66/42, but CHNG uses Store through Arg reference, so raw byte alone cannot establish backing identity or delayed capture; needs retained old Index witness or source proof. DUP returns65/MARK1 with disassembly TEST.FLD0: shadows globalFLD0, not duplicate evidence.

CopyObject Integer and Buffer into field both update only backingbyte0 to59 (rest42434445464748), ObjectType14 retained. This is field write behavior, not metadata replacement.

Original observations unchanged; reviewed-observations.json classifies diagnostics and limits. No CuBit correctness/GC/native claims. Small corrected type/alias/duplicate probes remain pending separate source and approval; no batch replay required.
