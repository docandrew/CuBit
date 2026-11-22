# Mid ordering — final bounded oracle addendum

Worker23086 TERMINAL0. Eight normal -oa compilations and executions (four cases,both widths), each5s/128MiBAS/64MiB stack; no forced AML, no production edits. Every execution semantically fails as deliberately requested; process exit0 is not success of AML. Each raw failure trace explicitly identifies Method TEST, Opcode009E Mid, with actual operand expression. All eight return SEEN marker10111, so source1+index10+count100+dynamic Index target10000 effects completed before conversion failure. No test relies on constant folding or raw opcode byte search.

* Invalid source Package / valid index,count / dynamic target => AE_AML_OPERAND_TYPE.
* Valid source / invalid index Package / valid count / dynamic target => AE_AML_OPERAND_TYPE.
* Source and index Package / count emptyBuffer / dynamic target => AE_AML_BUFFER_LIMIT, establishing count error wins before index/source type errors.
* Valid source / index emptyBuffer / count Package / dynamic target => AE_AML_OPERAND_TYPE, again count conversion precedes index conversion.

All observations agree at32/64 bits. Original logs preserve ACPICA cache-allocation warnings; no upstream leak-freedom claim.

## Implementation ordering/API refinement

Evaluate source,index,count,target expressions once in AML order, retaining/rooting their Datum results and earlier effects. After target expression is captured, resolve/coerce count first, then index, then source, matching primary exresop reverse runtime-argument resolution. Do not eagerly coerce saved index before count or target executes. Propagate Empty_Buffer distinctly, not Unsupported_Value. Target descriptor validation performed by evaluation must still precede these final conversions; no authority expansion. This batch establishes target effects before conversion and count-before-index; index-before-source follows inspected primary resolver and needs a corresponding focused hosted contract test, not a claimed independent oracle discriminator here.

Namespace callback remains Mid_And_Attach(Context,Width,authenticated primitive Item,normalized Start,Count,typed Destination,Result_Value,Cell_Value,Status). Required callback placement after To_Buffer_And_Attach. The evaluator resolves expression-result reference provenance once before primitive coercion; held Local references reject, as earlier oracle. Shared AML_Slices.Select_Range safely clamps already-normalized values. Preserve independent Middle/Middle_Slot stage roots and countRight dynamic-target resume. Owner allocates fresh result, uses direct implicit target policy/Arg-indirect replacement, atomically rolls back only its own unpublished allocation on failure. No ToBuffer same-cell or explicit-target shortcut. Earlier effect markers remain on failure.

Read together with frozen aml-mid-oracle-xqon789m/REPORT.md. No implementation authorized by this artifact alone; root reviews exact policy first.
