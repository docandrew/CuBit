# Full original ASLTS provoke (opt-in hosted gate)

Run from the repository root with TMPDIR set before and inside the pinned Nix development shell:
`python3 tests/aml-aslts-provoke/run.py --profile release --selection provoke-n64`

The full original eight-file collection and exact18,714-byte revision2AML are preserved. Exact upstream common n64 flags: -oa -r2 -of -cr -vs plus -l -sc -sa -ic -ta -ts -so -lm -ln -ls -li. No force/suppression/source reduction. Provoke is in upstream complex default target list; only this explicit n64 configuration is covered.

IMPORTANT: normal ACPICA fails m133/m134. Its process0 is not a semanticpass: MN00/ERRS1/4 then1/8, operand-type errors and cache diagnostics preserved. Original assertions expect caller-reference decrement to9, supported by ACPI6.6 Arg read/write rules and Decrement equivalence. The reviewed CuBit Arg-only predecessor passed original expectations (worker59738), NOT differentialcompatibility. Bothwidth focused1006/profile also passed separately. The portable Arg+Notify composition passed its release gate in worker80224 with original MN00/ERRS zero twice and full RMRC5/10 records. Combined strict release/checked regression checks also passed; three release results are explicitly carried from worker82760. See reference/combined-evidence.json.

Gate requires MN00/ERRS0 twice plus read-only RMRC5then10 and all full original PASS records in order m130,m132,m133,m134,m136. Child/parent retained handles are released. No skippedtestzero acceptance. Normal1m method/100kmodule,128continuations,180sbuild/64sruntime,64MiBstack1MiBlog,nice19j1. Guarded source/tool/compiler/ALI checks and fresh result directory; no automatic retry/limits escalation. Hosted only; no kernel/native/sharedapply. No wholecorpus/otherconfiguration claim.

Remaining broader Arg contexts, explicit reference-to-Arg updates and recursive reference chains are outside this narrow correction. Earlier incompatible reference observations are retained, not rewritten. See reference/evidence.json.
