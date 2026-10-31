# Original ASLTS local (explicit hosted gate)

Run from the repository root in the pinned development environment:

```sh
TMPDIR=/home/doc/cubit-build-tmp python3 tests/aml-aslts-local/run.py --profile release --selection local-n64
```

Set TMPDIR before entering Nix too. Hosted-only opt-in; no default dispatcher/native/kernel build. The runner rejects optimized Python or an existing results directory. Preserve prior evidence before another run. Source/input/tool/compiler/ALI checks reject changes instead of silently repinning.

This is the full original inventoried local entrypoint, exact 18,930-byte DSDT and nine-file include closure. Its nested method declarations and argument preservation checks are unchanged. Upstream functional MDIRS omits local, and local.asl notes it was not introduced into the default suite: this gate applies upstream common n64 flags explicitly, not a default local build target. Flags are recorded in corpus.json (-oa -r2 plus common -of -cr -vs and listing outputs), with no force/error-code suppression. Normal reference compile/disassembly/runtime passed; three warnings,110remarks and four cache-allocation diagnostics remain in raw logs. Historical paths in logs are provenance, not execution dependencies.

Two same-arena MN00 and public Evaluate_Named ERRS calls must return zero. Read-only stored observations additionally require RMRC1 then2 and every old/new RP0P record to match :STST:functional:local:CMO0:PASS: in full length/bytes. Observation handles are released and retained counts restored; zero from a skipped test alone cannot pass. Tested private predecessor77410 passed with MN00 charges888/903 and module1046. Portable relocation passed release under the same normal limits (MN00/ERRS zero twice, charges888/903, RMRC1then2 and exact CMO0 records, retained cleanup). Two isolated guard negatives rejected optimized Python and changed input before compilation; main/runner/protection helpers/GPR are exact predecessor bytes. Evidence hashes are in reference/evidence.json.

Normal limits: method fuel1,000,000; module100,000; explicit128continuations; build180s, whole runtime30s,64MiBstack/1MiBlog,nice19/j1. No inherited bfield2m/180 exception, default changes, automatic retries or cap escalation. The original release n64 pass is retained. The explicit checked profile currently times out; see the current checkpoint below. No other widths/wholeASLTS claim. Module is separately compiler-blocked under plain and upstream flags; no generatedAML or interpreter failure inferred. Repeated invocations/nested methods are not new entrypoints.

## Current checked-profile checkpoint

The runner accepts --profile checked as well as release. checked.gpr enables assertions and overflow checks; the runner audits each non-binder ALI for the requested profile and warnings-as-errors. Both profiles keep the full original18930-byte AML, original zero/PASS/completion expectations, two invocations in one arena and retained-handle checks. The normal runtime limit is30s, build180s; no automatic retries or limit increases.

Current release passes: MN00/ERRS zero twice, charges888/903, RMRC1then2 and exact full CMO0 PASS records with retained cleanup. Current checked compiles successfully, then reaches Freeze and times out before module completion at30s. It is an incomplete diagnostic gate, not a passing assertion-enabled conformance result or a compiler crash. A separate private local-sized instance also times out at the same point while passing release; that unhelpful sizing change is not published. Production AML sources and original test assertions are unchanged. The previous64s release allowance is not inherited. ACPICA reference provenance is unchanged; no fresh oracle/native/proof result is claimed.
