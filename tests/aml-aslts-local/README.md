# Original ASLTS local (explicit hosted gate)

Run from the repository root in the pinned development environment:

```sh
TMPDIR=/home/doc/cubit-build-tmp python3 tests/aml-aslts-local/run.py --profile release --selection local-n64
```

Set TMPDIR before entering Nix too. Hosted-only opt-in; no default dispatcher/native/kernel build. The runner rejects optimized Python or an existing results directory. Preserve prior evidence before another run. Source/input/tool/compiler/ALI checks reject changes instead of silently repinning.

This is the full original inventoried local entrypoint, exact 18,930-byte DSDT and nine-file include closure. Its nested method declarations and argument preservation checks are unchanged. Upstream functional MDIRS omits local, and local.asl notes it was not introduced into the default suite: this gate applies upstream common n64 flags explicitly, not a default local build target. Flags are recorded in corpus.json (-oa -r2 plus common -of -cr -vs and listing outputs), with no force/error-code suppression. Normal reference compile/disassembly/runtime passed; three warnings,110remarks and four cache-allocation diagnostics remain in raw logs. Historical paths in logs are provenance, not execution dependencies.

Two same-arena MN00 and public Evaluate_Named ERRS calls must return zero. Read-only stored observations additionally require RMRC1 then2 and every old/new RP0P record to match :STST:functional:local:CMO0:PASS: in full length/bytes. Observation handles are released and retained counts restored; zero from a skipped test alone cannot pass. Tested private predecessor77410 passed with MN00 charges888/903 and module1046. Portable relocation passed release under the same normal limits (MN00/ERRS zero twice, charges888/903, RMRC1then2 and exact CMO0 records, retained cleanup). Two isolated guard negatives rejected optimized Python and changed input before compilation; main/runner/protection helpers/GPR are exact predecessor bytes. Evidence hashes are in reference/evidence.json.

Normal limits: method fuel1,000,000; module100,000; explicit128continuations; build180s, whole runtime64s,64MiBstack/1MiBlog,nice19/j1. No inherited bfield2m/180 exception, default changes, automatic retries or cap escalation. Only release/explicit n64 configuration covered; no checked/other widths/wholeASLTS claim. Module is separately compiler-blocked under plain and upstream flags; no generatedAML or interpreter failure inferred. Repeated invocations/nested methods are not new entrypoints.
