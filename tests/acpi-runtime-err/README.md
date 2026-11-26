# Hosted runtime _ERR regression

Run in the pinned Nix development shell with a fresh output directory:

```sh
bash tests/acpi-runtime-err/run.sh --root "$PWD" --output /tmp/cubit-err-regression-unique
```

The checked and release profiles run 120 contracts using 24 small diagnostic AML tables. Both builds finish before timed cases. Tests cover default-off behavior, consumed invalid Unload operands, zero/nonzero and high-bit integer returns, retained status strings through actual collection, no-return/noninteger handlers, automatic recursion guarding, explicit recursion limits, low fuel and unavailable thread tags. Every run checks retained-result/copy-table cleanup and zero modeled hardware reads/writes.

This is a named hosted heap of64 objects,1024 payload bytes and64 elements, with copied-table capacity2/256. The tag1 provider is nonauthority fixture data; it is not a numeric OS thread-ID parity claim. Persistent namespace strings intentionally survive temporary-root cleanup. The collector test requires an increase in actual completed-collection and freed-byte counters while the retained error-name string remains equal.

Four raw synthetic negative/runtime contracts deliberately exercise malformed handler behavior. They are not compiler admissions, normal ACPICA parity claims or original ASLTS passes. The other synthetic inputs retain normal20260408 comparison observations. No original corpus admission, expectation, fuel5m, case wall12s or finite I/O model is changed. These regressions do not claim full AML completeness, table teardown, native boot or proof of the entire interpreter. Exact actual-component proof scope and inherited parent debt belong in the delivery manifest.

The source catalog pins every build input. Source drift, duplicate Ada basenames, failed output reuse, strict compiler errors and contract mismatches stop the entry point with a receipt. Only owned RAM data and isolated output files are used.

Consumed Release faults are also covered. The public entry executes 120 cases
across the two strict compiler profiles, retaining every original Unload case.
Eight additional compiler-admitted controls exercise an unowned mutex, zero
and nonzero handler returns, balanced and double release, handler recursion,
and acquisition within the handler's existing invocation domain. Seven have
normal ACPICA reference observations. The nested handler fault is a separate
guard contract. Twenty extra fuel/tag checks cover the Release entry.
These diagnostics do not add original ASLTS passes or prove the full interpreter.
