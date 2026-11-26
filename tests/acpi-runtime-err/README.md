# Hosted runtime _ERR regression

Run in the pinned Nix development shell with a fresh output directory:

```sh
bash tests/acpi-runtime-err/run.sh --root "$PWD" --output /tmp/cubit-err-regression-unique
```

The checked and release profiles run 508 contracts using 121 diagnostic AML tables. Both builds finish before timed cases. Tests cover default-off behavior, consumed invalid Unload operands, zero/nonzero and high-bit integer returns, retained status strings through actual collection, no-return/noninteger handlers, automatic recursion guarding, explicit recursion limits, low fuel and unavailable thread tags. Every run checks retained-result/copy-table cleanup and zero modeled hardware reads/writes.

This is a named hosted heap of64 objects,1024 payload bytes and64 elements, with copied-table capacity2/256. The tag1 provider is nonauthority fixture data; it is not a numeric OS thread-ID parity claim. Persistent namespace strings intentionally survive temporary-root cleanup. The collector test requires an increase in actual completed-collection and freed-byte counters while the retained error-name string remains equal.

Four raw synthetic negative/runtime contracts deliberately exercise malformed handler behavior. They are not compiler admissions, normal ACPICA parity claims or original ASLTS passes. The other synthetic inputs retain normal20260408 comparison observations. No original corpus admission, expectation, fuel5m, case wall12s or finite I/O model is changed. These regressions do not claim full AML completeness, table teardown, native boot or proof of the entire interpreter. Exact actual-component proof scope and inherited parent debt belong in the delivery manifest.

The source catalog pins every build input. Source drift, duplicate Ada basenames, failed output reuse, strict compiler errors and contract mismatches stop the entry point with a receipt. Only owned RAM data and isolated output files are used.

Consumed Release faults are also covered. The original set contributes 120 contracts
across the two strict profiles, retaining every original Unload case.
Eight additional compiler-admitted controls exercise an unowned mutex, zero
and nonzero handler returns, balanced and double release, handler recursion,
and acquisition within the handler's existing invocation domain. Seven have
normal ACPICA reference observations. The nested handler fault is a separate
guard contract. Twenty extra fuel/tag checks cover the Release entry.
These diagnostics do not add original ASLTS passes or prove the full interpreter.

The original 120 Unload/Release contracts remain unchanged. Another 48 normal
ACPICA diagnostic tables cover numeric and exact local faults, simple Store
parents, predicate branch/loop continuations, returns without objects, zero-
and nonzero-argument methods, and absent-result consumers. Each runs enabled
and disabled in both strict profiles. Disabled results also retain the prior
diagnostic baseline's exact AML charge. Two additional compiled guard tables
exercise recursive handler faults. One explicit refusal retains the error
for a nested indexed Store target whose side effect must fall between two
callbacks; it is not a claim that this reference behavior is implemented.

The actual recovery state/planner closes 56 checks after the latest fault-code
edit. The other 65 helper checks are reused unchanged. These counts do not prove
the inherited interpreter body. Original ASLTS pass credit belongs only to the separate full
original corpus receipts, never to these small diagnostic contracts.

The next 72 contracts add seven SizeOf-cell tables, eight authenticated package-
index-bound tables and three nearby type/buffer controls, enabled and disabled
in both profiles. Seventeen of these tables now match normal ACPICA observations.
The buffer bound uses its precise error; the wrong-kind source retains an explicit
refusal contract. These extensions preserve source membership.
The actual recovery unit was proved once after the latest fault-code edit: 56
checks, including 25 runtime-error checks, zero unproved. The other 65 helper checks are reused unchanged. Namespace and
executor adapters have a compositional argument; full interpreter AoRTE remains
unfinished. Diagnostic success is separate from original ASLTS corpus PASS.

The next 48 contracts add twelve normal string/buffer Index-bound tables,
enabled and disabled in both profiles. Actual authenticated object kind and
Byte_References.Out_Of_Bounds select string3011 or buffer300A; a consuming Store
may subsequently report3012. The earlier buffer-bound refusal is promoted to
its normal-reference behavior; wrong-kind sources and complex indexed Store
ordering remain explicit refusals. All prior fixture AML bytes are retained.
The actual edited recovery unit closes56checks/25runtimechecks with0unproved;
other65helper checks are reused unchanged. Full interpreter AoRTE is unfinished.

Sixteen further normal tables add fully consumed direct-local Increment and
Decrement failures, Store consumers, predicates and returns, repeated faults,
ObjectType after failure, initialized controls and integer wraparound. They run
enabled and disabled in both profiles. A failed update preserves the unset
local, reports3005, and may give a consuming Store its later3012 report.
Named, argument and complex reference update recovery remain separate work.
The121 actual helper proof checks are reused with exact source/report hashes;
the parent adds a guarded call and no new state or storage invariant. The full
interpreter proof remains unfinished.
