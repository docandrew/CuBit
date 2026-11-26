# Explicit copied-table owner conformance profile

This separate hosted profile declares two copied tables,256 copied bytes and Load slots equal to catalog tables plus two. The existing runner and core zero defaults are unchanged. It uses the finite-memory-io-v1 model:65536 bytes per Memory/IO space,128 geometry handles, no real hardware, async support, exception continuation or Unload support.

The observed24 Load cases retain at most one table/88bytes; one table/168bytes are headroom. Early stops and timeouts leave this a lower-bound sizing experiment. Do not claim complete capacity sufficiency or production boot sizing. Cases keep their original assertion/classification rules and first failures.

```
export TMPDIR=/home/doc/cubit-build-tmp
nix develop --command bash tests/acpi-aslts-copied-conformance/run.sh /path/to/pinned/acpica /path/to/new-owner-results
```

Use a new output directory per policy. The unchanged upstream run.py controls compiler admissions, original12s case limits,5m AML fuel,24GiB address space and128MiB stack. Its documented compatibility and source-cache options remain available. Runner/compiler overrides are rejected to preserve this lane's identity. Publish strict and compatibility results separately; positive complete original reports alone count as PASS.

The builder verifies the closed source/model/policy inputs and pinned tools, materializes an independent source snapshot and uses the original O1/gnatwa/gnatwe/gnatp profile. Cache keys include the complete source catalog and tools. Failed/incomplete builds remain available for inspection. No proof is rerun and no native execution is performed.
