# Owned copied-table storage tests

Run bash tests/aml-copied-tables/run.sh inside the pinned hosted Nix environment with TMPDIR=/home/doc/cubit-build-tmp set first. --validate-only checks source/fixture/runner hashes without compiling. Builds use fresh TMPDIR outputs, warnings-as-errors, checked gnata+gnato and release gnatp. Limits: build180s,runtime30s,stack64MiB,logs1MiB,nice19/j1.

729checks per AML width/profile,2916total. Covers copied bytes after source mutation, old tables after append/refusal, full-byte duplicates at capacity, zero/table/byte quotas, every insufficient first-admission fuel, wide malformed lengths/checksum/FACS/short headers, non-one bounds and exact/zeroed windows. A1,048,577-byte SDT framing case confirms no1MiB storage cap; it is not AML parse/admission evidence.

The freshly compiled62-byte SSDTs exactly match the existing normal ACPICA dynamic-load reference. That independent reference confirms module results/copy lifetime, with its known four-cache-allocation exit diagnostic. CuBit Load is not enabled by these tests. Scoped proof: tests/aml-copied-table-proof. Full byte semantics and interpreter/native integration remain unfinished.
