# Pinned ACPICA runtime corpus

This hosted census runs every 342 runtime `MAIN.asl` entrypoint in ACPICA
232ff3f8ae1a4da11c709f61d9154482cfe8e6df in n32, n64, o32 and o64 modes:
1,368 configurations. `upstream.json` pins all 1,430 ASLTS source inputs and
the original archive digest. Supply an already extracted, unmodified source
checkout; the runner verifies it and copies sources to disjoint mode trees.
It never edits the supplied upstream sources or original assertions.

From the repository's pinned Nix shell:

```sh
export TMPDIR=/home/doc/cubit-build-tmp
nix develop -c timeout --kill-after=5s 1500s bash tests/acpi-aslts-corpus/run.sh \
  /path/to/pinned/acpica-source /path/to/private/results --wall-seconds 1200
```

Run the same command to resume. Completed results are reused only when the
manifest, orchestration code, compiler binary, runner binary and runtime
wall limit match. `--rerun` explicitly replaces results; use a new output
directory to retain an earlier baseline. Build reuse verifies source pins and
binary digest. No production proofs are run by this hosted harness.

Each upstream Makefile is queried with GNU make for ASLTS_ASLFLAGS,
ADD_ASLFLAGS and ASLMOD/ASLMODADD. Commands follow upstream `compile_test_case`
semantics (Makefile.def lines255–270), with `-p` solely selecting a private
output prefix. Explicit upstream diagnostic suppressions and forced compile
flags are retained and recorded, never invented. Auxiliary source modules are
compiled but dynamic table loading still needs an admitted provider adapter.
Where the pinned source has no local Makefile, compile MAIN with the global
Makefile.def mode flags and no invented local overrides; record that contract
explicitly. Missing entry/report adapters are reported BLOCKED after compilation.

The actual ACPI_Service_Core stages and executes the table, initializes its
members and invokes MN00. PASS requires MN00=0, ERRS=0, positive RMRC, every
original RP0P status record reporting PASS, and no retained result handles.
This is a single invocation per configuration, not a leak or repeat-run proof.
FAIL records original assertion failures or a nonzero MN00 result. Original
SKIPPED/BLOCKED records and missing entry/report contracts produce BLOCKED,
never PASS. Compiler failures are BLOCKED with the tool command and available
diagnostic codes. Typed execution refusals are UNSUPPORTED, recording the
execution status and last passive opcode visit. That visit is diagnostic
context, not proof of the causal instruction. Budget exhaustion or wall limits
produce TIMEOUT. Harness crashes or malformed reports produce BLOCKED.

The passive hook does not allocate or reenter the service. Host delays and
finite event waits use real elapsed time; there are no asynchronous execution
threads. This baseline supplies no hardware address-space, async, exception
continuation or dynamic-table provider. An UNSUPPORTED result can therefore
expose an absent harness adapter as well as an interpreter feature gap; read
the typed execution status and retained log before prioritizing a fix.

Default limits are two workers, 12s runtime/case, 30s compiler/source, 15s
Makefile query, 1,200s batch wall time, 128MiB process stack and explicit
input-dependent service capacities (16MiB input ceiling). Capacities and
fuel are hosted admission limits, not service completeness claims. All
original source hashes, exact commands, AML hashes and logs are retained.
`RESULT.json` contains every required row, including NOT_RUN. The final
ASLTS line reports each outcome count, PASS/configurations completed, and
configurations completed/full1,368. A green selected subset is not full
corpus conformance.

`selftest.py --output ... --runner ... --iasl ...` runs six synthetic result
classification controls. They are stored separately and excluded from corpus
percentages. This harness makes no native, hardware, ACPICA performance parity
or whole-interpreter SPARK proof claim.
