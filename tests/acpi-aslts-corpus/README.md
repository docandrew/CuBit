# Pinned ACPICA runtime corpus

This hosted census covers342runtime `MAIN.asl` entrypoints in ACPICA
232ff3f8ae1a4da11c709f61d9154482cfe8e6df, each in n32/n64/o32/o64:
1,368configurations. `upstream.json` pins all1,430ASLTS inputs. Supply the
unmodified extracted tree; every source is verified. There is no download.

Export TMPDIR before entering the pinned Nix development shell:

```sh
export TMPDIR=/home/doc/cubit-build-tmp
nix develop -c timeout --kill-after=5s 1500s bash tests/acpi-aslts-corpus/run.sh \
  /path/to/pinned/acpica /path/to/results/strict --wall-seconds 1200
```

The default strict lane uses upstream install/test-sequence common flags
`-of -cr -vs`, four mode flags, and each collection's original Makefile flags.
Where a standalone leaf has no Makefile, a pinned static include graph maps
its own DECL.asl and RUN.asl into a configured aggregate. Only expected
compiler diagnostics already declared by that aggregate **and actually
emitted by this exact strict preflight** are applied. This avoids demanding
absent diagnostics from a leaf split out of a larger aggregate. The map
retains Makefile hashes and include evidence; unmapped leaves gain no flags.
Generated AML header revision must match the32/64mode.

Upstream's own install target with IASL20260408 rejects some namespace
fixtures with6141: Device lacks _HID/_ADR. A separately labelled compatibility
lane, authorized in coordinator103000, retries only when the current strict
compile's sole blocking error is precisely that Device6141lint. It adds
`-vx6141` (an **expected diagnostic**, not blanket error suppression). Every
other diagnostic remains effective; the strict result is retained separately.

```sh
nix develop -c timeout --kill-after=5s 1500s bash tests/acpi-aslts-corpus/run.sh \
  /path/to/pinned/acpica /path/to/results/compat --compat-6141 \
  --source-cache /path/to/results/strict/source \
  --runner /path/to/results/strict/build/bin/aslts_corpus_runner \
  --reuse-runtime /path/to/results/strict/RESULT.json --wall-seconds 1200
```

The shared source cache is read-only after initial creation and hash-checked.
Keeping its absolute paths stable also preserves `__FILE__` expansion. Runtime
reuse is explicit: source, AML and runner hashes plus timeout must match, and
the previous case receipt/log is retained and pinned. It is reported as reuse,
not a fresh execution. Default repeated commands resume completed case
receipts only when their full input key matches. `--rerun` forces results;
use a new output directory to retain an earlier baseline.

PASS requires actual Core MN00=0,ERRS=0,positive RMRC,every original RP0P
record PASS,and no retained result handles. FAIL records assertion errors or
nonzero MN00. Skipped/blocked original records, incomplete reports, missing
entry adapters and compiler errors are BLOCKED. Typed execution refusals are
UNSUPPORTED with execution status and last passive opcode/extension/position;
that visit is context, not proof of the causal instruction. Argument mismatch
can indicate a missing nonstandard entry adapter. Fuel/wall limits are
TIMEOUT. Harness crashes cannot pass.

Each case retains commands, logs, source/AML hashes, strict preflights,
expected-diagnostic evidence and any reused runtime receipt. `RESULT.json`
contains every required row, including NOT_RUN; its summary reports status
counts, PASS/completed,and completion/full1,368. Always publish **both strict
and compatibility corpus lines**. No compiler-negative exclusions are inferred
from names: upstream src/README puts those tests under src/compilation and
states that runtime collections should compile and execute. The runtime grid
keeps all342entrypoints unless their own metadata establishes another role.

Limits: two workers,12s runtime/case,30s compiler invocation,15s make query,
1,200s batch,24GiB address space,128MiB stack,16MiB table input ceiling,
explicit hosted capacities. Runtime commands (including selftests) install
these address-space/stack limits explicitly with `prlimit`.
The runner validates the complete input as one DSDT or a concatenation of
DSDT/SSDT tables before staging any table. It rejects malformed lengths,
checksums, trailing bytes, other signatures, and a DSDT after the first table.
Each original table is staged as its exact slice in file order. An SSDT-only
input receives a separate empty revision-2 DSDT, matching acpiexec's default
64-bit policy; the supplied AML and compiler/source hashes stay unchanged.
A DSDT supplied first retains its own revision and integer width. The catalog
has an explicit 32-table ceiling including a synthetic DSDT, while the existing
16-MiB input and value/fuel/time limits stay in effect. The adapter handles
functional/external's four-table output; it installs no field handlers and
adds no exception-continuation or dynamic-load support.

The single-threaded runner uses real host delays and finite waits, with no
hardware address-space, async, exception-continuation or dynamic-load adapter.
This is release hosted coverage, not native/hardware integration, whole-service
SPARK proof, leak freedom, or ACPICA performance parity. No proofs are run;
accepted unchanged packages retain their receipts. Twenty-two separate synthetic
classification/bundle controls are excluded from corpus percentages.
