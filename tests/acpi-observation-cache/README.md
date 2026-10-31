# Owned observation cache milestone

Run `bash tests/acpi-observation-cache/run.sh` in the pinned Nix environment,
setting TMPDIR=/home/doc/cubit-build-tmp before and inside Nix. The runner hashes
inputs/tools,uses disjoint outputs,audits ALI profiles and raw proof results.
`--validate-only` checks source pins without compiling or proving. Limits:
nice19/j1,64MiBstack,core0,build/proof180s,test30s,solver5s,logs1MiB.

ACPI_Observation_Values holds the actual owned lid and power-source sample
records,independent of interpreter state. Drivers retain named state aliases
and constants; both Bootstrap caches use these exact sample types. No handles,
borrowed descriptors or authority enter the records. Moving types out of the
Core context permits a small proof without substituting a replica data model.

The shared ACPI_Sample_Cache contracts specify initial state,exact invalidation,
publication priority and payload/time preservation,terminal nonwrapping counters,
normalized unavailable timestamps,and exact freshness/no-payload outcomes.
The cache's executable policy is unchanged. Ghost observations expose no runtime
payload access. Checked/release runtime checks cover both real record types,
stale/non-pending writes,absence/failure,clock boundaries and revision exhaustion:
31checks/type,62/profile. Generic callers still choose valid sample contents;
the lid/power consumers enforce their separate value semantics.

Level1/2 target the two actual record instantiations. The audit requires14proved
checks,16flow/termination checks,23SPARKentities,no explicit Assume/skip and
no external claim dependency. Solver goals and internal modular dependencies
are retained. These contracts prove the shared cache instance behavior,not the
owner adapters,notification fanout,AML interpreter,concurrency,clock source,
kernel/runtime IPC or native/hardware execution. One serialized owner and one
clock unit/epoch remain integration requirements. The full ACPI goal is open.
