# Owner-admitted orphan EC/GPIO _REG notices

Inside the pinned Nix development shell, with TMPDIR exported before Nix:

    bash tests/acpi-orphan-reg/run.sh --output-root <private-directory>

Node.js must be available (NODE_BIN selects its executable); ACPICA_BIN selects
the directory containing iasl/acpiexec. The runner copies/hashes an explicit
manifest to private storage, builds checked (-gnata/-gnato) and release (-gnatp),
and records tools, commands, statuses, logs, source hashes and binary hashes.
One compiler job,180s build,30s runtime,5s ACPICA,64MiB stack,no core dumps,
1MiB captured logs. Invoke nice -n19. No native/kernel/image/QEMU build/run.

The actual Core and ACPI_Region_Initialization.Orphans execute19scenarios per AML
revision (1/2),886checks/profile. Explicit provider admission binds a current
Device to EC or GPIO space. Metadata alone never provides readiness or authority.
Local _REG aliases/alias chains and absent methods are covered; ancestor methods
are not inherited. Other region spaces permit orphan notices, while a matching
direct EC/GPIO region defers to the ordinary region coordinator.

Provider readiness is a hosted model, rechecked before and after invocation.
Unavailable/lost providers, foreign device/disconnect/cleanup contexts, zero
budget, bad arity/nonmethod aliases, overcharge, missing handles, unknown Field
completion and copied Fatal are tested. Failed compound-result cleanup retains
the root for authenticated release-only retry; no operation replay occurs.
Callbacks never perform real EC/GPIO operations. The deferred metadata case uses
legacy declaration import of an unresolved DataTableRegion: normal module
execution rejects that missing table earlier. It tests this admission boundary,
not a claim that native module startup can proceed with unresolved regions.

Fresh ACPICA explicit calls to the same EC/GPIO aliases agree with counters,
space/code values and disconnect effects (0,3,3,1,6,0). This comparison does not
exercise ACPICA's automatic orphan scheduling. Its implementation reference is
[AcpiEvExecuteOrphanRegMethod](https://github.com/acpica/acpica/blob/master/source/components/events/evregion.c).

No new SPARK proof is claimed. Callback bodies, provider capability association,
timing/termination, exceptional callback behavior, native scheduling and hardware
effects remain trusted obligations or unvalidated integration. This child API
does not discover or install providers, authorize addresses, certify the global
region barrier, run device initialization, or expose an observer/wire operation.
