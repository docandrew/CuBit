# Canonical local _REG aliases

Run inside the pinned Nix development environment, exporting TMPDIR before Nix:

    bash tests/acpi-region-alias/run.sh --output-root <private-directory>

Node.js and ACPICA must be available. NODE_BIN selects Node when it is outside
PATH; ACPICA_BIN selects the directory containing iasl/acpiexec. The runner
copies its explicit source manifest to a private workspace and checks source
hashes before and after compilation/execution. It records tools, commands,
statuses, logs, binaries and comparison results. Checked uses -gnata/-gnato;
release uses -gnatp. One compiler job,180s build,30s process,5s ACPICA,64MiB stack,
no core dumps,1MiB captured logs. Invoke nice -n 19. No native or QEMU builds/runs.

The actual Core and region coordinator exercise direct aliases and alias chains
from a local _REG selector to a shared method. Four regions cover two local EC
notifications, a child with no local method (no ancestor fallback), and a memory
region whose notification remains suppressed. Connect/disconnect effects are
checked through actual AML counters/arguments. Duplicate attempts, failed whole-
inventory readiness, nonmethod aliases and foreign-owner disconnect are refused
without additional callbacks. The new matrix has172checks/profile over AML
revisions1/2. The unchanged existing region-initialization function also runs
once/profile, including its two revisions and previously encoded ACPICA corpus:
776existing checks,948total per profile.

Fresh ACPICA execution of the same local aliases agrees with integer observations
0,2,3,1,4,0 (initial count, connected count/space/code, disconnected count/code).
Those are explicit hosted method calls, not ACPICA automatic _REG scheduling.
ACPICA's [region dispatch](https://github.com/acpica/acpica/blob/master/source/components/events/evregion.c)
uses its [single-scope method-alias lookup](https://github.com/acpica/acpica/blob/master/source/components/namespace/nssearch.c);
these provide the implementation reference for canonical local selection.
The fixtures do not access EC hardware. Handler readiness is a modeled trusted
backend assertion; the alias fixture's methods only change namespace integers.
The existing matrix separately exercises modeled Field/provider effects. Neither
model installs a native provider or grants access to firmware-selected addresses.

Evidence is hosted regression-tested, not a new SPARK proof. Native provider
readiness, actual _REG delivery, SCI/device initialization and laptop behavior
remain unvalidated. Namespace metadata and method aliases confer no authority.
