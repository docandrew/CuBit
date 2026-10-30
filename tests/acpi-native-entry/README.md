# Direct native ACPI API checks

`bash tests/acpi-native-entry/run.sh --mode checked` or `--mode release` runs
13 semantic compile selections, producing12ALIs and no objects or executables.
It checks main, clock, logger, native owner, observer endpoint/runner, native
snapshot bindings and generic local copy/startup units, with the native service
clock instantiation and exact generated Startup spec/body.

All fixtures have explicit repository originals in mirrors.tsv. Captured APIs
must match the live tree; missing or changed runtime/capability/CCL declarations
stop validation. The provisional owner API record is api-pending.json: it does
not authorize bypassing mirrors, native testing, publication or refreshing hashes
without reviewed final inputs. This is intentionally not a passing gate until
the runtime/launcher checkpoint is available in the same source tree.

The generated binding requires Tables, Logstore and Registration. Generation's
manifest/catalog/schema inputs are pinned. Generated startup.adb retains a
redundant Services with-clause; only that file suppresses redundant-with warnings.
Every handwritten unit keeps all warnings as errors. Profiles use gnata+gnato
or gnatp, always gnatc, consistently with the hosted predefined runtime. Native
System/runtime objects are not mixed into the host runtime.

`--validate-only` checks inputs, closed Ada inventory and every live mirror.
No compiler executes then. Set TMPDIR before and inside Nix. Build180s,
stack64MiB, logs1MiB, nice19/j1. Native ABI execution, allocation, register/clock
syscalls, logger delivery, kernel authorization, link/stack fit, QEMU and laptop
remain separate acceptance requirements. No new SPARK proof is claimed here.
