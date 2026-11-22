# Controlled snapshot startup

Run in the Nix development environment, with TMPDIR exported before entering it:

```sh
export TMPDIR=/home/doc/cubit-build-tmp
nix develop -c bash tests/acpi-snapshot-driver/run.sh checked
nix develop -c bash tests/acpi-snapshot-driver/run.sh release
```

The opt-in hosted suite compiles the actual ACPI loader/interpreter with strict
Ada warnings and executes nine controlled startup cases plus the three existing
default-loader/diagnostic suites. Each profile checks 1,252 assertions in twelve
isolated Ada processes. Source hashes are checked before and after execution.
Builds use one worker with a 180-second limit per build target; executions have 30-second limits.
All outputs remain in a unique results directory.

The real selected evaluator runs the fixture root `_INI`; its observable value
is compared with ACPICA. The barrier inspects that value before completing.
Failure cases cover an unavailable provider, callback exceptions, early/late
deadlines, source exceptions and module Fatal. Repeated contexts do no work;
an already used driver is refused before another snapshot import.

The source and hardware provider callbacks are hosted models. This establishes
no physical register access, native startup wiring, provider authority, lifetime
recovery protocol, module-level hardware support or whole-service SPARK proof.
ACPICA cache teardown diagnostics are retained in the oracle log and are not
treated as interpreter conformance evidence. Only the normal returned marker
value is used as the independent control.


The controlled module-startup milestone is recorded in MODULE-STARTUP-EVIDENCE.json.
Its current-source input/closure refresh does not renew historical proof results.
Fresh hosted checks cover both profiles; no native execution or hardware
authority/region connection follows from these tests. The native API gate
selects 59 units and produces 56 ALIs with zero objects per profile.
