# EC retained observations

Run with TMPDIR=/home/doc/cubit-build-tmp in the Nix environment: bash tests/acpi-ec-observations/run.sh.

Executes the actual EC transaction engine, command owner, passive getters, copied-diagnostic forwarding, and complete ACPI logger in checked and release hosted builds. Registers, clock, command guard and runtime publisher are modeled. Fifteen engine cases cover typed deadline/budget failure, retained effects, burst/status/access/clock failure, normal read/write/query, stop, and passive observation with no extra I/O. Three logger cases cover unopened, ready and disabled publishers, unsafe open refusal, shed/invalid records, terminal peer notice and dropped counts.

Expected per profile:257 engine/forwarding checks plus23 logger checks. No full Core, new SPARK proof, native execution or hardware claim. Runtime Logging.Emit/Channels.Put/Kick are pinned for the audited one-time-open versus append/shed/nonblocking-wakeup boundary; their execution is modeled, not proved here. No ACPICA run is needed for this observer/logger-only increment.
