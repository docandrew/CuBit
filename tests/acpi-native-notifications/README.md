Hosted EC notification/lifecycle regressions

Run `bash tests/acpi-native-notifications/run.sh` in the Nix environment.
`--validate-only` checks the exact input/case catalog without compiling.
Outputs and RESULT.json remain in a fresh private TMPDIR directory.

All26 lifecycle cases run at both AML widths, checked and release. Core and
Namespace code generation use -O0 -gnatp; checked enables assertions/overflow
checks on the serving loop, EC owner wrapper and test body. Exact142-unit ALI
inventories and per-unit profiles are checked. Warnings remain errors.

Physical/source/IPC callbacks are models. The actual Core/AML, notification
owners and serving loops execute; no kernel/image/QEMU/native/hardware behavior
or full service proof is established. ACPICA validates Q10's lid value and six
notifications per width only. Its shutdown diagnostics remain in logs.

Single worker, 180s builds,30s cases,24GiB address space and1MiB output. ACPICA
has five CPU/wall seconds,128MiB and2MiB per-thread stack; Core uses64MiB stack.
No proof or shared source mutation is performed by this entry point.
