# Retained Stop observation

Run in the Nix environment with TMPDIR=/home/doc/cubit-build-tmp. Hosted only; no kernel/image/QEMU build. run.sh --validate-only validates pinned production/test inputs.

Actual ACPI_Native_Events and ACPI_System_Events execute with real opaque Process_ID, control and grant codecs. The hosted Message facade is extracted verbatim from the runtime declarations, including representation clauses. Only channel bells and logging are recorders. The native Park call site is pinned here and independently checked through the real SDK; this suite does not execute Park or kernel event syscalls.

Checks every incorrect 8-bit Stop length, 4-bit protocol, nonzero 16-bit reserved value, all nonempty IPC flag combinations, each one-bit opaque publisher identity, control kinds and nearby labels before one accepted kernel Stop. Repeated Stop attempts must not retry even when the modeled logger sheds the first record. Normal serving Stop handling remains intact. Checked assertions/overflow and suppressed profiles must agree. Legacy runtime aggregate syntax is permitted; other compiler warnings are errors.

The sole native copied append uses Peer_Quiet=False and cannot first-open/flush the logger. No retirement, field, AML, EC or grant operation is dispatched by the retained-note helper. No new proof or native/hardware validation is claimed.
