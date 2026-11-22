# CuBit register-call binding

`ACPI_Register_Transport` instantiates the register client with the runtime's
typed Endpoint and `CuBit.Handles.IPC.Call`. Dispatch reads the monotonic
millisecond clock; Exchange checks it again immediately before IPC. An absent
endpoint, expired/unavailable clock, infinite deadline or invalid request is
rejected locally. No timeout or kernel error after Call is assumed to mean that
the backend did nothing. There is one call and no retry loop.

The authoritative returned MessageTag replaces the old request tag. All protocol
and IPC flag bits, length, reserved fields, status and value halves must satisfy
the register protocol. Malformed replies retain uncertainty, including replies
that arrive after an executed write. The adapter never supplies a receive-side
authorityTag and never sends addresses, offsets, widths or cleanup tickets.

Startup must supply a scoped endpoint to a lower layer that cannot call back
synchronously. Kernel IPC is responsible for authenticating and associating the
reply with this exact call. A trusted owner retains backend backing and produces
attributable terminal completion evidence for uncertain requests; client cleanup
does not establish that evidence. These ownership, live provisioning, service
startup and live AML service integration gates remain unfinished. The provider/
adapter now supplies the Core.Field_IO interface and layered receipt retention.

The hosted tests use the production adapter/client and real backend dispatcher
with modeled IPC, clock and hardware. A separate compile-only gate checks the
production adapter against exact copies of the current runtime declarations.
Neither gate is native execution, a QEMU/hardware acceptance test or a SPARK
proof of the syscall boundary.
