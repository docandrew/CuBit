# Userspace named-register transport client

`ACPI_Region_Client` is a generic caller for the backend's existing scalar
protocol. It supplies no hardware authority. Its transport must map the opaque
Endpoint to a scoped native capability, authenticate/associate each reply with
its exact call, and return without retrying unknown completion. Calls go down
the service layering. `Now` and `Deadline` use monotonic milliseconds; expired
deadlines and the native Wait_Forever sentinel are rejected before dispatch.

A context installs one endpoint for its lifetime. Requests carry only epoch,
named register ID, value and a reserved zero word. The backend independently
chooses register geometry and checks authority. Geometry, offsets and addresses
are not caller wire arguments. Reply decoding validates every envelope field,
outcome, both 32-bit value halves and reserved word before admitting a result.
Write replies and non-success replies must have a zero value.

One request ID is reserved before each external call. Lost/malformed replies
and explicit backend uncertainty retain the exact request with a process-local
context identity. No automatic retry or retarget is available. A trusted
controller confirms attributable terminal completion, the consumer reconciles
that exact receipt, then explicitly retires the confirmation. Confirmation and
retirement perform no IPC and never re-enable revoked backend authority. A
callback that raises leaves the context quarantined, including its in-flight
latch; transports should return explicit delivery facts for recoverable errors.
The owning service serializes context access and issuer initialization.

This is a userspace component, not a running native service. The CuBit call
adapter is provided in transport/ with separate hosted and runtime-API gates.
The AML Field_IO adapter is in provider/ with a typed native binding; live
capability provisioning and broker backing lifetime remain integration work.
`Not_Dispatched` is a trusted transport
fact that no backend operation could have happened. Confirmation evidence and
consumer reconciliation are trusted controller facts. Local identities are
neither kernel capabilities nor persistent credentials across process restart.
Backend pending tickets remain internal. The separate selected proof milestone
in tests/acpi-client-proof covers codec and client transition contracts in an
explicit transport model. It does not prove native IPC or hardware completion.
