# AML Fields through the register client

`ACPI_Field_Remote_Provider` adapts the named-register client to the actual
Service_Core.Field_IO interface. The generic binding in binding/ supplies the
CuBit typed Endpoint and concrete register transport. Startup supplies a bounded
absolute call window and independently admitted register descriptors. This is
not a running service or a source of hardware authority.

Descriptors associate opaque register IDs and epochs with geometry and cached
permissions. They are trusted controller inputs, never AML OperationRegion
declarations. Validation rejects wrapping extents, misalignment, invalid IO
geometry and inconsistent masks. Describe establishes association only. Writes
require explicit whole-register permission and Exact_Write_Safe; partial masks
do not silently become whole-register authority. Every actual request still
passes the backend's independent authority and epoch checks. No address, offset,
width or backend cleanup ticket is sent on the wire.

Contexts install one native endpoint and public endpoint symbol for their
lifetime. Used register slots retain their epoch high-water marks: a revoked
association can only be replaced by a strictly newer epoch. Revocation can
disable a pending association without preventing completion reconciliation or
restoring its permissions. A completed read with an out-of-width value disables
the association and reports a completed fault, never an unknown operation.

Dispatch reserves a free confirmation slot before effects. Unknown completion
captures the exact outer request and inner client receipt. The trusted controller
first establishes attributable terminal evidence. The provider consumes the
confirmed inner fact and retains an outer fact for the AML arena; retiring that
inner receipt performs no IPC. Later calls can proceed while older outer facts
remain retained, until bounded history fills. Only exact arena-consumed facts
can be retired. Neither confirmation nor retirement replays a request, releases
backend backing or restores revoked authority. Context access and issuer use
must be serialized; callback exceptions quarantine the context.

Live endpoint provisioning, descriptor provenance, backing retention and terminal
completion evidence remain integration obligations. A trusted boolean evidence
argument is not a native recovery protocol. Tests use actual Core/AML/client and
backend code with a hosted packet transport; separate compile-only checks use
exact runtime declarations for the concrete native binding. The separate gate
in tests/acpi-client-proof proves client transition contracts and selected
provider runtime safety at default capacities. Complete provider functional
history/admission and native QEMU/hardware acceptance remain unfinished.


## EmbeddedControl Fields

The common provider accepts bounded EmbeddedControl geometry (space3) using
byte-width named descriptors. The declared region must fit entirely in logical
EC cells0..255; ordinary Fields require explicit ByteAcc and NoLock. Memory/IO
behavior is unchanged. Each datum still requires its own independently admitted
named register; no EC cell number or hardware address is added to wire requests.

The hosted tests/acpi-ec-fields suite joins actual AML/Field/provider code to
the deferred EC endpoint,catalog and transaction engine. Hardware,clock,protocol
ownership and authenticated transport metadata are modeled. Whole-Field Lock,
native admission,reservation/GlobalLock,startup and interrupts remain pending.
Selected layout/plan proofs do not prove this provider or the whole interpreter.
