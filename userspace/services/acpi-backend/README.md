# Privileged ACPI access backend

This directory contains backend policy, scalar-request execution and the native
IPC envelope adapter. It must execute outside the AML service's address space.
The backend owns independently admitted region records and resource authority;
the AML service receives only scoped endpoint access and immutable table copies.
The explicit native library source list excludes AML and table-service code.

This is not a running service. There is no startup manifest, receive loop,
resource-admission authority, hardware callback, FACS lock adapter or MMIO access.
The native envelope adapter uses only the kernel-received authorityTag; payload
words and reserved fields cannot substitute for it. Pending cleanup tickets stay
internal and are never copied into outgoing messages. Real integration must
serialize state transitions and retain backing through confirmed completion.

Hosted region tests instantiate the executor with a SPARK mock. Their proof does
not establish the semantics of a future real hardware callback. The draft wire
contract and remaining obligations are in docs/acpi-service-contract.md.

The public scalar protocol now uses [epoch, register ID, value, zero]; callers
cannot choose offset or width. The trusted record fixes the register geometry
and allowed write bits for that epoch. ID zero disables public access. Internal
range validation remains defense in depth. The planned live resolver belongs in
the kernel; this library is not a substitute for kernel enforcement.
`Hardware_Grants` and `Hardware_Grants.Cspace` now implement internal kernel
policy for group installation, attenuated child grants, descendant revocation
and authenticated access reservation. Boot provisioning, current-caller syscall
dispatch and actual hardware transactions still need to connect those policies
to the running system. See the service contract.
