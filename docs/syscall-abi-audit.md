# System-call ABI Audit

Status: active migration ledger

CuBit is not a UNIX compatibility kernel. Its public kernel ABI should consist
only of capability-directed IPC primitives. Service policy, discovery,
configuration, and ordinary I/O are typed IPC protocols. Operations that must
execute in ring 0 are capability invocations on kernel objects, reached through
the same IPC envelope rather than through ambient, operation-specific syscalls.

This ledger is source-backed. CuBit has no legacy-binary compatibility promise,
so an unused entry can be removed rather than permanently reserving its number.
Before removal, every definition, dispatcher arm, wrapper, and first-party call
site must be checked. Before the endpoint migration is declared complete, linked
images must also be scanned for removed syscall numbers.

Inside the kernel, live operations are represented by the `SyscallNumber`
enumeration with an explicit representation clause. The exported assembly/C
boundary still accepts an untrusted 64-bit integer and decodes it with a total
checked mapping before entering the typed dispatcher. Numeric diagnostics use
`SyscallNumber'Enum_Rep`; positional enum conversions are not wire values.

## Target public ABI

The exact register ABI still needs measurement, but the semantic surface is:

- `invoke`: send or submit a typed operation through a capability slot. Endpoint
  targets use the existing mailbox fast path; kernel-object targets dispatch
  directly without a process switch.
- `wait`: wait on an endpoint or wait-set capability and receive a message,
  notification, completion, fault, or timer event.
- `reply_wait`: consume a kernel-minted one-use reply capability and atomically
  wait for the next request.

Synchronous `call` may remain an explicit fast-path primitive or be an `invoke`
mode. Nonblocking poll should be a wait mode, not a separate authority path.
The choice is a performance/ABI decision; it does not change the security model.

`WRITE`, `EXIT`, and similar conventional calls may remain during migration, but
they are compatibility scaffolding rather than the destination architecture.

## Kernel objects reached through IPC

| Kernel object | Representative operations |
|---|---|
| Address space / frame | map, unmap, share, translate when authorized |
| Process control | create, start, stop, inspect, set scheduling policy |
| IRQ / I/O region / DMA | bind notification, bounded port access, map MMIO, allocate DMA |
| Timer | read monotonic time, arm deadline, cancel a cancelable deadline |
| Capability space | derive, attenuate, transfer, inspect, revoke where policy permits |
| Debug / observation | write diagnostic record, control tracing, obtain summaries |

Kernel-object dispatch begins by resolving the caller's capability slot and
checking its type, generation, rights, and operation schema. This preserves
ring-0 performance without creating an ambient-authority side door.

## Remove now

| Numbers | Entry points | Evidence |
|---|---|---|
| 3, 4, 5, 9, 10, 11 | `EXECVE`, `FORK`, `FSTAT`, `TIMES`, `UNLINK`, `WAIT` | Constants only; never dispatched |
| 1, 2, 13 | `READ`, `CLOSE`, `OPEN` | Dispatcher reached no-op stubs; the only `OPEN` caller was the 2020 bootstrap's `A:/asdf.txt` test |
| 100 | `CONTROLACCESS` | No callers; obsolete self-service derive/mint/remove/revoke interface |
| 101 | `GETTICKET` | No callers; exported a scrubbed copy of a kernel capability record |
| 79 | `SET_SUPERVISOR` | No callers; old direct process-management operation |
| 35 | `OUTPS16` | No userspace wrapper or caller |
| 34 | `INPS16` | Removed after the current ATA service stopped using it; the auxiliary older ATA tree now performs repeated capability-checked `INP16` reads |
| 44–47 | notification wait/poll/bind/unbind | No callers; notification delivery currently uses live `NOTIFY` plus event polling |

The deprecated `RECEIVE_NB` spelling for number 22 was also removed. Its sole C
caller now uses the canonical `POLL_ANY_IPC` name; this removes an alias, not an
additional numeric slot.

The numeric slots are intentionally not reserved. A future syscall may reuse a
number only after the current tree and produced images contain no old use.

## Live specialized calls: keep temporarily, then migrate

| Entry point | Current problem | Destination |
|---|---|---|
| Raw `SUBMIT` | Caller names a PID and bypasses endpoint-capability mediation | Give protocols endpoint/session handles and use `CAP_SUBMIT` |
| `SEND_EVENT` | Any IRQ capability currently permits delivery to any PID | Interrupt ownership and destination authority must be separate |
| PID-spelled `REPLY` | Core consumes one-use reply authority, but public ABI still names the peer PID | Reply only through kernel-minted reply handles |
| Raw `GRANT`/`REVOKE` | Memory sharing and destination selection need a typed authority review | Capability-scoped shared-memory/session operations |
| `INFO`, `SET_SYSINFO`, `REGISTER_DRIVER`, `SET_WELL_KNOWN` | Mix discovery and mutable service policy into kernel ABI | Typed bootstrap/device/service-directory protocols |
| `MAPFB` | Ambient display mapping is too broad | Display-session authority and a display-service protocol |
| Trace reset/summary | Currently not authority-gated | Explicit observation/debug authority |
| Process lifecycle and memory calls | Conventional direct operations have authority implied by syscall choice or PID | Process/address-space kernel-object invocations |
| Port I/O, IRQ, MMIO, DMA calls | Already perform some capability checks, but expose parallel authority-specific ABIs | Device kernel-object invocations |
| `GETTIME`, `SLEEP` | Direct time ABI | Timer service/object capability; deadlines arrive through `wait` |

The detailed raw-IPC call-site migration remains in
[`legacy-ipc-audit.md`](legacy-ipc-audit.md).

## EndpointTable migration

The existing `mailtab` implementation is live endpoint transport machinery, not dead
compatibility code. Its rings, waiter queues, notification state, completion
synchronization, and direct handoff are retained.

The problem is identity: today a mailbox is indexed by `ProcessID`, making the
transport object and process identity appear interchangeable. The migration is:

1. Introduce an opaque, generation-bound `EndpointID`.
2. Separate a small, SPARK-friendly endpoint authority/ownership table from the
   lock-heavy endpoint transport table.
3. Move the existing mailbox records under the transport table without changing
   queue algorithms or wakeup behavior.
4. Make generic invocation resolve the capability type first. Endpoint
   capabilities resolve to `EndpointID`; kernel-object capabilities dispatch to
   their object implementation. Neither path treats a PID as authority.
5. Let process creation receive a default endpoint as policy, while allowing a
   process to own zero, one, or several endpoints later.
6. Migrate replies, events, notifications, and completion routing before removing
   the remaining PID-directed compatibility entry points.

During the transition a default endpoint may have the same numeric value as its
owner PID internally, but that equality is an implementation bridge, not ABI or
authority semantics.

## Security invariants

- Possessing a PID is never authority to contact or modify that process.
- Endpoint lookup checks type, rights, object generation, and endpoint liveness.
- Derivation and transfer cannot add rights or widen object scope.
- Reply authority is kernel-minted, single-use, and bound to one request.
- Queue occupancy and wakeup state confer no authority by themselves.
- Introspection, tracing, discovery, and approval are capabilities too.
- Every compatibility syscall has an owner, call-site list, and deletion gate.
