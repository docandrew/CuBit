# Native I2C adapters

Main consumes immutable Generic_PCI launch facts and generated Startup handles.
The native register adapter checks device-memory kind, read/write rights, BAR
identity, page alignment and span before mapping one named device page. It
never accepts client-supplied addresses or register offsets. Initialize admits
only the supported PCI profile and attempts mapping once; successful mapping
does not itself mean hardware startup completed.

Startup uses finite work budgets without sampling GETTIME. The owner resets,
probes, masks interrupts and establishes disabled/idle/empty state before the
generic Ready report. No target or timing is selected before a client is
admitted. Main validates the report acknowledgement and optional primary role
registration. Readiness is not a configure RPC or saved configure reply.

I2C_Native_Loop owns controller/request state and exactly one accepted client
reply. The kernel-stamped caller remains an opaque64bit identity. Badge data
provide that channel's target and speed; grant-copy callbacks acquire with the
exact expected owner, copy only the admitted slice, and return each acquisition.
Serialization covers disabled reconfiguration, transfer, cleanup and retirement.
An uncollected completion blocks reuse; uncertainty freezes rather than replays.
Reentry takes no new reply authority and performs no hardware work.

Kernel Stop closes admission and drains only the existing bounded request,
preserving its original saved reply. Stopped_Quiescent separately requires
disabled/idle state and no pending completion. A stopped fault is not a clean
stop. Main polls a bounded event batch, routes kernel channel wakes through the
runtime registry, and uses the tested local logger-disconnect guard on lifetime
notices. The global logger is unchanged. The power service's lifecycle
acknowledgement protocol and MSI/EOI are not integrated yet.

Declared pure wire/identity contracts and runtime safety have a scoped134check
proof milestone. The stateful loop, owner, event/work policy, volatile/syscall
adapters and actual hardware are not thereby proved. Hosted models test them;
entry-point validation uses -gnatc with generated Startup and current runtime,
zero native objects and no native execution. Trusted kernel metadata, mapping
lifetime/permissions/exclusivity and serialized bounded non-raising callbacks
remain assumptions. See the service README and tests/i2c-dw-native for scope.
