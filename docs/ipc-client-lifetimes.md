# Shared IPC client lifetimes

The request lifecycle belongs to CuBit's common runtime. Service adapters supply
operation schemas, reply validation, and resource-specific cleanup. CCL supplies
language ownership and suspended-program handling. None of these replaces the
kernel's endpoint/reply authority checks.

## Implemented common runtime

`userspace/runtime/gnat/cubit-async_requests.ads/.adb` is a SPARK state machine
with no dependency on CCL, Config, storage, message payloads, or allocation.
Config's native object client and the asynchronous filesystem `Storage_Channel`
both use it for submission tokens and completion consumption.

One tracker describes one outstanding request slot:

```text
Idle -> Reserved -> In_Flight -> Completion_Ready -> Idle
           |
           +-- definite submission rejection ------------> Idle
```

Reservation burns its token even when the submission queue rejects the call.
Invalid, stale, foreign-token and duplicate completions cannot advance the slot.
A completion remains owned until consumed; a new request cannot overwrite it.
The dispatcher must allocate unique tokens across all its slots, including
replacement clients. The tracker only establishes monotonicity within its own
lifetime. Token exhaustion is explicit; counters must not wrap or reset.

`Stop` irreversibly detaches this tracker from its consumer. It prevents future
requests and resumption, but retains the pending token and accepts a late receipt
for draining. It performs no cancellation, rollback, handle closure or memory
release. It may be applied during reservation, flight, or completed-result
ownership. A failed or noncancelable request has the same transport lifetime as
a successful request.

The tracker is single-owner. Submission, Stop and completion dispatch must be
serialized by the owning event loop or a surrounding lock. Concurrent clients
can have independent trackers; this does not introduce a global lock.

## What stays with the adapter

Only kernel-authenticated completion-queue entries may enter a native client.
Matching tokens do not authenticate a message. The adapter validates transport
status, operation-specific reply shape, approved schemas and returned lengths.
It distinguishes definite failure from an uncertain effect; generic machinery
cannot infer that a timed-out write did not happen.

The adapter also owns shared grants and remote handles. Consuming a receipt is
not proof that a grant is retired or a handle is closed. Existing Config and
storage clients retain their service-specific failure and retirement behavior.
Their terminal retirement paths are not implemented by calling Stop and then
assuming memory is safe to reclaim.

Config's collection owner may stop script access while using the same underlying
client to close the remote collection. That is a separate resource lifetime:
stopping a script should not detach the cleanup client's own request tracker.

## What stays with CCL

`CCL.Resources` is a generic, typed resource registry. Its run identities,
opaque references and operation tickets are useful to every CCL service binding,
but depend on CCL's nominal types. It therefore remains in the CCL library.
Likewise `CCL.Imports` and the VM manage language borrow/move rules and suspension.
Native applications using the common tracker do not link these packages.

`Config_Object_Client.Resources.Calls` connects those existing layers for typed
one-shot reads and writes. It pins the receiver and registry operation ticket,
checks input/result types before I/O, acknowledges accepted VM borrows, and
resumes with typed outcomes. Stop leaves the receipt available for host draining.
The host must retain the same program/machine/registry/collection association
until the call drains; a numeric import binding alone is not a run identity.

This is a reusable service adapter, not code embedded in Workbench. Config
operations and their read/write outcome schemas remain Config-specific.

## Boundaries and remaining work

The shared tracker does not change the kernel ABI and has no payload copy or
serialization step. It applies equally to ordinary calls, resource acquisition,
stream setup and teardown, and asynchronous grant transactions. A high-rate
stream's individual ring entries follow that stream's ownership protocol;
they do not require a syscall/request tracker per element.

Network IPC can reuse the lifecycle concepts, but needs an authenticated network
adapter and explicit disconnect/retry semantics. This work does not establish
exactly-once remote execution, crash durability, or network cancellation.

Config's completion milestone is one-shot typed read/write from Workbench,
safe repeated Run/Stop, and durable readback after reboot. Generic source factory
syntax and typed streams are independent follow-ons; Config does not require
watch/subscription support to meet that milestone.

## Validation

`tests/async-requests` exercises Stop at lifecycle boundaries, queue rejection,
token exhaustion, duplicate and foreign completions, and drain-before-reuse.
GNATprove checks the common state machine's transition contracts. The Config
and storage suites exercise actual adapter code with modeled kernel IPC/grants.
These results do not prove kernel authentication or grant retirement; native
regression tests cover the actual IPC boundary separately.

Validation for this extraction (2026-09-25): 155 common tracker checks and
19 SPARK checks (including five functional transition contracts), with none
unproved or justified. Existing storage tests and the full hosted Config suite
pass; the new owned-receiver adapter has 372 checks. Native Config write and
fresh-boot recovery both pass, including independent SQLite/WAL/ext2 validation
of the exported disks. The new CCL receiver adapter itself remains host-tested;
Workbench has not yet adopted it.

## Workbench and remote-host integration progress

Workbench's bytecode debugger now uses `CCL.VM.Native_Objects`, including shared
read-only PC/stack/local inspection. Stopping clears local object storage and
debug inspection no longer exposes those retired references. This is the same
VM implementation available to a headless host, not a GUI-specific object VM.
The interpreted REPL remains a separate execution path.

`Config_Object_Interfaces` in `userspace/lib/config` builds the approved
collection's open/read/write/close signatures, resource ownership policy and
typed outcome metadata in one catalog transaction. The compiler, linker and
`CCL.Catalog.Completion` consume those same descriptions; native and future
remote hosts need not maintain parallel lists of function names/type hints.
The builder supports multiple scalar or application-defined record collections
and reuses their common write-outcome schema. Rejected publication leaves the
visible catalog unchanged.

This builder is not live endpoint enumeration and does not grant authority.
Hosts must obtain an authorized discovery view, authenticate/pin the provider,
and install separately approved runtime bindings. A remote shell must receive
only its session's visible metadata; it must not inherit Workbench's grants or
persist process-local handles. Its wire adapter and authentication remain
separate work.

The event-loop integration is now implemented by
`Config_Object_Client.Resources.Runs` and the Workbench execution facade.
The reusable runner owns its program, machine, registry, collection, and pending
invocation until drain. A replacement Load is rejected without mutating them
while a request or unretired resource remains. Stop removes script access but
does not pretend an outstanding request has been canceled. Ambiguous acquisition
or close is quarantined, without a busy cleanup retry loop.

Native Workbench and the shared UI window client use asynchronous input waits.
Desktop replies and Config receipts share one process-wide monotonic token
allocator and the kernel completion queue. Workbench drains a bounded batch and
waits for activity rather than blocking on a desktop-only reply. Only delayed
cleanup uses a retry deadline; data writes are never automatically replayed.
Reads establish an observed revision; writes use that revision and expose
conflicts as typed outcomes.

The native `config-values` bootstrap exposes one narrowly authorized demo
collection, with signatures/type hints supplied by the shared catalog builder.
It is not live provider discovery. See `tests/config-workbench/README.md` for
the explicit persistent-backend boot profile and native keyboard-driven test.
Linux preview deliberately does not advertise these operations. The interpreted
REPL and authenticated remote execution remain separate integrations.

Validation (2026-09-25): 55 shared-descriptor/completion checks, 253 native-object
VM checks and the existing 372 owned Config-call checks pass on Linux. The
focused native-object SPARK run reports 96 checks with none unproved; the
descriptor builder reports 11 initialization/termination checks, not a proof
of provider authentication or authorization. Offscreen Workbench syntax/paused
debugger tests and the native QEMU Workbench smoke pass. The normal ISO was
restored after testing.

Validation of the event-loop integration (2026-09-25): the full hosted Config
client suite passes; the runner adds 378 checks covering repeated runs, Stop at
open/read/write/close, rejected submission, foreign/duplicate completion,
revision conflicts without replay, delayed grant retirement, quarantine, token
exhaustion, and compilation of both shipped samples. Focused SPARK reruns retain
96 native-object VM checks and 19 shared request-tracker checks, none unproved
or justified. The new Ada coordinator and GUI dispatch are regression-tested,
not included in those proof totals. Linux offscreen hover/file-dialog and all
six unsaved-edit workflows also pass.

The native Workbench QEMU test types a script through the ordinary keyboard
path, compiles and runs two writes, then boots the same disk again and executes
the shipped typed-match read sample. Both commits and the recovered value `42`
were verified in screenshots; independent SQLite/WAL/ext2 checks confirm two
revisions containing the correct schema and CBOR payload. These are acknowledged
write/reboot regressions, not an arbitrary-power-loss guarantee. The normal ISO
remains built with its normal boot profile; the test's startup plan lives only
on its disposable disk.
