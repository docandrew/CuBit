# Host-owned CCL resource lifetimes

```sh
nix develop -c bash tests/ccl-resources/run.sh --prove
nix develop -c bash tests/config-object-client/run.sh
```

`CCL.Resources` separates a script's ability to use an opaque reference from
the host's obligation to drain and clean up its backing resource. It is pure
SPARK, bounded to 32 slots, single-owner, with no pointers, global mutable
state, implicit wait, serialization, or kernel-handle representation.

- Start pins one type registry for a run. Only Resource types can be reserved.
- Reserve before issuing a factory operation; capacity failure has no effect.
- Publish only after the host validates the actual factory completion. A
  stopped run cannot receive a reference, even when the operation succeeded.
- Each use has a fresh completion ticket. A duplicate or old completion cannot
  finish another use; the resource permits one outstanding operation at a time.
- Stop immediately denies script access but preserves pending calls and slots.
- Host cleanup can issue a new ticket on a retiring lease, e.g. to close the
  handle returned by Create after Stop. Completion cannot revive that lease.
- Reclaim requires a drained, retiring lease and a matching lifetime ticket,
  not a bare slot number. A delayed cleanup cannot reclaim a reused slot.
- A new run cannot start until the old run is stopped and every slot reclaimed.
  Run and operation counters never wrap; exhaustion fails closed.

The host supplies a distinct `Context_ID` for each registry lifetime and must
not reconstruct a registry under an identity whose references still exist.
This package does not allocate process-global context IDs or synchronize
threads. That host association is an explicit integration obligation, like
authenticating IPC completions; an identifier is never itself a grant.

The host must establish remote-handle cleanup and confirmed grant retirement
**before** Reclaim. The registry cannot prove external DMA/IPC quiescence.
`Position_Of` is for the host's stable backing array and remains usable after
completion until reclamation; `Valid_Ticket` specifically means an outstanding
operation. Neither is a script API. These references are not yet a source or
VM value kind, and the registry is not a substitute for static move/borrow rules.

## Evidence and boundaries

2026-09-25: 38,206 hosted checks, plus a test-only child exercising the final
64-bit counter values. Tests cover foreign contexts, wrong types, stale run
requests, factory/use ticket confusion, duplicate completions, all slots busy,
reverse-order draining, 1,024 slot reuses and stop during acquisition/use.
No production counter-reset or integer-to-reference function was added.

GNATprove: 65 checks, zero unproved/justified (31 runtime, 9 functional contracts,
2 assertions, 14 initialization, 9 termination). The contracts establish the
checked publication, ticket consumption, stop and retirement properties. The
stop proof uses a proved induction invariant, no added runtime guards or Assume.
This is not a proof of complete resource/CCL/OS soundness or external authority.

`tests/config-object-client/resource_tests.adb` adds 135 hosted checks using the
production asynchronous Config client and modeled IPC/grants. Normal close,
denied Create, Stop during Create/Get/Set, wrong completion tokens, post-Stop
close and delayed grant retirement all pass. This is **not** a native VM test
or a completed `Config.create(type)` source binding. The whole Config client
suite also passes; logs are `/tmp/cubit-resource-lifetime-final.log`.

## Next integration obligations

Use this registry in the shared host resource-value/factory path, not as a
Workbench-only integer table. Generic factory signatures need static type
arguments, owned return/outcome metadata and source/bytecode lifetime checks.
A sum containing a live resource is not an `Objects.Binding` persistence value.

Do not recycle a poisoned Config client just because its local grant retired.
The initial unknown-handle concern now has a fix at the existing reply boundary:
`Config_Object_Receiver` closes a newly minted Open/Create handle if the kernel
reports non-delivery. The durable collection/value remains intact. Kernel
`replyCap` returns 1 only after synchronous handoff or insertion into the
caller's reserved completion queue; 0 means the reply was not delivered.

A delivered completion remains the host's obligation to drain and close,
including after Stop. This registry does not permit Stop to discard that work.
Do not infer external cleanup from a malformed/untrusted transport receipt;
provider restart, process-death cleanup and future network IPC need their own
established lifetime boundaries. Native kernel receipt semantics are not a
claim that a remote browser acknowledged or retained a handle.
