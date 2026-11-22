# Typed commands, events, and discovery

An instruction such as "refresh", "rotate this certificate", or "reload this
configuration" is normally an ordinary typed service operation. It does not need
a persistent stream or an asynchronous signal interrupting arbitrary application
code. Discovering its signature and possessing authority to invoke it are separate.

## Existing mechanisms

- `capCall`: synchronous endpoint call with a reply.
- `capSubmit`: asynchronous endpoint submission with token-correlated completion.
  Successful submission is not evidence of successful execution; inspect the
  completion status and typed service result.
- `trySendEvent`: bounded event publication through existing authority checks.
  Acceptance into a queue does not promise that a requested action took effect.
  Publication stamps the authentic authority tag, not the caller-supplied field.

Use asynchronous calls for commands whose outcome matters, even when the logical
result carries no data beyond success/failure. Events are suitable for facts or
wakeups with an explicitly declared delivery contract. They are not an alternate
path around an operation's invocation authority. Cancellation, deadlines,
idempotency and retry behavior must be specified; a lost reply does not establish
that an action did not execute. A durable exactly-once job protocol is additional
work, not a property of the primitive IPC call.

CCL already has bounded `Interface_Catalog` views containing descriptor-pinned
operation metadata and separate `Granted_Bindings` supplied by a trusted host.
Those mechanisms support static checking and authorized linkage. They do not
constitute a completed live, system-wide discovery service. Existing interfaces
also have representational limits; do not assume arbitrary Unit/record payloads
or all event kinds are already exposed through CCL.

## Intended discovery contract

An authorized catalog view exposes only permitted interface/operation metadata:
name, pinned provider/interface identity, request/result types, authority class,
ownership/completion effects and cancellation behavior. Knowing a schema or
operation ordinal never provides a callable handle. The host binds an invocation
only after resolving actual endpoint/session authority and checking the pinned
contract and provider identity.

The graphical system browser and REPL should consume the same catalog view.
Methods appear as callable actions; typed inputs/outputs appear as connectable
ports. A signature tooltip is discovery, an action button is invocation, and
dragging a stream edge is connection reconfiguration. Each needs its own relevant
authority. There is no universal "visible therefore callable" rule.

Stream prepare/commit/abort and inspection will themselves be typed operations
on the authorized connection manager. The current pure lifecycle ADT is not yet
such a published endpoint. See [stream wiring](stream-wiring.md).
