# CuBit Name Resolution

Status: design direction informed by the initial NetSurf integration. The
current implementation remains inside `netstack.svc` while its protocol and
failure modes are hardened.

## Purpose

CuBit needs extensible name resolution without recreating the UNIX Name
Service Switch. Applications should not load resolver modules, read global
resolver files, inherit ambient network access, or decide which naming system
is authoritative. They should request a typed network destination using
authority granted to that process.

The long-term design separates name policy from packet transport:

```text
application
    | typed endpoint request
    v
netstack.svc -----------------------> connected endpoint handle
    |
    | bounded resolution request
    v
resolver.svc
    |-- policy and namespace selection
    |-- positive and negative cache
    |-- classic DNS
    |-- authenticated DNS through SPARKTLS
    |-- private or capability-provided namespaces
    `-- optional local discovery
```

This is not an in-process compatibility interface analogous to
`getaddrinfo(3)` or NSS. Resolution is a service boundary with explicit
authority, typed requests, bounded resources, observable decisions, and no
dynamically loaded application-side plugins.

## Current Architecture

Today the responsibilities are divided as follows:

- `netmgr.svc` configures interfaces and supplies DNS-server addresses to
  `netstack.svc`. Under QEMU, the configured resolver is normally `10.0.2.3`.
- `netstack.svc` constructs DNS queries, parses responses, resolves names, and
  opens TCP connections.
- An application can submit `OP_NET_OPEN` with a bounded scheme such as
  `@net:tcp:example.com:80` through its netstack service capability.
- The application receives a channel handle after resolution and connection;
  it does not receive raw DNS authority or ambient resolver configuration.

The initial parser handled a direct A response. NetSurf testing exposed that
real sites commonly return compressed CNAME chains followed by multiple A
records. The parser now walks bounded answer records and selects a valid IPv4
A record, but it is not yet a complete resolver.

Keeping this implementation in `netstack.svc` is acceptable during the first
HTTP milestone. It minimizes premature protocol design while integration tests
reveal the required semantics. It is not intended to be the final ownership
boundary.

## Proposed Application Interface

Applications should describe intent rather than manipulate DNS directly. A
conceptual request is:

```text
Endpoint_Request {
    host       = "www.example.com",
    service    = HTTP,
    transport  = Reliable_Stream,
    policy     = Application_Default
}
```

The result is either a connected endpoint handle or a typed failure. Raw IP
addresses remain valid endpoint subjects and bypass name resolution where
policy permits.

Important properties:

- The application needs authority to request the service category and
  destination scope.
- The returned endpoint is a bounded dynamic handle, not permanent global
  network authority.
- Resolver configuration and upstream credentials are not disclosed.
- A resolution result does not itself grant permission to connect.
- Cancellation, timeout, and process death deterministically release pending
  resolver and network resources.
- Errors distinguish malformed names, policy denial, negative answers,
  timeout, transport failure, and resource exhaustion.

`netstack.svc` may initially remain the application-facing coordinator. It can
ask `resolver.svc` for candidate addresses, apply connection policy, and return
only the connected endpoint. Applications that genuinely require resolution
data could receive a narrower resolver capability explicitly.

## Resolver Service Responsibilities

`resolver.svc` should eventually own:

- DNS message construction and bounded parsing.
- CNAME traversal with loop and depth limits.
- A and AAAA results and ordered address candidates.
- TTL-aware positive caching.
- Bounded negative caching for NXDOMAIN and no-data answers.
- DNS response-code interpretation.
- Request deadlines, retry limits, and alternate upstream selection.
- UDP truncation detection and bounded TCP fallback.
- Concurrent request correlation with non-repeating transaction identifiers.
- Optional authenticated resolution using SPARKTLS-managed channels.
- Explicitly configured private namespaces and local discovery providers.
- Structured security/audit events describing what, who, when, where, and why.

Search domains and hostname rewriting should not be assumed. If supported,
they must be explicit policy visible to the caller and security tooling. A
short name silently acquiring organization-wide meaning is a security decision,
not a convenience hidden in process-global configuration.

## Security Model

Resolution is advisory data, not authority. A malicious DNS response must not
allow an application to escape its destination or service policy. Connection
authorization is checked against the application request and applicable policy
even when the resolver returns an address.

The service must use fixed limits for at least:

- encoded name length and label count;
- response size;
- compression-pointer traversal;
- CNAME depth;
- answer and additional-record count;
- concurrent requests per caller;
- cache entries and bytes;
- retries and elapsed time.

Malformed and oversized responses fail closed. Cache entries retain their
source, validation status, expiry, namespace, and policy context so results
cannot cross security boundaries accidentally.

The DNS wire parser is a strong candidate for RecordFlux and SPARK. Parsing,
cache bookkeeping, and request-state transitions should be separated so each
can carry useful invariants rather than relying on defensive guards scattered
through the service loop.

## Observability

The Workbench and future security-posture application should be able to answer:

- What name was requested?
- Who requested it and under which declared authority?
- When was it requested, answered, expired, retried, or cancelled?
- Where was the query sent and which namespace/provider answered it?
- Why was that provider and policy selected?
- Which addresses were returned, rejected, or attempted?
- Was the result cached, authenticated, truncated, or policy-modified?

Normal applications should see only the minimum typed result. Diagnostic
visibility is a separate authority and must not become an ambient information
leak.

## Migration Plan

1. Harden the current `netstack.svc` resolver enough for HTTP integration:
   bounded record traversal, CNAMEs, explicit failures, retries, timeouts, and
   cancellation.
2. Define typed resolution and connected-endpoint IPC metadata in the shared
   network protocol.
3. Extract parsing and request state into independently testable packages.
4. Introduce `resolver.svc` with classic DNS and a bounded cache.
5. Move upstream-selection policy and authenticated DNS into the resolver
   boundary while keeping connection authorization in `netstack.svc`.
6. Add RecordFlux/SPARK verification and adversarial packet corpora before the
   resolver is treated as a hardened core service.

## Non-Goals

- Reproducing NSS configuration or module loading.
- Providing ambient POSIX resolver APIs as the native CuBit model.
- Letting applications read system-wide resolver secrets or configuration.
- Treating a successful DNS answer as permission to connect.
- Hiding namespace selection, search paths, or fallback behavior from security
  observability.
