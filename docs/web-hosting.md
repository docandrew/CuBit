# CuBit Web Hosting

Status: design direction for the first production-shaped CuBit server
application. Implementation should begin after SPARKTLS can provide stable
stream termination and identity services.

## Purpose

Web hosting is a useful demonstration of CuBit's security model because it
combines hostile network input, cryptography, parsing, application logic,
storage, configuration, logging, overload control, and software updates. The
goal is not to reproduce a traditional UNIX web-server process. CuBit should
split those responsibilities across explicit authority boundaries and make
every delegation inspectable.

The initial system should host a real static site and small typed dynamic
services securely enough to exercise CuBit under realistic load. It should
also provide a credible account of what remains trusted, tested, or proved.

## Security Story

The principal claim is deliberately narrow:

> Untrusted network traffic terminates in a memory-safe, capability-confined
> service chain. TLS keys, site content, HTTP parsing, application logic, and
> audit storage occupy separate authority domains with explicit bounds.

No single component should receive all of the following:

- public network-listener authority;
- private keys or certificate-management authority;
- unrestricted access to hosted content;
- authority to invoke every application service;
- mutable audit storage; and
- package or system-management authority.

A defect in an HTTP handler must not disclose TLS private keys or unrelated
files. A defect in SPARKTLS must not grant access to site content or application
state. A compromised logging service must not control request routing. These
are enforced separations, not deployment conventions.

## Architecture

```text
                         certificate authority / operator
                                      |
                                      v
                              certificate service
                                      |
                                      | rotated identity handle
                                      v
Internet ---> netstack.svc ---> SPARKTLS gateway
                                      |
                                      | authenticated plaintext stream
                                      | + peer/connection metadata
                                      v
                                http.svc
                              /     |      \
                             /      |       \
                            v       v        v
                  static-content  router   audit events
                     service        |           |
                         |           v           v
                  read-only tree  typed app   logstore.svc
                                   services
```

The network-facing services communicate through typed IPC and bounded shared
streams. Bulk request and response bodies should not be copied through ordinary
register-sized IPC messages.

### SPARKTLS gateway

SPARKTLS owns:

- listener and accepted-connection handles;
- TLS protocol state and cryptographic policy;
- server identity handles and private-key use authority;
- peer certificate validation and optional mutual authentication;
- bounded plaintext input and output streams; and
- TLS-specific security events and negotiated connection metadata.

It does not own site content, application databases, routing policy, package
management, or arbitrary outbound network authority. Private keys should
remain behind a secret or signing service where practical; possessing a TLS
session must not imply possession of key material.

### HTTP service

`http.svc` consumes authenticated plaintext connection streams and performs
bounded HTTP framing, parsing, normalization, and routing. Its first version
should support:

- HTTP/1.1;
- `GET` and `HEAD`;
- a deliberately constrained `POST` path for typed handlers;
- fixed limits for request lines, header count, header bytes, body bytes,
  pipeline depth, connection count, and idle time;
- explicit handling of keep-alive and connection closure;
- typed parse and policy failures; and
- deterministic cleanup after timeout, cancellation, peer closure, or service
  failure.

The initial service should not implement CGI, shell execution, inherited
environment variables, arbitrary filesystem paths, loadable modules, or
in-process application plugins.

RecordFlux is a candidate for wire framing. A SPARK state machine should own
request lifecycle, bounds, connection accounting, and transition policy. Any
unavoidable non-SPARK transport boundary must be small and documented.

### Static-content service

Static content is exposed through a storage-session handle scoped to one
declared tree or immutable package artifact. The HTTP service should request
content by a normalized typed key, not concatenate a request path into an
ambient filesystem pathname.

The static service should provide:

- read-only access by default;
- bounded metadata and content streams;
- explicit media type and cache metadata;
- no traversal above the granted content root;
- immutable deployment generations where possible; and
- atomic switching between validated site generations.

### Dynamic application services

Dynamic handlers are separate processes or bounded CCL isolates. They receive
a typed request containing only the fields and body stream admitted by routing
policy. A handler returns a typed response and bounded body stream.

A handler does not automatically receive the client connection, TLS session,
listener, certificate, audit sink, filesystem, or outbound network handles.
Additional authority is declared by its package and granted by deployment
policy.

The router should bind paths and methods to typed interfaces. Schema or version
mismatch must fail before request delivery.

## Authority Model

The three-tier CuBit vocabulary applies directly:

1. A package permission allows a service to request a category of operation,
   such as accepting traffic through a named TLS gateway or reading a named
   content deployment.
2. Dynamic handles represent admitted listeners, connections, streams,
   content sessions, application sessions, and audit channels.
3. One-use reply authorities complete individual requests and transitions.

Illustrative permissions include:

- `network.listen` restricted to a declared endpoint;
- `tls.terminate` restricted to an identity and protocol policy;
- `content.read` restricted to a deployment identity;
- `http.route` restricted to a typed application interface;
- `audit.append` restricted to a bounded event schema; and
- `secret.use` restricted to a named signing operation, not secret export.

Handles should carry generation, schema, ownership mode, resource quota, and
provenance metadata. Closing or replacing a listener must not accidentally
authorize a stale connection or deployment handle.

## CCL Configuration

CCL should be the readable deployment and control language. Plain-language
syntax and the equivalent Lisp form must compile to the same typed package and
runtime configuration model.

An illustrative configuration is:

```text
WEB SERVICE documentation
    LISTEN THROUGH public-tls
    SERVE docs-release READONLY

    ROUTE GET  "/health" TO health-handler
    ROUTE POST "/search" TO search.v1

    LIMIT CONNECTIONS 256
    LIMIT REQUEST HEADERS 32
    LIMIT HEADER BYTES 32 KiB
    LIMIT REQUEST BODY 1 MiB
    LIMIT PIPELINE 4
    TIMEOUT IDLE 30 seconds

    LOG ACCESS TO web-access
    LOG SECURITY TO security-audit
END
```

CCL compilation must resolve every friendly name to a typed package,
permission, schema, or deployment identity. Configuration cannot mint
authority; installation and launch policy decide whether declared requests are
granted.

Live CCL controls may inspect health, drain a listener, activate a validated
content generation, or alter a bounded policy when the controlling session has
the corresponding authority. Changes must emit an explanation record.

## Resource and Overload Policy

Every externally influenced resource must be bounded:

- listeners and active connections;
- TLS handshakes and cryptographic work;
- header and body buffering;
- parser state and pipeline entries;
- outstanding handler calls;
- shared-memory grants and stream capacity;
- handler CPU, memory, and elapsed deadlines;
- queued response bytes; and
- audit-event rate and retained storage.

Admission should occur before scarce state is committed. Overload behavior
must be deterministic: reject, shed, defer, or close according to declared
policy. An unauthenticated client must not be able to exhaust unrelated system
services or prevent security events from being recorded.

Scheduling should preserve low and predictable latency without granting web
handlers ambient real-time priority. Priority inheritance may follow a bounded
request IPC chain and must end when that request completes.

## Observability and Explanation

Web hosting must participate in CuBit's WHAT-WHO-WHEN-WHERE-WHY model.
The future security-posture application and the CCL Workbench should expose:

- which package and policy created each listener;
- which SPARKTLS identity and policy admitted a connection;
- peer identity where authenticated, without leaking secret material;
- which route selected which handler and schema;
- every dynamic handle and its generation, owner, quota, and lifecycle state;
- request latency across TLS, HTTP, handler, storage, and response stages;
- rejections, timeouts, cancellations, overload decisions, and dropped audit
  records;
- certificate activation and rotation history; and
- proof, test, fuzzing, and trusted-boundary status for every component.

Useful CCL interactions include:

```text
SHOW WEB SERVICES
SHOW CONNECTIONS FOR documentation
WATCH REQUEST LATENCY FOR documentation
EXPLAIN REQUEST 1842
EXPLAIN TLS REJECTION 991
DRAIN WEB SERVICE documentation
```

Audit events should be structured and bounded. They must not contain request
bodies, credentials, cookies, authorization headers, private keys, or other
secrets by default.

## Verification and Testing

SPARK proof targets should include:

- absence of runtime errors in bounded parser and lifecycle cores;
- request and connection counters never exceeding admitted capacity;
- each accepted connection reaching exactly one terminal disposition;
- normalized routes never escaping their granted content namespace;
- one-use replies and ownership-bearing streams being consumed or returned
  exactly as declared;
- cancellation and timeout preserving handle and buffer ownership;
- handler selection requiring method, route, interface, and schema agreement;
- monotonic resource accounting; and
- certificate-generation changes never reusing stale identity handles.

Protocol and integration testing should include:

- adversarial and malformed HTTP corpora;
- request smuggling and ambiguous framing cases;
- slow clients and partial writes;
- pipelining, early close, cancellation, and duplicated completion races;
- large or excessive headers and bodies;
- TLS handshake and certificate failures;
- handler crashes and restarts;
- storage and audit-service unavailability;
- sustained overload and recovery; and
- fuzzing at every non-SPARK byte-to-type boundary.

Proof claims must distinguish proved properties from tests, reviews, and
assumptions. Netstack, SPARKTLS, compiler, kernel, cryptographic implementation,
hardware, and explicitly identified non-SPARK wrappers remain part of the
published trusted computing base according to their actual assurance status.

## Packaging and Supply Chain

Each deployed component should carry an assurance record containing:

- source and dependency hashes;
- compiler, builder, and proof-tool identities;
- reproducibility result and SBOM;
- requested and granted permissions;
- IPC and stream schema versions;
- proved properties and outstanding obligations;
- non-SPARK boundaries and their validation strategy;
- test and fuzzing results; and
- signing, installation, and activation provenance.

Content and application deployments should be immutable, validated packages.
Activation should atomically select a generation; rollback selects another
known generation rather than mutating a live tree.

## Initial Milestone

The first credible demonstration is intentionally small:

1. Terminate TLS 1.3 in SPARKTLS using one managed server identity.
2. Pass a bounded authenticated plaintext stream to `http.svc`.
3. Serve one immutable static-content package using `GET` and `HEAD`.
4. Route `/health` to one typed SPARK service.
5. Enforce fixed connection, header, body, and timeout budgets.
6. Emit structured access, denial, lifecycle, and latency events.
7. Display live authorities, handles, limits, and request explanations in CCL.
8. Demonstrate that crashing the HTTP or dynamic handler does not expose TLS
   keys, unrelated content, or system-management authority.

## Later Work

After the initial architecture is measured and hardened:

- mutual-TLS identities for administrative and node-to-node endpoints;
- HTTP/2 through a separately bounded protocol service;
- authenticated DNS and remote CuBit IPC through SPARKTLS;
- certificate automation, OCSP, and CRL policy;
- bounded reverse proxying and load balancing;
- multiple immutable sites and application deployments;
- transactional uploads through explicit write sessions;
- CCL-defined bounded adapters and event-driven handlers; and
- replicated configuration and deployment across mutually authenticated CuBit
  nodes.

These additions must preserve the authority separation of the initial design.
Compatibility pressure must not collapse TLS, HTTP, application execution,
storage, and management back into one privileged server process.
