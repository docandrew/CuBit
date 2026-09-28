# CuBit Remote Management and Provisioning

Status: brainstorming document; no wire protocol or activation policy is
stable.

Implementation checkpoint: the loopback-only Observatory lab now exchanges
bounded HTTP/CBOR messages directly with a native CuBit control app, evaluates
pure fuel-bounded CCL, and observes its own clock/network bindings. This is not
yet authenticated remote management or full service discovery. The browser and
native smoke tests use real results, not a Linux evaluator or relay. See
`userspace/ccl/tools/ccl-observatory/README.md` for scope and reproduction.

CuBit should make declarative, reproducible management the ordinary path for
servers and cloud infrastructure. It should still permit useful diagnosis and
recovery without recreating remote login accounts, privileged shells, PTYs, or
an ambient `root` session.

The design tension is deliberate:

* server fleets should normally be cattle described by desired state;
* operators still need understandable, responsive tools when systems fail;
* unfamiliar security machinery must not force an unnecessarily strange user
  experience; and
* authentication must never silently become unrestricted machine authority.

The typed protocol is enforcement machinery, not a demand that every operator
write Lisp or manipulate raw messages. A Workbench, web console, automation
controller, and familiar command-oriented client can present normal status
tables, configuration forms, diffs, buttons, and concise commands over the same
typed operations.

## Goals

* Provision a node from an immutable image plus signed declarative state.
* Describe init services, configuration, packages, policy, and requested
  authority as checked values.
* Keep configuration separate from secrets and both separate from authority.
* Authenticate machines and management principals without Unix user accounts.
* Give each session only a filtered catalog and exact handles selected by local
  policy.
* Make staging, approval, activation, reboot, rollback, and observation
  separately authorizable and explainable.
* Support slow links through structured deltas and event streams rather than
  terminal or framebuffer repainting.
* Retain and resolve asynchronous obligations when a client disconnects.
* Provide useful air-gapped and physical-recovery workflows.

## Non-goals

The native management plane is not:

* SSH with a different port number;
* a remotely accessible interpreter holding all system authority;
* a PTY, stdin/stdout byte stream, terminal emulator, or login shell;
* arbitrary remote reads and writes to `config.svc`;
* execution of cloud-init shell fragments; or
* serialization of kernel capabilities, local handles, reply capabilities, or
  raw memory grants.

Compatibility gateways may exist as confined adapters, but they must expose a
narrow native protocol and must not define CuBit's internal security model.

## Desired-state path

The normal server workflow should be:

```text
CCL package and deployment source
        |
        | pure, bounded evaluation
        v
typed desired Deployment
        |
        | canonical encoding, signature and policy validation
        v
proposed generation
        |
        | diff, approval and realization
        v
staged generation
        |
        | separately authorized activation
        v
active generation
```

Evaluation produces a plan; it does not apply that plan or grant its requested
authority. Active state identifies the exact generation, schemas, artifact
digests, policy digest, and provenance evidence that produced it.

Routine fleet management replaces nodes or reconciles them to checked desired
state. Interactive mutation remains possible only through explicitly exported
operations and is visible as drift from the declared generation.

## Staging and activation models

The protocol should support policy choices instead of baking one operational
style into every installation.

### Direct scoped activation

An authorized controller may validate, stage, and activate a generation
atomically. This is convenient for development and controlled automated fleets,
but its activation authority must be explicit and narrow.

### Separate proposer and activator

One principal may upload or select the next configuration while another holds
`Activate<Deployment>` authority. This supports review, two-person control,
change windows, and separation between CI and production operations.

### Activate at next boot

A principal may stage an immutable boot candidate but may not activate it. A
distinct actor selects that exact generation and requests a reboot. Generic
reboot authority must not accidentally activate whichever mutable value happens
to be marked "next"; boot authority should be bound to an inspected generation
or explicit policy rule.

### Pull and reconcile

A node may fetch a signed desired-state reference, validate it locally, and
stage or activate it according to local policy. A compromised controller must
not override node trust roots, authority ceilings, or rollback rules.

Open policy questions include whether production permits live activation,
requires a separate approval handle, requires physical presence for selected
changes, or permits only stage-plus-reboot.

## Provisioning envelope

A native provisioning envelope is typed data, not an executable startup
script. Possible carriers include cloud metadata, a config drive, QEMU
`fw_cfg`, an enrollment service, serial recovery, and signed removable media.

The carrier is not trusted merely because a cloud platform supplied it. A
provisioning service validates the envelope's signature, target node or class,
schema version, generation, size bounds, freshness or replay state, and local
policy before producing a proposed deployment.

An envelope may contain:

* desired deployment identity or a complete bounded description;
* enabled init services and dependency bindings;
* typed configuration values;
* trust roots and management-policy proposals;
* a one-time enrollment credential; and
* encrypted or node-sealed secret imports.

Secret declarations identify a secret and permitted purpose. Material enters
the secret service through a dedicated import protocol and never becomes
ordinary CCL text, configuration, logs, diagnostics, or build input.

A cloud-init compatibility adapter may translate a deliberately small
declarative subset. It should reject shell commands, `runcmd`, arbitrary file
mutation, and imperative escape hatches rather than quietly granting equivalent
power.

## Management sessions

```text
native Workbench    web console    automation controller
        \                |                 /
         +------- typed management protocol -------+
                             |
                       mutual SPARKTLS
                             |
                   management-gateway.svc
              authentication, framing and quotas
                             |
                 authenticated remote principal
                  plus requested session profile
                             |
                    management-broker.svc
                    local policy and approval
                             |
             filtered catalog plus exact handles
                             |
             isolated CCL session / plan evaluator
                             |
                         typed IPC
                             |
                       CuBit services
```

Mutual TLS establishes peer identity and channel security. Local policy decides
which interface descriptions the peer may see and which operations receive
usable session handles. Discovery, compilation, linkage, invocation, and
approval remain different observable states.

A principal may be a controller certificate, organization key, signed
delegation, hardware-backed administrator key, or node identity. It is not a
Unix account: it has no UID, home directory, inherited environment, or login
shell. A policy profile is a recipe for minting exact session authority, not an
ambient role bit.

Example profiles might permit observing selected health data, operating one
deployment, proposing configuration in one namespace, staging signed packages,
activating one inspected generation, or performing time-limited recovery.

## Typed exchange, familiar clients

The wire protocol carries bounded, versioned messages rather than terminal
bytes. Candidate operations include:

```text
OpenSession                 GetVisibleInterfaces
AnalyzeSource               CompileSource
Evaluate                    InspectValue
ProposeConfiguration        PreviewDeploymentDiff
StageDeployment             ApproveAndActivate
SubscribeEvents             CancelTask
DetachTask                  ReattachTask
CloseSession
```

Messages carry typed values, diagnostics, source spans, explanations, progress
events, and operation handles. A target accepting CCLB independently decodes,
verifies, resource-checks, and links it against the session's exact grants. It
does not trust compilation performed by the client.

This need not feel exotic. Clients may provide a graphical Workbench; an HTTPS
console with forms and diffs; a concise local client such as `cubit status` or
`cubit deployment stage`; declarative CI integration; or a recovery REPL. The
REPL is one presentation of a management session, not the network protocol or
the source of its authority.

## Configuration transactions

Remote management should not expose the current config service's administrative
key-value operations directly. A future broker issues a scoped transaction:

```text
ConfigTransaction<network>
    set typed values
    validate schema and expected generation
    preview dependent service changes
    commit | rollback | return
```

Transactions have bounded change sets and produce an immutable proposed
generation. Committing configuration does not imply authority to restart
services, activate a deployment, use referenced secrets, or reboot the system.

## Asynchronous work and disconnects

Remote calls expose latency, timeout, cancellation policy, authentication loss,
and partial failure. Some accepted operations are not cancellable. Closing a
socket cannot pretend accepted work disappeared or release moved and borrowed
resources early.

Long-running work returns a durable job or task handle. A client may observe,
request cancellation where supported, detach, and later reattach. The broker
retains bounded state to account for completion and applies a declared orphan
policy when the session expires.

## Web console and remote desktop

An HTTPS console is a useful zero-install client, especially for observation
and routine operations. It is an adapter to the same broker, not a second
privileged administration implementation. Browser authentication, request
forgery, script injection, origin policy, session binding, and cached secret
data require a separate threat model. A cookie or bearer token must not become
a transferable representation of broad machine authority.

Framebuffer sharing is optional and not the primary server-management path. A
desktop-sharing service should be view-only by default, prefer delegating one
surface over the whole desktop, require a separate input-injection handle, show
visible local indication where applicable, and enforce duration and bandwidth
limits. Input possession must not confer approval authority.

## Explainability

Every remote decision answers WHAT, WHO, WHEN, WHERE, and WHY: the exact typed
operation or generation; authenticated requester and approver; lifecycle times;
node, service, and namespace scope; and the policy rule that allowed or denied
it. Audit visibility is separately authorized and must not reveal secrets or
hidden interface descriptions.

## Candidate implementation sequence

1. Define typed local management operations and session state without a network
   transport.
2. Prototype a broker used by the Linux Workbench and a CuBit CCL process.
3. Generate bounded codecs and protocol validators from the same interface
   descriptions used for local IPC.
4. Add signed provisioning envelopes and proposed-generation inspection.
5. Add configuration transactions, staging, and activation-policy simulation.
6. Put the protocol behind a minimal SPARKTLS mutual-authentication gateway.
7. Add native Workbench and automation clients.
8. Add a separately confined and threatened HTTPS adapter.
9. Consider restricted surface sharing after display and input authority have
   suitable session types.

## Open questions

* Should production default to remote activation or staging only?
* Is next-boot selection configuration, activation, or a distinct must-handle
  boot transaction?
* Must reboot authority name the exact generation that will boot?
* Which changes require a second approver, local presence, or hardware key?
* How is break-glass authority minted, bounded, expired, and highlighted later?
* Which interactive mutations are permitted, and how is drift reconciled?
* Does a node accept source, verified CCLB, typed deployments, or all three
  under different policies?
* Which encoding best preserves CCL types, versions, bounded decoding, and
  SPARK verification?
* How are credentials enrolled, rotated, revoked, and recovered in cloud and
  air-gapped installations?
* What bounded state survives a disconnected session, and for how long?
* Which jobs are cancellable, detachable, resumable, or transferable?
* Can a web console authenticate strongly without browser bearer state becoming
  ambient authority?
* How much metadata may an observation-only peer discover?
* Which cloud-init constructs are safe enough to translate?
* How is freshness proven without a trusted wall clock during early boot?
* What is the smallest useful recovery surface when ordinary configuration,
  networking, or the management broker is broken?
