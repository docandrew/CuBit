# CCL Package and System Configuration

Status: design proposal, with a Linux-hosted ELF manifest compiler and native
userspace CCL boot-configuration evaluation and initial checked image profiles

The CuBit Control Language (CCL) is also CuBit's package-description and system
configuration language. It should provide the reproducibility and composability
associated with Nix and Guix without importing Unix shell semantics, ambient
filesystem access, or an untyped configuration language into CuBit.

> Evaluating a description produces a plan. It does not perform the plan or
> grant the authority requested by it.

The language and type system are specified in
[`control-language.md`](control-language.md). Runtime authority and
explainability are specified in [`security-model.md`](security-model.md).
Remote provisioning carriers, management sessions, staged activation, and
recovery are explored in [`remote-management.md`](remote-management.md).

The [unified typed-document roadmap](ccl-unified-documents-roadmap.md) makes the
language-convergence goal explicit: ordinary CCL constructs typed descriptions;
separately authorized consumers realize or activate them. Current declaration
parsers are transitional, not a second permanent language.

## Goals

### Implemented first slice: executable manifests

The Linux-hosted `ccl-manifest` tool now generates `.cubit.id`, `.cubit.caps`,
`.cubit.access`, and `.cubit.streams` from checked declarations. In addition to
`ccl-vm`, 28 Ada apps/services now use it instead of `manifest.c`, including
Workbench, Files, Clock, Desktop, Display, and the IPC test apps. All C manifest
inputs in the Ada app/service directories are gone, including the old shell's
separate access manifest. Their authority bytes are preserved; three reviewed
identity corrections are detailed below. C copies remain only as test fixtures.
The Linux SDL preview's C window adapter and C application ports are outside
this manifest migration.
This is a bounded declaration profile using the existing CCL expression
evaluator for scalar/string fields, **not yet general first-class Package
values**, package-construction functions, plain-language syntax, a build-graph
executor, or an ISO builder. Those remain the direction below.

```lisp
(executable-manifest v1
  (identity "com.cubit.ccl-vm")
  (version "0.1.0")
  (request-service ccl-test-host read-write test-host)
  (request-service clock read-write clock))
```

The leading `1` identifies the declaration format, not the application's
version. Identity/version are required once each, nonempty, at most 64 bytes,
and currently limited to ASCII letters, digits, dot, hyphen, and underscore.
Field values use normal CCL strings, arithmetic, `let`, `if`, `concat`, etc.
`#` comments have the same meaning as in ordinary CCL. Service names, rights,
and local binding names are declaration symbols, not evaluated strings or
runtime handles. The last argument names the application's binding, not a
numeric capability slot. Binding names are unique lowercase kebab identifiers
(letters/digits, starting with a letter, no consecutive or trailing hyphens).

Service definitions are a separate explicit build input:

```lisp
(service-catalog v1
  (application-slots 24 62)
  (service ccl-test-host 18 read-write)
  (service clock 19 read-write))
```

The compiler does not know Desktop, Filesystem, Clock, or any other particular
service. `userspace/ccl/catalogs/bootstrap-services.ccl` supplies today's
transitional name-to-registered-service-ID bindings and offered endpoint rights.
A new named service requires catalog data, not a compiler change. Bindings
must have unique names and IDs. Requested endpoint rights must be contained in
the catalog entry's offered rights. `read`, `write`, and `read-write` describe
kernel endpoint transport rights, **not** filesystem or other domain permissions.

The catalog also supplies exactly one allocatable application-slot range,
within the kernel's usable slots 1–62. The bootstrap profile uses 24–62 to avoid
existing runtime conventions. Slots are assigned in request declaration order;
exhaustion is a compilation error, never wraparound or silent omission.
The range is build-supplied ABI layout data, not authority granted to the app.
Numeric destination-slot arguments are no longer accepted in app declarations.

`native-runtime-services.ccl` additionally supports `(fixed-binding NAME SLOT)`
for existing shared-runtime ABI conventions, such as Filesystem at slot 1.
These are catalog declarations, not numeric arguments in individual apps.
All catalog fixed slots are reserved before automatic allocation, regardless
of request order. Alternative client bindings may name the same slot, but an
executable requesting both is rejected. This is transitional ABI layout, not
an approval rule; private slot literals in Files and the IPC clients now use
generated Ada bindings, while common runtime APIs retain their existing slots.

The optional `--ada-output PATH` writes `CCL_Manifest_Bindings` in the same
compiler invocation that generates the ELF section assembly. For this app it
contains `Slot_Test_Host` and `Slot_Clock`. Hyphens become underscores with a
`Slot_` prefix; the restricted naming grammar prevents Ada-name collisions and
source injection. `ccl-vm` builds against this generated package, so changing
the slot range or request order updates both code and metadata. Generated files
live under the ignored application build directory, not checked-in source.

The catalog is supplied by the build/system composer. It is neither discovered
from live services nor accepted as proof of publisher identity, approval, or
provider authenticity. At this stage it is checked compatibility data, not a
complete typed IPC schema. Its role IDs must agree with the composed system.
Future service packages should produce these definitions and richer schemas as
explicit pinned outputs; the language must not acquire service-specific builtins.

The compiler understands the kernel manifest wire format; catalogs define
service-specific bindings. The intended dependency graph starts at kernel
bootstrap authority and continues through service-defined operations to apps.
Defining an operation does not create the authority needed to implement it.
There is no universal root user or permanent superuser implied by this graph.

For now the embedded ELF remains the runtime request declaration, and the
existing process-manager launch path provisions capabilities subject to its
current checks. We are **not** waiting for a new policy engine or asserting
that a complete installation-policy engine exists. A later broker can narrow
or defer the same requests without replacing the package language. Build-time
validation does not itself admit an executable or grant anything.

Evaluation receives no host adapter or visible runtime services. It cannot
fetch sources, invoke tools, read ambient files, query a live clock, or change
policy. Only the hosted wrapper reads the two explicitly named input files
and writes generated assembly to stdout plus the explicitly requested Ada
bindings file. Both outputs derive from the same checked result. Metadata is emitted only after both
inputs validate; unknown fields are errors, not silently ignored extensions.
The assembler produces a relocatable object linked into the individual ELF.

Filesystem scopes preserve the existing service's path-based request metadata:

```lisp
(executable-manifest v1
  (identity "com.cubit.example")
  (version "0.1.0")
  (request-service filesystem read-write filesystem)
  (filesystem-scope (rights read write create) "@mem:0/work")
  (stream stdout text 4))
```

Filesystem rights are `read`, `write`, `execute`, and `create`, distinct from
endpoint transport rights. Paths are preserved exactly, never silently
normalized. The profile rejects control/non-ASCII characters, wildcards,
backslashes, repeated separators, and dot/dot-dot components. It does not prove
that a path exists or that runtime policy will approve access. Stream names are
the existing `stdout`, `stderr`, and `log` ABI entries; formats are `text` or
`raw-bytes`, with 1–256 pages. These describe native CuBit streams, not a TTY.

`(config-scope (rights read write) "some.prefix")` emits the existing Config
access-domain entry. These two domain names reflect today's access wire ABI;
they are not a new service discovery mechanism. Config scopes accept only
`read` and `write`. A broad scope must say `all`,
for example `(filesystem-scope (rights read) all)`. Empty strings are rejected
and cannot accidentally mean broad access. The old shell explicitly requests
filesystem read-all and Config read/write-all, preserving its existing bytes;
this is not a recommended default for ordinary apps or an approved policy.

Network scopes reuse `CuBit.Network_Authority.Valid` and `Descriptor` from the
runtime's pure SPARK package; the host tool does not duplicate those rules:

```lisp
(request-network tcp-connect (ipv4 "0.0.0.0" 0)
  (ports 80 443) (dns allow) (connections 16) network)
(request-network tcp-listen (ipv4 "10.0.2.15" 32)
  (ports 8080 8080) (dns deny) (connections 4) listener)
```

The last symbol is the generated local binding name. Ports denote an inclusive
range (80–443 in the first example, not just those two ports). IPv4 strings use
canonical dotted decimal; host bits in a network prefix, leading-zero octets,
ambiguous shorthand, and out-of-range values are errors. Connect supports
CIDR/port ranges and explicit DNS permission. Listen currently requires one
nonzero unicast address with prefix 32, one port, and no DNS permission.
An outbound-any request is `"0.0.0.0" 0`, ports `1 65535`; launch approval is
still independently required. `(connections N)` (1–32767, required) is how
many channels the program may hold open at once under that request, counting
outbound streams, datagram channels and accepted streams. netstack reserves
them when it installs the scope and refuses a scope that would overcommit its
capacity, so no program can exhaust channels another was promised. The network-check and ccl-control restrictions
have not been broadened by migration.

The catalog can declare `(notification keyboard 1 publish-and-manage)`.
Apps use `(request-notification keyboard publish keyboard)` or
`(request-notification keyboard manage keyboard)`. `publish` maps to the legacy
notification read bit (send events to the registered role); `manage` maps to
the write bit (registration/focus). Requests cannot exceed the catalog's
offered operation set. Role IDs remain catalog data, currently bounded to 1–17
by the process manager's notification ABI. Service and notification kinds
cannot be substituted for one another merely because an ID matches.

`(request-framebuffer read-write framebuffer)` declares the existing bootstrap
framebuffer request with a named slot; it is not a normal GUI application's
window API. This primitive and network-scope encoding are ABI concepts, not
compiler knowledge of a particular provider identity.

An identity-only declaration omits `.cubit.caps`. Use `(requests-none)` to emit
an explicit zero-request header when required by an existing executable's
metadata; it cannot be combined with requests. Empty scope/stream sections are
omitted as well.

Current bounds: 4096 bytes per input, 32 catalog service/notification entries,
32 fixed bindings, 32 total requests, 16 access scopes of at most 64 bytes each, three
distinct streams, and at most 2048 bytes per generated section. Ordinary CCL
expression size/nesting bounds apply, with 1024 fuel per field. Service IDs are
positive 32-bit values. No raw-capability escape hatch is accepted. Other
resource/hardware forms not needed by these manifests remain unsupported and
must not be silently lost when future users require them.

Three deliberate identity changes are separately asserted against the old
fixtures: Devices' declared identity length is corrected from 19 to 17;
network-check gains `com.cubit.network-check` / `0.1.0`; and ccl-control retains
its identity and gains version `0.1.0`. All other section bytes remain exact.
The process manager currently returns the first identity value before checking
the remaining TLVs, which explains how the malformed Devices identity could
reach the runtime. Full identity-section validation remains a hardening task;
compiler-side validation is not a substitute for distrust of arbitrary ELFs.

Full package build graphs and per-target ISO composition remain future work;
these declarations do not yet build an ISO.

Build and test from the repository root:

```sh
nix develop -c make -C kernel test-ccl-manifests ccl-migrated-manifests
nix develop -c bash tests/headless/run.sh --test ccl-vm --accel kvm --timeout 35 --keep-logs
```

`test-ccl-manifests` builds the Linux tool and native executable, compares both
generated sections with the retained legacy-C test fixture and the linked ELF,
and exercises expression evaluation, data-driven service bindings, named-slot
generation, layout/order changes, exhaustion, invalid binding names, rights
restriction, duplicate rejection, malformed input, and bounds. The pure
frontend is SPARK-mode Ada with no assumptions or SPARK-off regions; this first
slice has executable regression coverage, **not a completed GNATprove proof**.
The Linux tool enables checks/assertions; it is not linked into the kernel or
the native application.

`ccl-migrated-manifests` independently compiles the 28 retained C fixtures and
their CCL replacements, then compares every `.cubit.*` section for exact
membership and bytes, with explicit tests for the three identity updates. The
shell's additional access fixture is included. The expanded compiler suite also checks fixed-slot
collisions/reservations, scope encoding and rejection, stream encoding and
rejection, network/notification validation, and explicit-empty versus absent
capability sections. All 28 native targets built in Nix. To additionally compare
their already-built linked ELFs, run:

```sh
nix develop -c bash -lc 'cd kernel && alr exec -- python3 ../tests/ccl-manifests/test-migrations.py --linked'
```

These tests do not replace formal verification or hardware testing, and do not
rebuild the laptop ISO.

### Broader package-system goals

The package system should:

* describe packages, builds, deployments, configuration, services, and requested
  authority as strongly typed CCL values;
* support approachable plain-language syntax and Lisp syntax;
* evaluate descriptions deterministically without ambient effects;
* produce content-addressed, reproducible results where inputs permit it;
* express dependencies as typed interfaces rather than paths or executable
  names;
* keep package provenance separate from permission to install or execute it;
* generate existing CuBit ELF metadata from checked package values;
* support atomic deployment, rollback, comparison, and garbage collection; and
* make every realization and activation decision explainable.

It is not a POSIX package manager, a privileged installation-script runner, a
mutable global namespace, or a mechanism through which authors grant their own
software authority. Compatibility builders may exist as explicitly confined
boundaries, but they are not the native model.

## Two syntaxes, one language

Both surface forms elaborate to the same typed CCL core. They must not develop
different features or security semantics.

Plain-language form:

```ccl
package sparktls
  version "0.8.0"
  source git "https://example.invalid/sparktls" revision "8ac3..."
  build with ada-builder
    project "sparktls.gpr"
    profile release
  provides service tls.gateway version 1
  requires service network.transport version 1
  authority
    network listen ports [443]
    secret use "gateway-key" for tls-signing
    config read "tls.gateway"
  resources
    memory 64 MiB
    cpu normal
```

Equivalent Lisp form:

```lisp
(package sparktls
  (version "0.8.0")
  (source
    (git "https://example.invalid/sparktls"
         (revision "8ac3...")))
  (build
    (with ada-builder)
    (project "sparktls.gpr")
    (profile release))
  (provides (service tls.gateway (version 1)))
  (requires (service network.transport (version 1)))
  (authority
    (network (listen (ports 443)))
    (secret (use "gateway-key" tls-signing))
    (config (read "tls.gateway")))
  (resources
    (memory (MiB 64))
    (cpu normal)))
```

Formatting tools may render either syntax. Source spans and expansion history
survive elaboration so authority explanations can identify the exact declaration
and package-construction function that produced a request.

## Core values and plain-language names

* **Package**: immutable description of software and its requirements;
* **Source**: authenticated or hashed input material;
* **Build Plan**: typed steps and tools used to produce artifacts;
* **Artifact**: immutable output of a completed build;
* **Deployment**: composed packages, services, configuration, and policy
  requests;
* **Realization**: content-addressed result of carrying out a plan;
* **Policy Profile**: local decisions about requested authority and resources;
* **Activation**: switching selected realized objects into service.

A package is an ordinary immutable CCL value:

```text
Package {
  identity, version, sources, build_plan, artifacts,
  provided_interfaces, required_interfaces,
  configuration_schema, requested_authority,
  declared_streams, resource_request, provenance_requirements
}
```

Package-construction functions provide Guix-like composition without requiring
unrestricted macros:

```lisp
(define (ada-service name project interface)
  (package name
    (build (with ada-builder) (project project) (profile release))
    (provides (service interface (version 1)))
    (resources (cpu normal))))
```

Typed functions are the default because they retain predictable evaluation,
diagnostics, and proof obligations. Bounded typed macros may be considered
later.

## Description and execution are separate

Package evaluation is pure. It may construct values, validate schemas, resolve
supplied inputs, calculate hashes, and produce a proposed deployment. It may not
fetch from the network, read ambient files, use secrets, install objects, start
processes, or change policy.

```text
CCL source
   | pure, deterministic, fuel-bounded evaluation
   v
typed Package / Build Plan / Deployment values
   | validation, dependency resolution, policy preview
   v
immutable proposed plan
   | explicit authority held by package services
   v
fetch -> build -> verify -> realize -> approve -> activate
```

Effectful work is performed by isolated services through typed IPC:

* a source service fetches or imports content;
* a build service realizes a `Build_Plan` in a confined process;
* a trust service verifies digests, signatures, and provenance;
* a package store retains immutable artifacts;
* a policy service evaluates requested authority against local policy; and
* an activation service starts or switches deployments.

These responsibilities may initially share processes, but their protocols and
authority boundaries remain distinct.

## Native CuBit builds

A native plan invokes typed builder interfaces, not command strings:

```ccl
build with ada-builder
  project "sparktls.gpr"
  inputs [source, spark-runtime]
  outputs [service "sparktls.svc"]
  profile release
```

The builder receives only declared immutable inputs, a bounded writable output
area, resource quotas, and explicitly approved build-time effects. It does not
inherit package-manager network, secret, device, process, or store authority.
Build-time and runtime authority are separate.

## Typed dependencies

Packages depend primarily on interfaces and schemas:

```ccl
requires service network.transport version 1
requires stream audit.events schema AuditEvent version 2
provides service tls.gateway version 1
```

Resolution checks compatibility before activation. A package name alone is not
evidence that an implementation provides the expected protocol. Cycles are
rejected unless each edge participates in an explicitly supported late-binding
protocol. Remote dependencies retain visible authentication, latency, timeout,
and partial-failure semantics.

## Authority and installation policy

A package's authority block is a request, not a grant. Only local installation,
launch, and session policy can approve it.

```text
declared -> requested -> approved -> installed -> effective -> exercised
```

Before activation, the security-posture application and the Workbench show:

* which expression requested the operation;
* content digest and authenticated publisher, when available;
* affected service, stream, configuration scope, secret purpose, device, or
  remote node;
* the local policy rule that approved or denied it;
* any compatibility or bootstrap exception;
* kernel capabilities and service ACLs to be installed; and
* the impact of changing the decision.

Authority is not inferred merely from a dependency. Depending on a network
interface does not approve every operation offered by that service.

### Filesystem policy and installation identity

See [filesystem maturity and policy implementation](filesystem-maturity.md#fs-policy-implementation-and-package-lifecycle)
for the proposed implementation sequence. These rules are design obligations,
not assertions that the current launch path already enforces them all:

* Keep the artifact digest, admitted publisher/application identity, local
  installation instance, and running process generation distinct. Policy must
  not key durable data access by a PID, mutable pathname, or display name.
* Bind private app data to the stable installation identity, not the version
  digest. An update can preserve that root without inheriting additional rights.
  Reinstall, publisher changes, and signing-key rotation need explicit identity
  continuity decisions. An ELF identity string alone cannot make those decisions.
* Separate immutable package roots, confined build output, private app data,
  selected user documents, and the active-deployment record. Each is supplied
  through the existing typed root/file handle model with its own rights.
* Intersect manifest requests with approved installation policy. Then issue
  concrete handles within that ceiling. Optional approval eligibility is not
  a startup grant. A trusted signature never bypasses the intersection.
* Preview new authority requests on upgrade. Installing an artifact does not
  approve those requests, and approving them does not activate the artifact.
* Run migrations with only explicitly supplied old/new data roots and bounded
  resources. Do not introduce globally privileged package scripts. Report
  irreversible effects and accepted noncancelable work explicitly.
* Rolling back executable versions does not roll back data. Retained data
  generations or an explicit schema-compatibility decision are required.
* Store finalization and deployment activation need a durable transaction
  boundary. The current non-overwriting, single-block ext2 rename does not
  provide atomic replacement or a power-loss-safe deployment switch.

## Mapping to CuBit today

| Package value | Current mechanism |
|---|---|
| identity metadata | `.cubit.id` ELF section |
| requested kernel authority | `.cubit.caps`, interpreted by `procmgr` |
| declared streams | `.cubit.streams` and stream-service policy |
| filesystem/configuration scope | `.cubit.access` and service ACLs |
| memory and CPU request | `CAP_RESOURCE` and configured quotas |
| service dependency | registered service plus endpoint capability |
| runtime artifact | application, service, or driver ELF object |

This mapping is transitional. `.cubit.id` is metadata rather than authenticated
provenance, and `procmgr` still contains compatibility and hard-coded identity
policy. Package tooling must expose those gaps rather than imply that generating
a manifest solves them.

## Reproducibility and content addressing

A realization identity covers every input that can influence output:

* source, builder, and toolchain digests;
* the build-plan value and CCL version;
* target architecture and declared hardware assumptions;
* supplied configuration inputs;
* interface and schema versions; and
* permitted build effects and captured results.

Wall-clock time, ambient environment variables, mutable paths, undeclared
network results, and host filesystem state are unavailable to native builders.
Non-reproducible inputs are recorded honestly rather than hidden.

Signatures establish provenance and trust evidence; they never grant runtime
authority. Local policy separately decides whether the artifact may be installed
and what it may do when activated.

## Deployments, activation, and rollback

Deployments are immutable graphs. Realizing a deployment does not mutate the
active one. Activation is an explicit transition with a previewable diff:

```text
realized -> validated -> policy-approved -> staged -> active -> retired
```

Activation defines service ordering, readiness checks, failure behavior, state
migration, and rollback safety. Rolling back code cannot undo consumed secrets,
external actions, persistent-data changes, or accepted non-cancellable work.

Garbage collection removes only unreachable realizations after checking active
deployments, rollback roots, debugging retention, build dependencies, and open
handles. Losing a friendly name is not proof that an object is unreachable.

## Configuration and secrets

Packages declare typed configuration schemas and defaults. Deployments bind
values to them; packages do not conventionally read global text files or
environment variables.

Secret declarations contain identity and purpose, never contents:

```ccl
secret use "gateway-key" for tls-signing
```

Activation supplies a secret-use handle through policy. Package values, build
caches, logs, and explanations never contain the private value. Build-time
secret use is exceptional and separately authorized.

## Workbench experience

The CCL Workbench should:

* edit either syntax and show the equivalent structured form;
* evaluate untrusted descriptions without installing them;
* inspect typed values, dependency graphs, and source spans;
* compare deployments by artifact, configuration, authority, resources, and
  service relationships;
* simulate policy and explain denials before activation;
* display bounded structured build progress;
* inspect active and inactive realizations; and
* realize or activate only through separately supplied handles.

This is a natural first substantial use of CCL UI widgets: dependency graphs,
authority-stage tables, deployment diffs, build timelines, and explicit
activation controls exercise the same UI and observability protocols CuBit needs
elsewhere.

## Bounded evaluation and verification

Evaluation has explicit fuel, memory, recursion, collection, and output limits.
Exceeding a limit returns a typed diagnostic and no partial plan. Large solving
or scheduling tasks may be delegated to a typed, bounded SPARK service; CCL
remains the description language rather than an unbounded privileged daemon.

Verification targets include:

* both syntaxes elaborate to the same typed package value;
* evaluation cannot perform undeclared effects;
* package functions cannot forge authority or artifact identities;
* realization identities cover every declared influential input;
* generated ELF metadata cannot exceed checked requested authority;
* policy approval cannot exceed its approving authority;
* builders cannot access undeclared inputs or escape output allocations;
* bounded evaluation and protocol queues cannot overflow silently; and
* inspection cannot amplify installation, activation, or policy authority.

## Proposed implementation sequence

### Implemented first slice: checked system-image plans

Keep three separate inputs: the executable's embedded authority requests,
the image's artifact placement/startup plan, and the independent launch-policy
approval. Inclusion on a CD is neither approval to run nor an authority grant.

The first image profiles preserve the existing boot contents:

* normal two-stage QEMU desktop (bootstrap initrd plus writable ext2 content);
* self-contained laptop initrd fallback; and
* USB optical laptop image (minimal bootstrap, ISO9660 application payload,
  and a seed ext2 RAM filesystem).

A bounded pure CCL image declaration should produce a checked plan before any
Linux tool runs. Name pinned artifacts, boot-stage placement, destination paths,
init/system configuration inputs, and entry points. Reject duplicate output
paths, traversal, missing boot dependencies, and incompatible provider catalogs.
Bootstrap placement must close over the drivers/services needed to read stage
two, or the image can never finish loading itself. Keep test publishers and
development-only plaintext listeners out of ordinary image profiles.

Initially a Linux realization adapter can drive the existing archive, ext2,
and GRUB tools using explicit plan fields, not CCL-generated shell commands.
Do not execute arbitrary declaration text or claim hermetic builds before
builder isolation exists. Private ROMs remain explicit local inputs, excluded
from version control and distributable image profiles. Test both plan rejection
and the actual ISO/initrd contents, then boot each image variant in QEMU.

The first implementation is now in [images/README.md](../images/README.md):
a pure Ada/SPARK-mode planner resolves CCL artifact catalogs and image profiles;
a Linux adapter snapshots/hashes inputs and verifies resulting CPIO/ISO bytes.
Normal and fallback initrd membership and the USB optical image contents use
these profiles. Development ext2 payloads, outer fallback GRUB staging, and
source build-target dependencies remain in Make. This is not yet a hermetic
CCL build graph, signed store, or completed SPARK proof.

### Longer-term package values and realization

The [native boot-configuration frontend](ccl-boot-configuration.md) now evaluates
`system.ccl` and `init.ccl` in userspace, using the same pure CCL evaluator as
the Linux preflight tool. Image membership now has its own checked CCL frontend;
general build graphs remain follow-on work.

1. Define pure `Package`, `Source`, `Build_Plan`, `Artifact`, `Deployment`, and
   `Policy_Profile` CCL types.
2. Parse equivalent minimal declarations in plain and Lisp syntax.
3. Canonically encode and hash evaluated package values.
4. Generate `.cubit.id`, `.cubit.caps`, `.cubit.streams`, `.cubit.access`, and
   resource metadata from a checked package.
5. Add a Workbench inspector with dependency and authority previews.
6. Define typed source, build, store, trust, policy, and activation protocols.
7. Realize a small native package with an isolated Ada/SPARK builder.
8. Add immutable deployments, activation diffs, rollback roots, and safe garbage
   collection.
9. Integrate decisions with the shared security-observability records.
10. Prove pure evaluation, canonical encoding, metadata non-amplification, and
    bounded package-service state machines.
