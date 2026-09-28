# Typed desktop protocol

Status: incremental implementation, 2026-09-09. This is the application-facing
`desktop.svc` protocol, not the separate compositor-to-`display.svc` scanout
protocol. Neither protocol is being replaced with X11 or Wayland.

## Requirements

Security is primary. A surface name is not authority: endpoint admission,
kernel-authenticated caller identity and service-owned object state determine
access. Discovering an interface description is separately authorized. A schema
digest identifies an interface; it does not authenticate its publisher.

The performance target remains [the input latency contract](input-latency.md):
p99 below 1 ms from device interrupt/completion to focused-application delivery
under an admitted workload, followed by the first achievable display refresh.
This is a target, not a measured guarantee or a hard-real-time proof.

Protocol requirements for subsequent slices:

- Batch available work without waiting for a batch to fill.
- No mandatory synchronous round trip for each widget mutation or repaint.
- Bounded atomic commits, explicit rejection and backpressure; validate the
  whole transaction before publishing any of its changes.
- Small inline control messages; grants for bounded bulk content. Copy a bulk
  command description into bounded private storage before validating/applying
  it, unless the transport establishes enforceable immutability. A writable
  sender must not change commands between validation and use.
- Keep acceptance, buffer release and actual presentation distinct. Acceptance
  is not permission to overwrite a buffer still in use.
- Bound queued visual updates and coalesce superseded presentation work.
  Preserve ordered input transitions; report overflow and resynchronize.
- Keep pointer feedback, input delivery and other clients responsive while an
  application renders. Enforce per-client resource and scheduling budgets.
- Resolve schemas and bindings during setup, not on every frame. Generated
  bindings should encode directly into native IPC, without another relay.

Wayland's typed, versioned object interfaces and buffer commit/release model,
and X11/XCB's asynchronous requests, are useful precedents:
[Wayland model](https://wayland.freedesktop.org/docs/book/Protocol.html),
[Wayland protocol](https://wayland.freedesktop.org/docs/html/apa.html),
[X.Org communication model](https://www.x.org/guide/communication/).
CuBit's authority model and timing requirements remain its own.

## Implemented schema

`CuBit.Desktop_Protocol` is a pure SPARK unit with no IPC, addresses, allocation,
rendering or device dependencies. `CuBit.Desktop_Messages` converts between its
wire record and the native IPC message. Sender identity and authority tags are
deliberately not serialized into the portable schema.

The shared `Operation` enum fixes all thirteen existing desktop operation codes.
Service and toolkit numeric constants derive from that enum. Typed payloads and
checked decoding cover **Create_Surface**, **Present_Surface**, **Attach_Buffer**,
**Resize_Surface**, **Set_Window_Limits**, **Set_Pointer_Cursor**, and
**Destroy_Surface**, **Set_Window_Title**, **Hello**, **Goodbye**, and
**Get_Information**, **Poll_Input**, and **Wait_Input**. Input replies use a
checked event envelope; their service-side producers still use packed payloads.

| Operation | Code | Four-word request |
|---|---|---|
| Create_Surface | `0x0810` | width, height, surface kind, reserved zero |
| Present_Surface | `0x0812` | surface name, packed x/y, packed width/height, reserved zero |
| Attach_Buffer | `0x0814` | surface name, grant slot, grant generation, packed BGRA layout |
| Resize_Surface | `0x0813` | surface name, width, height, reserved zero |
| Set_Window_Limits | `0x0841` | surface name, packed minimum width/height, packed maximum width/height, feature bits |
| Set_Pointer_Cursor | `0x0815` | surface name, cursor style, reserved zero, reserved zero |
| Destroy_Surface | `0x0811` | surface name, reserved zero, reserved zero, reserved zero |
| Set_Window_Title | `0x0842` | surface name, title bytes 1..8, bytes 9..16, bytes 17..23 plus length in the high byte |
| Hello | `0x0800` | revision (major low 32 bits, minor high 32 bits), three reserved zero words |
| Goodbye | `0x0801` | four zero words |
| Get_Information | `0x0802` | four zero words |
| Poll_Input | `0x0821` | surface name, acknowledged serial, two reserved zero words |
| Wait_Input | `0x0822` | surface name, acknowledged serial, absolute monotonic-millisecond deadline (zero means indefinite), reserved zero |

All request decoders require exactly four words and zero message flags/reserved
header fields. Create/present/resize also require zero reserved payload. Coordinates and extents are bounded
to 0..65,535 before narrowing conversions. The packed pairs retain their
existing low/high 32-bit wire fields; values exceeding the semantic bounds
are rejected, not truncated.

DOOM and NetSurf's native adapters now declare four presentation words too;
their existing fourth syscall payload is already zero. Three-word presentations
are rejected rather than retained as an alternate layout. Note that the NetSurf
adapter lives under the repository's currently ignored `userspace/c/netsurf/`
directory and must be preserved separately with the other port sources.

Create width/height zero requests a compositor-chosen size. Surface kinds are
plain, shell and window, with representations 0, 1 and 2. Unknown kinds are
rejected. The formerly ignored parent word must be zero; the desktop-shell
client has been updated. This does not implement parent/child surface ownership.

Creation results are a discriminated `Creation_Result`:

- Success: four words `[nonzero surface, positive width, positive height,
  nonzero serial]`.
- Failure: two words `[0, status]`, with remaining words zero. Status is an
  enum: denied, bad object/state, invalid request, unsupported, or exhausted.

This fixes the old ambiguous failure `[3]`, which clients checking only for a
nonzero first word could mistake for a valid surface. The toolkit checks the
complete reply before installing a surface name. Existing success layouts are
unchanged; clients testing a zero name also recognize the new failure layout.

Present's all-zero rectangle means whole-surface damage. Otherwise both extents
must be positive; mixed zero extents and an empty rectangle at a nonzero origin
are invalid. Valid rectangles are clipped in local coordinates before being
translated into compositor coordinates. Wholly outside rectangles become empty.

The service checks surface existence and ownership before scheduling present
damage. This fixes a missing ownership check in that handler. The current
success reply still means **damage accepted**, not scanned out or buffer released.
The toolkit's present call remains synchronous in this slice.

### Resize and window limits

Resize and limits requests are decoded before narrowing or changing scene
state. A well-formed request still requires the authenticated caller to own the
named surface. Unknown names return `Bad_Object`; another owner's surface
returns `Denied`. Malformed requests return `Invalid_Request` without changing
window geometry or constraints.

Resize retains zero's compositor-chosen sizing semantics. Window limits use
zero maxima to mean no application-supplied upper bound; explicit nonzero maxima
below their corresponding minima are rejected. The compositor retains its
minimum window size, fixed-size normalization and resize screen clipping.
The eight existing window features now have enum names in the schema; unknown
feature bits are rejected rather than silently retained.

Replies are discriminated typed results, with exact labels and headers:

- Resize success: four words `[0, width, height, serial]`.
- Limits success: four words `[0, packed minima, packed maxima, serial]`.
- Either failure: one status word, with unused words zero. A short reply
  containing zero is malformed, not success.

Reply decoding checks geometry too. The shared toolkit encodes limits through
the codec and closes a new surface if the limits reply fails validation; it no
longer ignores that result. The desktop-shell resize exercise also decodes its
reply before adopting geometry. Existing C clients' four-word limits requests
already conform. These remain synchronous calls, not atomic multi-operation
commits or presentation fences.

### Cursor selection and surface destruction

Both control requests require the same exact four-word header and a nonzero
surface name. Destruction permits no other payload. Cursor styles are an enum:
default, text, horizontal resize, vertical resize and diagonal resize (0..4).
Unknown styles and nonzero reserved fields return `Invalid_Request` before any
scene, focus, cursor, input-queue or buffer-acquisition state changes. Valid
requests still require surface ownership; typed decoding grants no authority.

Their acknowledgements use the common status codec: matching operation label,
exactly one status word, zero header flags/reserved fields and zero unused
words. Unknown status codes are rejected. The toolkit explicitly maps widget
cursor meanings to protocol enum values and updates its interaction cursor
cache only after a checked success reply. The public cursor setter reports
failures rather than ignoring the reply.

Native tests first reproduced the old behavior: malformed headers/reserved
words were accepted, including five malformed requests that destroyed their
owned fixture surfaces. The regression tests require those surfaces to remain
usable, then exercise valid destruction and rejection of a second destruction.
This was a validation gap, not a demonstrated cross-process ownership bypass.

### Bounded window titles

`Inline_Title` uses a 0..23 length discriminant to constrain its string, so
there is no independent buffer/count invariant to maintain. The compositor
stores this same type and replaces the whole checked value on success.
The toolkit's `Make_Title` constructor preserves the existing first-23-byte
clipping policy and supports Ada strings with arbitrary lower bounds.

The wire retains the existing little-endian byte packing and high-byte length.
All bytes following the declared length must be zero; malformed headers,
zero surface names, lengths above 23, and nonzero padding are rejected before
scene changes. Unknown/foreign targets still return `Bad_Object`/`Denied`.
The toolkit validates the status-only acknowledgement instead of ignoring it.
An empty title retains the existing default-caption behavior.

This is a byte-string boundary, not a new Unicode policy. NUL and high-bit
bytes within the declared length are preserved as before. UTF-8 validation,
code-point-aware clipping, control-character handling and longer title
transport remain separate work; this codec does not claim to provide them.
Captions are application-controlled text, not evidence of publisher identity
or an authority approval.

### Retained BGRA buffer acquisition

Attachment replaces the old numeric-grant layout; no fallback decoder remains.
The last word contains width in bits 0..15, height in bits 16..31 and byte pitch
in bits 32..63. Width and height must be positive, pitch at least `width * 4`,
and `pitch <= 16 MiB / height`. BGRA8888 is fixed by this operation's schema;
other pixel formats require explicit protocol support rather than guessing.
The reply is one status word. Invalid wire data returns Invalid_Request;
failed authoritative acquisition returns Denied. No address comes from the wire.

The compositor uses `CuBit.Memory_Grants.Acquire` with the authenticated sender,
read access, zero offset and checked `pitch * height` length. It holds the loan
until replacement/removal. Replacement first acquires the new loan and then
returns the old one; failure preserves the previous attachment. Close, destroy,
goodbye and dead-client reaping all return their acquisitions.

The shared toolkit, desktop-shell, DOOM and NetSurf use the new layout. C ports
share `userspace/c/cubit_desktop.h`. This is lifetime protection, not an immutable
pixel snapshot or a per-frame release fence. See
[shared IPC buffer lifetimes](ipc-buffer-lifetimes.md) for the kernel audit,
asynchronous follow-up contract and remaining migrations.

### Session handshake and cleanup

Hello accepts the implemented revision 0.1, with exact headers and reserved
words. Unsupported revisions return `Unsupported`; malformed requests return
`Invalid_Request`. It remains an informational handshake, not an authority
grant, authentication exchange or prerequisite for requests through an already
authorized endpoint. The successful reply is `[session identifier, 0, shared
surface-table capacity, revision]`; the identifier is not a capability, and
the capacity does not reserve a per-client quota.

Information queries return checked positive dimensions, the BGRA8888 format
enum and a positive unsigned 16.16 scale. Both Hello and information failures
use two words `[0, status]`, with unused words zero, so a failure cannot look
like a successful nonzero session identifier or display width. Native Ada
clients validate complete replies before continuing initialization.

Goodbye requires an empty request and tears down only the authenticated
caller's surfaces, input channels and acquisitions. Repeating it is harmless.
Malformed requests are rejected before teardown; a pending revocation must
retain its acquisition until actual cleanup. The toolkit checks acknowledgement
and retains its local window/grant state on failure, allowing retry rather than
pretending the session has closed. The current one-window-per-process harness
still uses caller-wide Goodbye; multiwindow client teardown remains separate
API work. Existing C port requests already use revision 0.1 and zero padding.

### Input admission and checked replies

Poll and wait share a discriminated request type: only a wait can carry a
deadline. Both require a nonzero surface name and exact headers/padding.
Admission validates the request, then checks the authenticated caller against
the live surface owner **before dequeuing input or installing a saved reply**.
Malformed requests return `Invalid_Request`; missing and foreign surfaces
return `Bad_Object` and `Denied`. Rejection preserves queued events. These are
the same object-ownership checks as the other desktop operations, not another
authority mechanism.

Success retains the four-word envelope `[event kind, serial, payload0,
payload1]` and the single `More_Pending` flag. Event kinds now have enum names;
the portable decoder checks each kind's packed payload bounds before native
clients interpret or narrow them. It checks byte text, physical key/modifier
fields, bounded geometry, button/wheel fields and recovery snapshots. No-input
replies require zero payloads and no pending flag. Serial ordering, queue state
and Unicode text semantics are not established by envelope decoding.

Errors use canonical one-word status replies. A short reply containing zero
is malformed, not no-input success; an error number cannot be interpreted as
a key or pointer event. The shared toolkit and desktop-shell validate complete
replies. DOOM and NetSurf use the C validator in `cubit_desktop.h`; hosted tests
compare that actual helper with the SPARK decoder on boundary envelopes,
single-bit mutations of every payload word, and malformed headers.

This is a checked wire envelope, not yet a fully discriminated toolkit event
API. Service queue records and reply producers still use the existing packed
representation. The existing event-driven saved-reply path remains: a finite
wait expires at its absolute deadline, clears its waiter, and consumes the
one-use reply capability. This change introduces no polling loop or new
per-event allocation.

## Proof boundary

GNATprove discharges all 187 checks in the portable protocol unit, including:

- absence of runtime errors in decoding arbitrary wire values;
- `Decode_Create (Encode_Create (request))` reproduces the typed request;
- every encoded creation failure has a zero surface-name word;
- clipping stays within the supplied bounds and never enlarges damage;
- every accepted attachment has sufficient row pitch and bounded byte extent;
- every accepted window-limits request has consistent minimum/maximum bounds;
- successful input decoding satisfies the per-kind envelope constraints;
- the access predicate succeeds exactly when the object exists and caller is
  its nonzero owner.

The last property assumes that the integration supplies authentic caller and
owner values. It is not a proof of kernel IPC identity or compositor state.
No `pragma Assume`, SPARK-Off escape, or runtime assertion requirement is used.
Production uses the existing optimized runtime build with assertions disabled.

Complete round-trip properties for creation replies, presentation, attachment,
resize requests/results and window-limits requests/results
as well as cursor/destruction/title/session/input requests, replies and status acknowledgements are tested,
but not claimed proved. Earlier attempts to prove the
creation-reply and presentation record equalities
did not discharge within the configured solver budget; the unproved contracts
were removed rather than retained as assumptions. Decoder expression unfolding
annotations expose implementations to the prover; they introduce no axioms.

## Explicitly unfinished

The 2026-09-10 [kernel request/reply lifetime fixes](ipc-request-lifetimes.md)
address duplicate initial async IDs and stranded reply slots after caller death.
They do not replace the remaining compositor-specific audit below.

This is not a proof of the compositor or complete display stack. In particular:

- Input queue state, saved-reply lifetime under client death or concurrent
  waits, serial exhaustion and compositor event producers still need a
  broader audit. Checked request/reply envelopes do not prove these state
  transitions. The existing already-waiting fallback still returns no-input;
  a distinct busy outcome remains an API decision.
- App-to-compositor and compositor-to-display buffers now use generation-checked
  acquisition. See [display buffer lifetimes](display-buffer-lifetimes.md).
  The display-to-GPU scanout mapping still needs migration; this is not a
  full-stack display grant proof.
- A receiver's read-only mapping does not make the sender's buffer immutable.
  Release/fence semantics must be enforceable against hostile native clients,
  not merely expressed by CCL ownership types.
- Shell roles, trusted chrome, focus, per-client quotas, process death, name
  generation/allocator exhaustion and every other operation's ownership checks
  need a broader service-state audit. Schema membership confers no privilege.
- No batched transaction, asynchronous commit/release/presentation protocol,
  discoverable signed desktop descriptor or CCL widget API is implemented yet.
- The Workbench staging copy, compositor/display copies, frame pacing and
  scheduling remain measurable performance work; this change claims no speedup.

Next finish the service-side input-state audit and implement a bounded commit
state machine with distinct accepted/released/presented outcomes. Prove its lifecycle
and rejection atomicity before integrating asynchronous buffer release or CCL UI
construction.

### Required gates before asynchronous commits

The existing native gate covers object ownership, malformed requests,
acquisition replacement, pending revocation, destruction and client-exit
reaping. It does not yet test an asynchronous commit implementation: none
exists. Before integrating that implementation, add deterministic hosted
state-machine tests (and corresponding SPARK properties where feasible) for:

- An invalid command anywhere in a batch changes no published state.
- A full queue rejects work without acquiring or leaking resources; admission
  resumes when capacity is returned.
- Acceptance, buffer release and presentation are distinct events. Release
  happens exactly once, only after the last reader stops; stale generations
  cannot complete or release a new submission.
- Cancellation, surface destruction and client death at each lifecycle state
  neither release an in-use buffer nor strand a completed acquisition.

Then exercise these transitions over native IPC, including a malicious client
changing writable command memory during validation. Keep performance runs
separate: measure queue delay, input latency and repaint latency under load;
a passing correctness test is not evidence of the p99 latency target.

## Reproducing validation

All commands run from the repository root:

```sh
nix develop -c make -C kernel test-desktop-protocol prove-desktop-protocol
nix develop -c make -C kernel desktop-check desktop
nix develop -c tests/headless/run.sh --test desktop-protocol --accel kvm --cpus 4 --timeout 30 --keep-logs
```

The hosted build stages only the portable units under its ignored build
directory, so CuBit's freestanding runtime cannot shadow Linux GNAT packages.
Hosted assertions are test instrumentation only. The native adversary uses
ordinary error checks, not runtime assertions, and receives only a desktop
endpoint. It exercises malformed headers/dimensions, foreign and missing
surfaces, clipped damage, resource exhaustion, cleanup and subsequent creation.
Geometry tests additionally exercise oversized resize/limit words, unknown
features, contradictory bounds, foreign-object resize/limits and preservation
of existing bounds after malformed requests.

Initial validation on 2026-09-09: hosted codec tests and all 46 original SPARK checks passed.
Native QEMU/KVM gates `desktop-protocol`, `ccl-workspace`, `files` and
`desktop-doom` passed. Both C ports rebuilt; NetSurf browsing was not exercised
in this validation. These are regression checks, not latency benchmarks.

Attachment follow-up, 2026-09-09: all 64 portable checks and 116 checks in the
focused existing kernel grant gate passed. The extended native desktop test
passed malformed/short/stale grants, 140 balanced replacements, pending revoke,
destruction and owner-exit reaping. `ccl-workspace`, `files`, `desktop-doom` and
`storage-grants` passed again after migration. All Ada toolkit consumers and
both C ports rebuilt; NetSurf browsing remains untested in this follow-up.

Geometry follow-up, 2026-09-09: hosted boundary/hostile-input tests and all 110
portable SPARK checks passed. The native four-vCPU QEMU/KVM adversary passed
resize/limits ownership, malformed requests and rejection-state preservation;
`files` and `ccl-workspace` passed after rebuilding the Ada toolkit consumers.
The unchanged four-vCPU `desktop-doom` smoke gate passed on retry. This checks
integration, not audio quality or a latency bound.

Test-infrastructure follow-up: concurrent serial writers can interleave a
readiness marker. In the first geometry `desktop-doom` run, PS/2's consumer-ready
line was interleaved with HDA/DOOM output, so the injector timed out although
rendering and mixer reports continued. Keep this distinction explicit: a
readiness timeout is not a successful input test. Reliable framed readiness
reporting is still needed; do not weaken the input gate to hide this failure.

Control follow-up, 2026-09-09: added native negative tests before changing the
handlers and observed five malformed cursor requests accepted and five malformed
destroys deleting their fixtures. After migration, all 118 portable SPARK checks,
the expanded hosted tests, and the four-vCPU KVM `desktop-protocol`, `files` and
`ccl-workspace` gates passed. No ownership bypass was observed. The common
status codec's round trips are tested, not claimed as a new functional proof.
The final native gate also checked exact invalid-request acknowledgements and
verified that malformed destruction during pending revocation retains the
buffer acquisition until a subsequent valid destroy releases it.
The native `input-stream` gate also passed: its recovery/stress interval
reported 6 presentation requests and 138 input requests (gate limits 20 and
160). This is a repaint/IPC regression check, not a p99 latency measurement.

Title follow-up, 2026-09-09: native negative tests first demonstrated acceptance
of malformed headers/padding and the old oversized-length status mismatch.
After migration, hosted tests covered lengths 0..23, all 256 byte values,
every unused padding position, word boundaries, non-1-based strings, clipping
and an empty string at the maximum index bound. All 148 portable SPARK checks
passed using the standard level-2 gate. The existing creation round-trip
postcondition was reformulated equivalently as valid decoding plus value
equality after whole-wrapper equality became solver-sensitive; no property was
weakened and no assumptions or extra proof budget were introduced.

The four-vCPU QEMU/KVM `desktop-protocol`, `files` and `ccl-workspace` gates
passed with rebuilt native toolkit apps. Native title tests exercise every
supported length, empty captions, ownership denial and exact rejection replies.
They do not provide a title-readback or pixel-level text-rendering proof.

Session and input follow-up, 2026-09-09: negative native tests first reproduced
acceptance of malformed session requests (including destructive Goodbye) and
malformed input requests consuming queued configure events. Validation now
precedes those side effects. No cross-process ownership bypass was observed.
The native adversary passes exact error checks, valid handshake/information,
caller-scoped repeated Goodbye, pending-grant release at cleanup, rejected-input
queue preservation, four successive finite waiter timeouts without early
return, and subsequent configure delivery on the reused channel.

Hosted tests pass request/reply round trips, all header lengths, reserved fields,
boundary payloads and C/Ada input-decoder parity. Independent rejection oracles
cover out-of-range keys, modifiers, text, coordinates, buttons, recovery fields
and unknown kinds; parity checks alone would not exclude a shared mistake.
The final standard level-2 GNATprove gate discharges 187 checks, with none
unproved, justified or assumed. Native Ada clients and both C ports rebuilt in
Nix. NetSurf's ignored adapter change still needs preservation with that port;
a successful rebuild is not a browsing regression test.

Local investigation logs are `/tmp/cubit-session-red.log` and
`/tmp/cubit-input-codec-red.log`; final proof output is
`/tmp/cubit-input-codec-proof-final.log`. The native protocol result is
`/tmp/cubit-input-codec-green.run.log`. These temporary logs are not repository
artifacts; the regression sources and commands above are the durable evidence.

The final four-vCPU QEMU/KVM gates passed: `desktop-protocol`, `files`,
`ccl-workspace`, `input-stream` and `desktop-doom` (timeouts 30/40/50/30/40
seconds respectively). The input stress interval again reported 6 client
presentations and 138 input requests, within the existing 20/160 budgets.
DOOM received injected key events and produced active mixer periods alongside
display submissions. These results check integration and bounded repaint/IPC
activity, not audio fidelity, p99 latency or full compositor correctness.
