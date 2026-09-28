# Native typed CCL objects and Config

Approved type handoff and its tests: [native schema metadata](SCHEMAS.md).

## Canonical-byte validation (2026-09-25)

Text tails and fixed padding are compared against immutable zero arrays instead
of scanned byte by byte. All unused bytes must still be zero; no covert payload
space is accepted. `Canonical_Bytes` has a SPARK-proved equivalence contract:
its result is exactly the conjunction of every unused text byte being zero and
every padding byte being zero, including full text capacity's empty tail.
The new helper and its bounds use ordinary SPARK; no assumptions, unchecked
conversion or SPARK-Off sections were introduced. The native runtime's block
comparison implementation remains part of the trusted compiler/runtime boundary,
not newly proven machine code.

Object tests now pass **29,499 checks**: every unused text/padding byte is
individually corrupted, both words of every unused cell are checked, and all
8,193 possible text-tail offsets are exercised with nonzero legitimate payload
and forbidden trailing data. Existing nested-type/bounds checks remain. Focused
object/bridge/correspondence/publication analysis discharges **137 checks**,
including the comparison equivalence; catalog/authority/store discharges 157.
See [hosted CPU measurements](../config-collections/README.md#cpu-path-measurement)
for scope and results. This does not establish whole-service correctness or
power-loss durability.

The change also passes hosted client/channel and real-Turso recovery suites,
native Config/worker/client builds, and separate KVM writer/read-only-reopen
boots with independent SQLite/WAL/ext2 verification. The normal desktop ISO
was restored afterward. Log: `/tmp/cubit-config-canonical-native.log`. These are
native correctness regressions, not measurements of native performance.

`CCL.Objects.Same_Schema` now applies that same nominal correspondence to two
approved bindings, additionally requiring equal schema keys and both bindings
to be bound. Config registration, worker provisioning and durable Create use
it instead of whole-registry/byte equality. The expanded correspondence suite
has 10,589 checks, including shifted/reordered dependencies, unrelated metadata,
different keys and nested shape/name/order changes. Object/bridge/publication
proofs discharge 132 checks; collection/authority/store proofs discharge 157.
These prove their implemented contracts and runtime checks, not a complete
nominal-equivalence theorem. See [durable Create](SCHEMA-PERSISTENCE.md).

## Explicit local type correspondence (2026-09-25)

The VM/object bridge now requires `Local_Types`, the actual compiled program's
registry. It resolves that registry against the approved binding using the
shared `CCL.Types.Correspondence` package. Same local numbers are not evidence
of the same type, and different local numbers do not prevent a match. Require
the same nominal name, shape, field/alternative order and names, and recursively
matching payload definitions. Ignore unrelated declarations and inactive
representation padding. Never install or mutate either registry while matching.

The bounded resolver uses one pass through backward-only declarations, so shared
subgraphs do not cause recursive expansion. A constrained target-reference
subtype makes every published mapping a target index or Invalid_Type. It has
no global state. A successful match does **not** authenticate the source schema,
authorize an import, change the schema key or permit live handles to persist.
The old bridge signatures without an explicit local registry were removed;
there are no compatibility overloads that silently assume shared numbering.

Hosted tests execute actual CCL parse/type-check/compile/VM programs with shifted
IDs. Their values produce identical native objects and reconstruct under the
receiving registry. Same-ID/different-name, different payload and reordered
alternative definitions are rejected in both directions. The general resolver
has 8,270 checks covering nested products/sums, all possible padding shifts,
all 32 declaration depths, shared subgraphs, missing/conflicting types and
unrelated declarations. Value-bridge coverage is now 101 checks.

Focused SPARK analysis discharges all **130 checks**: 51 runtime checks, 11
functional contracts, 42 initialization checks, 25 termination checks and one
global-dependency check. This includes the resolver's no-global-state and
valid-target-reference contracts and the bridge's validated accepted exports;
it is not a formal theorem of complete nominal equivalence or source trust.
There are no new Assume/SPARK-Off sections or native runtime assertions.
Hosted type/enum/variant/hostile-CCLB suites and the native-runtime static-library
build also pass. Logs: `/tmp/cubit-correspondence-{tests2,full-proof,types,native-build}.log`.

These are Linux-hosted tests, not a new live CCL Config import in CuBit. General
owned aggregate host values, approved catalog linking, private Config bindings
and suspension for persistence remain separate integration work. Native objects
and the public Config service already transport/persist wider values; do not
confuse that with the narrower current VM execution representation.

Run from the repository root:

```sh
nix develop -c bash tests/ccl-objects/run.sh --prove
flock --exclusive coordination/build.lock nix develop -c bash -c \
  'cd kernel && alr exec -- gprbuild -p -P ../tests/ccl-objects/native.gpr'
```

The first command runs **Linux-hosted** tests and focused SPARK analysis. The
second compiles an isolated static library against the **CuBit userspace
runtime**, without assertion code generation. Neither launches a VM or replaces
the running Config service.

## Representation

`CCL.Objects` reuses `CCL.Types`: integers, booleans, characters, strings, unit,
and nested products/sums. Values contain no pointers or local type references.
An approved binding supplies the registry, root type and schema identity. This
package neither authenticates that binding nor computes its schema digest;
matching an identity is not authorization.

The initial bounded layout is a 16 KiB, page-aligned image: a 48-byte header,
256 16-byte cells, an 8 KiB text arena, and explicit zero padding. Cells occur in
schema declaration order, depth first; string offsets address the same image.
All external fields have valid representations for every bit pattern. Validation
checks the complete shape, alternatives, scalar bounds, packed text ranges,
exact consumption, and zero unused storage. No recursion, pointer relocation or
CBOR decoding is required. The fixed size is scaffolding, **not** an optimized
inline representation for a single integer.

Native byte order and layout are a local ABI, not a portable persistence format.
The implemented disk CBOR codec lives in `userspace/ccl/persistence`, outside
this ABI. Authenticated discovery remains separate from schema validation.
Byte values and collections must extend the shared language model;
there is no Config-only parallel type system here.

## Ownership and publication

`Config_Objects` is one schema-bound slot owned by a single dispatcher. The
initial immediate `Store` API has been removed: `Begin_Commit` now snapshots
and validates a candidate without publishing it. A matching `Finish_Commit`
with the expected durable revision flips the active index. Staging, rejection,
and uncertain outcomes preserve the visible value. Reads require the expected
schema. Schema binding cannot be replaced after initialization. Namespace/context
authority enforcement is handled by the live public Config receiver; this
state machine does not itself authenticate callers or grant namespace access.

This is **not zero-copy**: store takes one explicit snapshot, reads copy, and
existing VM/host adapters export/import values. It avoids serialization but does
not yet construct VM values directly in shared arenas. Before any future IPC
overlay, the receiver must establish a valid complete mapping and lifetime.
Direct validation/use requires stable owned storage or kernel-enforced
immutability: making only the receiver's grant read-only is insufficient if the
sender can still write. The pure Ada proof does not establish shared-memory
concurrency or grant lifetime correctness.

Persistability rejects handler-bearing schemas, including inactive alternatives.
The VM bridge additionally rejects noncopyable/tagged values rather than
discarding ownership metadata. Current VM adapters support integers, booleans
and scalar variants; the host bridge also supports bounded text. Wider native
objects do not imply the current VM can execute every aggregate shape.

## Evidence (2026-09-24)

- 359 object/storage checks: nested schemas, relocated images, different local
  registry numbering, integer extrema, full arenas/stacks, malformed metadata,
  unused-byte rejection, publication failure atomicity and copy isolation.
- 70 value bridge checks, including actual parse/type-check/compile/execute of
  integer, boolean and `Reading` variant expressions before Config round trips;
  text, handler rejection, ownership rejection and unsupported host sizes.
- Focused GNATprove after durable staging: 124 checks discharged (42
  initialization, 48 runtime, 10 contracts, 24 termination), zero unproved or
  justified checks.
  No `pragma Assume` or SPARK-Off sections in these new packages.
- Native-runtime static library compile passed, including VM/host adapters.
  Existing hosted type, enum, variant and hostile-CCLB regression suites passed.

The proved contracts include failed-builder preservation, validated accepted
exports, and Config publication/revision preservation on rejection. This is not
a proof of every validator semantic rule, authenticated schema resolution,
the entire existing CCL VM, live IPC, ACL enforcement or database durability.
The slot is now wired into public native Config IPC and its Turso worker; see
[native client tests](../config-object-client/native-app/README.md). General CCL
source-level typed Config imports remain separate work, as described above.

## Durable publication and recovery

The typed state machine now requires an explicitly authorized session followed
by a load before it accepts writes. The dispatcher must authenticate the worker
and its completions; session numbers and request tokens are correlation data,
not capabilities. Session replacements increase monotonically and request
tokens cannot be reused, including across replacement sessions.

During a pending write the previous committed value remains readable. A
definite no-change rejection allows another write; conflict, bad acknowledgment,
timeout or worker death requires recovery. Cached values are then returned as
`Stale`, not fresh `Found`; unknown absence is `Unavailable`, not `Missing`.
Recovery rejects revision rollback and different contents at the same revision.
Revisions are bounded by the actual signed 64-bit SQL representation.

The 583 `durable_tests` checks cover the completion/revision matrix, wrong
sessions/tokens/operations, source/export copy isolation, no publication while
pending, dropped acknowledgments, stale cache visibility, malformed recovery,
worker replacement and revision/token exhaustion. These use synthetic backend
outcomes. They do not prove backend authentication or actual disk durability.

The separate real-storage test runs:

```sh
nix develop -c bash tests/ccl-objects/run-durable-turso.sh
```

This is **Linux hosted**. Ada retains the actual `Config_Objects.State` while
`Config_Database` calls Rust/Turso directly in the same process through a fixed
ABI (no C source). The test-only Rust open/close exports own the database;
production startup will supply authorized native I/O instead of a Linux path.
After the second commit, the syscall model deliberately denies acquisition of
the response grant. The test closes the database, marks the worker lost and reopens/reloads through
a new session. Ada must retain stale revision 1 until
the schema-checked revision 2 arrives. Independent read-only SQLite inspection
checks that precisely two revisions exist: recovery did not retry the write.

The fixture now runs public messages through `Config_Object_Service`,
`Config_Object_Receiver`,
`Config_Object_Dispatch`,
`Config_Typed_Store`, `Config_Worker_Channel`,
`Config_Worker_Receiver` and `Config_Worker`: authorized handles, staged native
objects, request/reply validation, commit encoding and load decoding use the
service implementations (198 checks total). The collection uses machine context;
denied callers cannot read its cached value or stage a write. No success reply
is emitted until publication; a lost response after a real commit produces an
unavailable outcome and restores without retry. Client mappings are released
before database calls, and the modeled saved reply cannot be overwritten by
cached reads. Reply reservation is modeled, not a real kernel capability move.
Shared-memory grants and
kernel completions are modeled; database calls and storage are real. The old
subprocess/hex-file transport was removed, not kept as a parallel implementation.
See [worker protocol](../config-worker/README.md). This does not change the
fixture into live IPC or establish peer authentication. The controlled close
is not a simulated power failure; the backend's separate crash tests cover
process death. This test covers publication/recovery across replacement.

The service owner now submits staged work and consumes authenticated completion
entries itself; the test no longer manually joins each storage/publication step.
Rejected enqueue unwinds the saved application reply and marks the cache stale,
without another database call. Retirement confirmation is required even if
revocation was rejected. Tokens survive replacement of a service state within
the same process/package instance: a late completion from its predecessor is
ignored. An instance attaches only once; process-wide orchestration must retire
it before replacing it and must keep this single completion-token domain.

The SPARK contracts prove staging preserves the visible value/revision,
accepted staging validates its candidate, and acknowledged publication selects
exactly that candidate and increments the revision. This composes with exclusive
ownership of private buffers; it is not a proof of kernel completions, SQL
commit/flush semantics, grant lifetime, or policy. The immediate publication API
was removed rather than retained as a bypass. The unused text-only publication
prototype and its tests were also removed when the typed store replaced it.

## Typed object persistence (2026-09-24)

The optional [shared persistence codec](../../userspace/ccl/persistence/README.md)
uses the pinned SPARK CBOR library. Local IPC remains native; this codec is for
the storage/network boundary. It supports the same schema-checked aggregate
objects, not a separate Config-only scalar language.

```sh
nix develop -c bash tests/ccl-objects/run-persistence.sh --prove
nix develop -c bash tests/ccl-objects/run-turso.sh
flock --exclusive coordination/build.lock nix develop -c bash -c \
  'cd kernel && alr exec -- gprbuild -p -P ../tests/ccl-objects/persistence_native.gpr'
```

Do not run the two hosted scripts concurrently: they share the codec test build.
The Turso script saves artifacts in a new `/tmp/cubit-typed-objects.*` directory.
It performs **Linux-hosted** Ada encode → real Turso commit/close/reopen → Ada
decode, then independent read-only SQLite extraction → Ada decode. Its record
contains a string with NUL/high bytes, a variant carrying a nested record with
both signed integer extrema, and a boolean. The fixture occupies 82 CBOR bytes.
The SQLite reader checks integrity, exact schema bytes and exact payload bytes.

Codec tests include every single-byte mutation of that fixture, every truncated
prefix, indefinite/noncanonical encodings, schema mismatches, full text arenas,
shifted/negative input bounds, and a large product tree beyond the upstream
`Decode_All` item limit. Accepted mutated objects must validate and re-encode
to exactly the received bytes; rejection must leave no partial output.
27,306 checks pass. The native-runtime static library compiles without `-gnata`.
Focused codec GNATprove discharges all 76 checks (9 initialization, 51 runtime,
16 contracts), with no unproved/justified checks or added assumptions/SPARK-Off
sections. The successful-decode contract guarantees native schema validation;
failure exposes only the empty image. Decoder-wrapper contracts preserve the
pinned library's checked reference bounds. Encoder helpers explicitly accept
one-based slices, avoiding unrepresentable lengths across the full signed
`Storage_Array` index range. These are proved helper preconditions, not extra
runtime rejection guards in the native build.

The mutation/edge-case work caught and fixed acceptance of an unfinished
indefinite byte-string head as empty text. The single-head parser is working as
documented; the profile must reject indefinite forms explicitly.

The Rust backend's 21 tests include typed close/reopen, revision conflicts,
immutable history, schema-change rejection, wrong-typed reads and corrupt SQL
payloads, plus all earlier storage fault tests. Rust validates the envelope only;
Ada's trusted-schema decoder is still required before treating it as a CCL value.
No live VM Config IPC, power-failure guarantee or end-to-end zero-copy claim is
made by these hosted tests.
