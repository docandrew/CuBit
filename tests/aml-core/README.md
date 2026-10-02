# Userspace AML validation

2026-10-02 method-time Field declarations are now executed by the table-aware
service evaluator. The executor parses the package, region name and flags and
charges FieldList bytes before mutation. The namespace stages bounded field
descriptors, validates names, duplicates, capacity and table bounds, then commits.
Failed declarations leave the namespace unchanged; successful declarations use
the current method owner and are removed at its final exit.

Current support is immutable table regions, AnyAcc/ByteAcc with NoLock/Preserve,
named/reserved entries and ordinary AccessField entries with those access types
and zero attribute. Other access flags, connection entries and hardware-backed
forms are rejected explicitly. DataTableRegion still uses the service binding
API; module-level Field loading remains unsupported. This is not full Field or
DataTableRegion coverage and adds no upstream ASLTS passes.

Validation: 1523 declaration/lifetime checks, 978 truncation/budget/repeated-call
checks, 54 ACPICA comparisons using actual Field AML, and existing service/typed/
call/Store/method-storage regressions passed. The final service-instantiated
SPARK run (87954) passed 2458 proof and 322 flow checks, zero unproved/justified.
The unchanged generic executor also passed in run4531. Native link passed with
426 input hashes preserved. The checked Define_Fields frame fell from 725328 to
489744 bytes after removing the explicit namespace copy and redundant loop
snapshots; this is still substantial and is not a whole-call-chain stack proof.
Saved report: `/tmp/cubit-aml-declarations-small-proof.out`; test/native logs:
`/tmp/cubit-aml-declarations-small-{r2-check,regression,native-r2}.log` and
`/tmp/cubit-field-boundaries.log`. The standard workflow now registers both new
hosted fixtures; registered session11334 passed1523 declaration and978 boundary
checks from the shared checkout. The existing ACPICA service-fields fixture
executes Field.


2026-10-02 service field evaluation: `ACPI_Service.Invoke` now supplies owned,
immutable table backing to the namespace evaluator. Evaluated fields return
integers at the AML revision's integer width, or materialize wider values as AML
buffers. ObjectType inspection retains field type 5 without allocating. Invalid
backing spans are rejected; value-store exhaustion returns `Value_Limit`.
The backing is an ordinary record passed through explicit aliased read-only
parameters, compatible with the native runtime. Service invariants tie each
retained span to its admitted catalog entry; invocation preserves the catalog.

Final validation: 847 field execution checks, 1811 explicit-input checks,
54 ACPICA field comparisons, 212 typed and 26 dynamic-method comparisons,
and existing namespace/service/request/endpoint regressions passed. Session
20247 exited 0: 2701 proof checks and 379 flow checks, zero unproved or justified;
no Assume statements in the changed production units. This selected-unit report
includes instantiated namespace/executor code, not a fresh whole-project proof.
Native service link passed and all 426 input hashes remained intact. Promoted
sources byte-match the verified inputs; registered hosted tests passed again
(session 44826, 847 field/1811 input checks). Evidence:
`/tmp/cubit-aml-fields-live-guard-{proof,native,acpica}.log` and
`/tmp/cubit-aml-fields-live-guard-proof.out`.

The focused reference uses real ACPICA DataTableRegion/Field declarations;
CuBit binds those declarations through service APIs and executes the field reads
through actual AML methods. Declaration opcodes remain unsupported. Buffers are
retained in the bounded value store; reclamation and full lifetime management
remain unfinished. This adds no upstream ASLTS passes and establishes no
whole-call-chain or secondary-stack bound. Tests use a 64 MiB stack budget.


2026-10-02 explicit immutable executor input: `Execute_With_Input` accepts a
limited, potentially discriminated read-only context and passes it through
nested calls without copying table backing. Lookup distinguishes metadata
inspection from value evaluation, so field identity can remain type 5 while
field evaluation returns its value. `Execute_Typed` supplies an empty context
for callers that need only namespace state. The public `Global => null`,
execution-budget and context-validity contracts remain intact; nested routines
explicitly declare their input dependencies.

Private final validation: 1811 read-only input checks (including >1 MiB backing,
recursive calls, 32/64-bit reads, ObjectType, budget and depth exits), existing
call4818/typed11938/inspect234/store4137/method-storage141/namespace-field351 and
service1051688 regressions passed. Executor proof: 418 proof checks plus 75 flow
checks, zero unproved/justified/Assume; native service link passed with 424 input
hashes preserved. Focused ACPICA typed212/dynamic-method26 comparisons passed.
Evidence: `/tmp/cubit-aml-input-purpose-{verify,acpica,native}.log` and
`/tmp/cubit-aml-input-purpose-proof.out`; private hosted sources are in
`/tmp/cubit-aml-input-67ypbglg` and native sources in
`/tmp/cubit-aml-input-native-vbjf5k2r`. This is the context prerequisite, not
implemented DataTableRegion/Field execution or new upstream ASLTS coverage.
The later service-field implementation above supplies wide-field buffer
materialization; declaration execution and value reclamation remain pending.


The current core includes bounded namespace/data loading, integer and named
object execution, synchronous serialized calls, temporary method declarations,
Store expressions, and bootstrap table
admission, plus bounded snapshot-upload and metric-query request handling.
Full mutable-reference semantics, hardware integration and CCL
service endpoints remain unfinished. The sections below record incremental
validation evidence; early milestones describe the implementation at that time.

Run from the repository root with the pinned Nix environment:

```sh
nix develop -c bash tests/aml-core/run.sh --prove --acpica
```

Outputs are private to this test's ignored `build/` directory. No kernel,
staging, native service or shared build definitions are changed. Run only one
instance of this target at a time. The proof uses at most two jobs.
The standard workflow includes the pinned ACPICA differential and upstream
ASLTS checks. Their unsupported and unselected cases remain explicit coverage
gaps; a successful development gate is not full ACPICA-suite compatibility.

2026-10-02 request/grant capacities verified: request state has fixed-at-
construction capacities. Start/admission, chunk buffering, bulk import, observer
readback and metrics page 4 use those same values. Native grant metadata is
rejected before acquisition if it exceeds the per-table limit. Provider authority,
exact read-only extent and loan cleanup rules are unchanged. The actual native
adapter, with mocked kernel acquisition, passes 150 checks including a >1 MiB
grant, 35-table completion, failed return/retry and retained-byte readback after
source mutation. Chunk fixtures reject oversized declarations and partial commit.

Service/bootstrap proof 40571 passed earlier. Final transport proof 86013 also
passed: all 108 request/endpoint checks, zero unproved/justified checks or Assume
statements. The report's 5553-result aggregate includes cached units; it is not a
fresh whole-project proof. Saved report:
`/tmp/cubit-acpi-runtime-transport-proof.out`. Verifier budgets were 30 seconds
and 2000 MB per prover, two jobs. These are verification resources, not runtime
allocation quotas. Explicit initialization avoids a verifier crash; retaining
the initial revision and splitting wire representability from integer quota
comparison discharged the remaining resource-limited obligations without
weakening contracts.

The full hosted workflow passed in 35535, including restored namespace-field
coverage, with ACPICA comparisons passing 80 bit reads and 36 lookups. Targeted
regressions passed after the constructor/chunk changes; final request tests
passed 92680 checks, including Natural'Last and the next wire value (91295).
The final native build/link passed and all 424 source inputs match
`/tmp/cubit-acpi-runtime-transport-native-r3ddq2qw`.

Native startup still constructs a statically constrained default instance. An
unconstrained library object would require __gnat_malloc, absent from the
freestanding runtime. Discovery-sized allocation must therefore be explicit;
this milestone is not a live kernel grant handoff or end-to-end dynamic sizing.

2026-10-02 service/bootstrap capacity follow-up: constructors accept table-count,
aggregate-byte and per-table budgets, retained in non-defaulted discriminants.
State objects have fixed capacity after construction. Metadata and metrics no
longer impose prototype bounds; their contracts tie values to each instance.
Table installation copies with a slice assignment, preserving atomic rejection
and exact-byte contracts without a quadratic assertion-enabled copy loop.

Hosted service 1051688, bootstrap 1314, request 92670 and endpoint 589972 checks
passed. New cases retain a >1 MiB description table, fill 40 service slots, read
a namespace field at the large table's end after source mutation, check exact
fit and independent quota failures, and complete/freeze a 35-table bootstrap.
These do not prove that arbitrarily large AML bodies fit the separate namespace,
value and execution budgets. The request adapter still uses default capacities.

Freestanding compilation/link passed (69874); 424 recorded inputs match
`/tmp/cubit-acpi-runtime-service-native-z70yjx2c`. Largest reported static frame
is 2713376 bytes, with 29 dynamic frame records: neither whole-call-chain nor
secondary-stack bounds have been proved. Session 40571 exited 0: service, bootstrap and request/endpoint proofs passed
with zero unproved/justified checks and no Assume statements. The aggregate
5515-result report includes cached units, not a new whole-project proof.
Saved report: `/tmp/cubit-acpi-runtime-service-proof.out`; log:
`/tmp/cubit-acpi-runtime-service-tests.log`.

2026-10-02 table-capacity progress: immutable snapshots now pack payload into
runtime-sized storage with separate count/byte/per-table quotas. The shared
catalog no longer imposes a 1 MiB length policy, and physical-window metadata
derives its page bound from the representable length. Hosted tests cover 39
tables and a single table over 1 MiB, exact fit and rejection, copied ownership
and zero padding. All 95 snapshot and 63 physical-window proof checks pass;
rebuilt kernel capture/rejection passes six Multiboot cases. See
[snapshot evidence](snapshots/README.md). Kernel capture now allocates from measured catalog requirements (see the later
snapshot evidence); native userspace startup and grant sizing still need integration; this is not an end-to-end
dynamic handoff or additional AML/ASLTS coverage.

2026-10-02 namespace table bindings: distinct region/field objects retain
service-local immutable table identities and extents. Fields store the resolved
region value rather than a recyclable namespace node reference. Binding checks
scope, owner activity, name validity, capacity and bit extent before publishing
an entry; rejected bindings leave the namespace unchanged. `ObjectType` sees
Region=10 and FieldUnit=5; these objects are not ordinary integer storage.

`Declare_Table_Region`, `Declare_Table_Field` and `Read_Namespace_Field` connect
these objects to the service-owned catalog. This is an internal API, not a new
CCL request or hardware capability. Namespace fixtures passed 351 checks;
service tests passed 1051624. The 80 ACPICA bit-range comparisons now use this
namespace binding/read path rather than the direct table-bit accessor. Existing
method-storage and inspection fixtures pass. Native compilation/link and the full standard hosted regression run passed.
SPARK proves all 68 checks for the new namespace binding/accessor operations,
with zero unproved checks or assumptions. Service-adapter proof also passed:
Declare_Table_Region 12, Declare_Table_Field 10 and Read_Namespace_Field 7
checks, zero unproved checks or assumptions (session 23652 exited 0). The saved
report is `/tmp/cubit-acpi-namespace-fields-service-proof.out`; its 5484-check
aggregate includes cached earlier units and is not a fresh whole-project proof.

AML DataTableRegion/Field declarations are not yet dispatched by the evaluator,
and ordinary operand lookup does not read fields yet. Dynamic owner cleanup
uses the existing namespace machinery, but declaration callbacks have not yet
been connected or exercised during active AML calls. This is not an upstream
ASLTS pass or a live service launch claim.

2026-10-02 immutable table bit reader: `AML_Field_Data.Read_Bits` extracts
little-endian bit ranges from an immutable supplied slice. It returns up to
1024 bytes, explicitly rejects larger results, checks the selected extent
without overflowing, and zeros unused result bits and bytes. The arithmetic
model uses a bounded power-of-two table. SPARK proves **85 checks** across
`Fits`, `Window` and `Read_Bits`, including exact byte values and zero padding;
there are no unproved checks or assumptions in those functions.

`ACPI_Service.Table_Field` applies this reader to one retained table, using the
same local table index as `Table_Info`. It exposes neither a physical address
nor an OperationRegion hardware handler. Hosted tests sweep every adjacent
byte pair and bit alignment, short windows, empty/maximal results, overflows,
and highest legal array bounds: **639397 checks passed**. Service tests add
1710 checks for owned copies, unaligned reads and rejection at a table boundary
even with another table immediately following it in storage (total 1051609).

The ACPICA fixture compares its actual DataTableRegion/Field evaluation with
the direct CuBit table-bit reader over 80 offset/length pairs. It does not yet
execute CuBit Field opcodes. AML namespace field objects, integer-versus-buffer
result semantics, access/update rules and dynamic region binding remain
unfinished. Reads here always return raw bytes. All 80 ACPICA comparisons passed on the
final source. The native service compiled and linked in a private snapshot with
424 matching input hashes. Service-unit SPARK verification also passed with no
unproved checks; the combined report includes earlier cached units and is not
a fresh whole-project proof. This is not a live service boot or hardware test.

2026-10-02 field-list framing: `AML_Fields.Read_Entry` handles named and
reserved fields, ordinary and extended access changes, and name/buffer
connections. Successful entries consume a positive bounded span; named fields
have validated four-byte NameSegs. SPARK proves all **59 checks**, including
exact name-byte and access-operand preservation, with no unproved checks or
assumptions. The combined report includes cached other-unit results.

**AML-FIELD-CHECK: PASS 131520** covers all access type/attribute byte pairs,
name characters, prefix truncations, mixed lists, long/zero field lengths,
malformed buffer packages and highest legal array bounds. The iASL fixture
comparison adds **32 entry checks** across eight lengths (1 through 65535 bits)
for named, reserved and AccessAs entries. Connection and extended-access forms
currently have hosted fixtures only. Both tests and the proof unit are in the
standard runners. The new unit also compiles with the freestanding native
runtime in a private snapshot (422 unchanged input hashes).

This parser is not yet wired to namespace field execution. It retains access
attributes without validating their region-specific meaning, and treats
connection BufferSize as an unevaluated TermArg. Namespace lookup, connection
resource validation, field offsets/access semantics and hardware authorization
remain separate unfinished work. Neither iASL framing comparison nor native
compilation proves field execution, service startup, or hardware access.

2026-10-02 field-length framing: **AML-DECODE-CHECK: PASS 789819**.
`Read_Field_Length` decodes the AML PkgLength representation as a bit count,
including zero and nonminimal encodings, without requiring that many input
bytes. `Read_Package` shares that decoder and still requires the package extent
to include its encoding and fit the supplied slice. Reserved bits and truncated
encodings are rejected. Tests extend the independent arithmetic oracle across
all first-two-byte combinations, continuation-byte values, truncations and
highest legal array bounds. Field counts never allocate memory or authorize I/O.

Focused SPARK level 2 proves all 37 checks for `Read_Field_Length`, including
its exact numeric postcondition, and all 12 checks for `Read_Package`; there are
no unproved checks or assumptions in these functions. The aggregate report
also contains cached results for other units and is not a fresh whole-project
proof. This is framing support only: AML Field and DataTableRegion execution
remain unsupported.

Validation for this change: the full hosted `run.sh --acpica` workflow passed,
including 1,530 focused AML comparisons, 12 package, 38 package-count, 268
coercion, 212 typed-object, 520 store, 48 named-store, 1,200 serialized-method,
26 dynamic-method and 28 method comparisons. Table checks passed 113 ACPICA
FADT, 48 FWTS checksum and 36 lookup cases. Upstream ASLTS remains **0 passed,
12 unsupported, 0 unexpected failures, 339 entrypoints not run**. The checked
native ACPI executable compiled and linked in a private snapshot whose 420
input hashes matched the checkout. This is not a live service boot test.

2026-09-30 result: **AML-DECODE-CHECK: PASS 395831** with assertions and overflow
checks enabled. Tests cover Zero/One/Ones and Byte/Word/DWord/QWord literals,
32-bit truncation, little-endian values, every literal prefix truncation,
unsupported opcodes, every byte value, and non-1 / highest legal array bounds.
Package tests exhaust all 65,536 first-two-byte combinations at six extents,
exercise every byte value at all three continuation positions, reserved bits,
zero/undersized lengths, empty bodies, trailing input, and the maximum encoded
length. The test oracle uses arithmetic division/remainders independently of
the decoder's masks. These are hand-built AML fixtures, not iASL-generated or
ACPICA differential tests.

SPARK level 2: **36 analysis results, zero unproved or justified**: 25 runtime
checks, 4 loop assertions, 2 extent postconditions, 3 initialization and 2
termination results. Detailed report: `build/obj/gnatprove/gnatprove.out`.
The proved postconditions place successful consumed/package extents within
input and package encoding within package extent. There are no assumptions,
address overlays, hardware callbacks or SPARK-off sections in the core.
Integer numeric semantics and full AML conformance are regression-tested only;
these are not formal functional correctness claims for an interpreter.

Caller obligations: stable readable input, correct operand start and enclosing
slice, and integer width selected from the DSDT. `Read_Package` is for package
framing, not Field bit lengths. Unsupported input yields an explicit result;
no cursor or namespace is modified. Both decoders do constant bounded work
(maximum eight payload bytes / three continuation bytes) with no allocation.

See [service contract](../../docs/acpi-service-contract.md) for the audit,
authority model, interpreter decision and roadmap. No native QEMU or physical
N95/N100/laptop behavior has been validated by this target.

## NameString milestone (2026-10-01)

`AML_Names` now decodes NullName, single/dual/multi-segment paths, root and
parent prefixes. MultiNamePath supports all 1–255 segments; zero segments are
malformed. Parent prefixes have an explicit 255-prefix resource limit and
return Limit_Exceeded without publishing a partial name. Successful names
retain root/parent information for later namespace resolution; this parser
performs no namespace search or upward fallback. Rooted NullName is retained
as a rooted path, not conflated with an unrooted null target.

**AML-NAME-CHECK: PASS 166277**, plus the original 395831 decoder checks.
Tests include every MultiNamePath count and every prefix truncation, every byte
at every position of a dual path, the parent-prefix budget boundary, illegal
root/parent combinations and highest legal array indices.

Combined SPARK level 2: **102 results, zero unproved or justified** (68 runtime
checks, 20 assertions, 3 functional contracts, 4 initialization, 7 termination).
The name postcondition proves successful extents fit the input, rooted names
have no parent prefixes, and every published segment satisfies AML's leading
and trailing character grammar. It does not yet prove namespace resolution,
source-to-result byte correspondence or a complete interpreter.

Grammar reference: [ACPI name objects, section 20.2.2](https://uefi.org/specs/ACPI/6.5_A/20_AML_Specification.html).

## Namespace storage milestone (2026-10-01)

`AML_Namespace` is a bounded generic append-only parent/name store. Root is ID0;
IDs are local to a tree lifetime, not service capabilities. Successful insertion
preserves all previous entries. Duplicate, invalid and full insertions leave
state unchanged and return no node. Parent ordering prevents cycles. Child
lookup contracts prove matching parent/name on success and absence on failure.
Full AML resolution (including ancestor-search rules), object values and table
loading are still pending.

Hosted **AML-NAMESPACE-CHECK: PASS 8136**, alongside both decoder suites.
Combined SPARK **164 results, zero unproved/justified**, using the explicit
128-node `namespace_instance.ads` instance: 106 runtime checks, 22 assertions,
18 functional contracts, 6 initialization and 12 termination. The hosted test
procedure itself is not SPARK-proved. An initial run targeting only that test
silently skipped its enclosed generic instance; the explicit SPARK instance
corrects that coverage gap. Future production capacities need their own instance
verification. No native integration or whole-interpreter claim.

## Existing-object resolution (2026-10-01)

`Resolve` implements ACPI section 5.3's structural lookup: nearest-ancestor
search only for unprefixed single-segment names; absolute, parent-prefixed and
multi-segment paths use exact traversal. Going above root fails explicitly.
Null paths return the selected scope; callers must apply opcode-specific
NullName/target semantics. This operation never inserts a declaration, creates
an intermediate scope or resolves aliases (object types are not present yet).

**AML-RESOLVE-CHECK: PASS383**, including local shadowing, root/explicit-parent
selection, no ancestor fallback for multipart paths, malformed paths and deep
successful/failed search. All earlier hosted suites still pass.

Combined explicit 128-node instance proof: **214 results, zero unproved or
justified**: 138 runtime, 30 assertions, 25 contracts, 7 initialization and 14
termination results. Resolve proves termination, that successful results exist,
and that nonempty paths match the requested final segment. Nearest-ancestor and
complete path semantics currently have regression evidence, not a complete
functional refinement proof; do not read the aggregate as full AML correctness.

Primary reference: [ACPI namespace section 5.3](https://uefi.org/sites/default/files/resources/ACPI_Spec_6.6.pdf).

## Transactional integer declarations (2026-10-01)

`Load_Names` consumes an admitted table's AML payload containing integer-valued
Name declarations. A private candidate namespace is published only after every
term succeeds. Unsupported opcodes, malformed values/names, missing intermediate
scopes, duplicates and capacity exhaustion leave the supplied tree unchanged.
Root-relative and explicitly rooted names are supported; parent prefixes at this
root-only loading stage are rejected. Existing intermediate scope nodes may be
used, but the loader does not synthesize missing scopes or traverse integers as
scopes. Integer width is supplied from the admitted DSDT, including for SSDTs.

This is a deliberately incomplete definition-block loader: Scope/Device bodies,
strings/buffers/packages, methods, table installation order and dynamic loading
are not implemented. It cannot load typical platform DSDTs yet. Empty input is
a successful no-op. Raw table admission/lifetimes remain caller obligations.

**AML-LOAD-CHECK: PASS3868** includes literal values in both widths, duplicate
loads, every prefix truncation of a fixture, unsupported OperationRegion bytes
after valid declarations, malformed NullName, existing nested scopes, full
storage, highest-index input and every byte mutation of a two-name fixture.
Mutation checks assert rollback on rejection; they do not prove that every
accepted alternative encoding has the right semantic meaning.

Combined explicit-instance SPARK: **276 results, zero unproved/justified**
(178 runtime, 38 assertions, 30 contracts, 13 initialization, 17 termination).
The loader's failure-atomicity postcondition is proved, as are bounded access
and termination. Successful loading's complete semantic correspondence to AML
is still regression evidence, not a full functional proof. Candidate storage is
one fixed-capacity tree copy, with no heap allocation or hardware callbacks.

## Nested namespace bodies (2026-10-01)

The loader now consumes Scope and Device package bodies using a fixed 64-frame
stack. Scope resolves an existing non-integer node; Device declares a fresh
structural node. Relative, rooted and parent-prefixed declarations use exact
placement from the current frame. This supersedes the root-only loader limit
above. Device nodes currently have no device-specific payload or activation;
_HID/_STA enumeration, predefined namespace seeding and richer object kinds
remain work. Typical DSDTs still require unsupported values and methods.

All operand slices end at the current package boundary, even when the table has
more trailing bytes. Returning from a package restores the preceding scope.
Depth exhaustion, malformed packages and missing scopes reject the entire load.
The frame budget is implementation policy, not an ACPI specification limit.

**AML-LOAD-CHECK: PASS3948** adds Device + reopened Scope values, every Device
prefix truncation, missing-scope rejection, an integer operand outside its
package, and all nesting depths 1–65 (64 accepted, 65 explicitly rejected).
All earlier suites pass. No generated-ASL/differential or native result yet.

Combined SPARK **315 results, zero unproved/justified**: 202 runtime checks,
44 assertions, 33 contracts, 19 initialization, 17 termination. The nested-frame
containment invariant and lexicographic byte-offset/depth termination measure
are proved along with whole-load failure atomicity. Full Scope/Device semantic
refinement is not yet proved; these are structural loading foundations.

## Object kinds and strings (2026-10-01)

Namespace nodes now distinguish Scope, Device, Integer and String. Reopening a
Device with Scope preserves its kind; integer/string objects cannot be opened
or traversed as scopes by the loader. Name declarations accept ASCII string
literals with an explicit 255-byte implementation budget. Exceeding it returns
Value_Limit and rolls back the load. The budget is not an ACPI string-length
limit. Storage is currently fixed per node, not a compact arena.

**AML-STRING-CHECK: PASS66310** covers all lengths 0–255 and every truncation,
every possible byte, over-budget input, non-1/highest indices, stored _HID text,
and malformed/over-budget declaration rollback. Existing suites pass, including
Device/Integer type assertions. ASCII control bytes 1–127 are AML data; callers
must escape untrusted firmware strings when displaying/logging them.

Combined SPARK **369 results, zero unproved/justified**: 244 runtime checks,
48 assertions, 36 contracts, 21 initialization and 20 termination. Read_String
now proves consumed extent and exact source-byte correspondence/ASCII range of
accepted characters. No whole-interpreter or CCL/native integration claim.

## Buffer constants (2026-10-01)

Read_Buffer accepts BufferOp with an integer-constant size and bounded byte
initializer. Result length is max(declared size, initializer size), with zero
extension as specified by ACPI 19.6.10. Dynamic TermArg sizes explicitly fail;
there is no expression evaluator yet. Namespace Name loading stores Buffer
objects and rejects them as scopes. Decoding _CRS bytes is not resource parsing,
device admission or hardware access authority.

The current per-buffer resource budget is 1024 bytes, not an ACPI limit. Both
initializer and declared size are checked before copying; the parser only reads
within the declared package. It uses fixed storage per namespace node, which
will need an arena design before larger production capacities.

**AML-BUFFER-CHECK: PASS48399**, including a 33x33 declared/initializer-size grid,
all prefix truncations of those fixtures, complete zero-fill checks, maximum and
over-budget declared sizes, dynamic-size rejection, DSDT width behavior, highest
indices, stored _CRS bytes, and failed-load rollback. All previous suites pass.

Combined SPARK **429 results, zero unproved/justified**: 294 runtime, 48
assertions, 38 contracts, 26 initialization and 23 termination. Buffer extent
safety is proved; max-size and initializer/zero-fill semantics currently have
regression evidence, not a complete functional refinement proof. Existing
transaction failure atomicity remains proved. No native/differential results.

## Raw method execution foundation (2026-10-01)

`AML_Execute.Run` executes integer constants, Arg0–6 and Local0–7 operands,
Store to locals, Noop and Return. Invocation-local storage is fresh on each
call; reading an unset local or unavailable argument fails explicitly. Arguments
are normalized to DSDT integer width. Return stops immediately; unreachable
trailing bytes are not executed. Falling through reports No_Return, distinct
from returning integer zero. Unsupported expression, target and statement forms
fail without invoking any callback.

The caller supplies raw body bytes, integer arguments/count and a work budget.
One unit is charged before each statement and each source operand, including
failed operands. Operand-byte decoding has its own fixed bounds. The primitive
has no namespace access, shared state, hardware access or synchronization.
Method declarations are not yet loaded/bound to this primitive, and the method
flags/serialization/call stack, general values/references, expressions, loops,
namespace mutation and operation regions remain unfinished.

**AML-EXECUTE-CHECK: PASS2099** covers each argument and both widths, every local
and byte value, invocation-local initialization, every small budget boundary,
truncated return, unsupported hardware opcode, unreachable bytes after Return
and maximum-index input. These are hand-encoded hosted fixtures; no native,
ACPICA differential or real-firmware method execution is claimed.

Combined SPARK **476 results, zero unproved/justified**: 325 runtime checks,
50 assertions, 41 contracts, 33 initialization and 27 termination. Execution
proves its charged work never exceeds the supplied budget and terminates;
operand charging/offsets are monotonic. Opcode value semantics remain covered
by regression tests, not a full functional interpreter refinement proof.

## Method declaration and invocation (2026-10-01)

The namespace loader now retains Method objects with flags, DSDT integer width
and an owned body copy, bounded at 1024 bytes. Loading skips the body as code:
its bytes are not executed or mistaken for load-time declarations. Invoke checks
object kind and exact declared argument count before entering AML_Execute. It
explicitly refuses Serialized and nonzero SyncLevel flags while synchronization
is unimplemented. This is a coverage restriction, not an assertion that those
flags are invalid AML. The current pure executor has no nested calls or shared
namespace side effects, and no method gets hardware access.

**AML-METHOD-CHECK: PASS533**, including both widths, all 256 flag combinations,
argument mismatch, budget exhaustion, truncated declarations, missing flags,
following load-time declarations, and proof by test that mutating the caller's
original byte array cannot change the stored body. Method parsing is not full
body validation: unsupported reachable instructions fail at invocation.

Combined SPARK **505 results, zero unproved/justified**: 349 runtime checks,
50 assertions, 44 contracts, 34 initialization and 28 termination. Invocation
budget and loader rollback contracts remain proved. End-to-end AML method
semantics, synchronization and service authority are not proved by this result.

## Nested integer expressions (2026-10-01)

AML_Execute now evaluates Add/Subtract/Multiply/And/Or/Xor through a fixed
64-frame operand stack. Left operands complete before right operands; nested
local-target effects are visible to subsequent operands. Targets currently
support NullName (discard) and locals only. Results are normalized at each
operation to DSDT width. Expressions work inside Return/Store and as standalone
statements. The latter intentionally discard their computed value after target
handling; GNATprove reports that unused output as a warning.

Fuel charges each operator/source operand before decoding it. Unwinding is
bounded by the explicit stack depth; there is no host-language recursion.
Excess nesting reports Expression_Limit. This does not implement general AML
coercions, references, arguments as targets, name targets or other operators.

**AML-EXPRESSION-CHECK: PASS12367**: six operators x32x32 operands x2 widths,
depths1–65, nested target ordering, standalone expressions, fuel/truncation
boundaries and 64-bit wraparound. All prior suites pass.
Combined SPARK **544 results, zero unproved/justified**: 374 runtime checks,
60 assertions, 45 contracts, 35 initialization and 30 termination. Stack bounds,
monotonic charging and termination are proved; expression numeric semantics
remain regression-tested, not yet a complete formal semantic refinement.

## Table-facing service core (2026-10-01)

New `userspace/services/acpi/ACPI_Service` admits copied complete DSDT/SSDT
snapshots with the shared Firmware_Tables parser and invokes the transactional
loader. It retains DSDT width across SSDTs, rejects duplicate provider IDs and
out-of-order tables, and exposes bounded table/byte/object/rejection metrics.
This is an in-process service state machine, not native IPC/CCL integration.

**ACPI-SERVICE-CHECK: PASS85**: order, duplicate ID, every single-byte checksum
mutation, AML rollback, DSDT vs SSDT width, exact input extent, per-table bytes,
32-table capacity and clean restart width selection. All earlier suites pass.
The total-byte/rejection-counter ceilings have type/proof coverage, not tests
that exhaust every counter value. Provider identity/authentication and stable
snapshot memory are trusted adapter obligations.

Combined SPARK **856 results, zero unproved/justified**: 586 runtime checks,
84 assertions, 83 contracts, 56 initialization, 47 termination. This aggregate
includes a second namespace instantiation inside the actual service core; it
must not be interpreted as 856 independent AML semantic properties. Install's
failure namespace-preservation postcondition is proved. Full service readiness,
capability/IPC correctness and all AML semantics remain outstanding.

## Integer result contracts (2026-10-01)

The executor now calls AML_Integers.Apply. Its postcondition specifies the exact
result of each supported integer opcode in Ada modular arithmetic, followed by
the DSDT-width mask; 32-bit results are also explicitly bounded. This proves
local integer-operation semantics for integer inputs, not AML implicit
conversion, target storage, parsing or whole-expression evaluation semantics.

**AML-INTEGER-ORACLE: PASS1728** runs actual AML_Execute Return expressions against
Python arbitrary-precision arithmetic followed by modulo 2**width. Vectors cover
both widths and boundaries around2**31,2**32,2**63 and2**64, including underflow
and multiplication overflow. The generator runs inside the Nix test command;
no third-party interpreter dependency is added. All prior suites pass.

Combined SPARK **858 results, zero unproved/justified**: 582 runtime checks,
84 assertions, 85 contracts, 57 initialization and50 termination. Aggregate
changes reflect factoring/instances, not a monotonically increasing measure of
correctness. Whole-expression and whole-interpreter semantic proof remains work.

## Conditional execution (2026-10-01)

If/Else now uses a fixed64-frame block stack. Predicates use the existing
integer expression evaluator, bounded by their If package; zero chooses Else
and nonzero chooses the If body. Completion resumes after the corresponding
Else package, restoring the enclosing operand limit. Unselected bodies are not
executed. Else framing is checked before predicate execution; malformed skipped
Else extents are rejected. Standalone Else is unsupported. No While yet.

**AML-BRANCH-CHECK: PASS588** checks256 predicates with immediate Return and
with local stores followed by a common Return, depths1–65, an operand cut off by
its package, skipped vs selected unsupported bytes, standalone Else and fuel
boundaries. All previous hosted suites pass.
Combined SPARK **895 results, zero unproved/justified**:610 runtime checks,
86 assertions,86 contracts,63 initialization,50 termination. Bounds and
lexicographic budget/block-depth termination are proved; complete branch
semantics remain regression evidence, not full interpreter refinement.

## Loops and ACPICA validation (2026-10-01)

While reevaluates its predicate on each iteration. Break and Continue unwind to
only the nearest active loop, including from inside If/Else. All iterations use
one invocation work budget. AML-LOOP-CHECK309 covers countdowns, exact fuel,
infinite loops, nested-loop selection, nested If unwinding and invalid control.
The loop milestone proof contained901 results, zero unproved/justified.

The standard validation command now includes the independent ACPICA workflow:

```sh
nix develop -c bash tests/aml-core/run.sh --prove --acpica
# Reference integration alone:
nix develop -c bash tests/aml-core/run-acpica.sh
# Completion gate: fail for any unsupported or unselected upstream entrypoint:
nix develop -c bash tests/aml-core/run-acpica.sh --require-full
```

`run-acpica.sh` obtains ACPICA tools from the repository's locked Nixpkgs
(currently20260408), without changing the shell, global installation, or native
artifacts. `acpica_upstream.py` downloads upstream release20260408 commit
`232ff3f8ae1a4da11c709f61d9154482cfe8e6df`, verifies archive SHA256
`91addf34cf6f00c310dcff1d456c2e04629c2b45efe409385068e9fc5e9fe43b`, and
extracts unmodified sources with their license notices into ignored build output.
No upstream source is copied into the shipped interpreter. First use needs
network access; the verified archive can subsequently be reused offline.

Initial upstream selection is functional arithmetic, control and logic, each in
n32/n64/o32/o64 (unoptimized/optimized,32/64-bit) modes. Compilation uses upstream
common `-of -cr -vs` flags and control's expected diagnostics6152/6163/6022.
Control additionally suppresses6141: its deliberately empty Device lacks a
_HID/_ADR and this release's compiler otherwise rejects the fixture. This is a
harness compatibility adjustment; ASL source remains unchanged. Execution uses
upstream normal-mode entrypoint MN00 and `-ef -el -to 60`; slack mode, fatal-op
special builds, other runtime collections and compiler-negative tests remain
outside this initial selection.

Each compiled table runs first under AcpiExec, then through the real
ACPI_Service.Install/Namespace.Invoke path via a hosted file adapter. Reference
success requires the suite's explicit PASS marker, not merely exit status.
Generated JSON records reference root-method results (including BLOCKED),
CuBit passed/unsupported/failed outcomes, and every unselected runtime MAIN.asl
entrypoint. Known current CuBit gaps are INVALID_AML for arithmetic/logic's
unsupported framework and BYTE_LIMIT for the larger control table. These are
not interpreter passes. Other errors fail the workflow; `--require-full` also
fails known gaps and unselected entries. Reports and complete logs live in
`build/aslts/`; generated comparison logs live in `build/acpica/`.

`acpica_compare.py` separately compiles our small ASL cases with optimizations
disabled and compares integer results with AcpiExec. These are generated tests,
not upstream ASLTS coverage. They exercise arithmetic, nesting, If/Else and
While/Break/Continue through the table service. Comparison found that raw
external argument reads must retain the supplied value even in32-bit table
mode (also through local Store and direct Return). The executor and its old
truncation expectations were corrected. Literal decoding and arithmetic result
truncation still follow table width. Reference implementation detail:
`source/components/utilities/utcopy.c` preserves the external integer object's
value; `dispatcher/dsmthdat.c` retrieves the method argument object. This is
external API behavior evidence, not a proof of whole AML semantics.

A second differential failure located predicate conversion: raw arguments are
preserved, but If/While predicates are normalized to table integer width before
testing zero. A high-only64-bit external argument therefore tests false in32-bit
mode. Branch regression coverage now includes both widths (590 cases).

Validation result:142 generated differential comparisons pass. All12 selected
upstream configurations pass their ACPICA reference run; CuBit reports0 passes,
12 explicit unsupported cases,0 unexpected failures, and339 unselected runtime
entrypoints. These counts are configurations/entrypoints, not assertions or
unique AML operators. Full upstream-suite completion is not claimed. Existing
hosted suites pass (including branch590/loop309); final SPARK analysis contains
900 results with zero unproved/justified. Proof totals reflect actual analyzed
contracts and safety/termination checks, not a complete AML semantic proof.

## Integer logical expressions (2026-10-01)

`AML_Logic` specifies integer LAnd/LOr/LNot/LEqual/LGreater/LLess results, with
zero for false and all bits set at the table integer width for true. Its Apply
contract proves the exact result against the comparison/Boolean predicate.
String/buffer comparisons and implicit object conversions are not implemented.
The executor shares its bounded expression stack with these operations; LNot
consumes one source, binary logical operations consume two, and neither has a
target operand. Nested unary expressions unwind correctly into binary parents.
Binary evaluation is eager: the second operand executes even when the first
would determine the Boolean result. Existing budget and depth limits still apply.

AML-LOGIC-CHECK2013 covers both integer widths, values at32/64-bit boundaries,
all six operations, exact fuel exhaustion, absent source operands, unary stack
depths1–65, and uninitialized right operands that must not be short-circuited.
The ACPICA generated cases additionally compare nested Boolean expressions and
an observable local-store side effect in a logical right operand.

Logical milestone validation: hosted2013 and all prior suites PASS; expanded
ACPICA generated comparisons254 PASS. Upstream reference12 configurations PASS,
CuBit12 unsupported/0unexpected failures,339 unselected entrypoints unchanged.
SPARK912 results with zero unproved/justified, including local logical-result
contracts and expression safety/termination; no whole-interpreter semantic or
native integration claim.

## Read-only method namespace binding (2026-10-01)

`AML_Execute.Run_Bound` accepts an immutable context and a NameString lookup
function. `Run` remains the raw-body entrypoint without a namespace. Namespace
Invoke now uses the bound executor with the method's own namespace node as its
initial scope. Named integer operands work in Return, expressions, predicates
and Store-to-local; relative ancestor lookup, parent prefixes, rooted, dual and
multi-segment paths reuse AML_Names and Namespace.Resolve. Lookup of missing
names returns Unknown_Name; existing non-integer objects (including methods)
return Unsupported_Value. Malformed and truncated names remain distinct errors.
Method calls, namespace writes and general object conversion remain unfinished.

The namespace callback checks the existing parent-before-child invariant at its
entry before calling namespace operations. A nested generic formal precondition
for that invariant triggered GNATprove Assert_Failure spark_definition.adb:12572
in this pinned toolchain. The bounded runtime check avoids that compiler bug
without assuming the invariant or disabling proof. This adds an O(namespace
capacity) validation cost per name lookup; no allocation or hardware access is
introduced. The generic raw executor and both concrete namespace instances are
included in the proof run.

AML-BINDING-CHECK120 exercises both widths, shadowed names, one/two parent
prefixes, absolute/dual/multi paths, rejection of unrooted multi-segment ancestor search,
arithmetic/local storage, missing names,
non-integer objects, invalid parent traversal, malformed/truncated encodings,
budget exhaustion and unchanged namespace state after invocation. The hosted
table runner now accepts dotted method paths so ACPICA can independently check
methods nested under a device as well as root methods.

Binding milestone validation: all hosted suites pass; expanded generated ACPICA
comparisons352 PASS. SPARK1236 results have zero unproved/justified; inspection
of the report confirms Operand/Run_Bound and Read_Binding are proved in both
concrete namespace instances, plus the unbound executor. This aggregate includes
repeated generic-instance checks and is not a measure of full AML coverage.
Upstream reference12 configurations PASS; CuBit12 unsupported/0unexpected
failures,339 unselected runtime entrypoints remain. No native/CCL/hardware
integration claim is made by these hosted results.

## Bounded integer method calls (2026-10-01)

Name lookup now distinguishes methods from integer/non-integer data objects.
The bound executor obtains a copied method definition through a separate
read-only callback and evaluates its0–7 arguments left-to-right. Pending call
arguments share the existing expression stack, but each callee has its own
locals, arguments, block state and method namespace scope. Calls can appear
inside expressions or as statements. A method falling through without Return
is allowed as a statement; use where a value is required returns Missing_Result.
Serialized/synchronization flags remain explicitly unsupported.

All callees consume the same invocation work budget. A callee receives only the
remaining fuel, and its charged work is added to the caller even on failure.
The default call-depth budget permits32 nested calls beyond the initial method;
exhaustion reports Call_Limit. Budget exhaustion remains separately reported.
There is no general object/reference argument passing or namespace mutation yet.

Run_Bound, Operand and Dispatch have compatible lexicographic termination
measures: remaining call depth followed by their phase (2,1,0). Expression and
block loops retain their independent fuel/depth measures. Explicit Global
contracts preserve caller locals/parser position across dispatch; only the
caller's charged counter changes. This hosted implementation uses bounded Ada
recursion. Native service stack sizing remains an integration requirement; no
native stack-capacity or hardware execution claim follows from hosted proof.

AML-CALL-CHECK180 covers nested calls/arguments, caller-local isolation, ordered
argument effects, void statement/value/argument contexts, all seven parameters,
serialized sync-level ordering, truncated arguments, unbounded self-recursion rejection,
and countdown depths0–40 in both integer widths. Successful cases also run with
one fewer than their measured required work units and must exhaust the budget.
Generated ACPICA comparisons add nested calls, argument effects, void statements,
recursive countdowns and callee namespace scope. The historical read-only
binding milestone's rejection of method references is superseded by this work.

Call milestone validation: hosted180 and all previous suites PASS; generated
ACPICA comparisons430 PASS. SPARK1456 results have zero unproved/justified;
concrete recursive executor, Operand and Dispatch instances are included, with
termination and shared fuel accounting checked. This is not a full AML semantic
refinement proof. Upstream reference12 configurations PASS; CuBit12 unsupported/
0unexpected failures and339 unselected runtime entrypoints remain unchanged.

## Shared AML object storage (2026-10-01)

`AML_Objects` provides arena-local handles for integers, byte-backed strings and
buffers, and packages of handles. Object slots (1024), byte storage (65536) and
package-element storage (4096) have separate limits. Zero is an uninitialized
package element, distinct from an allocated Integer zero. Packages can reference
existing objects, themselves, or other packages. Allocations are append-only:
there is no slot reuse or collector yet, and handles must not cross arena
lifetimes. General reference ownership/reclamation remains future work.

Named integers, strings and buffers in Namespace now use the arena; method
bodies retain separate copied code storage. Namespace invariants tie every data
node's handle to an existing object of the matching kind. Table loading still
builds a candidate State and publishes only on complete success. Aggregate byte
exhaustion reports Value_Limit and leaves the original namespace unchanged.
Existing namespace buffer getters retain their one-based result bounds; arena
byte slices are unconstrained and callers must respect their returned bounds.

The arena contracts cover validity, allocation failures leaving State unchanged,
new object kinds/contents, zero-initialized package elements and valid element
handles. The current executor still evaluates integer values. Package AML
loading/execution, general object arguments/results, mutable namespace semantics
and reclamation are not implemented by this storage milestone.

Storage milestone validation: hosted object checks1107, service checks86 and
all previous suites PASS. SPARK1633 results have zero unproved/justified checks;
these include arena contracts and both concrete namespace instances. Generated
ACPICA comparisons430 PASS. All12 selected upstream configurations PASS under
ACPICA; CuBit reports12 unsupported,0 unexpected failures and339 unselected
runtime entrypoints. The combined validation command exited0.

Allocation frame contracts additionally require `Extends(Store, Store'Old)`:
all previously allocated object records, byte storage and package-element
storage remain unchanged. This covers stored links without traversing package
graphs, including cycles. The byte/element copy loops carry explicit prefix
preservation invariants. This is an allocation property; it does not claim
that package mutation preserves other elements or establish AML Store semantics.

Frame-contract validation completed: SPARK1648 results, zero unproved or
justified; all hosted suites and430 ACPICA differential comparisons PASS.
Upstream reference12 configurations PASS; CuBit12 unsupported,0 unexpected
failures and339 unselected entrypoints. No native execution is covered here.

## Constant package loading (2026-10-01)

`AML_Data.Load` builds integer/string/buffer and nested Package/VarPackage
constant objects in the arena. An explicit 64-frame stack bounds nesting;
there is no recursive parser stack. PackageOp uses its byte count; VarPackageOp
currently accepts an integer-constant count. Unfilled elements retain the
uninitialized handle. Initializers beyond the declared count are rejected as
malformed. Name references, computed counts and computed buffer sizes remain
unsupported rather than being evaluated with invented values.

Named packages now load through the actual namespace/service table path.
`Value_Store` and `Data_Object` permit read-only hosted inspection of their
objects; these are internal APIs, not native CCL endpoints. The existing VM
still cannot execute package-valued arguments, locals or returns.

The loader publishes a candidate arena only on success. Its SPARK contracts
cover valid handles, consumed-byte bounds, preservation of old kinds/lengths
and complete rollback on failure; loop variants prove termination. They do not
constitute a full package semantic refinement proof. Hosted checks exercise
every supported nesting depth, the first refused depth, truncated initializers,
partial-allocation failure, element exhaustion and high array bounds.

`acpica_packages.py` compares complete preorder package/string/integer/null
trees loaded from identical iASL-produced tables, in both integer widths.
This is a data-loading comparison, not evidence of package execution in our VM.

Validation: package checks3263 PASS, all prior hosted suites PASS; SPARK1841
results with zero unproved/justified checks. Existing generated comparisons430
PASS and new whole-package comparisons12 PASS. The selected upstream reference
configurations12 PASS; CuBit12 unsupported/0 unexpected failures,339 unselected
entrypoints. Combined run15853, expanded tests35086 and package comparison4366
all exited0. The ACPICA workflow now includes both generated comparison tools.

## Object inspection during method execution (2026-10-01)

The integer-result executor implements SizeOf and ObjectType for named loaded
objects and the currently supported integer locals/arguments. Read-only binding
metadata supplies string/buffer/package sizes and namespace object type codes.
ObjectType inspects method names without invocation or argument consumption;
uninitialized locals yield type zero. Debug inspection yields its type without
emitting output. Integer SizeOf follows ACPICA's implicit conversion behavior,
returning the admitted table's integer byte width (4 or8).

Inspection results compose with arithmetic, logic, local stores and calls.
The query and its source consume execution fuel. Reference-producing operands
(Index, RefOf, DerefOf), general typed locals/arguments and type conversion
remain incomplete. This does not turn loaded packages into executable VM values.

Validation completed: hosted inspection234 and all prior suites PASS; SPARK1946
results with zero unproved/justified, including Inspect in all three concrete
executors. Generated method comparisons668 and package comparisons12 PASS
against pinned ACPICA. Upstream reference12 configurations PASS; CuBit12
unsupported/0 unexpected failures,339 unselected entrypoints. Combined27526
exited0. These are hosted results, not native integration or full AML semantics.

## Shift and bitwise operators (2026-10-01)

The integer executor additionally supports ShiftLeft, ShiftRight, NAnd, NOr
and Not, with Null/local targets and nesting in existing expressions. Not is
unary; its target is consumed after one source operand. Shift counts at least
the admitted table's bit width produce zero. External integer arguments retain
their raw values through operand reads; shift inputs/counts are not truncated
before the operation, and the result is normalized to the table width.

`AML_Integers.Apply` specifies each operator's exact word operation and result
normalization. The independent Python oracle now includes boundary counts and
raw high-bit arguments; its previous assumption that arguments were normalized
first was harmless for the old operators but would be incorrect for right
shift. Hosted expression checks additionally cover each local target, nested
unary operations, truncation and insufficient fuel. ACPICA comparisons exercise
shift widths/boundaries, unary targets and mixed nested operations.

Validation completed: integer oracle6516/expression12769 and all prior suites
PASS; SPARK1955 results, zero unproved/justified. Generated ACPICA method862
and package12 comparisons PASS. Upstream reference12 configurations PASS;
CuBit12 unsupported/0 unexpected failures,339 unselected entrypoints. Combined
22900 exited0. General object execution, further operators and native service
integration remain incomplete.

## Integer division and remainder (2026-10-01)

Divide and Mod use unsigned integer operands and return `Division_By_Zero`
before host division when the divisor is zero. The arithmetic contract requires
a nonzero divisor and specifies quotient/remainder with result normalization.
Divide consumes two targets, remainder first and quotient second, so aliasing
local targets leave the quotient. Null targets discard their corresponding
result. General reference/namespace targets remain unsupported.

The remainder written by Divide is a separate value from its quotient result;
the implementation preserves that raw remainder when storing to a local.
Mod and the Divide expression result use the table width. Dedicated ACPICA
comparisons check this distinction with high external arguments in legacy
tables, plus shared targets and explicit zero-divisor failures. Hosted tests
cover every pair of local targets, nested consumption of the remainder,
truncation, unsupported targets and budget exhaustion.

Validation completed: division1048/integer oracle7740 and prior suites PASS;
SPARK1989 results, zero unproved/justified. Generated ACPICA method/error962
and package12 comparisons PASS. Upstream reference12 configurations PASS;
CuBit12 unsupported/0 unexpected failures,339 unselected entrypoints. Combined
23431 exited0. The raw remainder behavior is differential-tested, not a complete
formal model of object identity, coercion or general target semantics.

## Implicit integer conversion (2026-10-01)

`AML_Coercions` converts string/buffer byte sequences to integers. Strings use
hexadecimal accumulation, stopping at the first invalid byte or before the
table integer width would overflow. Leading whitespace, optional 0x/0X and
empty strings follow the pinned ACPICA behavior. Buffers use little-endian
bytes up to the table integer width; empty buffers report `Empty_Buffer`.

The executor requests conversion for integer arithmetic/logical operands,
predicates and the right operand of integer-first comparisons. Both named and
inline constant strings/buffers are supported. It does not coerce direct object
returns, object-valued method arguments or string-first comparisons into a
different meaning: those still require the unfinished general typed VM.
Read-only bindings carry conversion results for the current method width.

Contracts establish converted status, width bounds, buffer emptiness behavior
and safe terminating parsing. The strengthened buffer contract specifies all
eight result octets: each is the corresponding input byte within the table
width, or zero otherwise. Its fixed eight-step implementation is proved against
that exact little-endian relation. Complete string parsing semantics remain
unproved. Hosted checks cover byte classes and
array bounds; the ACPICA coercion harness compares actual same-table method
results and empty-buffer failures in both widths.

Validation completed: hosted coercion646 +prior suites PASS; SPARK2159 results
with zero unproved/justified. ACPICA method/error962, package12 and coercion268
comparisons PASS. Upstream reference12 configurations PASS; CuBit12 unsupported/
0 unexpected failures,339 unselected entrypoints. Hosted/proof73066, targeted
comparison26579 and full ACPICA69029 exited0. No native execution is covered.

The buffer semantic proof extension adds exhaustive place-value checks for all
256 byte values at each of12 positions, in both widths, with array bounds at
Positive'Last. This includes every retained byte and ignored suffix positions;
the existing mixed-byte and empty-buffer checks remain in place.

Exact-buffer validation completed: conversion6790 +prior hosted suites PASS;
SPARK2162 results, zero unproved/justified, including the exact octet contract.
ACPICA method/error962, package12 and coercion268 comparisons PASS. Upstream
reference12 configurations PASS; CuBit12 unsupported/0 unexpected failures,
339 unselected entrypoints. Combined32871 exited0. Full string semantics and
the broader interpreter/service goal remain incomplete.

## Snapshot bootstrap and native compilation (2026-10-01)

`ACPI_Bootstrap` requires exactly the trusted provider's advertised table count
before Finish publishes Complete. It admits DSDT first, then ordered SSDTs via
the existing service core. Failed imports, extra tables before Finish, and
premature Finish are sticky failures. Completion does not establish that the
provider included all mandatory platform tables or that devices are activated.
The hosted checks exercise all supported snapshot sizes, partial completion,
duplicate identities, failed AML admission and calls after completion/failure.

The native compilation gate is separate:

```sh
nix develop -c bash tests/aml-core/run-native.sh
```

It acquires the shared build lock and compiles an assertion-enabled static
library against the CuBit userspace runtime. It neither stages nor launches a
service. `build/native/stack-report.json` records compiler frame estimates;
the current largest frame is 963152 bytes, with nine dynamic frame records.
These figures do not establish a whole-call-chain or secondary-stack bound.
Native stack sizing, table transport, the IPC loop and CCL bindings remain open.

Validation completed: hosted bootstrap1257 and all prior suites PASS; SPARK2196
analysis results, zero unproved/justified. Combined87295 exited0 with ACPICA
method/error962, package12 and coercion268 comparisons PASS. All12 selected
upstream configurations passed the ACPICA reference run; CuBit reports0 passes,
12 unsupported,0 unexpected failures and339 unselected entrypoints. Native42552
exited0 with the frame estimates above. No native service execution is claimed.

## Named object values in execution (2026-10-01)

Locals and internal method arguments now carry a discriminated integer/object
value. Named strings, buffers and packages can pass through Store-to-local,
method arguments, nested calls and returns. ObjectType and SizeOf inspect their
actual type and length. Integer consumers convert at the consumer boundary,
including after a method returns an object; both table widths are retained in
the immutable binding metadata. Integer entrypoints remain convenience wrappers
around the same typed executor.

An object result identifies its backing value in the same immutable namespace
snapshot. This is value provenance, not an AML RefOf/Index reference or mutation
authority. Sharing that immutable backing storage preserves the supported read
behavior; it does not implement mutable object-copy semantics. Do not carry IDs
into a different namespace snapshot. Mutation, reference targets, inline object
allocation and general object comparison remain unfinished.

The new ACPICA comparison executes methods returning actual strings, buffers
and nested packages, including uninitialized tails. It compares complete output
contents, plus type/size queries and conversion through local/call boundaries.
The hosted fixture exercises local overwrites and insufficient execution budgets.

Typed execution validation: hosted7314 (including all seven argument positions,
all eight locals, caller-local preservation and namespace-root rejection) and
prior suites PASS. SPARK2383 analysis results, zero unproved/justified; the
proved scope is runtime safety, termination and the published contracts, not
full AML semantic refinement. Typed ACPICA212 comparisons PASS, alongside
method/error962, package12 and coercion268. The current typed revision has not
been compiled against the CuBit runtime: its nonblocking native gate found the
shared build lock busy. Earlier native stack estimates describe the preceding
revision only; no native service execution is claimed.

Full combined50033 exited0. All12 selected upstream configurations pass the
ACPICA reference run; CuBit still reports0 passes,12 unsupported,0 unexpected
failures and339 unselected entrypoints. These remain explicit compatibility
gaps. Typed7314 was verified by the supplemental69722 run after adding the
root regression; combined50033's earlier hosted phase checked7300 cases.

## Store expressions and method argument writes (2026-10-01)

Store can now appear as a source expression, preserving its source value for
its enclosing expression after the target write. A bounded expression frame
keeps Store's source unconverted until its consumer requests an integer.
Methods have invocation-local argument slots and readiness bits: Store and
arithmetic targets can replace Arg0 through Arg6, and subsequent reads,
ObjectType and SizeOf see the replacement. Such replacement does not mutate
the caller's argument or the backing namespace object. This covers values;
the special automatic dereference rule for RefOf arguments remains unfinished.

The slot-assignment contract specifies the exact updated value/readiness bit
and preservation of all other local and argument slots. The target parser
preserves state on rejection or truncation. Zero, One and Ones targets discard
the result, following pinned ACPICA's constant-target behavior. Division still
writes remainder before quotient, including when both targets name one argument.
All targets remain single-byte locals, arguments or constants; named targets,
fields and reference targets are not enabled by this change.

Hosted cases cover nested Store to depth64, insufficient fuel, every local and
argument slot, unrelated-slot preservation, and all225 pairs of local/argument
division targets. ACPICA comparisons check actual expression values and target
side effects, complete returned objects, invocation isolation and both widths.

ACPICA exposed a necessary evaluation-order correction: a Local/Arg source
operand is a pending slot read until its parent operation resolves operands.
For example, after Local0 is9, Add(Local0, Store(One, Local0)) produces2,
not10. Source slots now resolve after sibling expressions have executed, for
both arithmetic operands and method arguments. Computed expression results
remain captured values. Conversion then observes the resolved value's type.
These internal pending reads cannot escape a method as AML references.

Targeted validation completed: hosted Store4137/typed11938 and prior suites
PASS; SPARK2527 analysis results, zero unproved/justified. The520 new ACPICA
comparisons PASS, including18 raw-AML constant-target cases (ASL cannot spell
those targets directly). Native13003 compiles the current core against the
CuBit runtime; largest compiler frame963152bytes,12 dynamic frame records.
This supersedes the previous typed revision's deferred native compile, but
still does not prove total stack use or native service execution.

Full ACPICA82560 exited0:1974 differential comparisons PASS (962 method/error,
12 package,268 coercion,212 typed,520 Store). All12 selected upstream
configurations pass under ACPICA; CuBit reports0 passes,12 unsupported,
0 unexpected failures and339 unselected entrypoints. These remain compatibility
gaps. Hosted/proof5890 and native13003 also exited0; no service was launched.

## Bounded service requests (2026-10-01)

`ACPI_Requests` separates observer queries from trusted snapshot-provider writes.
It stages at most one 64KiB table in sequential 16-byte chunks and passes
complete tables to the existing bootstrap admission core. Revisions reject stale
mutations and stop at signed 64-bit maximum. All observation reply words fit
CCL integers. The trusted adapter's caller classification remains an assumption;
there is no native IPC endpoint, table provider or CCL binding yet. See the
[draft packet and metric layouts](../../userspace/services/acpi/README.md).

Hosted request checks pass 91856 cases: all snapshot sizes, exact metric pages,
unclassified and observer callers, every invalid length/flag/reserved header
value, malformed padding, stale replays, partial uploads, maximum table size,
sticky admission/completion failure and revision exhaustion. SPARK reports
2578 analysis results with zero unproved or justified results. Contracts include
unchanged state for queries/unauthorized calls and rejected requests, monotonic
revision and signed-safe replies. They do not prove native authentication or
complete AML semantics.

The native compile attempt69959 found the shared build lock busy and exited1
before compilation. Native13003 predates this request handler; its successful
archive build and frame estimates do not validate this revision.

Full combined28468 exited0: all hosted fixtures pass, SPARK2578 has zero
unproved/justified results, and all1974 differential comparisons pass against
ACPICA. All12 selected upstream configurations pass the ACPICA reference;
CuBit reports0 passes,12 unsupported,0 unexpected failures and339 unselected
entrypoints. This is a development regression gate, not full ASLTS conformance.

## Endpoint authority and native message adapter (2026-10-01)

`ACPI_Endpoint` classifies exact trusted authority stamps against distinct,
nonzero observer/provider tags. Invalid configuration denies all requests.
Dispatch calls the existing request handler and encodes canonical four-word
success/error responses. The native adapter consumes the actual runtime
Message type, including its separate `authorityTag`, and clears outgoing
authority metadata. Trusted configuration and a kernel-received input are
caller obligations, not properties established by this adapter.

Hosted endpoint checks pass589963 cases, including the full16-bit stamp space,
64-bit boundary tags, invalid configurations, all reserved header values,
forged payload tags, observer queries/writes, stale mutations and sticky finish
failure. SPARK2585 analysis results have zero unproved/justified results;
portable dispatch proves observer preservation, denied replies without revision
data, canonical reply headers and signed-safe words. The mechanical native
wrapper is outside the SPARK proof scope.

Native98353 exited0 compiling the request core, portable endpoint and actual
CuBit.Messages adapter. This supersedes the preceding request revision's
deferred native compile. Compiler frame estimates remain largest963152bytes
with12dynamic records; total call-chain/secondary-stack bounds are unverified.
No service loop, capability allocation, CCL binding or native execution is
claimed. The standard full ACPICA regression remains part of this workflow.

Combined33500 exited0: hosted endpoint589963/request91856 and all prior suites
PASS, SPARK2585 zero unproved/justified, all1974 ACPICA differential comparisons
PASS. All12 selected upstream configurations pass the ACPICA reference; CuBit
reports0 passes,12 unsupported,0 unexpected failures,339 unselected entrypoints.
These remain explicit compatibility gaps, not completed interpreter validation.

## Shared method-code storage (2026-10-01)

The pinned arithmetic table contains the1170-byte CST0 method, exceeding the
old1024-byte buffer-literal storage reused for every method. Namespace nodes now
store extents into one append-only65536-byte method pool. Buffer-literal limits
are unchanged. Method definitions carry exactly their body's length, and both
direct invocation and nested calls use the copied bytes. Failed table loading
rolls back the pool along with the namespace. Metric page5 reports used,
capacity and remaining method-code bytes.

Append_Code's contract specifies exact copied bytes and preservation of the old
prefix; a loader assertion connects the admitted method's extent to its source.
The proof exposed a real empty-method slice overflow when the source ended at
Positive'Last. The empty path now avoids computing that unrepresentable index.
Hosted141 cases cover that regression, body sizes0..65536 at selected boundaries,
pool exhaustion, multiple admissions, rollback, independent caller storage,
instruction budgets and high input indices. Request92021 and endpoint589966
checks include the new metric page and a nonzero method upload.

The standard ACPICA workflow now includes28 large-method comparisons across both
integer widths, invoking methods directly and through another method. These
passed in targeted19436. Native65782 exited0; the largest compiler-reported
frame is703088bytes (previously963152), with12dynamic records. This is not a
whole-call-chain or secondary-stack bound and does not establish native launch.

The upstream tables still exceed other development budgets: a scan of their
top-level declarations finds482 for arithmetic and433 for logic, above the
128-node namespace capacity. Removing the method-body restriction does not
establish that these complete suites can load or execute.

Final-source SPARK validation reports2686 analysis results with zero unproved
or justified results, including25 checks for Append_Code in each namespace
instantiation and30 checks for the six-page request handler. The first rerun
encountered a damaged generated proof session after the intentional overflow-fix
interruption. That cache was preserved outside the active proof directory, and
the unchanged source passed on33747. This is a proved storage/bounds increment,
not a complete formal model of all AML semantics.

Full33747 exited0: hosted method-storage141/request92021/endpoint589966 and
prior suites PASS; SPARK2686 zero unproved/justified; ACPICA2002 differential
comparisons PASS (including28 new large-method cases). All12 selected upstream
configurations pass the ACPICA reference; CuBit remains0PASS/12unsupported/
0unexpected failures, with339 unselected entrypoints. Native65782 also exited0.

## Namespace-bound VarPackage counts (2026-10-01)

Precise load diagnostics identified the upstream arithmetic failure at
declaration74, ERRP: its VarPackage count references ETR0. The data loader now
has a bound-count variant, and namespace loading resolves already-admitted
named integers from an immutable snapshot in the initializer's lexical scope.
It supports ancestor search, root/parent prefixes and qualified names. The
parser validates the callback's consumed extent before advancing and retains
transactional rollback. Plain AML_Data.Load remains literal-only.

Counts are evaluated once. Arbitrary count expressions, method calls, forward
references, non-integer conversion and truncating surplus initializers remain
unsupported. This is not a general load-time AML evaluator or a complete formal
semantic refinement of VarPackage.

The service namespace development budget is now512 nodes. Hosted tests fill it
exactly and verify that an additional declaration rejects without changing the
namespace. Last_Load_Code records the last actual AML load attempt; it survives
header/checksum rejection. Metric page6 exposes it with namespace capacity and
remaining nodes. Logical reply words are structurally bounded to signed64 range;
upload packet words still hold arbitrary byte payloads.

Validation: named-count8758/service93/request92157/endpoint589969 and all prior
hosted checks PASS; SPARK3005 analysis results, zero unproved/justified. The38
new ACPICA comparisons pass both widths and scope/nested-package cases. These
fixtures are now included in the standard runner. Native89187 exited0 compiling
the current core; largest compiler frame1174000bytes,13dynamic records. This
increase needs stack work before native launch; whole-call/secondary-stack
bounds remain unverified. The previous703088-byte report predates this change.

The actual upstream arithmetic admission now reaches declaration392 P000 before
VALUE_LIMIT. All eight arithmetic/logic configurations report VALUE_LIMIT under
the current loader; control remains above the table-byte budget. No complete
upstream configuration is claimed passed. The detailed hosted runner prints
the namespace load status instead of only the service's generic INVALID_AML.

Final integrated run35968 exited0: all hosted checks and2040 ACPICA differential
comparisons passed, including the38 named-count comparisons. All12 selected
upstream configurations pass the ACPICA reference; CuBit reports0PASS,
12unsupported and0unexpected failures, with339 unselected entrypoints. A normal
runner exit therefore establishes the current regression baseline, not upstream
conformance; `--require-full` rejects these coverage gaps. SPARK3005 and the
successful native compilation above cover the same core source.

## Complete arithmetic/logic table admission (2026-10-01)

The hosted runner accepts `table_runner table.aml --metrics` to report admitted
namespace nodes, value objects, value bytes, package elements and method bytes
without executing AML. Failed installation still exits nonzero and reports the
load status; transactional rollback means service metrics cannot expose a
partially installed table.

Prefix admission isolated two storage limits in the unoptimized 32-bit upstream
arithmetic table: P000 required 21 objects with 1,014 of 1,024 already used;
after increasing that budget, P052 required 26 elements with 4,083 of 4,096
used. The arena now permits 2,048 objects and 8,192 elements. Exact exhaustion
and rollback tests use the configured constants; named-count tests retain the
4,096/4,097 cases and add the new boundary and boundary plus one.

All eight arithmetic/logic tables now admit successfully. Arithmetic uses 482
nodes, 1,808 objects, 5,199 value bytes and 4,211 package elements; logic uses
433 nodes, 1,922 objects, 7,033 value bytes and 4,366 elements. MN00 execution still
returns UNSUPPORTED: its first assignments update named SLCK/MLVL, whereas the
current evaluator can only assign local/argument slots. Admission is not a
passed suite execution. Control still exceeds the table-byte budget.

Hosted run 6308 passes (4,287 data checks, 33,348 named-count checks, 2,131 object
checks and prior suites); SPARK reports 3,005 results with zero unproved or
justified. Native run 21701 compiles the current core successfully; the largest
reported frame is 1,624,560 bytes with 13 dynamic records. This stack cost remains unsuitable as
evidence of safe native launch; whole-call and secondary-stack bounds are still
unverified. The upstream runner now requires arithmetic/logic admission to
succeed and records usage before classifying the known execution gap. An
installation regression is a failure, not an accepted unsupported result.

Final differential run 47406 passed all 2,040 comparisons. Upstream run 57767
passed all eight required admission checks and all 12 ACPICA reference runs.
CuBit execution remains at zero passes, 12 unsupported configurations and zero
unexpected failures, with 339 unselected entrypoints. These results cover the
same core source as the hosted tests, proof and native compilation above.

## Existing integer mutation (2026-10-01)

`AML_Objects.Set_Integer` changes an existing integer's payload in place.
Its ghost `Integer_Updated` relation specifies the exact resulting state as
the old state with only that payload replaced. The contract also preserves
validity, allocation usage, object kinds and lengths. This works at full
capacity and retains package links and cycles without allocating a replacement.

`AML_Namespace.Set_Integer` uses that operation for an already resolved integer
node. Its exact frame relation additionally preserves namespace entries and
all method storage. This is a storage primitive: it performs neither AML
implicit conversion nor destination-name resolution and grants no service
execution authority. The evaluator's namespace remains read-only during an
invocation; mutable `Store`, references and shared state across nested calls
still require evaluator integration.

The object fixture now passes 12,378 checks, including full-arena mutation,
integer-width boundary payloads, other-value preservation, retained cyclic
package links and namespace method reads after writes. Native run 14725
compiles the current core; the largest reported frame remains 1,624,560 bytes
with 13 dynamic records. Whole-call and secondary-stack bounds are unverified.

Final-source full SPARK verification in run 46054 reports 3,070 analysis results
with zero unproved or justified checks. The first attempt left two namespace
invariant checks unproved; explicitly preserving `Count` in the object update
contract resolved both without weakening the namespace invariant or using
assumptions. The full hosted suite passes on this same source.

Run 46054 finished successfully: all 2,040 ACPICA differential comparisons pass;
the upstream reference passes all 12 selected configurations, and all eight
required CuBit admission checks pass. Suite execution remains zero passes,
12 unsupported configurations and zero unexpected failures, with 339 unselected
entrypoints. Native run 14725 predates the explicit `Count` postcondition;
recompiling that final contract was deferred because the shared build lock was
occupied. No native launch is claimed.

## Mutable integer namespace execution (2026-10-01)

The evaluator core is now `Execute_Typed`, a procedure with an owned mutable
context and a write callback. Nested calls share that context. Read-only
`Run_Typed`/`Run_Bound` wrappers supply a rejecting callback and retain their
existing behavior. `Namespace.Invoke_Mutable` permits integer sources to update
existing named integer destinations, including integer-operation targets, using
the executing method's integer width. Completed writes survive later execution
errors. The hosted table runner uses this entrypoint; service IPC still exposes
no AML execution authority.

ACPICA caught a distinction in operand evaluation: with NUM0 initially 9,
`Add(NUM0, Store(7, NUM0))` returns 16, not 14. Named integer reads capture the
earlier value; Local/Arg slots retain deferred resolution. The new 48-case
`acpica_named_store.py` comparison covers that distinction, nested calls,
scoped destinations, integer widths, repeated targets and argument replacement.
It is included in `run-acpica.sh`. Hosted call checks increase to 204, including
write visibility, a later division error and rejection by read-only invocation.

All eight upstream arithmetic/logic modes now reach `SYNC_UNSUPPORTED` at a
serialized call after the initial named writes. The upstream runner requires
that exact blocker and successful table admission. It does not count this as a
suite execution pass. General object conversion on assignment, mutable
references, non-integer destinations, Debug output and serialized execution
remain unfinished.

The mutable generic exposes a context-validity predicate, preserved explicitly
by helper contracts and loop invariants. Concrete write callbacks carry the
same pre/postcondition. Applying these aspects to the formal callback itself
triggered a GNAT 16 internal compiler error; the concrete-callback arrangement
compiles and the focused namespace proof passes (34183). No namespace invariant
was weakened and no proof assumptions were added. Final full validation is
running on this source.

Run 46346 passes the complete hosted suite and full SPARK analysis: 4,101
results, zero unproved or justified checks. This includes the mutable and
read-only instantiations, bounded recursion and the explicit context-validity
contracts. The empty unbound context also has an explicit, proved always-valid
postcondition. These are safety and published-contract proofs, not a complete
formal refinement of AML semantics. ACPICA validation is still running.

Final run 46346 exited successfully: all 2,088 differential comparisons pass,
including the 48 new named-store cases. All 12 selected upstream configurations
pass under ACPICA; CuBit passes all eight required admission checks and reports
zero execution passes, 12 unsupported configurations and zero unexpected
failures, with 339 unselected entrypoints. Arithmetic/logic now stop at the
explicit serialized-call blocker. Final native compilation was deferred because
the shared build lock remained occupied; earlier native frame measurements do
not validate this evaluator revision. No native service launch is claimed.


## Synchronous serialized calls (2026-10-01)

Serialized methods now enforce sync-level ordering. Nonserialized callees
inherit their caller's current level and ignore their encoded sync-level bits,
matching ACPICA. Each synchronous call carries its level as an immutable
parameter, so returning or unwinding an error restores the caller's level.
Same-level recursive entry succeeds; reentry into a lower-level serialized
method returns `Mutex_Order`. This implementation requires exclusive context
ownership for the entire invocation. It does not implement concurrent AML
threads, yielding or the explicit mutex opcodes.

Hosted method checks pass 533 assertions and call checks pass 4,072 assertions.
The new ACPICA differential matrix passes 1,200 comparisons across both integer
widths: every pair of 16 sync levels, nonserialized bridges, recursive entry,
sibling-level restoration and a fresh invocation after an ordering error.

All eight upstream arithmetic/logic modes now reach `UNSUPPORTED` at the
method-local `Method` declaration at the start of `STRT` in the pinned
`runtime/cntl/common.asl`. This replaces the earlier serialized-call blocker;
it is still an unsupported execution result, not a suite execution pass.


The standard `run-acpica.sh` now runs `acpica_serialized.py` alongside the
existing comparisons and the pinned, checksum-verified upstream ASLTS
collections. `acpica_upstream.py --require-full` remains strict: unsupported
configurations and unselected entrypoints prevent a full-compatibility pass.
The development baseline does not relabel unsupported cases as passed.

Run 72364 completed all hosted regressions and SPARK analysis successfully:
4,117 results, zero unproved or justified checks. These prove the published
safety/contracts, not complete AML semantic conformance. Native run 95098
compiled this evaluator revision against the CuBit runtime successfully.
The largest reported frame is 1,624,560 bytes, with 18 dynamic frame records;
a whole-call-chain and secondary-stack bound is still unverified. This was
compilation only, with no service launch or hardware execution.

Final comparison runs 72945 and 42057 both exited successfully: all 3,288
ACPICA differential comparisons pass, including 1,200 serialized-call cases.
The pinned upstream reference passes 12 selected configurations. CuBit passes
the eight required arithmetic/logic admission checks and reports zero execution
passes, 12 unsupported configurations and zero unexpected failures, with 339
unselected entrypoints. Arithmetic/logic stop at the method-local declaration;
control exceeds the table-byte limit. All verification jobs are complete.


## Temporary method declarations (2026-10-01)

Mutable execution now handles `Method` declarations at the point of execution.
Declarations use the executing method as their owner, including names placed at
the root or under another scope. Each method tracks its active invocation count;
recursive returns preserve its declarations until the last invocation exits.
Final exit removes both that method's temporary subtree and its declarations
elsewhere, matching the pinned ACPICA `AcpiDsTerminateControlMethod` behavior.
Cleanup also runs after budget exhaustion or execution failure. Completed writes
to existing integers remain visible.

Live node IDs remain stable during execution. Removed interior slots stay
reserved while later live nodes exist; unused tail slots and method bytes are
reclaimed. `Count` is a slot high-water mark during execution, and `Present` and
`Child` distinguish live names. Repeated top-level calls reclaim their temporary
storage. These IDs remain internal state-lifetime IDs, not CCL capabilities.
The object arena is unchanged by method declarations; temporary data objects,
regions, fields and escaped mutable references still need their own lifetimes.

Hosted run 15295 passes 4,818 call checks, including repeated invocation, recursive
retention, duplicate names, capacity limits, truncation, and cleanup that preserves
completed integer writes. ACPICA run 92427 passes 26 comparisons covering both
integer widths, arguments, empty methods, sync-level errors, root/parent paths,
recursive lifetime and cross-owner subtree cleanup. Full proof validation of the
strengthened insertion and cleanup contracts is in progress.


Final run 26651 passes all hosted tests and full SPARK analysis: **4,616 results,
zero unproved or justified checks**. The proofs cover the published safety and
frame/lifecycle contracts; this is not full AML semantic refinement. Insertion
now explicitly preserves existing records and payload stores and establishes
that the new node is live. Cleanup preserves the value arena, never increases
node/code storage, and leaves no live declarations owned by a method on final
exit. No proof assumptions or weakened invariants were introduced.

`acpica_dynamic_methods.py` is installed in the standard runner. Run 36651 passes
all 3,288 existing differential comparisons, and final run 93072 passes its 26
new comparisons: **3,314 total**. Error cases require ACPICA's final evaluation
status, not merely a diagnostic substring. The same run passes the 12 selected
upstream reference configurations and eight required CuBit admission checks;
CuBit execution remains 0 passed / 12 unsupported / 0 unexpected failures,
with 339 unselected entrypoints. Arithmetic/logic now stop at `DataTableRegion`
in `STRT`, after the temporary `M555` method declaration; control still exceeds
the table-byte limit.

Native compilation of this revision remains pending: attempts 87445 and 85144
exited at the occupied shared lock before compilation. The earlier 95098 result
predates temporary declarations and does not validate this revision. No native
service launch or hardware execution is claimed.


## Retained description-table catalog (2026-10-01)

The service retains every successfully admitted SDT in a bounded 1 MiB byte
pool with provider identity, signature, extent and revision metadata. DSDT must
come first; subsequent SSDTs load AML while Description tables only pass common
header/extent/checksum validation. DSDT/SSDT/FACS misclassification is rejected.
No table-specific semantic decoder or live CCL endpoint is implied.

Service tests cover shifted input bounds, exact owned byte copies after input
mutation, FACP/APIC/MCFG/HPET/DMAR/SRAT/SLIT/ASF! signatures, failed admission
preservation, and filling all 1 MiB before rejecting another table. Request
tests cover metadata and every byte offset through EOF, both authorized roles,
stale revisions, failed/incomplete snapshots, invalid indices/offsets and
reserved words, with exact state preservation. Focused hosted results are
service 1,049,105, request 92,574 and endpoint 589,972 passing checks.

The first checked run failed with stack overflow at the default 8 MiB hosted
limit. Compiler estimates include a 4,315,504-byte static Install frame and a
3,500,176-byte dynamic service-test frame. Both hosted runners now explicitly
set a 64 MiB soft stack limit; contracts and overflow checks remain enabled.
Those same checked tests pass with this budget. This is a hosted test setting,
not a whole-call-chain or secondary-stack proof, and native allocation/stack
work remains required before deployment.

Full run 31247 exited successfully: all hosted suites, 4,712 SPARK checks with
zero unproved or justified checks, and all 3,314 ACPICA differential comparisons
passed. An earlier run exposed three unproved index/offset conversions in the
query handler; checking bounded Natural values after fixed wire-limit checks
resolved them without changing the supported protocol. Pinned upstream ASLTS
still reports 0 CuBit execution passes, 12 unsupported cases, no unexpected
failures and 339 unselected entrypoints. Native attempt 67235 stopped at the
busy shared build lock before compilation; native estimates from earlier
revisions do not validate this catalog revision.


## FADT description decoding (2026-10-01)

`ACPI_FADT` decodes the FACP layout from ACPI 6.6 section 5.2.9, including the
legacy 116-byte prefix and complete optional fields through hypervisor identity
at byte 268. Structural acceptance requires an exact checksummed table, but
preserves unknown revision/flag/GAS values for later compatibility decisions.
Optional fields have explicit presence rather than conflating absence with zero.
The pointer selector prefers a nonzero addressable extended pointer and proves
the result is within the supplied limit; that limit grants no physical access.
No register normalization, operation, or power policy is implemented here.

`fadt_tests` runs inside the existing service suite. Its 46,748 checks cover all
lengths from 0 through 300, three input bases including near Positive'Last,
every optional field boundary, little-endian values, corrupt checksums and
signatures, truncated buffers, pointer fallback and revision/flag combinations.
Service tests additionally decode retained FACP bytes and reject a DSDT passed
to the FADT accessor. Focused decoder proof96529 passed with no unproved checks.
The standard runner includes the decoder in its SPARK units. Integrated
run55172 exited successfully: all hosted tests, 4,800 SPARK checks with zero
unproved or justified checks, and all 3,314 AML differential comparisons passed.
Pinned upstream ASLTS still reports 0 CuBit passes, 12 unsupported, 0 unexpected
failures and 339 unselected entrypoints.

Native56908 compiled this integrated revision successfully. Largest reported
frame: 4,315,488 bytes; 18 dynamic frame records. This supersedes the previous
lock-busy native attempts, but still does not establish a whole-call-chain or
secondary-stack bound or safe service launch. Runtime contracts remain enabled.
Source: https://uefi.org/specs/ACPI/6.6/05_ACPI_Software_Programming_Model.html#fixed-acpi-description-table-fadt


`acpica_fadt.py --tools <acpica-bin-directory>` adds an independent data-table
check: generate the compiler's own FACP template, compile it with iASL, derive
expected field values from the labeled source, and compare all 113 admission,
field and presence checks through `ACPI_FADT`. It compiles its probe in an
isolated temporary project and fails if the expected template shape changes.
Standalone55592 passed. The harness invocation is now in run-acpica.sh;
run21462 completed successfully in the standard workflow. These checks
are separate from the 3,314 AML differential comparisons and do not establish
FADT hardware validity or operation.


The FWTS checksum fixture adapter described below adds independent table
admission coverage beyond the generated ACPICA fixture. Canonical's [FWTS table tests](https://canonical.com/blog/debug-acpi-tables-with-firmware-test-suite-fwts)
check table-specific firmware semantics, and [saved-table input](https://canonical.com/blog/analyze-acpi-tables-in-a-text-file-with-fwts)
permits offline fixtures. They validate firmware contents rather than directly
calling CuBit's Ada parser; an adapter and pinned fixture provenance are needed
before reporting our own parser passes. AAPITS documents table-management API
tests, whose reusable coverage still needs inspection. Neither the full FWTS suite nor AAPITS is included in the pass counts above.


## FADT register extents and FWTS fixtures (2026-10-01)

`ACPI_FADT.Registers` selects full fixed-block extents using FADT byte lengths,
suppresses blocks under hardware-reduced ACPI, validates block-specific lengths,
and prefers addressable extended memory/I/O spans. Any legacy fallback records
that a nonzero extended descriptor was rejected. Subtraction-based bounds cover
the entire span, including the final byte. PM1 event and GPE blocks expose their
equal status/enable bank split. This does not validate GAS access widths or
individual transactions, establish platform-required blocks, or grant I/O.

Focused96434 and service80929 passed register26,282, FADT46,748 and
service1,049,108 checks. The focused proof has zero unproved checks; native30943
compiled the new child successfully with unchanged largest reported frame
4,315,488 bytes and 18 dynamic records. Full standard21462 completed with both FADT proof units and the ACPICA
template harness included: 4,837 SPARK checks, zero unproved or justified;
113 FADT checks and all 3,314 focused AML comparisons passed. The selected
actual upstream suite reported zero passes, 12 unsupported, zero unexpected
failures, and 339 unselected entrypoints; this is not full AML conformance.

`nix develop -c python3 tests/aml-core/fwts_tables.py` downloads and hash-checks
FWTS commit `f06eeafe26509961bfdffa60feb855623d79224e`, archive SHA256
`eb18a529ca5deaf76cd81812feea8f64535f6132e5a98d6ab3f8e57ca90bd6db`.
An optional `--archive` accepts an offline copy with the same required hash.
The harness consumes unmodified checksum-0001 fixtures 0001/0003/0004 and their
upstream golden diagnoses. A separate temporary Ada probe calls the shared
`Firmware_Tables.Read_Table`; no FWTS code or hardware test is executed.

Standalone7595 passed all 48 standard SDT comparisons (32 accepted, 16 bad
checksums). FACS and RSDP are explicitly omitted here; the golden synthetic
RSDT has no fixture bytes and is also omitted. This is selected checksum-fixture
coverage, not a complete FWTS run, FADT body conformance, or AML execution.
Results and omissions are recorded in `build/fwts/report.json`. Adding this
harness to the standard runner was completed under the shared build lock
after run21462. Run52586 passed all 48 cases through the default
download-and-hash-check path. The one-line runner addition has not triggered
a redundant rerun of the unchanged AML comparisons.


## Retained kernel table inventory (2026-10-01)

`catalog_tests.adb` exercises `Firmware_Tables.Catalog` through all capacities,
late and repeated DSDT discovery, preserved non-DSDT order, publication,
conflicting DSDTs, FACS rejection, overflow, address wrap, sticky failure and
reset. Current integrated44486 passed 66,574 checks and focused SPARK proof;
the aggregate proof report has 4,905 checks with zero unproved or justified.
The standard runner includes this test and proof unit. The earlier full4837
run remains the evidence for unchanged AML units; 4905 is an aggregate report,
not a newly repeated whole-suite proof run.

Kernel discovery calls the same catalog model and publishes it after successful
setup. Direct native compile54410 passed. An initial whole-record `Fresh`
return caused a 6,176-byte native frame and a 6,544-byte setup frame; it was
replaced with in-place `Reset`, now 8 bytes, while setup is 384 bytes. Existing
kernel per-function stack limits were not relaxed. These measurements are not
a whole-call-chain stack proof. Native58019 completed successfully: full
kernel build (including its stack gate) and the 60-second QEMU desktop-protocol
regression passed. The serial log records ACPI loading successfully at line123
and DESKTOP-PROTOCOL-CHECK: PASS at line657; no inventory-unavailable message
was emitted. Logs: `/tmp/cubit-acpi-catalog-native.log` and
`/tmp/cubit-acpi-catalog-desktop.serial`. This exercises QEMU boot, not physical
laptop sleep or a userspace table grant.
The kernel queries expose metadata internally; there is no userspace memory
grant, startup transport, or live ACPI service yet.


## Whole-page content exposure (2026-10-01)

`Firmware_Tables.Exposure` and `exposure_tests.adb` are now in the standard
runner. The planner rounds to 4-KiB pages and follows covering SDT intervals;
its retained-candidate postcondition proves that every exposed byte belongs to
an admitted table. Tests cover all 4,096 offsets with five lengths, exact
adjacency, overlaps, reverse discovery order, one-byte gaps, unknown leading
and trailing bytes, maximum table size and the final physical page. They test
content eligibility; no mapping, backing-kind or lifetime authority is inferred.

Private48032 passed 61,468 exposure checks and 66,574 catalog checks, with
137 focused SPARK checks and zero unproved or justified. The unchanged Fits
predicate was moved into the catalog specification so callers can use its
arithmetic meaning. The promoted sources match that private proof exactly.
Integrated63020 passed both test targets and focused proof; the aggregate
report is now 4,974 checks, zero unproved or justified. This aggregate is not
a newly repeated whole AML-suite proof run.

Native54669 compiled the kernel query and models. Maximum new planner frame:
160 bytes; query wrapper16 bytes; ACPI setup remains384 bytes. Full kernel
build95779 passed its stack gate and link; log is
`/tmp/cubit-acpi-exposure-native.log`. No new QEMU run was claimed for the new
query; the previous catalog boot/desktop regression58019 remains the latest
boot evidence. The planner has no userspace transport or firmware grant yet.


## ACPI-reclaim backing classification (2026-10-01)

`Multiboot_Memory_Map.Reclaim` classifies complete byte ranges against the
published map shape. It handles adjacent/overlapping reclaim entries in any
order and rejects gaps, malformed nonempty entries, and any overlapping entry
with another kind (including NVS). It is not a cache, ownership or grant policy.
`backing_tests.adb` compares interval scanning with an independent per-byte
oracle across region kinds, range endpoints and permutations, plus empty maps,
non-one-based arrays, gaps and the final physical byte.

Focused53980 passed all 24,759 comparisons and 47 SPARK checks with zero
unproved or justified. The interval-composition proof hides the implementation
of the byte predicate locally, proving composition for an arbitrary predicate;
Certify still proves membership from actual map entries. This uses GNATprove's
[proof-context scoping](https://docs.adacore.com/spark2014-docs/html/ug/en/appendix/additional_annotate_pragmas.html),
not an assumption or skipped check. The standard five-second prover limit
passes; increasing timeout alone did not resolve the earlier proof failures.

Kernel query adapters and `backing.gpr`/runner integration were applied under
the shared lock after the initial busy attempt. Native47229 passed the full
kernel build, stack gate and link; `/tmp/cubit-acpi-backing-native.log` records
the build. Reported frames: Covers32 bytes, Firmware_Reclaim_Pages32 bytes,
Table_Backing_Is_Reclaim_RAM80 bytes; ACPI setup remains384 bytes. Integrated
41220 passed the same 24,759 tests and all47 proof checks through persistent
`backing.gpr`; the standard runner now invokes this separate kernel-model
project. No new boot run or actual grant is claimed for these query additions.
The previous catalog boot/desktop regression remains the latest ACPI boot test.


## Literal objects during method execution (2026-10-02)

`Execute_With_Input` now materializes string and buffer literals through its
namespace callback, allowing object returns, locals and nested method arguments.
Integer-expected operands retain their existing conversion path. The table-aware
service uses its bounded value pool; the legacy empty-input wrapper rejects
object materialization explicitly. Allocation failure returns `Value_Limit`.

The frozen final source passed 160 literal checks, 1811 readonly-input checks
and 1523 declaration checks. ACPICA comparisons passed 70 literal cases and
212 typed cases; the final native service linked successfully. Focused executor
and instantiated service proof discharged 3046 proof checks and 414 flow checks,
with zero unproved or justified checks. Evidence: `/tmp/cubit-aml-literals-proof-r3.out`,
`/tmp/cubit-aml-literals-acpica-r3.log`, `/tmp/cubit-aml-literals-native-r3.log`.
All 125 hosted and 426 native input hashes were revalidated before promotion.

This does not complete Buffer semantics: decoding still limits strings to255
bytes and buffers to1024 bytes, and BufferSize accepts literal integers only.
Value-pool reclamation, implicit conversion to string for DataTableRegion
selectors, actual DataTableRegion execution and whole-interpreter stack bounds
remain unfinished. The upstream ASLTS completion count has not advanced.

After promotion, the registered disjoint integration build passed all160 literal
and1811 readonly-input checks; all six source/fixture files matched the verified
snapshot byte-for-byte. Log: `/tmp/cubit-aml-literals-integration.log`.


## Selection from owned table backing (2026-10-02)

`AML_Table_Backing.Find_Table` selects the first matching admitted table using
only its owned header bytes. Invalid counts or any malformed admitted span
reject the inventory before lookup. Its postcondition establishes a matching
result within the admitted count and no earlier match. This index conveys no
physical-address or hardware authority. The existing field reader is unchanged.

Private verification passed 33,811 checks and 23 proof +4 flow checks with zero
unproved/justified checks (`/tmp/cubit-acpi-table-selection-proof.out`).
Promoted source and fixture bytes match that snapshot. Registered integration
passed 33,811 selection and 847 field-execution checks, with separate outputs
(`/tmp/cubit-table-selection-integration.log`). The standard runner includes
the fixture and already proves the backing unit. Actual DataTableRegion opcode
execution is still pending; this helper alone does not resolve the ASLTS blocker.


## Implicit string conversion primitives (2026-10-02)

`AML_Coercions.Strings` implements integer-to-string conversion at the selected
AML integer width and hexadecimal buffer-to-string conversion. Contracts specify
each output byte and length; the buffer precondition prevents length arithmetic
overflow. These are pure conversions, not allocation policy or selector parsing.
They are not yet connected to DataTableRegion or other interpreter operations.

The final frozen source passed 65,542 hosted checks and 94 proof +4 flow checks
with zero unproved/justified checks; source hashes and promoted bytes match.
The unit compiles against the isolated CuBit native runtime. Registered hosted
integration repeats all65,542 checks successfully. See
`/tmp/cubit-aml-string-conversion/verified-proof.out`,
`/tmp/cubit-aml-string-native.log` and `/tmp/cubit-aml-string-integration.log`.
Native unit compilation is not a new service link or whole-stack bound.

`acpica_string_conversions.py` compiles runtime Concatenate operations whose
first operand is a string and compares the converted second operand against
`conversion_runner`. Both integer widths and empty/nonempty buffers are covered.
The registered runner matched50 values. ACPICA20260408 reports four outstanding
cache allocations at shutdown in all8 batches; the same exact diagnostic occurs
in a constant-return control with no conversions. The fixture records this
control diagnostic separately in its JSON report, and rejects any different
error or a diagnostic before the final returned string. This is value agreement
with a recorded reference-tool diagnostic, not an error-free ACPICA run.
Evidence: `build/acpica-string-conversions/report.json` and retained logs there.
The standard `run-acpica.sh` builds and runs the fixture; `--runner` supports
checking a separately built executable. The upstream ASLTS status is unchanged.
