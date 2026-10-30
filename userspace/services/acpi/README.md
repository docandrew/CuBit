# ACPI service core

## Direct-entry architecture

The current native entry in `native/` declares a process-manager-launched
`acpi.svc`. Generated `Startup.Tables` supplies the kernel's sealed, read-only
snapshot authority. Startup copies bounded chunks into owned, descriptor-sized
storage, validates and initializes the namespace, and registers the observer
endpoint only after obtaining a complete owner. The kernel retains early-boot
ACPI responsibilities; no firmware pages or arbitrary physical addresses are
mapped into this service by this interface.

One serialized event loop owns the service state. It dispatches stamped
observer requests, attempts each reply once, and sleeps through the runtime's
activity wait when idle. Upload/configure/provider authority is absent from
this entry. SCI/GPE/EC delivery, device-state refresh and CCL subscriptions are
future integration work, not implemented native event handling. ACPI supplies
firmware mechanisms; power policy and I2C/HID controller operations belong to
their respective services and drivers.

The direct startup, Fatal-report lifetime, owner lifecycle and observer loop
have hosted functional tests. Actual service linking, stack fit, boot, logger
delivery and hardware events still require native validation. The whole AML
interpreter is not yet SPARK-proved; earlier scoped proofs apply only to their
recorded units and assumptions. See `native/README.md` for the entry contract.

## Source layout

The ACPI service owns its implementation library in `lib/`. It contains the
AML interpreter and ACPI-specific pure helpers; it is not a general userspace
library. The adjacent privileged backend consumes a small register-validation
subset through an explicit source list.

| Directory | Responsibility |
| --- | --- |
| `lib/` | AML values, namespace, decoding, execution and fixed-table helpers |
| This directory | Service lifecycle, snapshot admission, requests and device policy |
| `resource-data/` | Pure decoding of device resource descriptors |
| `resource-consumer/`, `pci-routing/`, `lid/`, `hid-i2c/` | Typed consumers of the service namespace |
| `field-client/`, `region-init/` | Scoped OperationRegion access and initialization |
| `native/`, `native-clock/`, `provider-native/`, `request-driver/` | CuBit adapters and injectable transport logic |
| `../acpi-backend/` | Privileged access policy and envelope adapter; not yet a running service |
| `../../../shared/firmware/` | Portable firmware table parsers also used by the kernel |

Early boot discovery and CPU/interrupt setup remain in `kernel/src/acpi.*`.
Hosted tests live under `tests/acpi-*` and `tests/aml-*`. Historical validation
records retain the paths and hashes of the sources they originally checked;
they are not evidence of native integration or a complete interpreter proof.

## Implementation and validation records

Hosted checkpoint (2026-10-14): static Alias lookup preserves source-node
identity and method scope. `Find_Child` selects physical bindings; `Find_Object`
returns a discriminated canonical lookup, including a valid root result distinct
from a missing object. Root references are owner-generation-bound metadata and
cannot be used as data-cell authority.

Core also has trusted-owner notification configure/count/take operations. The
bounded queue starts disabled, validates targets before admission, preserves an
admitted event after later AML failure, and copies target identity/path metadata
with a freshness indication when drained. Notification class uses the low 32 bits;
the delivered number uses the low 16 bits. This internal queue does not provide
CCL transport, subscriber authorization or asynchronous completion handling.

Focused tests pass 290 checks per strict profile; the unchanged selected ACPICA
Notify corpus matches 66 cases per profile. These are hosted results,
not a new proof or native-service validation. Runtime Alias, broader metadata
operations and full AML semantics remain incomplete. Run `python3 tests/acpi-hosted/run.py --group notify-alias --mode release`
or use `--mode checked` inside the Nix shell. The public group also runs 8,568
protocol checks per profile; see the coverage ledger for exact scope.

`ACPI_Service` is the pure service state machine. Historical provider/configure
models remain in explicitly hosted tests; the native entry uses direct snapshot
startup instead. Native service launch validation, SCI/EC handling and hardware
callbacks remain unfinished.

Runtime AML BufferField support includes all six Create* field instructions,
authenticated bit views into owned Buffers, bounded reads/writes, and field-value
capture in named or unnamed runtime packages. Namespace and active-frame roots
keep the backing storage live; returned Buffer values follow the existing
retained-result rules. Generic fields return Buffer values; fixed fields use
Integer values when their width fits the effective AML integer width. Field
inspection retains ObjectType 14 and does not allocate a value.

Explicit CopyObject preserves authenticated Index references, including valid
references to uninitialized Package elements; implicit value copies continue to
resolve Index sources. A BufferField destination rejects an indexed reference
operand with the observed operand-type failure before changing its backing bytes. Package field capture and failed multi-member
construction are tested for atomic publication. None of these data operations
authorizes a memory-mapped register or expands a caller's capabilities.

The `buffer-fields` hosted group runs eleven runtime programs, 102 cached ACPICA
comparisons and compilation of ten evaluator adapters in either strict release
or assertion-enabled mode. The current evidence is hosted testing, not a proof
of the complete interpreter or native startup. Static/deferred buffer-field
loading and broader field/region access semantics remain unfinished. See the
[coverage ledger](../../../docs/acpi-coverage.md) for scope and remaining work.

The current collecting service exposes opaque retained results and copied
observations. After table loading, Core callers use `Initialize_Members`, which initializes
and seals the collecting namespace before execution. `Invoke_Retained`, `Describe_Result`, bounded
read/dereference operations and `Release_Result` keep result lifetimes explicit;
`Invoke_Scalar` handles integer results. Service callers cannot obtain raw runtime
namespace snapshots or borrowed object descriptors. Collection traces namespace,
active-frame, expression and retained-result roots at allocation boundaries.
Released temporary values can be reclaimed; retained values remain protected.

`ACPI_Service_Core` accepts separate generic budgets for namespace nodes,
aggregate method bytes and retained results. Defaults remain 512, 65536 and 512.
Every individual method still has a 65536-byte limit. These are explicit instance
budgets, not firmware-derived runtime sizing. Requests metrics page 5 reports the
aggregate method capacity and remaining storage.

Portable hosted checks are registered in
[the capacity suite](../../../tests/acpi-capacity/README.md) and
[the static Buffer suite](../../../tests/aml-static-buffer/README.md), and
[the mixed comparison suite](../../../tests/aml-mixed/README.md).
The latter checks authenticated String/Buffer-left primitive comparisons and
cached operand-effect cases. Its scoped helper proof establishes bounds and
current failure contracts, not full ordering semantics or interpreter correctness.
The current integrated milestone is hosted-tested, not newly proved or boot-tested.
Historical proof/native evidence below applies only to its recorded source set;
it does not establish proof or native compatibility of the changed ownership,
initialization, comparison and metadata-query sources. See the scoped evidence in
[the integration tests](../../../tests/acpi-integration/README.md).

Install accepts stable copied SDT bytes and a provider-assigned identity. The
first successful table must be a DSDT; subsequent tables may be ordered SSDTs or
other standard description tables (`Description`). The
shared Firmware_Tables parser checks signature, length and checksum. This API
requires an exact table-sized input, rejects duplicate identities, and limits
one table by default to 64 KiB, the session to 32 tables/1 MiB and namespace storage to 512 nodes.
These are explicit development budgets, not ACPI specification limits.
Service, Bootstrap and Requests state are limited objects: construct them in
place with explicit table-count, aggregate-byte and single-table discriminants
when different budgets are needed. Requests also takes `Initial_Revision`.
There is no copying `Fresh` constructor. The direct native loader selects table
count, aggregate table storage and largest-table bounds from validated snapshot
metadata, with initial revision zero. This removes the fixed table-byte defaults
from that entry; interpreter node/method/result budgets remain separate limits.

After all advertised tables have been admitted, Bootstrap `Finish_Snapshot`
initializes static package members once and retains the Bound/Missing/Unsupported
report and seals the namespace before publishing completion. Direct hosted callers
call Core `Initialize_Members` after loading; it seals through `Values.Seal`. Execution
requires the ready lifecycle; loading cannot resume after sealing.
Initialization resolves supported forward package-member references against the
completed namespace; it is not general module execution or `_INI`/`_REG` policy.
Missing or unsupported members remain uninitialized values with retained
diagnostics; they do not prevent safe immutable table metadata reads from a
Complete snapshot. This completion does not schedule a later retry.


Every admitted SDT is retained byte-for-byte in owned storage. Description
installation validates only the common SDT header, exact length and checksum;
it does not decode table-specific structures or load AML. DSDT, SSDT and FACS
cannot be mislabeled Description. FACS is a mutable firmware control structure
and is excluded from this immutable SDT catalog. Failed admission preserves the
previous catalog as well as the namespace.

`Fixed_Description` decodes an installed FACP table using the pure `ACPI_FADT`
unit. It exposes legacy fields, optional extended pointers and register GAS
records, reset/sleep descriptors and hypervisor identity. Every optional field
has explicit presence and is read only when fully within the exact checksummed
extent. This is structural decoding, not hardware validation: unknown revision,
flag and address-space values remain metadata. `Select_Pointer` prefers a
nonzero addressable extended pointer; it does not validate physical backing.
`Hardware_Reduced` identifies the flag for revision 5 onward; raw legacy fields
are retained but must not be used as hardware controls in that mode. No FADT
query is yet wired to CCL. `ACPI_FADT.Registers` now selects bounded fixed-block
extents, validates their byte lengths and suppresses hardware-reduced blocks.
It reports legacy fallback explicitly and computes paired status/enable banks.
GAS access widths, individual transactions and hardware authority remain
unvalidated by that extent selector; it performs no register access.

`ACPI_FADT.Transactions` checks transaction geometry after a backend-specific
access width has been resolved. It calculates the full byte range touched by a
GAS bit field, including edge bits, and requires that range to fit supplied
bounds without address wrap. It supports aligned 1/2/4/8-byte memory transactions
and 1/2/4-byte I/O transactions, with the 16-bit I/O address limit. This is not a
GAS access-width selector or an authority check: supplied bounds must eventually
come from backend-owned admitted resource records. No actual reads/writes occur.
The exact span and indexed transaction bounds have 111 SPARK checks with none
unproved/justified, and an independent bit oracle exercises every 8-bit offset
and nonzero 8-bit field width for all four transaction widths. The standalone
proof uses a 30-second prover timeout for its numeric-conversion lemma.

`ACPI_Requests` provides a transport-independent request handler. The draft
packet contains a label and exactly four 64-bit words; flags and reserved bits
must be zero. A future native adapter must classify the caller from authenticated
endpoint authority. `No_Authority` receives Denied and zero reply words;
`Observer` may read metrics and completed snapshot tables; `Snapshot_Provider` may upload the snapshot.
Caller-supplied words, process IDs and reserved bits cannot establish authority.
This handler does not register an IPC endpoint or implement CCL bindings.

`ACPI_Requests.Import_Block` is the bulk-table adapter entry point. After Start,
a trusted adapter supplies authenticated authority, the current revision, table
ID/kind and the exact mapped table slice. It imports directly through the same
bootstrap validation and retained catalog, without Begin/Write/Commit packets.
It rejects interleaving with an open chunk upload, consumes one revision for an
admission attempt, and preserves the state on authority/token/order/length errors.
Failed table admission still makes the snapshot fail. Finish remains explicit.

The adapter must validate the grant and extent and keep the source bytes stable
and mapped through the call; a read-only recipient mapping alone does not prevent
provider writes. Successful import owns its retained copy, so no source mapping
is borrowed afterward. This entry point does not authenticate a grant, freeze its
owner, define a wire handle layout or implement native startup. Those adapters
remain outstanding. Bulk grants are the preferred handoff; chunks are fallback.

`ACPI_Endpoint.Dispatch_Block` classifies the kernel stamp and encodes the bulk
result using the same observer/provider policy. `ACPI_Native_Blocks.Import_Grant`
is a typed native adapter: it first rejects non-provider authority, then acquires
the exact table extent through `CuBit.Memory_Grants.Acquire_Via_Capability` using
a startup-selected provider endpoint. It requests read access at offset zero,
imports while the acquisition is held, and returns the acquisition afterward.
The supervisor must bind that provider endpoint and authority tag consistently.
The provider's no-write promise is still required while the import runs.

The native adapter retains a failed return in its private state. While pending,
new imports are rejected; `Retry_Return` retries only cleanup, never table
admission. A cleanup-pending reply can already report a successfully installed
table, so callers must not repeat that import. This is serialized adapter code,
not a live receive loop or a SPARK proof of kernel mapping behavior. Hosted
mock tests in `tests/aml-core/native-blocks/blocks.gpr` exercise acquisition,
return failure/retry, authority checks and the mapped table reader. They do not
execute real grant syscalls.

The draft request layouts are:

| Label | Operation | Four request words |
| --- | --- | --- |
| 0 | Read metrics | page, 0, 0, 0 |
| 1 | Start snapshot | revision, table count, 0, 0 |
| 2 | Begin table | revision, positive table ID, kind (0 DSDT / 1 SSDT / 2 Description), byte length |
| 3 | Write chunk | revision, sequential byte offset, eight bytes, eight bytes |
| 4 | Commit table | revision, 0, 0, 0 |
| 5 | Finish snapshot | revision, 0, 0, 0 |
| 6 | Read table metadata | revision, one-based catalog index, 0, 0 |
| 7 | Read table bytes | revision, one-based catalog index, byte offset, 0 |
| 8 | Import table grant (native adapter only) | revision, packed grant reference, expected provider ID, packed kind/length |
| 9 | MCFG info | revision, table index, page, 0 |
| 10 | MCFG allocation | revision, table index, allocation index, page |
| 11 | SLIT info | revision, table index, 0, 0 |
| 12 | SLIT distance | revision, table index, source locality, destination locality |
| 13 | MADT info | revision, table index, page, 0 |
| 14 | MADT record metadata | revision, table index, record index, 0 |
| 15 | MADT typed fields | revision, table index, record index, page |
| 16 | SRAT info | revision, table index, page, 0 |
| 17 | SRAT record metadata | revision, table index, record index, 0 |
| 18 | SRAT typed fields | revision, table index, record index, page |

Chunks contain little-endian bytes; unused bytes in the last chunk must be zero.
Commit requires all declared bytes. Finish requires no open table. Start is
one-shot, and table admission or premature-finish failures are sticky. Accepted
mutations advance the revision, including mutations that fail table admission.
Malformed, stale, denied, out-of-order and resource-limit requests preserve the
entire state. Revisions stop at signed 64-bit maximum instead of wrapping.
They identify changes within one handler lifetime, not across service restarts.

Table queries require the current revision and a Complete snapshot. Metadata
replies are `[revision, provider ID, byte length, signature]`, with the four
signature bytes packed little-endian. Byte replies are `[revision, count, low32,
high32]`: at most eight bytes, packed little-endian into two unsigned 32-bit
values so every response word fits a signed CCL integer. Offset equal to length
returns zero bytes; larger offsets are malformed. Invalid catalog indices return
Not_Found. Queries preserve the entire state and never execute AML.

Decoded queries use the retained immutable table selected by its one-based catalog
index, require authenticated observer/provider authority, the exact current
request revision and a Complete snapshot, and preserve the entire state. Table,
allocation and record indices are one-based; SLIT localities and byte offsets
are zero-based. These are snapshot descriptions, never address authority.
The generic handler explicitly rejects reserved native label 8; metadata labels
cannot enter grant import or acquire a grant. Native imports acquire a reference
with its expected opaque 64-bit owner; no address is derived from a slot number.

The three payload words after the request revision are:

| Query/page | Payload |
| --- | --- |
| MCFG info 0 / 1 | firmware revision, allocation count, 0 / reserved low32, reserved high32, 0 |
| MCFG allocation 0 / 1 | base low32, base high32, segment / first bus, last bus, reserved32 |
| SLIT info / distance | firmware revision, locality count, 0 / distance, 0, 0 |
| MADT info 0 / 1 | firmware revision, record count, flags / local APIC address low32, high32, 0 |
| MADT record | wire type, byte offset, record length |
| MADT fields 0, types 0 or 9 | processor UID, controller ID, processor flags |
| MADT fields 0, type 1 | I/O APIC ID, address32, GSI base |
| MADT fields 0, type 2 | bus, source, GSI |
| MADT fields 1, type 2 only | override flags, 0, 0 |
| MADT fields 0, type 3 | GSI, NMI flags, 0 |
| MADT fields 0, types 4 or 10 | processor UID, LINT, flags |
| MADT fields 0, type 5 | address low32, address high32, 0 |

Only these pages are accepted. Unknown MADT records remain available as bounded
raw record metadata; typed access returns Unsupported_Record_Kind. Absent table,
wrong signature, malformed body and invalid element index produce distinct
Not_Found, Wrong_Table_Kind, Table_Rejected and Index_Out_Of_Range outcomes.
Malformed pages/reserved words fail closed. Wide metadata is split into 32-bit
words to fit signed CCL integers. Firmware table revision in payload is distinct
from the optimistic request-state revision in response word zero.

Authorized replies contain the current revision in word zero. Metrics replies
use the remaining three words as follows; readers compare revisions across
pages to detect intervening changes:

| Page | Three metric words |
| --- | --- |
| 0 | phase (0 idle / 1 receiving / 2 complete / 3 failed), advertised tables, installed tables |
| 1 | installed table bytes, namespace nodes, value objects |
| 2 | value bytes, package elements, admission rejections |
| 3 | rejection counter saturated, table open, received upload bytes |
| 4 | maximum table bytes, maximum aggregate table bytes, maximum table count |
| 5 | method-code bytes, method-code capacity, remaining method-code bytes |
| 6 | last AML-load code, namespace capacity, remaining namespace nodes |
| 7 | initialized member report: bound, missing, unsupported |
| 8 | members initialized (0/1), pending members, 0 |

Member report counts are zero before initialization. Metrics remain authenticated;
unknown pages and malformed reserved words are rejected.

The last AML-load code is zero before any load attempt, otherwise one plus
`ACPI_Service.Namespace.Load_Status'Pos`. Thus1 means Loaded,7 means Storage_Full
and11 means Value_Limit. It records the last AML loading attempt; header,
ordering and checksum rejections preserve it. The hosted table runner prints
the corresponding status name on admission failure.

Boolean metrics use 0/1. All reply words fit CCL's signed 64-bit integers.
Queries preserve state and execute no AML. `ACPI_Endpoint` encodes success as
label 0xF000 with the four response words; errors use label 0xF001 and words
`[outcome, revision, admission detail, 0]`. Outcome codes follow the declared
order: OK=0, Denied=1, Malformed=2, Stale=3, Wrong_Order=4, Resource_Limit=5,
Table_Rejected=6, Incomplete=7, Not_Found=8, Wrong_Table_Kind=9,
Index_Out_Of_Range=10, Unsupported_Record_Kind=11. Reply headers always have length four and zero
flags/reserved. An unclassified caller gets only `[Denied, 0, 0, 0]`. Metrics do
not replace common Logging/logstore integration or CCL event notifications.

`ACPI_Endpoint.Configuration` holds two trusted startup-assigned authority tags.
Both must be nonzero and distinct; otherwise all requests are denied. Exact
matches classify observer/provider access, and all other stamps are denied.
No tag numbers or startup policy are allocated here. Configuration must not
come from a request or an untrusted service parameter.

`native/ACPI_Native_Endpoint` mechanically maps actual `CuBit.Messages.Message`
records to this core, taking authority exclusively from `authorityTag` and
clearing outgoing authority metadata. Its input must come from the kernel's
receive path: a locally constructed record is not authenticated evidence.
This adapter is native-compiled, but not SPARK-analyzed or connected to a live
receive loop. The portable classifier, encoding and dispatch are SPARK-analyzed.

The bootstrap snapshot provider must assign one stable ID per table for this
service lifetime. ID is a deduplication key, not authenticated provenance or
access authority. Identical bytes under different IDs are not content-deduplicated.
No physical address or table reference grants hardware access.

Integer width comes from the admitted DSDT; SSDT revisions cannot override it.
Loading commits only supported complete table payloads. A failed installation
leaves the prior namespace visible, changes no table ID/width/count/byte usage,
and increments a saturating rejection counter. Continuing with an old namespace
after failure must not be represented as complete platform discovery.

`ACPI_Bootstrap` tracks an advertised snapshot of one DSDT followed by ordered
SSDTs and other description tables.
It requires exactly the advertised number of successful imports before Finish
can mark the snapshot Complete. A failed import, extra table before Finish, or
premature Finish makes the session Failed; later calls cannot revive it.
Completion only means the advertised snapshot was imported. The trusted provider
must include all required tables, and AML activation/device discovery remain
separate unfinished work. There is no native snapshot transport yet.

Observe exposes table counts and bytes, namespace-node count, value-object count,
value-byte usage, package-element usage, rejection count and saturation state.
Value storage currently permits 2048 objects, 65536 bytes and 8192 package
elements; these are development budgets. Value usage is derived from the arena
without executing AML.
Method bodies occupy a separately provisioned pool shared by the namespace,
with a default aggregate capacity of 65536 bytes. Temporary-method cleanup
reclaims a dead suffix without moving surviving method bodies. A method is no longer limited by the 1024-byte buffer-literal limit.
Definitions returned to the executor contain exactly the method's body length;
calls retain the same instruction and recursion budgets. Loading copies the
body, and failed admission rolls back code storage together with the namespace.
VarPackage counts may reference already-loaded named integers. Lookup uses the
initializer's lexical scope, including ancestor search and explicit parent/root
paths. A read-only namespace snapshot supplies counts, and the object loader
checks both consumed-byte bounds and allocation limits before committing.
Counts are resolved once during loading. Arbitrary count expressions, method
calls, forward references and truncating surplus initializers remain unsupported.
Static Buffer counts additionally admit already-defined canonical String and
nonempty Buffer values using implicit integer conversion, scoped name resolution
and the existing low-32-bit size rule. Empty Buffer and unsupported count forms
fail admission atomically. This does not execute methods or hardware reads during
loading, and does not broaden VarPackage count conversion.
The service exports copied observations and opaque retained result handles;
raw snapshots belong only to the lower-level namespace API.

`Namespace.Invoke_Mutable` is an internal execution entrypoint for an owned
namespace state. Named integer stores and integer-operation targets update that
state, and nested calls share the updates. Integer destinations normalize to
the executing method's width. Completed writes remain visible after a later
execution error or budget failure. `Invoke` continues to use a read-only binding
and rejects named writes. Neither entrypoint is exposed through CCL or grants
hardware access.

Named integer operands capture their value when evaluated; Local/Arg operands
retain the existing deferred slot behavior. ACPICA comparisons cover this
distinction. Admitted scalar and compound Store/CopyObject conversions are
covered by the hosted integration tests; the full conversion matrix, target
semantics and Debug output remain unfinished. Synchronous serialized-method calls
enforce sync-level ordering, including inherited levels through nonserialized
calls and recursive entry. Invocations require exclusive context ownership;
concurrent AML threads, yielding and explicit mutex operations remain unfinished.
Mutable invocation also supports temporary `Method` declarations, with ownership
and last-invocation cleanup across recursion and execution errors. It reclaims
namespace slots and method bytes while preserving completed integer writes.
Temporary data-name lifetimes are hosted-tested with collection; complete
region/field activation and hardware semantics remain unfinished. This is internal
interpreter support; it does not add a service execution request.
The eventual CCL endpoint must provide bounded queries, not serialize this entire
internal state or expose arbitrary invocation. Metrics are not a log/audit stream.

Standard validation: `nix develop -c bash tests/aml-core/run.sh --prove --acpica`.
This includes pinned upstream ACPICA tests and differential comparisons. Known
unsupported upstream cases are reported separately from passes; the development
gate does not establish full upstream compatibility.
`nix develop -c bash tests/aml-core/run-native.sh` separately compiles a static
library against the CuBit runtime under the shared build lock. It produces
`tests/aml-core/build/native/stack-report.json` with compiler frame estimates.
This does not launch a service or establish a whole-call-chain stack bound;
large bounded states and contract snapshots currently require substantial stack.
The hosted runners explicitly reserve a 64 MiB soft stack limit: the retained
1 MiB catalog and checked value-state copies overflowed the default 8 MiB hosted
stack. Runtime contracts remain enabled. This test budget does not establish
a safe native allocation strategy or a native stack bound.
See [test evidence](../../../tests/aml-core/README.md) and the
[service contract](../../../docs/acpi-service-contract.md) for remaining work.

The lower-level noncollecting namespace interface transports named strings, buffers and packages through
locals and method calls and can return them as `Object_Returned`. Such a result
identifies backing data in the same immutable namespace snapshot; it is not a
wire handle or writable AML reference. A lower-level caller must retain that
snapshot; this borrowed interface is not the collecting service API.
The future CCL query layer must serialize bounded value results instead of
exposing internal IDs. Mutable object-copy/reference semantics and native
execution remain incomplete.

The integer evaluator now supports FindSetLeftBit and FindSetRightBit, including
expression results and explicit targets in statement form. Positions are
one-based from the least significant bit, with zero for an all-zero operand.
External argument integers retain their raw input bits, consistent with the
existing arithmetic boundary; results follow the executing table's width.

The native endpoint now unifies scalar and bulk requests. Label 8 packs
[revision, grant-generation/slot, table ID, kind/byte-length] into four words;
see tests/aml-core/native-blocks/README.md for exact layout. It authenticates the
received provider stamp before acquisition and accepts no address or offset.
The provider endpoint is offered during authenticated startup and accepted by
the native server; request words do not designate a capability slot.
A failed grant return preserves the completed import reply and pending cleanup;
the eventual loop must retry return without replaying import. The old scalar-only
native Dispatch signature was replaced with an adapter-state/provider-endpoint form.
104 hosted adapter checks pass, and the unified native endpoint compiles against
the actual runtime. The native runner described below consumes this dispatcher; launch manifest
and provider table export are not wired yet; this native boundary is tested code, not itself SPARK-proved.

ACPI_Native_Server.Run now supplies the native IPC loop. Its caller retains the
service and adapter state; fatal wait/clock/completion returns never discard a
pending grant. It retries cleanup at 100 ms intervals while continuing to serve
requests, without replaying operations after failed replies. No authority is
inferred from sender PID. The loop deliberately handles the mixed IPC mailbox;
SCI/EC event subscription and dispatch are still absent. The native-loop target
extracts actual runtime ABI declarations and passes 39 scripted checks. Native
executable compilation/linking passes; kernel startup integration remains outstanding.


Authenticated process startup (2026-10-01): `ACPI_Launch.Decode` accepts label
0x4143 only with the kernel-stamped bootstrap tag 0x41435049424F4F54, exactly four
words, and zero flags/reserved fields in the decoded scalar packet. Words are
[observer tag, provider tag, zero, zero]. Tags must be nonzero, distinct, and
different from the bootstrap tag. The supervisor offers the provider endpoint
with the call; `Await_Configuration` accepts that offered capability only after
authenticating the configuration. The scalar decoder alone cannot establish
that binding. No capability slot or PID in the payload establishes authority. The kernel must prevent clients from minting the bootstrap tag.

`Await_Configuration` ignores denied configurations and accepts once, even if
its acknowledgment fails. Normal dispatch rejects subsequent configuration
messages. `ACPI_Native_Instance` keeps the service and limited grant adapter in
static process-lifetime storage and permits only one Start. Fatal returns exit
through the runtime; outstanding loan retirement then depends on kernel process
teardown, which has not been tested live here. Debug output is only a startup/
fatal diagnostic, not the planned CCL event or metrics stream integration.

`main.adb`, `acpi.gpr`, and the identity-only `manifest.ccl` now produce an ELF
service. The checked build requests a 64 MiB stack explicitly; this is a budget,
not a proved call-chain bound. The linked ELF has separate RX and RW segments,
a non-executable stack, no unresolved symbols, and only an identity manifest.
No raw-memory, port, DMA, capability-minting or hardware permission is requested.
No boot image, process-manager startup plan or provider export is installed.

To build in this checkout, with the manifest compiler and native runtime already
built, hold the shared lock for the entire operation (or use a private snapshot):

```sh
flock --exclusive --nonblock coordination/build.lock nix develop -c bash -c '
  set -e
  ulimit -S -s 65536
  mkdir -p userspace/services/acpi/build/generated
  userspace/ccl/build/manifest/ccl-manifest \
    userspace/ccl/catalogs/native-runtime-services.ccl \
    userspace/services/acpi/manifest.ccl \
    --ada-output userspace/services/acpi/build/generated/ccl_manifest_bindings.ads \
    > userspace/services/acpi/build/manifest.S
  cd kernel
  alr exec -- gcc -c ../userspace/services/acpi/build/manifest.S \
    -o ../userspace/services/acpi/build/manifest.o
  alr exec -- gprbuild -p -P ../userspace/services/acpi/acpi.gpr
'
```

Private executable validation: `/tmp/cubit-acpi-executable-nrie4zkx`, 420 inputs
matched their recorded hashes. Initial link needed the customary freestanding
builder switch; corrected link succeeded. Logs `/tmp/cubit-acpi-executable.log`
and `/tmp/cubit-acpi-executable-link.log`. Decoder proof: eight proved checks
plus one termination analysis, zero unproved/justified checks or Assume statements.
The standard `tests/aml-core/run.sh --prove` includes this proof.

Boot snapshot source (2026-10-01): kernel ACPI discovery now captures its complete
admitted inventory into `Firmware_Tables.Snapshots` once. The public kernel copy
routine reads this sealed cache, not live firmware memory. A failed capture
publishes no prefix. The userspace service uses the same count/size/payload
constants, so budget changes cannot drift between the producer and consumer.
The cache is ordinary kernel storage, never mapped/granted to userspace. The
future provider must copy into disjoint process-owned pages before granting
read-only access. Startup/transport wiring and firmware-page reclamation remain
separate unfinished work. See `tests/aml-core/snapshots/README.md` for proof scope,
regressions, storage cost and reproduction commands.

Retained table selection (2026-10-02): `Firmware_Tables.Identifiers` extracts
signature/OEM ID/OEM table ID byte-for-byte from the common header. Extraction
requires a complete header and is not checksum admission. Fixed-width fields
preserve NUL, spaces and high-bit bytes. `Selection` has a mandatory four-byte
signature and separate Match_OEM/Match_OEM_Table booleans; absent matching criteria
are explicit wildcards, not inferred from zero bytes. ASL string conversion and
validation remain separate future interpreter work.

`ACPI_Service.Table_Identity` reads only retained service bytes. `Find_Table`
returns the first matching service-local table index, or zero when no table
matches. Its contract specifies both matching and earliest-match behavior,
including complete absence on zero. No physical address or hardware capability
is returned, and this adds no new wire label or capability privilege.

This is a prerequisite for the DataTableRegion/Field sequence currently reached
by upstream ASLTS; those AML operations are NOT implemented by this change.
`acpica_table_find.py` compares the new lookup directly with ACPICA's execution
of DataTableRegion and a revision field read. It covers 36 exact/wildcard/missing
identifier selections using DSDT revisions 1 and 2. This is focused lookup
coverage, not successful execution of DataTableRegion in CuBit or an ASLTS pass.
The pinned upstream `source/components/tables/tbfind.c` was inspected alongside
the reference runs; no implicit short-string padding behavior is assumed here.

The service tests add 791 cases for selectors, duplicate ordering, zero-valued
IDs, every identifier byte value, and high/nondefault array bounds; total
1,049,899 checks pass. The ACPICA workflow now builds `table_find_runner` and runs
the comparison. The standard proof command includes the identifier extractor.
Native service link passes at `/tmp/cubit-acpi-identifiers-native-4ggvki0i`.
Logs `/tmp/cubit-acpi-identifiers.log`, `/tmp/cubit-acpi-table-find-compare.log`,
and `/tmp/cubit-acpi-identifiers-native.log`. The completed proof establishes
58 new checks: Read_Identity 37, Table_Identity 5, Find_Table 16, with no unproved
checks or Assume statements in those subprograms. The combined project report
contains 5,096 successful analysis results, including previously analyzed units;
this run selected the identifier and service units, not a clean whole-project
reproof. Existing generic-body flow warnings remain, without unproved checks.

## Bounded string Store and SRAT query integration

Named string-to-string Store preserves the destination identity and namespace
attachments, so package aliases see the new string. Local/argument Store captures
an independent string value. Authenticated descriptor refresh obtains current
length and coercion from owned storage. Unowned hosted adapters keep their
existing metadata behavior and gain no arena access.

Replacement uses append-only byte storage; old extents are not reclaimed and
repeated assignments may exhaust quota. Primitive replacement/clone failures
publish no mutation, but earlier source materialization is not rolled back by
an eventual target failure. This earlier String-specific milestone did not establish all named-destination
conversions or reference forms. The later buffer-field milestone adds explicit
CopyObject preservation of authenticated Index references. An Index
invalidated by shrinking a string fails closed on later reads/writes. ACPICA's
observed out-of-range byte result is not used as permission to access stale data.
These additions are tested, not newly proved.

SRAT labels 16–18 use the same authenticated Complete-snapshot/revision gates
and signed-safe split words as other table queries. No domain, address or handle
is authority. Record indices are one-based and metadata offsets zero-based.
| Label | Words2,3 | Reply words1,2,3 |
| --- | --- | --- |
| 16 Read_SRAT_Info | header page0,zero | SDT revision,record count,table revision |
| 16 Read_SRAT_Info | reserved page1,zero | reserved64 low32,high32,zero |
| 17 Read_SRAT_Record | record index,zero | wire type,byte offset,record length |
| 18 Read_SRAT_Fields | record index,page | fields below |

| Record | Page | Values |
| --- | --- | --- |
| Local APIC/SAPIC0 | common0 | domain low8,domain high24,APIC ID |
| Local APIC/SAPIC0 | detail1 | flags,SAPIC EID,clock domain |
| Memory1 | common0 | proximity domain,flags,zero |
| Memory1 | base2 | base low32,high32,zero |
| Memory1 | length3 | length low32,high32,zero |
| x2APIC2 / GICC3 / RINTC7 | common0 | proximity domain,APIC ID or processor UID,flags |
| x2APIC2 / GICC3 / RINTC7 | detail1 | clock domain,zero,zero |
| GIC ITS4 | common0 | proximity domain,ITS ID,zero |
| Generic initiator5 / port6 | common0 | proximity domain,raw handle type,flags |
| Generic initiator5 / port6 | handle4–9 | successive three raw handle bytes; final page has byte15,zero,zero |

Every other page/known-kind combination is Malformed. Globally unrecognized
pages>9 are Malformed before table lookup. Any recognized page0–9 on an unknown
kind is Unsupported_Record_Kind. Invalid table index, wrong signature, malformed
body and invalid record index retain distinct outcomes. Full table validation
precedes publication, including any malformed trailing record.

All64-bit values split into low/high32-bit words, preserving CCL signed-safe
response bounds. Raw type0 split-domain fields avoid invented legacy-revision
policy. Generic handles are not interpreted as namespace or PCI authority.
Domains are firmware identifiers, not compact operating-system topology indices.


String Store quota failures now propagate `Value_Limit` through local/argument
capture and named owner writes. The internal write result appends
`Write_Value_Limit`; existing result ordinals retain their values. Unsupported
kinds retain their previous classification. This does not make whole expressions
transactional: the quota fixture deliberately uses allocation-free earlier
expressions to check exact state preservation on failed assignment.

Additional ACPICA probes confirm missing metadata-value behavior in DerefOf,
CopyObject and root/Device package members. Those functionality gaps remain
open; rejecting metadata data-cell writes does not establish complete metadata
value semantics.

### Retained completion metrics

The pages below extend the existing authenticated `Read_Metrics`
request, `[page, 0, 0, 0]`, with response `[revision, a, b, c]`. Reading does not
execute or retry startup, advance the revision, or change upload state. Pages
0–12 retain their meaning.

| Page | a | b | c |
| --- | --- | --- | --- |
| 13 | Finish stage | Freeze outcome | Execution phase |
| 14 | Firmware preparation | Execution access status | Member access status |
| 15 | Module outcome | Completed table count | Failed catalog table ID |
| 16 | Module failure code | 0 | 0 |

Wire values are explicitly assigned, independent of internal enumeration order:
- Finish: not attempted=0, freeze failed=1, execution failed=2, members failed=3, finished=4.
- Freeze: frozen=0, wrong phase=1, empty=2.
- Execution phase: not started=0, preparation failed=1, module report=2.
- Preparation: not attempted=0, rejected=1, prepared=2, namespace capacity=3, value capacity=4.
- Access: available=0, invalid value=1, uninitialized element=2, wrong kind=3, out of bounds=4, root limit=5, identity exhausted=6, busy=7, wrong phase=8.
- Module: catalog rejected=0, completed=1, rejected=2.
- Module failure uses the existing explicit startup error codes (including No_Return=4, Unsupported=6, Budget_Exceeded=9).

Read page0's overall phase together with these pages. Premature finish or an
upload failure can leave completion at **not attempted** even though the session
has failed. Default Catalog_Rejected, Wrong_Phase and No_Return fields describe
an unattempted report; they are not independent evidence of a failed execution
or successful startup. Available means a report was obtained, not that its
module succeeded. Successful module completion also retains No_Return; phase
and module outcome qualify it. Module fuel remains page10 word3. Failed table
ID is a lifetime-local catalog identifier, never a PID, address or capability.
Snapshot completion remains separate from device/region readiness.

The private hosted gate passed all three release mains: completion metrics 732,
existing startup metrics 57 and endpoint 129 checks (918 total). The checked
completion main compiled but reached its unchanged 30-second runtime limit; the
other two checked mains subsequently passed in a separate bounded gate
(startup 57 and endpoint 129). This is functional release
evidence with incomplete supplementary checked coverage, not a proof or native
validation. The relocated portable completion test separately passed all 732 checks in
release mode. This single selection is not an all-24-suite rerun.
No transport, subscription, grants, native startup binding, region provider or
logger behavior changed.


## Retained Notify polling

Authenticated Snapshot_Provider requests own the bounded polling receipt; Observer
requests can only inspect it. Labels24–28 configure startup interest, claim one
FIFO event into a retained receipt, read copied metadata/path, acknowledge the
exact nonzero receipt, and inspect state. Retry of a lost claim response returns
the same receipt. Duplicate old ACK never clears a newer receipt. No payload PID
or path grants authority. Current_Target records dequeue-time authentication,
not fresh authority to invoke a node later.

Pre-INI interest is applied only after module completion/Ready. Module Notify
still evaluates its target and value, including effects and errors, while queue
admission remains disabled. The module entry guard requires a disabled empty
queue. Module event delivery and asynchronous SCI/event push remain future work.

Private hosted validation passed607checks in each release/checked profile across
six mains. Concrete receipt-policy instances0/2/default separately proved their
Issue/ACK contracts; this does not prove the full service. Native reply failure
is represented by direct endpoint retry tests, not native transport execution.
Portable registration and active-manifest repins are not reruns of predecessor
suites; historical validation records remain immutable.

Portable release polling379 and modulepolicy68 additionally passed via the new
relative-path runner; no full-six portable rerun is implied.

The bounded provider polling consumer policy and its hosted endpoint fixture are registered in `tests/acpi-notify-consumer`. It assembles copied observations using host-owned session/request correlation and requires explicit broker delivery confirmation before ACK. The provider endpoint remains trusted-broker-only; native CCL binding, completion routing and launcher distribution are not implemented by this policy. See the suite README for exact tested predecessor scope and portable commands.

The bounded snapshot provider orchestration policy and injectable endpoint/grant-lifecycle fixture are registered in `tests/acpi-provider-bootstrap`. It serializes a complete immutable catalog, separates provider grant retirement from admission, and never replays an uncertain import or Finish. Callbacks require exact synchronous authentication and exclusive provider ownership. This is hosted policy, not native catalog export, launch or kernel-grant validation. Notify interest must be configured by the broker before Finish when startup notifications are desired. See the suite README for evidence and remaining binding work.
