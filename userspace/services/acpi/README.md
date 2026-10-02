# ACPI service core

`ACPI_Service` is the pure, SPARK-analyzed state machine for future userspace
service integration. It is not yet a launched CuBit service: a native IPC runner now exists, but
there is no executable main, launch manifest, trusted startup configuration,
CCL binding, SCI/EC handler or hardware callback.

Install accepts stable copied SDT bytes and a provider-assigned identity. The
first successful table must be a DSDT; subsequent tables may be ordered SSDTs or
other standard description tables (`Description`). The
shared Firmware_Tables parser checks signature, length and checksum. This API
requires an exact table-sized input, rejects duplicate identities, and limits
one table to64KiB, the session to32 tables/1MiB and namespace storage to512 nodes.
These are explicit development budgets, not ACPI specification limits.

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
Table_Rejected=6, Incomplete=7, Not_Found=8. Reply headers always have length four and zero
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
Method bodies occupy a separate append-only 65536-byte pool shared by the
namespace. A method is no longer limited by the 1024-byte buffer-literal limit.
Definitions returned to the executor contain exactly the method's body length;
calls retain the same instruction and recursion budgets. Loading copies the
body, and failed admission rolls back code storage together with the namespace.
VarPackage counts may reference already-loaded named integers. Lookup uses the
initializer's lexical scope, including ancestor search and explicit parent/root
paths. A read-only namespace snapshot supplies counts, and the object loader
checks both consumed-byte bounds and allocation limits before committing.
Counts are resolved once during loading. Arbitrary count expressions, method
calls, forward references and truncating surplus initializers remain unsupported.
Snapshot returns a read-only-by-copy namespace view for current hosted consumers.

`Namespace.Invoke_Mutable` is an internal execution entrypoint for an owned
namespace state. Named integer stores and integer-operation targets update that
state, and nested calls share the updates. Integer destinations normalize to
the executing method's width. Completed writes remain visible after a later
execution error or budget failure. `Invoke` continues to use a read-only binding
and rejects named writes. Neither entrypoint is exposed through CCL or grants
hardware access.

Named integer operands capture their value when evaluated; Local/Arg operands
retain the existing deferred slot behavior. ACPICA comparisons cover this
distinction. Named stores currently require integer sources and integer
destinations; implicit object conversion, mutable references, other destination
types and Debug output remain unfinished. Synchronous serialized-method calls
enforce sync-level ordering, including inherited levels through nonserialized
calls and recursive entry. Invocations require exclusive context ownership;
concurrent AML threads, yielding and explicit mutex operations remain unfinished.
Mutable invocation also supports temporary `Method` declarations, with ownership
and last-invocation cleanup across recursion and execution errors. It reclaims
namespace slots and method bytes while preserving completed integer writes.
Temporary data objects, regions and fields remain unfinished. This is internal
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

The bound interpreter now transports named strings, buffers and packages through
locals and method calls and can return them as `Object_Returned`. Such a result
identifies backing data in the same immutable namespace snapshot; it is not a
wire handle or writable AML reference. The caller must retain that snapshot.
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
The configured provider capability slot is a separate trusted startup input.
A failed grant return preserves the completed import reply and pending cleanup;
the eventual loop must retry return without replaying import. The old scalar-only
native Dispatch signature was replaced with an adapter-state/provider-slot form.
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
words, and zero flags/reserved fields. Words are [observer tag, provider tag,
provider capability slot, zero]. Tags must be nonzero, distinct, and different
from the bootstrap tag. The slot must be 0..62; reply slot 63 is excluded.
The supervisor must install that capability bound to the same immutable-table
provider whose endpoint bears the provider tag. Neither the decoder nor a
payload can establish that binding: trusted capability installation is still
required. The kernel must prevent clients from minting the bootstrap tag.

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
