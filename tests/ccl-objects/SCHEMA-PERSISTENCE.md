# Portable CCL type persistence

Run all commands inside the Nix environment. This is storage preparation for
public typed Config creation, not a new authority system or executable CCL.

Current update (2026-09-26): private DB format4 adds immutable management class.
The schema CBOR format itself is unchanged. Recovery carries the class separately;
ordinary client Create/Commit cannot overwrite declaration-managed collections.
The Ada nominal-equivalence fallback also refuses to reinterpret a managed
registration as application state. Hosted native-service/real-Turso checks now
include two cold Config lifetimes; the schema channel covers three database
lifetimes,306 checks, with four original declarations and zero initial values.
See [declarative configuration](../../docs/config-declarative-state.md) for limits
and evidence. Earlier format3 artifact descriptions below are historical.

`CCL.Objects.Schemas.Persistence` stores the root's bounded transitive type
declarations, root type and schema key as deterministic CBOR. It shares the native schema wire IDs
and importer instead of adding a second semantic type checker. Type names are
byte strings, matching current CCL strings; no Unicode normalization is implied.
The maximum encoded buffer is 26,688 bytes. The largest current catalog fixture
uses all 32 declarations, 16 components each and 32-byte names.

Both native metadata and CBOR export use the shared `Root_Closure` importer.
Unrelated visible types, including opaque resource descriptions, are omitted;
references are translated to the exported registry. Therefore an in-memory
registry need not be byte-for-byte equal to the recovered registry. Schema
identity still uses full nominal correspondence and the approved key.
Resources and aggregates containing resources cannot be bound for persistence.
The CBOR decoder rejects unused declarations through its exact re-encoding
check. This does not promise identical bytes for every possible ordering of
reachable sibling declarations; the nominal-equivalence path below remains.

## Equivalent declarations and idempotent Create

CBOR canonical encoding does not make process-local type numbering or unrelated
declarations part of schema identity. Since 2026-09-25, the Ada database adapter
handles Rust's byte-level `DefinitionConflict` by reading and validating the
existing declaration, then comparing `CCL.Objects.Same_Schema`. The key, nominal
root name, shapes, field/alternative names and order, and recursive payload types
must agree. Only then is the result `Already_Exists`. Nothing is rewritten and
no value/revision is created. This is not a migration or an uncertain-write retry.

This slow path relies on the existing exclusive database ownership and immutable
declaration API. A missing, malformed or failed recovery after a conflict leaves
the outcome `Uncertain`, requiring worker retirement. Normal Get/Set and exact
byte-equal Create do not acquire an extra database read. The hosted real-Turso
channel fixture now performs equivalent Create in three independently opened
sessions, including shifted/reordered dependencies of a nominal product. Its
230 checks and independent SQLite oracle require exactly the original three
declarations and zero values/revisions. Existing shape/key conflicts still fail.

Evidence: `/tmp/cubit-schema-equivalence-turso-final.log`; artifacts
`/tmp/cubit-config-publication.FQZJgx`. Native KVM writer and independent reader
also pass with exact SQLite/WAL/ext2 validation. The writer recreates nested
Preferences under a shifted registry without changing its stored revision/value:
`/tmp/cubit-schema-equivalence-native.log` and corresponding native/reopen serial
logs. This is not an arbitrary power-failure or complete schema-soundness proof.

```sh
bash tests/ccl-objects/run-schema-codec.sh --prove
bash tests/ccl-objects/run-schema-turso.sh
cargo test --manifest-path tests/config-turso/Cargo.toml --lib
```

The codec suite checks 29,504 cases: golden bytes, all single-byte mutations of
the primitive golden frame, truncations (including the maximum catalog), shifted
input bounds, canonical encoding and failed imports exposing no partial binding.
The focused SPARK run, repeated after dependency-closure export, discharges
112 initialization/runtime/contract checks with none unproved/justified.
This is not a theorem of round-trip equivalence, full database correctness,
pointer/FFI lifetime or authority enforcement.

The second command is **Linux-hosted**, using actual Turso and an independent
SQLite reader. Ada emits a large connected sum schema and native variant value; Turso creates
an unset declaration, closes/reopens, saves its first value and closes/reopens
again. Both Turso and SQLite export their exact stored bytes; Ada reimports them
and compares the full binding and native object. The fixture has one revision,
not an invented initial value. Files are created under a fresh temporary path.

## Durable declarations and the worker

Config database format 3 adds `object_types`, keyed by namespace and context.
Create is transactional and immutable: identical requests are idempotent;
changed definitions, even with the same claimed key, conflict. Revisions cannot
be silently adopted by attaching a new type. A missing declaration with existing
revisions fails rather than looking like an absent object. Definitions carry no
handles, grants or restored approvals. The old format is rejected, never erased
or silently migrated.

`Config_Database.Schemas` provides private in-process Create/Recover over the
same exclusively owned Rust database as value operations. Ada encodes before
Create and semantically validates every recovered declaration. Rust checks
bounded canonical framing and exact stored keys, not full CCL type semantics.
Only worker-owned buffers cross this FFI; never client/grant pointers.
Uncertain creation or failed recovery requires abandoning the worker session.
The native probe exercises these calls alongside existing Ada value publication;
the production worker's public typed catalog and Create IPC are not yet wired.

On 2026-09-25 the two-boot **CuBit/TCG** probe passed with exact declaration
recovery and value revisions 1 and 2. Independent Linux SQLite and ext2 checks
passed for both exported disks. Artifacts:
`/tmp/cubit-schema-reboot-final.S8vTZD/run`. Reproduce under the shared lock:

```sh
bash tests/config-turso/native/run-reboot.sh /tmp/NEW-schema-reboot-results
```

The output directory must not exist; the script tests disposable disk copies,
not the user's base disk. The separate live `config-storage` boot test checks
worker startup and database initialization, not public client creation.

SQL readout bounds both declaration and key blobs before returning them across
the adapter. This limits adapter output allocations, not Turso's internal work
on an arbitrarily hostile SQLite file. No signature/digest authentication is
claimed for the schema key. Ext2 remains non-journaled, so reopen tests are not
proof of power-loss atomicity.
