# Config storage worker boundary

`userspace/lib/config/Config_Worker_Protocol` defines native, schema-bound load
and conditional-commit frames. It is not yet connected to live `config.svc`.

```sh
nix develop -c bash tests/config-worker/run.sh
nix develop -c bash tests/config-worker/run.sh --prove
flock --exclusive --nonblock coordination/build.lock nix develop -c bash -c 'cd kernel && alr exec -- gprbuild -p -P ../tests/config-worker/native.gpr'
nix develop -c bash tests/ccl-objects/run-durable-turso.sh
```

The first two commands are Linux-hosted tests/proof. The third compiles an
isolated native library (including a concrete executor instance) without
enabling runtime assertions. The last exercises
the frames and direct Ada/Rust ABI against real hosted Turso. It is not
CuBit IPC or a running native Config service.

## Worker execution

`Config_Worker` now executes those frames, parameterized only by the private
database operation. It validates before invoking storage, encodes commit
values using the shared persistence codec, and decodes/validates loaded values
against the supplied trusted binding. Load requests pass only the expected
schema; no exemplar value or CCL evaluation is needed. The backend is given
object/context names, never a client-selected database path or SQL statement.

The raw backend reply admits arbitrary status, revision and length fields.
The executor checks these before conversion/use. Invalid or uncertain replies
produce canonical `Load_Failed` / `Uncertain` responses and permanently retire
that executor instance from database operations. A later valid request gets a
failure without another backend call. Recovery requires disposing of the old
connection and creating a replacement worker/session, not clearing a flag.
Invalid incoming frames invoke no storage and do not poison a healthy worker.

The callback must translate backend failures into outcomes and must not unwind
through the boundary. `Rejected` must mean definitely no change; a commit that
may have happened must report `Uncertain`. The generic executor cannot prove
the database tells the truth about durability. The future IPC shell must still
authenticate, snapshot and correlate loans before entering this code.

## Native frame

The 20 KiB frame is a header page plus the existing 16 KiB `CCL.Objects.Image`.
Its layout is explicit, pointer-free and native-endian. Every input field admits
all bit patterns; validation precedes narrowing or enum interpretation. The
header carries format, operation, reply kind, session, request token, revision,
and bounded object/context names. Padding and unused name bytes must be zero.
The object schema comes from a trusted binding, never from the sender's claim.

Load requests contain a canonical empty image naming the expected schema.
Commit requests contain a fully validated candidate and expected revision.
Only a `Loaded` reply carries a value. Other replies contain a canonical empty
image, not a copy of the commit payload. A commit success must name exactly the
next revision; a definite rejection retains the expected revision; a conflict
must differ; uncertain outcomes do not claim a saved revision. Reply operation,
session, token, object and context must match the retained request.

Object/context names follow the existing Turso adapter's component-separated
ASCII grammar. They are storage addresses, not new authorization identities or
a replacement for Config's public namespace rules. This first protocol does
not add delete, enumeration, schema migration or cross-object transactions.

## Required IPC integration contract

- Only an explicitly attached Config endpoint may call the worker. The worker
  gets database-file authority, not authority to grant Config access. Authenticate
  both peers using kernel-backed identities/capabilities; do not trust names or
  these correlation numbers. Boot attachment remains pending procmgr coordination.
- Authorize client scope/context/schema before staging. Take an owned snapshot
  before validation; receiver-read-only grants do not freeze sender memory.
- Retain the original request privately. Never correlate a response against
  a request header taken back from the same writable response loan.
- A future adapter must check mapping bounds, grant rights, identity/lifetime,
  outer IPC tag/length and completion status before calling these routines.
  This package deliberately does not allocate syscall labels or grant slots.
- Route completions through the owning dispatcher/thread. An unexpected reply
  is not permission to publish or retry. Matching but malformed completion,
  worker loss or uncertain I/O requires recovery through `Config_Objects`.
- Successful transport validation is not a durable commit. The worker must
  issue `Committed` only after the promised storage durability boundary. The
  cache publishes through `Config_Objects`, not directly from this frame.
- Keep the existing Turso native bridge single-owner until thread-aware
  completion routing and synchronization are integrated. This protocol itself
  has no shared mutable globals, but does not make its callers thread-safe.

## Evidence

2026-09-24: 4,573 hosted protocol checks passed, covering operation/status/revision
combinations, session/token/name/context mismatch, arbitrary length fields,
schema mismatch, every header-padding byte, name tails and revision limits.
The executor adds 602 checks: the operation/status/revision matrix, actual
commit encoding, real load decoding, malformed/oversized backend output,
no storage calls for invalid frames, and no further calls after retirement.

All 51 focused SPARK checks discharge (12 initialization, 28 runtime, 5
contracts, 6 termination), including the actual generic instance in
`Worker_Proof`, not just its template. That instance's concrete SPARK callback
returns arbitrary backend fields; it is not an assumed external contract.
No assumptions or SPARK-Off sections were added.
The contracts establish accepted builders satisfy their validators and names
are bounded; this is not a proof that the entire protocol matches a security
model, nor a proof of IPC authentication, grant lifetime or database durability.

The real hosted Turso test runs THROUGH the reusable executor, not just
protocol builders around a separate codec path. It passes 46 checks, including
a dropped successful commit receipt, database close/reopen, worker replacement
and reload. Independent SQLite inspection finds exactly two revisions,
demonstrating no blind retry in this fixture.

`Config_Database` now supplies the in-process Rust FFI callback. Its private
x86-64 ABI has a 328-byte request and 13,136-byte reply with explicit Ada record
positions and Rust `repr(C)` layouts. Rust tests check every field offset. All
reply scalar bit patterns are valid; the SPARK executor checks status, length,
revision and decoded semantics before publication. Names, context and schema
cross this boundary, but no client-selected SQL or database path. The trusted
owner supplies an exclusively borrowed Rust database context. No pointers are
retained or transmitted through IPC. This is not a zero-copy storage claim.

The Rust adapter retires on uncertain database errors and load failures;
recovery requires a new instance. Definite pre-I/O malformed-payload rejection
does not poison the connection. The 26 Rust tests include ABI/null/length
checks, malformed metadata/payloads, context separation, revision conflicts and
retirement after a schema mismatch. Parallel MemoryIO tests use unique database
paths because Turso's database registry shares identity by path.

The old subprocess/hex-file publication fixture was replaced, not retained as
another transport. Test-only open/close exports use Linux paths; native startup
must instead open the Store against its authorized NativeIO adapter. The FFI
pointer/alignment/lifetime/exclusivity obligations, Rust SQL implementation,
crash durability and caller authorization are **not** established by SPARK.
The existing 51 focused proof checks still all discharge after the ABI change.

Final combined run `/tmp/cubit-config-worker-final.log` also passes the existing
359 object, 70 VM/host bridge and 583 publication checks, plus all 21 Rust
backend tests. The proof runner explicitly checks diagnostics: a zero
GNATprove exit status alone is not evidence that every obligation discharged.

Earlier native attempts were blocked by the shared build lock. The later
coordinated build window completed native compilation and QEMU validation;
see the evidence below.

## Host-independent native probe

`Config_Native_Probe.Run` exports `cubit_config_worker_probe(database, phase)`.
It exclusively borrows a live Rust `worker::Database`; it does not open a path,
retain the pointer, perform hosted I/O, or install live Config authority.
The caller owns database creation/destruction and must serialize all calls.

Phases are `Seed = 0`, `Advance = 1`, `Verify_Only = 2`. Seed requires absence
and commits integer 41; Advance requires exactly revision 1/value 41 and commits
42; Verify requires revision 2/value 42 and writes nothing. The namespace and
schema match the independent SQLite oracle. Each phase uses the actual
`Config_Objects` state machine and `Config_Worker` executor, checks old-value
visibility after the SQL commit, then checks publication after acknowledgment.
The probe also creates the native Integer declaration before its first value,
recovers the exact binding after reopen, checks identical-Create idempotence,
and rejects a different type using the same claimed key. The private metadata
ABI uses the same owned database as value publication, with Ada validating the
recovered type. These calls are not public Config IPC or restored authority.
Return 0 means success; 1–26 identify failed checkpoints. These are explicit
test checks, not assertions removed by native release compilation.

The hosted `native_probe_host` lifecycle adapter closes/reopens real Turso
between phases. It also tests null/invalid-phase rejection, repeated-seed and
repeated-advance refusal, and repeatable verification without durable changes. The existing
`run-durable-turso.sh` runs both scenarios and independently checks both files
with SQLite. This passes in Nix on Linux; it is NOT yet a CuBit execution.
The new test entry is not itself SPARK-proved.

The isolated native library project is:

```sh
flock --exclusive --nonblock coordination/build.lock nix develop -c bash -c \
  'cd kernel && alr exec -- gprbuild -p -P ../tests/config-worker/native_probe.gpr'
```

Native Rust startup now opens its authorized NativeIO-backed Store, wraps it
in `worker::Database`, calls this entry with an exclusive pointer, and closes
it. On 2026-09-24 the library compiled and **two independent CuBit/TCG boots
passed**: seed 41 at revision 1, restore it in a fresh guest, commit 42 at
revision 2, then close/reopen/read-only verify. Independent Linux SQLite checks
confirmed exact CCL CBOR history and ext2 checks passed on both guest disks.
The normal seed probe was restored afterward. Log:
`/tmp/cubit-config-worker-native-20260924.log`; artifacts directory:
`/tmp/cubit-config-worker-native-20260924/`.

This is the native test application, not live `config.svc` attachment or client
IPC authentication. Startup catalogs and live Config behavior are unchanged.
The probe deliberately tests clean reopen, not filesystem power-loss recovery.
