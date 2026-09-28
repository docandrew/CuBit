# Public typed Config, inside CuBit

The read fixture now also compiles `(config-test.read)`, links only its granted
operation and runs it with the native-object VM wrapper. It reaches
Waiting_For_Host, obtains the real Config response, resumes, and compares the
entire returned Missing/Found object after completion. This includes nested
records/variants and the 8 KiB value. No client serialization is involved.
The fixture's Invoke callback waits in its dedicated test process; the VM does
not wait or poll. This is not yet production Workbench async dispatch.
Native writer and independent disk validation pass in
`/tmp/cubit-object-vm-native.log`; independent read-only reboot and disk checks
also pass in `/tmp/cubit-object-vm-reopen.log`.

The fixture additionally projects `Preferences.name` through interpreted CCL
and exports it as a standalone typed String, compared against the actual stored
field after evaluation teardown. Missing returns an empty String; the populated
and rebooted cases include the full 8 KiB name. This adds no declarations,
revisions or filesystem authority. Writer/reboot and independent SQLite/WAL/ext2
checks pass: `/tmp/cubit-native-strings-{native,reopen}.log`. Native strings no
longer have to fit the scalar UI result buffer to flow through typed APIs.

The read fixture now also invokes `Interpret_Object_With_Values` on
`(config-test.read)`, expecting the approved NestedRead schema. After evaluation
returns (and temporary snapshots are cleared), it compares the entire owned
native result to the prior verified response: Missing, or Found with exact
revision, nested fields and full text. This adds a read, not a write/revision or
filesystem authority. Writer and independent disk validation pass in
`/tmp/cubit-native-result-native.log`. The fixture still uses a blocking test
host, not production Workbench async plumbing.
Independent read-only reboot validates the same complete returned objects and
passes the disk oracle: `/tmp/cubit-native-result-reopen.log`. Normal desktop
ISO restored; no additional stored declarations/revisions were introduced.

## Interpreted aggregate write

The first nested write now also goes through source construction: CCL extracts
the supplied Mode payload, builds `(Preferences (concat "Cub" "ie") (Mode.Active
active) -9223372036854775808)`, and sends it to Config. The fixture verifies
the exact canonical image and real Committed revision1, including the unchanged
5 KiB nested text payload. Thus this exercises actual source record and variant
constructors, not only forwarding an opaque host object. Larger supplied data
is reused as a typed subtree, not forced through the 1 KiB scalar-string API.
Writer and independent read-only reboot pass with SQLite/WAL/ext2 validation:
`/tmp/cubit-constructors-native.log`, `/tmp/cubit-constructors-reopen.log`.
Normal desktop ISO restored after these test boots.

`Read_Fixture.Store` now replaces the writer's second raw nested Set. A trusted
test-host operation supplies the already-owned object, and interpreted CCL calls
`(config-test.store (config-test.supplied))`, then matches the ordinary
ConfigWrite result. The store host submits through `Config_Object_Client.Host`
and waits for the real Committed receipt; it does not manufacture success.
The argument must equal the complete expected native image, including the full
8 KiB text value. This writes the same revision 2 as the former Ada-only call,
so the existing independent database oracle remains unchanged.

This tests native source aggregate construction, argument transfer and
persistence, not bytecode aggregates or production Workbench bindings.
Test names/grants are supplied by the fixture; data does not confer authority.
The nested read-to-write projection and missing-write-grant preflight rejection
are additionally covered by Linux-hosted source tests.

Native writer and independent read-only reboot pass with SQLite/WAL/ext2
validation: `/tmp/cubit-object-arguments-native.log` and
`/tmp/cubit-object-arguments-reopen.log`. The normal desktop ISO is restored
after the test boots. No additional declarations or revisions are introduced.

`Read_Fixture` executes interpreted CCL through the shared host adapter's ordinary
typed read results: Missing on an unset nested collection, and Found(revision, value)
with exact nested data and the full text block. The independent read-only boot
checks Found against recovered data with shifted local type numbers. Required
markers are `config-objects-read-outcome` and
`config-objects-read-outcome-reopen`. Source matches all result alternatives and
reads `(field snapshot revision)`; the fixture also compares the full native
value. Its host callback performs actual Config IPC and waits in this dedicated
test process. This is not Workbench async integration or compiled aggregate
execution. Writer and independent reboot pass, including SQLite/WAL/ext2 checks:
`/tmp/cubit-object-read-source-native.log` and
`/tmp/cubit-object-read-source-reopen.log`. The normal desktop ISO is restored.
The fixture retains
no filesystem/database authority and introduces no extra declarations or writes.

Write imports now return the ordinary `ConfigWrite` variant rather than a
Boolean. `Committed` carries the durable revision. The async writer matches
that alternative; the independent read-only boot additionally attempts a write,
matches `Denied`, and reads the same revision/value. Required final pass markers
are emitted only after that denial check. No database revision is added by the
reader. `config-objects-write-outcome-denied` identifies this extra scenario.
Other alternatives are exercised by the hosted interpreter and bytecode tests.

The asynchronous compiled fixture now submits and extracts outcomes through the
reusable `Config_Object_Client.VM.Calls` adapter. This process owns only the
granted binding dispatch, completion event loop and VM scheduling; native
get/set receipt conversion is no longer test-only. See the parent README for
freshness, uncertainty, single-owner lifetime and hosted regression coverage.

## Compiled, portable typed calls (CCLB v5, 2026-09-25)

`async_fixture.adb` now analyzes actual source, compiles it, encodes/decodes a
CCLB v5 module and links it against the authorized schema catalog and grants.
The resulting verified VM suspends for native Config calls; no script replay
or client-side value serialization is involved. The module contains schema
identities, not runtime endpoint bindings.

```lisp
(match (config-test.set (Reading.Value 42))
  ((ConfigWrite.Committed revision) (config-test.get))
  ((ConfigWrite.InvalidRequest) Reading.Unavailable)
  ((ConfigWrite.Denied) Reading.Unavailable)
  ((ConfigWrite.Busy) Reading.Unavailable)
  ((ConfigWrite.Unavailable) Reading.Unavailable)
  ((ConfigWrite.Conflict) Reading.Unavailable)
  ((ConfigWrite.Rejected) Reading.Unavailable)
  ((ConfigWrite.Uncertain) Reading.Unavailable))
```

The current fixture additionally saves the result in a let and performs two
reads: exactly three calls/completions. The read-only boot compiles
two Gets against shifted local type IDs and reads revision 2. Both fixtures
check that stepping a suspended VM does not change its PC, fuel or step count.
The first version of this fixture incorrectly put `let` inside a conditional
branch, an explicitly unsupported compiler form. The test above keeps locals
outside branches; no compiler restriction was bypassed.

Writer KVM and independent SQLite/WAL/ext2 verification pass:
`/tmp/cubit-cclb5-fixed.log`; seed `/tmp/cubit-cclb5-fixed-seed/disk.img`.
Independent read-only KVM and SQLite/WAL/ext2 verification also pass:
`/tmp/cubit-cclb5-reopen.log`, with no extra declarations or revisions.
The normal desktop ISO was restored after these isolated test boots.
The historical v4/manual-program limitations below are superseded by this test.
Test-only operation names and host bindings are still not the final public
Config API or Workbench integration. General aggregate execution remains future
work; no whole-system or power-loss durability proof is claimed.

## Typed VM suspension (2026-09-25)

The original `async_fixture.adb` constructed one verified internal VM program with two reads
and a nominal `Reading` return type. Execution yields at each import; the event
loop submits a single Config Get, dispatches authenticated completion entries,
converts the owned result under the program's registry and resumes that same VM.
Stepping it while pending preserves its PC, fuel and step count. Neither the VM
nor the shared Config client waits; only this headless test's event loop parks
on kernel activity. No script replay, timer polling, SQL or client codec.

The writer reads `Reading.Value(42)` at revision 1. An independent read-only
boot reads `Reading.Unavailable` at revision 2 with shifted local type IDs.
Required markers are `config-objects-async-vm` and
`config-objects-async-vm-reopen`. KVM runs pass, including unchanged independent
SQLite/WAL/ext2 oracles: `/tmp/cubit-async-typed-config.log` and
`/tmp/cubit-async-typed-config-reopen.log`; seed:
`/tmp/cubit-async-typed-config-seed/disk.img`.

That milestone exercised native IPC and resumable execution, but not typed
source-to-CCLB compilation. The v5 fixture above now covers compilation too;
Workbench integration remains separate.

## Source-level typed calls (2026-09-25)

The earlier synchronous source fixture evaluated this CCL source inside CuBit:

```lisp
(if (config-test.set (Reading.Value 42))
    (config-test.get)
    Reading.Unavailable)
```

`Reading` and the argument/result schemas come from the approved catalog, not
declarations in the snippet. `Config_Object_Client.Host` carries the owned native
object through Config IPC; the application has no SQL, CBOR codec or database
authority. The fixture subsequently saves `Reading.Unavailable` as revision 2
through the VM adapter. A separately booted read-only client evaluates
`(config-test.get)` against a shifted local type registry and recovers that value.

Both KVM boots pass their required source-host markers and independent
SQLite/WAL/ext2 checks: exactly three declarations and six revisions, with no
extra writes from the reader. Logs: `/tmp/cubit-source-host-config.log` and
`/tmp/cubit-source-host-config-reopen.log`; exported writer image:
`/tmp/cubit-source-host-config-seed/disk.img`.

Scope: `source_fixture.adb` supplies test-only names and a synchronous interpreter
host, waiting on kernel activity in this dedicated process. The shared adapter
itself never waits. This is not a final public Config API, GUI integration,
general aggregate interpreter/CCLB support or proof of power-loss durability.
The remaining sections record earlier VM-driven and native-image milestones.

Schema-equivalence regression (2026-09-25): the writer closes and recreates the
nested collection using a shifted registry, then requires the exact old value
and revision. `config-objects-equivalent-schema` is a required headless marker.
KVM writer and independent reader pass with the SQLite/WAL/ext2 oracle; logs
`/tmp/cubit-schema-equivalence-native.log`, `/tmp/cubit-schema-equivalence-native.serial`
and `/tmp/cubit-schema-equivalence-reopen.serial`. No declaration or value is
rewritten by equivalent Create. A separate hosted real-Turso test covers this
comparison on a cold database open; see the schema-persistence tests.

The discovered Reading fixture now obtains its approved binding through
`CCL.Objects.Catalog` before exposing the type to compilation. Native KVM
writer and KVM/TCG reader validation remains green, including independent
SQLite/WAL/ext2 checks with three declarations and six revisions. See
[schema catalog validation](../../ccl-schema-catalog/README.md) for the new
catalog's security boundary, proof scope and exact logs.

Run from the repository root in Nix while holding the shared build lock:

```sh
flock --exclusive --nonblock coordination/build.lock nix develop -c bash -c '
  make -C kernel config procmgr clock ccl-manifest &&
  bash userspace/services/config-storage/build.sh &&
  bash tests/config-object-client/native-app/build.sh &&
  QEMU_CPU_MODEL=host bash tests/headless/run.sh --test config-objects \
    --accel kvm --timeout 40 --keep-logs'
```

For TCG, omit `QEMU_CPU_MODEL=host` and select `--accel tcg,thread=multi`.
The runner creates a disposable copy of the base ext2 disk; never tests against
the original in place. The profile starts the real Config storage worker and
this Ada app. The app holds only its Config endpoint and the exact
`org.cubit.publication` scope, not filesystem/database or worker authority.

Checks cover Create/unset/Get/Set/Close, exact native integer values, revision
conflict preserving the value, read-only handle denial, wrong-namespace denial,
and idempotent Create. A second collection holds a nested Preferences product
whose Mode sum contains either Unit or an ActiveData product (Boolean + String),
along with a String name and signed integer score. Two revisions exercise a
5,000-byte nested string, the full 8,192-byte text budget, both integer extremes,
and changing variant payload shape. Full native-image equality also checks
that unused cells/text/padding do not retain earlier data. A malformed variant
is rejected by the client before submission; a stale write reaches the service
and must preserve the old committed snapshot.

A third collection uses the advertised `Reading` variant. The host publishes
its approved description into a discovery catalog, then compiles
`(Reading.Value 42)` and `Reading.Unavailable` **without declarations in the
source**. Those VM values are written/read through the shared adapter. The
reboot reader has an unrelated discovered type first, shifting Reading's local
ID; it must recover the correct variant and remain unable to write. Discovery
does not grant Config access: the manifest/endpoint/collection checks still do.

Validated with a KVM writer and separate KVM/TCG readers, including the unused
record-metadata verifier correction. All three independent SQLite/WAL/ext2
checks pass with three definitions/six revisions. Latest logs:
`/tmp/cubit-discovered-types-create.{log,serial}`,
`/tmp/cubit-discovered-types-reopen-fixed.{log,serial}` and
`/tmp/cubit-discovered-types-reopen-tcg.serial`;
seed `/tmp/cubit-discovered-types-seed/disk.img`.
Earlier two-collection seed images do not satisfy the expanded oracle.

The scalar writer compiles and executes `(+ 20 21)` and `(+ 20 22)` inside
CuBit, then passes those actual VM values through shared
`Config_Object_Client.VM.Set_Value`. Get reconstructs a VM value through that
same adapter. The read-only reboot app compares the recovered value against
its independently compiled `(+ 21 21)` and verifies adapter writes are denied.
Pass markers `config-objects-compiled-vm` and
`config-objects-compiled-vm-reopen` are required by the runner. The storage oracle
still expects exactly 41/42 plus the existing nested revisions, independently
of the compiler/adapter. This is **real CuBit IPC**, originally host-driven;
the source-level extension above now exercises a typed interpreter import too.

Validated 2026-09-25: KVM writer, independent KVM reader and independent TCG
reader pass these compiled-VM markers and the nested-object regression. Every
run passes the independent SQLite/WAL/ext2 oracle. Logs:
`/tmp/cubit-config-vm-create.{log,serial}`,
`/tmp/cubit-config-vm-reopen.log`,
`/tmp/cubit-config-vm-reopen{,-tcg}.serial`;
exported seed `/tmp/cubit-config-vm-seed/disk.img`.

Waiting uses completion queues and kernel activity wait,
not a timer-polling loop. One process-wide nonreusing token sequence covers all
client instances. Client code contains no serialization or SQL.

After the guest finishes and QEMU stops, the Linux oracle extracts the database
and WAL, checks three exact persisted declarations and six revisions (the scalar
41 then 42, both nested values and both discovered Reading variants), runs
SQLite integrity checking and read-only e2fsck. The oracle is independent of the
Ada/Rust encoders. This is quiescent durable publication, not an ext2 power-cut
guarantee.

## Owned receiver and native data calls

`resource_fixture.adb` now compiles actual source for open/get/close, using an
explicit fixture-approved resource interface and ownership catalog, then links
it through granted bindings. Its host issues real Config IPC and validates the
returned integer 42/revision 2. The resource VM marker therefore no longer
depends on a hand-written program for those calls. This is not yet generic
`Config.create(type)` syntax, portable resource bytecode, or Workbench dispatch.

The data result is independent of the handle lifetime: the program retains its
read-back snapshot, closes the collection, and only then exports the final value
for comparison with the expected native object. Config values are copyable data;
the ownership restrictions apply to the collection reference, not its contents.

`receiver_fixture.adb` compiles and links CCL source to acquire the
nested Preferences collection, read its first snapshot, write its second
snapshot, read it back, and close the collection. The resource receiver is
borrowed from an owned local; the write payload is a separate native object on
the VM operand stack. It includes maximum-length text and nested variants.
No resource reference enters an object image or the database.

The fixture supplies the approved descriptor, schema, ownership policy and
grants to the ordinary analyzer/compiler/linker. In its source, the write is
`(config-receiver.set collection (config-receiver.supplied))`; the get is
`(config-receiver.get collection)`. These are fixture-defined operations, not
public Workbench bindings or generic type-argument syntax. The compiler, rather
than the test host, chooses local ownership tags and bytecode import positions.
The host dispatches on granted runtime binding, not compiler import order.

This supplies the existing test's second nested write, so the independent
SQLite/WAL/ext2 and fresh-boot checks still require the same exact value and
revision 2, not a relaxed oracle. The headless writer requires
`TEST: PASS config-objects-receiver-vm`. Its host uses the shared
`Config_Object_Client.Resources` wrapper, which owns the reference/client
association, registry lifecycle and confirmed cleanup. The fixture dispatches
authenticated IPC completions; its blocking activity wait belongs only to this
dedicated test host.
It does not yet exercise public source-level `Config.create(type)`, portable
resource signatures, or a production Workbench resource pool.

Source receiver validation (2026-09-25): native writer and independent read-only
reboot passed with the unchanged SQLite/WAL/ext2 oracle. Logs:
`/tmp/cubit-source-receiver-native.log`,
`/tmp/cubit-source-receiver-native.serial`,
`/tmp/cubit-source-receiver-reopen.serial`. The native Workbench smoke also
passed; the normal desktop ISO was restored afterward.

## Read-only recovery across boots

```sh
flock --exclusive --nonblock coordination/build.lock nix develop -c \
  bash tests/config-object-client/native-app/run-reboot.sh /tmp/cubit-config-reboot-results
```

Choose a new results directory. This runs two independent TCG boots on
disposable disks: the writer creates the type and commits 41 then 42; the second
boot starts Config with an empty in-memory catalog and a different app granted
only Read_Config for that namespace. It recovers the persisted declaration and
native value 42/revision 2 without supplying type metadata to the service.
Create, read-write Open and Set are denied; missing names and mismatched schema
keys return distinct outcomes; cached reopen succeeds. Both boots receive the
independent SQLite/WAL/e2fsck checks. The second boot must leave exactly the same
types and six committed revisions. The reader also restores the nested
collection after defining an unrelated type locally, shifting its local type
numbers. It verifies the full maximum-text value under the original stable
schema key and rejects writes through its read-only handle. Recovery restores
data, not historical grants or process-local type numbers.

The `config-objects-reopen` case can also run with `--accel kvm` and
`QEMU_CPU_MODEL=host`, supplying `--disk RESULTS/create/disk.img`. The runner
copies that seed image; it does not modify it. This is recovery after quiescent
VM termination, not a power-cut, service-supervision or hot-restart guarantee.

Validated 2026-09-25 with nested values: KVM writer + independent KVM reader,
and a separate TCG reader of the same seed, all with independent SQLite/WAL and
e2fsck checks. Logs `/tmp/cubit-nested-{create,reopen,reopen-tcg}.{serial,log}`;
seed `/tmp/cubit-nested-seed/disk.img`. The host golden-vector encoder has
explicit integer/length boundary tests; corruption fixtures independently
remove/alter nested declarations, history, payloads and head revision.
These are native IPC/recovery regressions, not source-level CCL aggregate
host-call support. See the [shared-language roadmap](../../../docs/ccl-unified-documents-roadmap.md).

## Public IPC performance

```sh
flock --exclusive --nonblock coordination/build.lock nix develop -c \
  bash tests/config-object-client/native-app/run-benchmark.sh /tmp/cubit-config-benchmark-results
```

This is a **native CuBit** app using the production Config/worker/filesystem
path, not the Linux-hosted modeled-IPC test. The benchmark has the same narrow
Config-only manifest. It creates one integer object and uses a separate
read-only handle. Four phases collect 64 samples each:

- Cached Get after eight warm-up reads.
- Set through the service-acknowledged Turso commit, with an untimed read-back
  after each write. Value construction is outside the timed interval.
- Get submitted while a Set is outstanding at the client, and that Set's
  completion latency. Each read must return one of the adjacent committed
  snapshots with its matching revision, never a mixed snapshot.

Timings include client validation/copy, IPC, service execution and result
retrieval; they do not subtract instrumentation cost. Logging happens only
after the measurements. Clients dispatch completion tokens correctly in either
order and park on kernel activity rather than polling a timer. The report
counts reads returned before the write reply; this is an observation, not a
guarantee that both requests reached the server before the commit completed.

The app checks all results; the host independently checks the exact declared
type and all 129 committed revisions (integer values 1 through 129), SQLite
integrity and ext2 metadata. No new durability claim: ext2 is not journaled,
and a quiescent file check is not a power-cut test. A separate reboot recovery
test above exercises cold Open; this timing workload measures warm handles.

The runner captures environment and source/binary hashes and performs three
unpinned KVM runs. Percentiles are exact nearest-rank values; with 64 samples
p99 equals the maximum. TSC calibration rejects >2% spread, but cross-vCPU TSC
agreement is an assumption. Neither a latency guarantee nor a Linux comparison.
`--test config-objects-benchmark` can also be run directly after building the
app; optional `--config-export NEW_DIRECTORY` saves its independently validated
disk/database/WAL for inspection. Never reuse that benchmark disk as the seed
for the two-revision `config-objects-reopen` fixture.
