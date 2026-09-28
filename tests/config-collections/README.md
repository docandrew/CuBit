# Typed Config catalog and collection handles

## Declaration-managed collections

The shared catalog distinguishes application state from declaration-managed
collections at trusted registration. The class cannot change through
re-registration. Ordinary writes to managed collections are denied at Open and
Resolve, even under a wildcard Config read/write grant; read-only access retains
the existing subject/scope/grant-lifetime checks. No activation bypass is added.

`managed_tests` adds 68 hosted checks for both classes, all requested rights,
classification conflicts, wildcard authority, recovery/read, denied writes with
no storage work, and revoke/regrant. Existing catalog168/publication97 checks
also pass. Focused SPARK authority/catalog/store:162 obligations,10 functional
contracts, none unproved or justified. The extended successful-Resolve contract
includes the management restriction. Existing unused-ID/specialized-branch
warnings remain. No Assume or SPARK-Off added.

Activation now has an explicit scoped operation, but ordinary collection handles
reject it for both management classes, even if the subject has that authority.
`Read_Write` and the existing grant-wire mask do not include activation. The
separate controller's hosted fixtures live in `tests/config-activation`.

IPC dispatch143/receiver5149/client926/startup20 checks pass in the Linux-hosted
native-code fixtures, including re-Create denial without retaining a deferred
reply. These are not native QEMU results. No managed collection is boot-registered
yet: trusted initial name reservation and activation remain future work. Format4
now persists class; service recovery restores it before issuing handles and
cold-cache client Create cannot downgrade it. Those persistence tests live in
the real-Turso cross-language suite, rather than this pure catalog fixture.
See [declarative configuration](../../docs/config-declarative-state.md).

## CPU-path measurement

```sh
nix develop -c bash -c 'cd kernel && \
  alr exec -- gprbuild -p -P ../tests/config-collections/benchmark.gpr && \
  ../tests/config-collections/build/benchmark/store_benchmark'
```

This is a **Linux-hosted, single-dispatcher CPU benchmark**, not live IPC or
Turso/disk timing. Five batches of 10,000 operations cover authorized cached
Get and a complete Set → pending request → synthesized valid worker reply →
acknowledged publication cycle. It uses the production store/protocol, checks
outcomes, consumes revisions in a checksum and checks cross-subject denial.
Two schemas cover an integer and a full 8 KiB string. `-O2`, normal language
checks, and no `-gnata` match the service's policy (the hosted runtime and ISA
flags still differ). No executable ghost snapshots inflate the timed path.

On a Ryzen 7 5800X, Linux 7.0.0-31-generic, Nix toolchain, dirty tree based on
`577c1cf`, median batch means on 2026-09-25 were:

| CPU path | Byte-loop padding checks | Array-equality padding checks |
| --- | ---: | ---: |
| Integer Set/pending/reply/complete | 21.204 µs | 11.437 µs |
| Full-text Set/pending/reply/complete | 14.216 µs | 10.932 µs |
| Integer cached Get | 0.555 µs | 0.561 µs |
| Full-text cached Get | 0.555 µs | 0.557 µs |

These are warm, unpinned, uncontrolled-host-load measurements, **not p99,
native latency, or end-to-end durable-write improvements**. Get is effectively
unchanged. Replacing byte-at-a-time text/padding scans with Ada array equality
lets GNAT use block comparisons; native code uses CuBit's existing `memcmp`,
while this hosted run uses the host runtime. Used cells and all unused cells,
text and padding still receive the same validation. Request snapshots,
authorization/revocation checks and durability ordering are unchanged.

The two immutable comparison targets cost 12,240 bytes of read-only data per
linked image, not per object or call. No schema-cache/lifetime redesign was
needed. Logs: `/tmp/cubit-config-store-benchmark-before.log` and
`/tmp/cubit-config-canonical-checks.log`. See the object suite for exhaustive
tail-offset/mutation regressions and the proved comparison-equivalence contract.

## Catalog and publication semantics

```sh
nix develop -c bash tests/config-collections/run.sh --prove
flock --exclusive --nonblock --conflict-exit-code 75 coordination/build.lock \
  nix develop -c bash -c \
  'cd kernel && alr exec -- gprbuild -p -P ../tests/config-collections/native.gpr'
```

`Config_Collections` is production-side, single-dispatcher service code used by
live Config's native typed-object path. A trusted registration path supplies a CCL
binding and collection name. Registration does not grant access, create a
database revision, or replace the collection's type. It is NOT the future
client `Config.create(type)` operation. A schema identity cannot acquire a
different meaning in this catalog, including under a different name. Bindings
are compared by `CCL.Objects.Same_Schema`: the approved key and complete nominal
root graph must match, but local IDs and unrelated declarations may differ.
The first binding is retained rather than replaced. A regression reproduced
the old whole-record equality bug even at catalog capacity. Schema migrations
remain separate work; structural similarity or key equality alone is insufficient.

Open checks existing `Config_Authority` scope grants before catalog lookup,
binds the expected schema, and mints a subject-bound, nondelegable handle with
explicit requested read/write rights. Only machine context zero is supported;
arbitrary profile names/IDs do not select another authority domain. Later
accesses resolve that handle to a stable internal collection ID. Applications
must never use those internal IDs as a substitute for handle authorization.

Every access checks BOTH its minted rights and the currently installed scope
grant revision. `Config_Authority.Install` assigns a new, nonwrapping revision
even for an identical replacement. Thus revoke/regrant or replace/regrant
cannot resurrect an old handle. Revocation does not implicitly cancel an
already accepted durable operation. Close permits owners to release handles
even after policy changes. Subject cleanup releases its entries; freed slots
never reuse a handle token during this service lifetime.

The shell must supply authenticated IPC subjects, preserve the catalog and
authority state for the lifetime of the service endpoint, and drop all handles
when rebinding after service restart. Raw PID reuse is NOT solved by a policy
revision if a launch path fails to replace that PID's grant set: authenticated
process-incarnation binding is still pending the kernel ABI. This does not add
signature verification or infer publisher ownership from reverse-domain names.

159 hosted checks cover admission, schema collision, no implicit grants,
owner isolation, read-only handles, namespace boundaries, denied-lookup privacy,
unsupported context, forged/closed tokens, revoke/regrant, policy replacement,
unaffected subjects, and collection/handle capacity. Native compilation also
passes; no new native IPC/boot result is claimed here.

147 focused SPARK checks pass with no assumptions or SPARK-Off sections. The
ghost `Authorized_For` model proves that successful resolution has the matching
subject, collection, minted operation, grant-set revision, and current scope.
`Open` proves that each issued token exceeds the previous lifetime high-water
mark; failure exposes no token. Existing authority install/revoke contracts
still prove. These are local implementation properties, not a proof of kernel
sender authentication, database durability, or concurrency safety.

`Config_Typed_Store` now joins authorized handles to typed publication and
worker request routing. Its `Get` and `Set` consume/produce native CCL images;
no client codec or SQL is involved. A single outstanding storage operation
provides bounded backpressure without blocking cached reads. Machine context
is selected internally, never copied from a client's payload. Worker responses
are snapshotted and checked against the retained request before publication.
The IPC shell must authenticate them and preserve deferred client reply
authority; a matching token alone is insufficient.

97 additional hosted checks cover denied access, pre-restore writes, stale
revisions, invalid native objects, cached reads while staging, unchanged values
before receipt, wrong/duplicate completions, revocation during an accepted
commit, worker loss and malformed replies. SPARK additionally proves `Set`
preserves EVERY collection's published image/revision, and denied `Get` returns
no value/revision. Ghost snapshots have no native codegen. Two proof warnings
report constant branches when the shared prepare helper is specialized for
load versus commit; all checks discharge, without suppressions.

The real hosted Turso fixture now exercises this core rather than manually
calling publication primitives. 64 checks pass, including owner denial and
response-acquisition failure after a real commit. Reopen recovers exactly the
two expected revisions, independently checked by SQLite. See
`tests/ccl-objects/run-durable-turso.sh`; IPC/grants are modeled, not live.

Next: native IPC dispatch/worker startup and schema provisioning, then expose
native-object create/get/set messages to applications and CCL.
Client creation must wait for durable publication; do not confuse in-memory
catalog registration with successful persistence. Distinct create/delete rights,
typed schema registration/persistence, per-subject resource budgets, additional
contexts and live worker startup remain explicit integration work.
