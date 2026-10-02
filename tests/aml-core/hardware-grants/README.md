# Kernel hardware grant registry

`kernel/src/hardware_grants` stores group and register authority in kernel-owned
records. Derivation selects a catalog member and checks permission narrowing;
register children additionally need their parent's delegation permission.
Resolution takes the stored epoch and permission, never a caller-supplied scope.
Group handles cannot directly perform register accesses.

Each derived record retains its complete ancestor set. Revocation disables one
record; resolution checks both that record and every ancestor. The SPARK
contracts establish descendant invalidation and preservation of unrelated
liveness, and derivation preserves inherited ancestry. Catalog epochs separately
reject replacement inventories even when their resource IDs are reused.

This is bounded groundwork, not a deployed capability subsystem: 128 records,
no recycling or reset API, exhaustion denies new grants. The kernel must keep
the registry instance alive as long as any referring capability exists. A future
recycling scheme needs incarnation counters and safe ancestry retention.

Handles are not secrets or authentication. Eventual syscall glue must obtain the
handle from a checked cspace entry, intersect its read/write/grant rights, and
serialize registry/catalog operations. Boot-only Admit_Group must stay outside
the syscall API. No singleton, cspace binding, startup admission, access syscall,
physical mapping, or hardware callback is installed here. Resolve only returns
internal metadata; it does not reserve backing. The grant-aware Begin_Access wrapper reserves access using the stored epoch
and permission under the same serialization boundary. Its contract requires
an allowed reservation to equal grant resolution before the catalog mutation;
a denied request leaves the catalog unchanged and returns no ticket. Revocation must not release an in-flight mapping.

2026-10-01 validation: 156 hosted grant checks passed, plus 126 catalog regression
checks. The combined grant/catalog/shared permission SPARK run passed 215 analysis
checks, with no unproved or justified checks. Grant tests cover cross-group
rejection, masks, nondelegable children, deep ancestry, sibling preservation,
invalid handles, capacity exhaustion and stale epochs after inventory replacement.
The descriptor validity predicate moved unchanged from an expression in the spec
to a function body, allowing callers to reuse a stable predicate in modular proofs.
No assumptions or waivers were added. Live kernel behavior has not yet been checked.

The persistent grants.gpr and standard-runner integration are now installed
under the shared build lock. The integrated target passed all 156 checks;
run.sh passes bash syntax checking. `tests/aml-core/run.sh --prove` includes this
proof target. To run only these tests and proofs:

```sh
nix develop -c bash -c 'set -e; cd kernel; alr exec -- gprbuild -p -P ../tests/aml-core/hardware-grants/grants.gpr; ../tests/aml-core/build/hardware-grants/hardware_grants_tests; alr exec -- gnatprove -P ../tests/aml-core/hardware-grants/grants.gpr -u hardware_grants.adb hardware_catalog.adb hardware_authority.ads --mode=all --level=2 --prover=cvc5,z3 --checks-as-errors=on -j2'
```

Proof validation initially used the equivalent /tmp/cubit-hardware-grants.gpr;
logs are /tmp/cubit-hardware-grants.log and
/tmp/cubit-hardware-grants-integrated.log.

2026-10-01 reservation validation: 180 hosted tests passed, including group and
invalid-handle denial, narrowed read/write authority, busy rejection, ancestor
revocation during an outstanding operation, retained backing, wrong/duplicate
completion tickets and a still-live grant rejected after catalog replacement.
Combined SPARK analysis passed 231 checks, zero unproved or justified. An exact
Refined_Post on Resolve exposes its relationship to catalog resolution inside
the implementation without exporting stored scope to callers.

Private checked native static-library compilation passed against the actual
kernel runtime in /tmp/cubit-hardware-grants-native-h390p3p5. All 87 recorded
inputs still matched the checkout afterward. This does not establish boot,
syscall dispatch, physical hardware access, or a whole-call-chain stack bound.
Logs: /tmp/cubit-hardware-grants-reserved.log,
/tmp/cubit-hardware-grants-final-test.log, /tmp/cubit-hardware-grants-native.log.

Registries now require Initialize before admission. It binds a nonzero lifetime
identity from a package-owned nonwrapping counter, rejects repeat initialization,
and has no reset/reuse API. Callers must serialize initialization and preserve
live registry ownership. Identity is stored privately and checked directly by
the cspace child. Latest combined validation is 182 grant tests, 200 cspace tests
and 284 SPARK analysis checks with no unproved/justified checks; see the cspace
README for actual-kernel native compile evidence and integration limits.
