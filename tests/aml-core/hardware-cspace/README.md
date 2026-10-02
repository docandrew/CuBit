# Hardware cspace access boundary

Hardware_Grants.Cspace reads a slot from the caller's kernel-owned capability
table, checks CAP_HARDWARE_REGISTER, INITIAL_GENERATION, the registry
lifetime identity stored in the grant state, operation rights, grant liveness and the underlying grant
kind, then invokes grant-aware access reservation. Group handles cannot perform
register access even if incorrectly labeled as a register capability. Catalog
permissions, epochs and busy state remain enforced by the grant/catalog path.

The wire request contributes only a slot, read/write operation and value.
It does not supply the table, registry identity, grant handle, address,
offset, width or permission record. authorityTag does not authenticate access.
Kernel-owned object.ref denotes the never-reused grant handle; object.param
binds the registry lifetime identity. Ordinary capability attenuation may remove
rights on the same object, and this boundary enforces the resulting rights.

Integration requirements still outstanding:

- Call Hardware_Grants.Initialize under kernel serialization before admission.
  Keep each initialized registry with its owner; do not clone or reset live state.
- Install hardware capabilities only through trusted startup admission and
  checked delegation. Generic policy minting already denies both hardware types.
- Select the real current caller's table and serialize table, registry and
  catalog authorization/reservation. A copied user-supplied table is not valid.
- Wire the syscall, trusted completion, physical access and mapping lifetime.

This unit accepts trusted kernel state explicitly for verification. Its parent
allocates registry identities. Install_Group and Install_Child now install bound
capabilities into supplied kernel tables, but startup and syscall callers remain
absent. Tests exercise these helpers and separately construct malformed records
as a trusted rejection harness, not through a user API.
The hosted Config stub supplies the actual kernel's 64-slot table bound; tests
compile the actual kernel capability/grant/catalog source. Native compilation is recorded below; live hardware operation is not yet
established.

The regression tests exercise all 32 capability-right combinations for read and
write, wrong capability types/generations/registry IDs, invalid slots, wrong
underlying grant kind, write masks, malformed reads and ancestor revocation
while a transaction remains outstanding. Proofs include the parent grant and
catalog units, shared permission rules and capability definitions.

2026-10-01: 195 hosted checks passed. The combined cspace, grant, catalog,
permission and capability SPARK run passed 268 analysis checks, with zero
unproved or justified checks. Live's contract now exposes both the used-handle
bound and the absolute registry bound, and Can_Access guarantees the checked
slot/type/generation/identity/rights/liveness facts to callers. No assumptions,
waivers or new caller preconditions replace input rejection.

The persistent hardware_cspace.gpr and standard runner entries are now installed
under the shared build lock. The integrated target passes all 195 tests, runner
syntax checking and the combined 268-check proof. Run it independently with:

```sh
nix develop -c bash -c 'set -e; cd kernel; alr exec -- gprbuild -p -P ../tests/aml-core/hardware-cspace/hardware_cspace.gpr; ../tests/aml-core/build/hardware-cspace/hardware_cspace_tests; alr exec -- gnatprove -P ../tests/aml-core/hardware-cspace/hardware_cspace.gpr -u hardware_grants-cspace.adb hardware_grants.adb hardware_catalog.adb hardware_authority.ads capabilities.ads --mode=all --level=2 --prover=cvc5,z3 --checks-as-errors=on -j2'
```

Native validation compiled the cspace child, grants and catalog against the
actual kernel project/configuration/runtime in the private snapshot
/tmp/cubit-hardware-cspace-kernel-mrbb1b6a. This found that the grant specification
needed an explicit `pragma Ada_2022` for its array syntax; the kernel project
does not select that language version globally. The declaration is now present,
and native compilation plus the integrated tests/proof passed afterward.
All 367 snapshot inputs matched the checkout after compilation. This is compile
validation, not a link, boot or hardware test. Can_Access/Begin_Access frames
were 32/80 bytes under existing kernel flags; this does not bound whole call
chains or checked-contract builds. Logs: /tmp/cubit-hardware-cspace-native.log
and /tmp/cubit-hardware-cspace-integrated.log.

2026-10-01 identity binding supersedes the external Registry_ID parameter.
Hardware_Grants.Initialize assigns a nonzero lifetime ID from a kernel-owned,
monotonic allocator. Initialization fails for an already initialized registry
or exhausted counter, without changing either registry or allocator. Admission
rejects an uninitialized registry. The cspace gate reads Identity(S) directly;
there is no separate expected-ID argument that kernel glue could misassociate.
The allocator and registries require external kernel serialization; this is not
an atomic multi-core allocator. No user API can reset or write their identities.

Updated validation: 200 cspace tests, 182 grant tests, and 284 combined SPARK
analysis checks passed, with no unproved or justified checks. New tests cover
repeat initialization and matching numeric handles in distinct registries.
The allocator contract proves one-step advancement and no wrap on success.
Native compilation of child/grants/catalog also passed using the actual kernel
project/runtime at /tmp/cubit-hardware-identities-native-0xp69cln. Of 367
recorded inputs, only generated build.ads date/hash changed in the checkout;
all other inputs matched. This is compile evidence, not boot/hardware execution.
Logs: /tmp/cubit-hardware-identities.log and
/tmp/cubit-hardware-identities-native.log.

2026-10-01 capability installation: Install_Group is a trusted startup-policy
entry, never a user operation. Install_Child validates source slot/type, registry
identity, generation, grant right, actual grant kind and rights subset. Requested
read/write/delegation flags must match the desired capability rights; record
permission narrowing and catalog membership are then enforced by Derive. Both
installers reject occupied, invalid or reserved reply slots and executable rights.
They install the exact requested rights with INITIAL_GENERATION and no authority
tag. Parent linkage is retained in each new register record.

Failure leaves both registry and destination table unchanged; success changes
only the selected table slot. Source_Table must be a stable kernel snapshot under
the same serialization boundary; use a separate snapshot for same-table delegation
instead of aliased in/out actual arguments. The kernel must also authorize access
to the destination table when exposing delegation through a future syscall.

Latest validation: 353 hosted checks (including exhaustion without partial
insertion) and 332 combined SPARK analysis checks, zero unproved or justified.
Actual-kernel compilation passed in /tmp/cubit-hardware-install-native-8nd4q_rz,
with all 367 recorded inputs unchanged afterward. No boot or hardware execution
is claimed. Logs: /tmp/cubit-hardware-install.log,
/tmp/cubit-hardware-install-tests.log, /tmp/cubit-hardware-install-native.log.

2026-10-01 capability-authorized revocation: Can_Revoke/Revoke validate the
source slot, hardware type, INITIAL_GENERATION, registry identity, RIGHT_REVOKE,
liveness and actual grant kind. Revocation disables the referenced grant and
all descendants, including copies of that object. It does not clear table slots
or revoke unrelated grants. Repeated revocation is rejected without mutation.
The wrapper has no catalog or mapping-release argument: outstanding reservations
remain retained until trusted completion. Syscall selection of the caller's
actual table and serialization are still required; the existing shared-memory
revocation syscalls do not provide this hardware operation.

Updated validation passed 772 hosted checks and 362 combined SPARK analysis
checks, with zero unproved/justified checks. Tests cover all 32 rights combinations,
malformed/retyped/stale references, group and register revocation, unrelated
grant preservation, descendant denial, replay, and an in-flight operation across
revocation. Native actual-kernel compilation passed in
/tmp/cubit-hardware-revoke-native-ofolo3kk. Of 367 recorded inputs, only generated
kernel/src/build.ads changed in the checkout afterward. This does not establish
boot, syscalls or live hardware execution. Logs: /tmp/cubit-hardware-revoke.log
and /tmp/cubit-hardware-revoke-native.log.
