# Intel probe foundation (Linux-hosted)

## Bounded presentation retirement

`Buffer_Requests.Sharing.Poll` visits at most 16 mapping entries per call,
rotating across the table. Observers remain conservative until each grant is
confirmed retired. This bounds entry visits, not kernel-call latency; other
mapping-table scans are not made constant-time by this change.

Hosted tests exercise 33 pending readers, 52 stalled readers preceding a
completed reader, wraparound, and writer/presentation exclusion through delayed
retirement. The grant fixture uses distinct generations and exact completion
identities. A growth interleaving fills the inline table with pending grants,
leaves the poll cursor mid-pass, extends twice at a stable CPU address, and
checks both inline and extension grant identities survive. The last extension
reader drains despite older stalled readers; all drain after confirmation.
This is hosted retained-storage coverage, not physical GPU memory growth.
Run in Nix from `kernel`:

```sh
alr exec -- gprbuild -p -P../tests/intel-gpu/presentation_lifecycle.gpr
../tests/intel-gpu/build-presentation-lifecycle/poll_budget_tests
../tests/intel-gpu/build-presentation-lifecycle/presentation_exclusion_tests
```

The old full-table polling implementation fails the first budget assertion.
The exclusion suite also injects ownership loss, session closure and session
replacement during recipient lookup: each must deny before creating a grant.
Production recipient lookup is currently local; these tests harden the generic
boundary rather than demonstrate an existing dispatcher race.
`view_retention.gpr` additionally covers all six drain orders for three readers.
Native `run-demand.sh mappings` (under the shared lock and Nix) requires the
bounded-poll completion marker and tests real self-grants across metadata
growth, including a pending extension grant during a second expansion. It uses
the existing built kernel, recorded in `input.sha256`, and is
not Intel GPU rendering, cross-process isolation, or a new SPARK proof.

Common BO lookup, close and backing-release paths use one matched record
instead of repeated table searches. Identity/session/closed-state and retained
pin checks remain mandatory; handles are not indices. This reduces redundant
linear work but does not provide indexed lookup or a hardware speed estimate.

## Context preflight without a full image temporary

Application-image preflight now calls `Submission_Image.Valid_For_VM` instead
of constructing and discarding a full submission image. Page validity,
duplicates, root overlap and GGTT bounds use the same validation path as the
builder. Context/workaround `Admissible` predicates are shared with their
builders; no hardware command words or publication sequence changed.

The primary-tree submission-image, context-image and submission-buffer tests
pass, including bytewise image expectations, all ring sizes at overlapping and
adjacent positions, scattered backing and retirement failure cases. A fresh
three-unit SPARK run proves 22 checks, with zero unproved or justified checks
and no warnings/Assume pragmas in its report. Its scope is Initial, Workaround
and Context image builders, including validity-equivalence contracts; it does
not prove the whole driver, submission-page validator or hardware behavior.

```sh
flock --exclusive --nonblock coordination/build.lock nix develop -c bash -c 'cd kernel && alr exec -- gnatprove -P ../tests/intel-gpu/sparse_vm.gpr --subdirs=preflight-admission-20261004 -u intel_gpu_adln_lrc_initial.adb intel_gpu_adln_lrc_workaround.adb intel_gpu_adln_context_image.adb --level=1 --report=all --checks-as-errors=on -j2'
```

Evidence: `build-sparse-vm/preflight-admission-20261004/gnatprove/gnatprove.out`.
The private native driver also compiles/links. Its compiler reports the local
`Prepare_Image` frame reduced from 271232 to 624 bytes; the two admission
functions report 8 bytes each. These are individual frames, not a whole-stack
bound or a measured startup/FPS improvement. Actual materialization still
uses a large temporary. This change is not in the preserved v60 NUC image.

## Native metadata and retained-reader gates (2026-10-04)

These two modes run privileged disposable CuBit fixtures, not Linux-hosted
simulations or Intel hardware rendering. Run each under the shared build lock:

```sh
flock --exclusive --nonblock coordination/build.lock nix develop -c bash tests/intel-gpu/native/run-demand.sh metadata
flock --exclusive --nonblock coordination/build.lock nix develop -c bash tests/intel-gpu/native/run-demand.sh views
```

`metadata` uses real owned-memory reservations and committed prefixes. Sparse
records 17, 18 and 900 retain independent metadata; growth from 6 to 64 table
records preserves an existing pointer and sentinel. It checks one commit per
step, shared accounting, overlap rejection and sticky failure after the test's
owner callback becomes false. That callback is not kernel authority revocation.
Placement initialization of a fresh typed record intentionally triggers GNAT's
overlaid-storage initialization warning; this is not a warning-free build.

`views` now requires the retained-reader marker in addition to the existing
grant lifecycle markers. A producer BO name closes and its original pin is
returned while a separate read-only, nonforwardable reader remains alive.
Backing cannot be released until the real self-grant acquisition drains.
These self-grants do not establish interprocess isolation or GPU completion.

Both primary-tree runs passed on 2026-10-04: evidence directories
`demand-backing.j5ncsd` (metadata) and `demand-backing.7jwdye` (views), each
containing `serial.log` and `input.sha256`. They use the existing built kernel
`c3ccc9c442b4eef1bc8cdc6f31c4d92a493a2a6e7489d89c8c6ebe40c4664a01`;
the harness records it rather than rebuilding or certifying current kernel
sources. Production staging and the NUC image are not changed.

## Demand-grown replacement metadata

Replacement images now retain independent CPU-metadata arenas for the image
record, provenance ledger, table references, descriptors and table mirrors.
`Intel_GPU_Retained_Store` keeps each image's limited arena state at a stable
address behind a growable reference index. Revisiting an image does not move
or recreate its state. The native replacement request passes the topology's
required table count; it no longer eagerly backs all 64 mirror slots.

Virtual reservations and committed bytes are separate. Registry growth and
all per-image regions charge the same owner budget before requesting backing.
Growth starts at one page and increases in bounded increments. Failed or
revoked operations retain their charges and backing; these stores do not
authorize memory reclamation, GPU table publication or TLB retirement.
Existing native quotas, context limits and the 64-table VM policy still apply.
This is not an increase in device-local VRAM support or a replacement for the
physical extent allocator.

`Intel_GPU_Metadata_Ranges` indexes retained CPU image spans with embedded AVL
nodes, without a separate fixed-capacity node array. Registration checks
allocated address ranges rather than scanning every sparse allocation ID.
Fresh-image checking and publication are separate steps, guarded by a registry
epoch so intervening registration/index changes cannot invalidate the check.
Node storage survives candidate retirement along with the metadata record;
it must not be cleared or reused as part of GPU-backing retirement.

The hosted range test inserts 8,192 ranges in ascending, descending and
permuted orders, independently checks tree structure, and compares overlap
queries with a linear oracle. Application-state tests cover sparse IDs,
delayed publication, aliases, intervening changes and no replay. Run the
additional owner-loss matrix with:

```sh
nix develop -c bash tests/intel-gpu/test-update-mirror-failures.sh
```

Each injected failure runs in a fresh process. The runner derives every yield
boundary and commit number from a successful baseline, rather than assuming
the former allocator's step counts. These are hosted regression tests, not
SPARK proofs, native syscall tests or Intel hardware validation.

## Growable VM directory and descriptor metadata

`table_reference_store_tests.adb` exercises the candidate ordinal-to-provenance-ID
store now used by native context/update records: four inline entries,
stable growth to4096, rejected out-of-capacity/stale-generation access, and an
exact-next-generation logical reset that leaves retained metadata bytes intact.
The caller must independently confirm hardware/ledger retirement before reset;
the store is not retirement authority.
Initial contexts prepare reference capacity before ledger-ID installation;
replacement storage allocates a distinct reference region and checks readiness.
Native compilation and storage fixtures pass; extracted lifecycle fixtures are
being migrated from their former array model before the next image gate.

Directory receipts and table descriptors use retained CPU record storage rather
than quota-sized inline arrays. Native contexts bootstrap with two directory
links and four descriptors. Descriptor allocation precedes mirror expansion;
replacement images have disjoint ledger, descriptor and mirror regions. These
are CPU metadata allocations, not GPU backing or permission to publish a PTE.
The VM separately tracks allocated backing, used tables and metadata capacity.

`test-directory-metadata-admission.py` and `test-directory-metadata-resume.py`
extract the native deferred path and cover saved replies, owner/epoch loss,
bounded stepping, undersized publication and no replay, with negative controls.
`test-native-directory-admission.py` additionally checks real topology planning
and that insufficient metadata defers before table allocation or context hold.

The explicit Ada mains below use real hosted CPU storage and production library
code. They cover expansion, guard regions, failure retention, publication/rearm,
descriptor/mirror ordering, snapshots and retirement. They do not prove native
syscall behavior, Intel cache visibility, GPU invalidation or SPARK properties.

`vm_sparse_scale_tests.adb` uses quota4096 with four inline descriptors/mirrors,
then grows to 128 backed/87 used tables across low/high raw48 addresses and
forty GiB-spaced mappings. It checks stable mappings, bounded metadata growth
and suffix guards; the native table policy remains64. `vm_initial_stream_tests`
checks allocated-prefix initialization without a quota-sized input array and
one-attempt failure behavior. `test-native-mapping-capture.py` extracts native
capture admission and checks streamed table identity and post-callback ownership
with negative controls. The bootstrap extraction checks retained VM inventory,
not an implementation-local temporary array.

`vm_replacement_stream_tests.adb` checks the corresponding callback-fed
replacement builder: one read per page, source-table/data alias rejection,
rebased successor identity, unchanged source and retained failure without replay.
Array-based replacement callers delegate to this same core. Native replacement
requests now pass a page count and authenticated page reader through Binding,
without building a quota-sized temporary address array. The pipeline fixture
runs both array and direct-callback modes, including sparse backing and failed
invalidation. The native registration fixture extracts the page reader as well
as stepped registration, including ownership loss before publication.

```sh
nix develop -c bash -c '
  test_dir=$(mktemp -d /tmp/cubit-vm-metadata.XXXXXX)
  gprbuild -p -P tests/intel-gpu/vm_growth.gpr -XVM_GROWTH_OBJECT_DIR="$test_dir" \
    growth_boundary_tests.adb directory_metadata_growth_tests.adb \
    vm_growth_storage_tests.adb backed_inventory_tests.adb \
    descriptor_storage_tests.adb vm_descriptor_writer_tests.adb \
    context_descriptor_growth_tests.adb update_storage_tests.adb \
    vm_sparse_scale_tests.adb vm_initial_stream_tests.adb vm_replacement_stream_tests.adb
  for test in growth_boundary_tests directory_metadata_growth_tests \
    vm_growth_storage_tests backed_inventory_tests descriptor_storage_tests \
    vm_descriptor_writer_tests context_descriptor_growth_tests update_storage_tests \
    vm_sparse_scale_tests vm_initial_stream_tests vm_replacement_stream_tests
  do "$test_dir/$test" || exit; done
'
```

## Stepped VM update coordinator

`Intel_GPU_VM_Update.Begin_Update` closes submission before owner callbacks.
The generic `Advance` invokes at most one transaction-stage callback per call;
publication may report unfinished work and yield with admission closed. Every
advance rechecks ownership and terminal retirement before and after callbacks.
Nested advancement rejects without invoking a second callback. Only successful
invalidation and resume advance the public epoch. `Execute` now uses this same
state machine with a synchronous publication adapter, retaining existing callers.

`vm_update_async_tests.adb` covers seven success/failure scenarios with twelve
publication steps, cross-turn ownership loss, callback retirement, nested attempts
and no epoch advance on failure. `vm_growth_bind_tests.adb` composes the actual
directory writer and leaf insertion in both synchronous and stepped modes, with
full and partial bootstrap backing (20 scenarios), retaining the hardware-root identity, two visibility gates and one
public generation. These are hosted models, not hardware timing or invalidation
proofs. Native bind/unbind uses authenticated `Begin_In_Place`, one coordinator
`Advance` per service-loop turn, and `Finish_In_Place` before replying. It
retains the context hold and reply capability, blocks new service requests and
reclamation, and avoids the idle wait while work is pending. Replacement-image
updates still use synchronous `Execute`; incremental directory publication remains
unfinished. Bind and unbind publication now use insertion/removal `Start`/`Step`:
Start validates the range without hardware writes; each Step invokes at most one
compare/write callback. Insertion retains encoded replacement words; removal
uses the same immutable sealed source validated at Start. Publication completion
still requires later invalidation and metadata commit. Hardware invalidation
remains synchronous within its stage callback.

`vm_insertion_tests` adds ten stepped cases covering cross-turn owner/source
loss, retained words after caller-array mutation, callback reentry, premature
commit, failed writes and no replay. Existing synchronous and 4096-cycle reuse
coverage remains. Start validation and Commit metadata work scale with the
range; this is bounded hardware-write work per turn, not constant-time binds,
a measured speedup, or enabled native incremental directory allocation.

`vm_removal_steps_tests.adb` (explicit main through `vm_growth.gpr`) covers twenty
scratch/fault-fallback scenarios: one leaf per turn, full-range validation before
writes, changed caller arrays, owner/source loss, recursive Step/Commit, partial
write failure and unsuccessful invalidation. No metadata clears before commit;
failed attempts cannot restart or use a later invalidation to reclaim backing.
Existing removal/coordinator and application-image tests remain required. These
hosted checks do not establish Intel hardware visibility or TLB completion.

Insertion receipts now use stable `Record_Store` storage for exact encoded leaf
words. `Bootstrap_Insertion_Words` is independent of VM quota; the generic default
preserves full sizing, but the native service now uses 512 inline words and grows
CPU metadata before holding the context. `Extend_Insertion_Metadata` is allowed only before an attempt or between
successful attempts, never while publishing/pending or after a poisoned attempt.
Insufficient committed capacity rejects before any GPU callback. The owner must
authenticate and retain committed CPU metadata and enforce its byte budget.
`vm_insertion_storage_tests.adb` starts with two words, grows into committed host
RAM, checks captured values despite caller mutation, active/failed exclusion and
unchanged uncommitted suffix. This is hosted coverage, not GPU visibility proof.

```sh
nix develop -c bash -c '
  insertion_dir=$(mktemp -d /tmp/cubit-insertion-storage.XXXXXX)
  gprbuild -p -P tests/intel-gpu/vm_growth.gpr \
    -XVM_GROWTH_OBJECT_DIR="$insertion_dir" vm_insertion_storage_tests.adb vm_insertion_tests.adb
  "$insertion_dir/vm_insertion_storage_tests"
  "$insertion_dir/vm_insertion_tests"
'
```

`insertion_metadata_growth_tests.adb` composes the real `Record_Growth`, metadata
arena and insertion word store. Starting with two inline words, a request within
capacity reserves nothing; larger requests commit/publish 64KiB steps in a single
stable reservation. It checks a second extension preserves the old prefix,
quota rejection leaves the controller idle, and failed commit retains prior
capacity without retry or publication. Reserve/commit callbacks use hosted RAM;
this does not exercise native syscalls. The native handler captures a saved reply
and source epoch, blocks competing service mutations while committing metadata,
then revalidates admission with explicit saved-reply mode before taking the
context hold. `test-insertion-metadata-resume.py` extracts that continuation and
tests owner/epoch/hold/failure rejection, multi-turn progress and no duplicate
reply. Admission and IPC endpoints are modeled, not a native runtime test.
`test-insertion-metadata-admission.py` preserves the extracted native admission
and reply-routing regression: configure/save/request failures, recovery after a
failed save, already-saved rejection and a wrong-reply negative control. Each run
uses isolated temporary build outputs. Saving reply authority must precede the
transition into pending growth; rejection after a successful save must use that
capability rather than replying directly to the original sender.

```sh
nix develop -c python3 tests/intel-gpu/test-insertion-metadata-admission.py
```
Build it as an explicit main through `vm_growth.gpr`, using a unique
`VM_GROWTH_OBJECT_DIR`, then run `insertion_metadata_growth_tests`.

The successful `vm_growth_bind_tests` cases now run a second growth/bind on the
same VM and controllers, after both directory and leaf epochs have advanced.
The first operation adds three directory pages; the second needs only one PT.
Both preserve the separate retained hardware root and previous mappings, perform
fresh directory/leaf visibility gates, and advance the public generation once.
All four full/partial-bootstrap and synchronous/stepped combinations pass. Leaf
publication in this composition now uses the production stepped insertion API.
This validates receipt rearm across successive binds, not native allocation
dispatch, hardware cache behavior, or growth beyond the configured image capacity.

`vm_insertion_binding_tests.adb` runs ten synchronous/stepped scenarios, including
rejected starts, no early success reply, premature finish and session revocation
after commit but before reply. Existing removal/image and VM transaction tests
remain regression gates. Native compilation verifies this integration links;
the Intel event-loop path still needs physical hardware regression testing.

## Independent table-allocation backing roles

Private allocation tickets now retain a `Private_Table_Kind`: replacement image
or incremental tables. `Reserve_Private` admits incremental purpose only for a
reclaimable, session-owned allocation. `Is_Table_Allocation` checks the full
ticket, session, role, owner readiness and private lifetime; it rejects application
BOs, pinned parents and already acknowledged reusable slots. This is retained
allocation identity, not proof that backing exists or that hardware is quiescent.
Native replacement admission now requires this role explicitly. Incremental
resolution authenticates the private ticket/session/canonical slot without
requiring an `Update_Record`; the backing registry must still contain the matching
incremental role. New incremental tickets are not yet requested by the native
VM-growth path: multi-allocation retirement and publication integration remain.

`table_ticket_tests.adb` (explicit main through `buffer_requests.gpr`) exercises
256 generations with alternating roles and sessions, stale/wrong-owner rejection,
no application completion, pending-allocation exclusion, and live/closed-session
retirement. The existing `buffer_requests_tests.adb` remains a regression gate.

`table_role_resolution_tests.adb` (explicit main through `vm_growth.gpr`) composes
the actual ticket service, allocation registry and provenance backing resolver
for 128 cross-session/role generations without any replacement image storage.
Ticket reservation alone cannot resolve memory; registry installation is required.
Revocation, missing retirement acknowledgment, owner loss and stale generations
are checked. Supervisor receipts are modeled; no physical allocation or MMIO is
performed by this hosted test.

`Intel_GPU_Table_Allocations` separates replacement-image backing from incremental
table-only backing, using a growable metadata registry. Install and lookup require
an authenticated session/ticket/canonical-slot callback. Revocation blocks new
resolution without discarding retained backing; only exact retirement confirmation
permits slot reuse. Borrowed extent directories must outlive their registry entries.
This component does not allocate physical memory, publish PTEs, or establish that
two independently admitted allocations do not alias. Those remain transport-owner
responsibilities. Native main now routes replacement-table resolution through
this registry, grows its capacity in the metadata bundle, revokes superseded
replacements, and retires entries only after the table ledger has completed
retirement and before acknowledging reusable ticket slots. Initial combined
context/table parents retain their separate slice path. Incremental allocation
admission is not enabled yet; the new role alone does not authorize such tickets.

`table_allocations_tests.adb` checks 100 slots across metadata growth, preserved
lookups, invalid backing, loss of authority, wrong role/session/slot/ticket,
revocation, failed retirement, exact retirement and stale-ticket rejection after
reuse. It also connects the production provenance backing resolver and checks
page addresses before revocation and rejection after revocation/slot reuse.
Run through `vm_growth.gpr` as an explicit main with a private object dir:

```sh
nix develop -c bash -c 'cd kernel && alr exec -- gprbuild -p -P ../tests/intel-gpu/vm_growth.gpr -XVM_GROWTH_OBJECT_DIR=/tmp/cubit-table-allocation-roles table_allocations_tests.adb && /tmp/cubit-table-allocation-roles/table_allocations_tests'
```

This is Linux-hosted regression coverage, not a SPARK proof or Intel hardware test.

Retained allocation identities are enumerable without exposing backing addresses
or restoring revoked authority. A revision-bound `Scan_Session` visits at most
64 entries; a negative result is valid only through an unchanged registry revision.
Install, first revocation and retirement advance that revision without wrapping.
The native combined-parent retirement path now performs this census over event-loop
turns before releasing the parent, independently of replacement-image records.
Tests include a retained incremental allocation beyond the first scan chunk,
revocation/owner loss, and insertion behind an already-scanned prefix. Other
preexisting VM alias scans are unchanged; this is not a claim that all native
retirement work is already bounded.

## Asynchronous table retirement dispatcher

The growth topology planner traverses directories once per intersected leaf-table
span, instead of repeating the walk for every 4 KiB page. It still checks each
existing leaf for occupancy; absent subtrees need only prefix counting. This is
a CPU planning change, not a 2 MiB GPU-page mapping change. `vm_growth_tests`
checks 49 boundary ranges against an independent interval oracle and offline
mapper, plus all 512 possible occupied leaf positions, empty subranges and a
range entering from a missing previous table. No timing speedup is claimed from
these functional tests.

Native replacement registration now advances one authenticated page per service
turn in `Finish_VM_Update`, using the retained ledger count as its cursor. The
pending request, work exclusion and reply authority remain held until all pages
are registered and revalidated for publication. This does not yet enable native
incremental directory growth or change the replacement root-publication policy.
`test-replacement-registration.py` compiles that exact adapter block with real
registry/provenance code. It covers 2,208 size/failure combinations for 1–64 pages,
including ownership loss before final publication preflight. A negative control
removing the yield must fail. Backing ownership is modeled; no GPU executes.

```sh
nix develop -c python3 tests/intel-gpu/test-replacement-registration.py
```

`table_growth_lifecycle_tests.adb`, an explicit `vm_growth.gpr` main, composes
allocation append, authenticated volatile-RAM IO, directory publication, stepped
leaf binding and parent-last retirement. Directory adoption and leaf adoption
require separate modeled TLB confirmations. Ledger IDs 65–67 become image positions 5–7;
the retained root differs from the image root. Eleven cases cover success, missing
TLB confirmation, ownership loss after a store, incomplete append rejected before
publication, stale IO generation, failed child/parent receipts, failed child
finalization, delayed receipts without resubmission, a failed leaf flush after
the actual store, and failed leaf TLB confirmation. Both leaf failures preserve
the old logical mapping and all ownership records despite the changed RAM word;
they cannot commit or replay the failed attempt. Failed retirement cannot
reopen the ledger or release an unacknowledged parent; an already acknowledged
child stays swept. Only modeled consumer retirement permits
release, child allocation before parent; stale-generation IO rejects after reopen.
RAM accesses are real, but GPU exclusion, flush completion, TLB confirmation and
allocator receipts are modeled. This is integration regression evidence, not
hardware validation or a claim that native incremental growth is enabled.

Native initial-parent cleanup now uses the stepped group dispatcher with the
context allocation explicitly last. Its bounded census admits additional closed
incremental allocations only when every retained session allocation is accounted
for in the context's ownership ledger. Per-ticket preparation
checks other contexts and every candidate image, including a candidate at the
same numerical slot as the context allocation. Completion consumes the exact
supervisor receipt and completed dispatcher ledger before acknowledging the
context ticket, advancing generation and clearing cached image-to-ID references.
`test-context-retirement-completion.py` extracts both native completion and its
ledger helper, group start and context-group exclusion gate, using the real
provenance and dispatcher packages with 64 or 65 retained records.
Nineteen paths cover child-before-parent ordering, stale ledger generation,
missing receipt, identity rejection and quarantine. Additional gate checks reject
changed root, changed source epoch and an outstanding replacement ticket.
Successful cleanup checks
empty records, updated generation, cleared IDs and rejected stale lookup. Omitting
the recycle call or the parent-last selector are failing negative controls. Allocator and hardware-boundary
callbacks are modeled, not physical GPU validation.

`test-context-table-membership.py` extracts the native membership census and uses
real registry/provenance storage. Each call observes one registry slot and at most
64 ledger references. Captured registry revision/capacity and ledger generation/
count prevent stale partial scans from certifying a group. Fourteen cases cover
closed-ticket identity, missing/foreign membership, revoked retained entries,
cross-64-record searches and mutation restarts. Membership and generation bypass
negative controls must fail. The census itself grants no release authority;
each dispatcher ticket still needs alias checks, exclusion and an exact receipt.

`vm_growth_writer_tests.adb` also interrupts a directory-publication callback
with premature `Commit`, both at the first child store and final parent readback.
The sticky consumed receipt prevents the outer step from restoring a pending
phase or reporting publication success. No later IO or metadata adoption is
allowed. These two cases failed before the guard and pass alongside the original
28 writer cases; they model callback interruption, not a hardware fault.

Table ownership registration also has a stepped path:
`Authority.Begin_Append` captures a ledger's address, session, generation and
current count for one exact allocation ticket. Each `Step` resolves and registers
at most one page. Partial failure retains registered references and cannot replay;
the first ID becomes available only when the group is complete. These are ledger
registrations, not GPU publications. `Authority.Rearm` reuses only a successfully
completed controller on the exact same open ledger, generation, owner and record
count. It performs no resolution, backing release or ID reuse. Capture the prior
First_ID before rearming. Partial/failed operations stay terminal; changed counts,
retirement and stale generations reject reuse. The append test repeats ten further
allocations and checks preservation of earlier IDs and all rejection cases.

These are ledger IDs, not image ordinals or a GPU-publication receipt. Native bootstrap now uses
this path. `table_append_tests.adb` (explicit main via `vm_growth.gpr`) covers six
success/failure cases, including IDs 65–67 after an existing 64-page allocation,
metadata extension across yields and rejection of a different ledger, interleaved
mutation, revocation, invalid DMA and offset overflow. Hosted, not hardware proof.

`Start` accepts an optional exact `Last_Ticket` for a combined context/ring/scratch
parent. Bounded censuses defer that ticket until every other ledger allocation
has been acknowledged and swept. The parent still requires its own `May_Release`
and exact `Release_Confirmed`; ordering does not prove other consumers are gone.
No skipped parent references are cleared or counted as completed. Reopen still
requires the whole ledger to be complete. Native replacement-group recycling
requests its anchor ticket last, preserving the exact final `Buffer_Memory`
receipt for its finalizer. Incremental children require their own retained
registry identity/role, allocation authorization and alias preflight.
The real allocator/dispatcher fixture in `buffer_memory_tests` exercises seven
two-allocation cases: child-first/anchor-last success, stale completions, failed
send or wrong-generation acknowledgment on either allocation, and failed child
finalization. A completed child cannot authorize release of the retained parent.
On success the final receipt is for the anchor, not the earlier child. Supervisor
messages and hardware exclusion are modeled, not executed on a GPU.

`table_parent_retirement_tests.adb`, an explicit `vm_growth.gpr` main, tests 150
interleaved references across three allocations in five success/failure scenarios.
It asserts child-before-parent dispatch, independent parent-consumer exclusion,
partial failure retention and at most one transport callback per step. Receipts
and consumer readiness are modeled, not hardware-derived.

The asynchronous `Table_Provenance.Retirement.Dispatcher` component is covered by
`table_dispatcher_tests.adb` (explicit main through `vm_growth.gpr`). It drives
the production ledger over 150 records/three tickets, delayed exact acknowledgments,
and preparation/submit/poll/finalization/ownership failures at each ticket (42 scenarios).
Each step invokes at most one preparation/transport/finalization callback or one bounded
ledger sweep. Finalization requires the exact receipt still present and all
references to that ticket swept; it is not another physical release. A wrong ledger is rejected even
with the same session/generation; acknowledged groups clear while unacknowledged
references remain. Failure cannot replay or reopen. Backend polling must provide
its own deadline and classify uncertain expiry as failure. This is hosted
regression evidence, not SPARK proof. Native replacement-table cleanup now uses
this dispatcher to submit retirement once, poll the exact supervisor acknowledgment,
and sweep metadata across service-loop turns. The native adapter rechecks ticket,
session, snapshot identity and scheduling exclusion across yields; a pending
request without a receipt remains pending, not a second submission. Its finalization callback retires
the backing-registry entry exactly once. Children are marked reusable only after
their own receipt and sweep, while the group admission gate prevents new requests
until completion; anchor reuse remains after the ledger's generation-changing
reopen. Failed finalization stops the group
without another dispatch or reuse. It compiles/links natively but is not hardware-tested
or packaged in v48/v49/v50. Native incremental allocation/publication and cleanup
of incremental children attached to the initial context parent remain unfinished.

The dispatcher requires an explicit `Prepare_Ticket` callback before submission.
Incomplete preparation yields without sending a release; failure wins even if
the callback also reports completion. Session, ledger generation, pending ticket
and context exclusion are rechecked after preparation. Native preparation scans
one retained context or candidate image per service-loop turn, while retirement
admission remains closed. An individual image scan still depends on that image's
configured capacity. The closed context's own source is exempt from disjointness
only with the exact context index, current group anchor, sealed root and revision.
Other live sources and candidate images must be sealed and disjoint. Incremental
children use the same group-source identity but their own backing overlap query.
This does not enable native incremental directory allocation/publication.

`nix develop -c python3 tests/intel-gpu/test-retirement-preparation.py` extracts
the actual native preparation procedure and tests 17 scenarios over real VM
images and backing-disjointness checks. Coverage includes yields, aliases,
unsealed images, closed-owner identity, capacity changes and ownership loss.
Range checks use the actual backing registry, including revoked entries; a
registry revision change between preparation steps rejects the entire scan.
Negative controls remove alias checks, broaden the closed-owner exception, or
substitute the child for the group anchor; all must fail. These are hosted
adapter checks, not hardware retirement proof.

Prepare, submit and poll callbacks must leave the ledger's owner, generation,
phase and pending ticket unchanged. The dispatcher revalidates that transaction
on return, before accepting progress. The dispatcher fixture deliberately
acknowledges the ledger inside each callback at each of three tickets; the
controller must fail without sweeping that ticket's references or replaying a
request. The pre-fix submit path failed this regression by allowing progress
without its own polling step. This is backend-contract hardening, not evidence
that the current native backend performs such a mutation.

`Table_Allocations.Check_Retained_Range` observes overlap with an exact retained
session/ticket at an expected registry revision. It works after live admission
is revoked, without returning backing addresses or granting release permission.
Missing/stale identity, invalid backing, zero length and overflowing ranges
return a conservative overlap with an unsuccessful observation. Native
preparation uses this per-allocation observation rather than treating anchor
backing as the range for every queried ticket. `table_allocations_tests` checks range boundaries, revoked
and owner-lost entries, stale revisions, retirement and slot reuse for 100 slots.

`nix develop -c python3 tests/intel-gpu/test-grouped-table-transport.py` executes
the native admission/submit/poll/finalize callbacks and registry receipt predicate
with a real backing registry and modeled supervisor/request-slot state. Twelve
scenarios cover child/anchor slot separation, stale generations, delayed receipt,
wrong role, revoked retention, lost ownership, failed send and failed finalization.
Context-parent cases use an anchor with no table-registry entry and require its
exact context-ticket authority. Children still require table-only registry and
closed-ticket authority; their acknowledgment does not acknowledge the parent.
Negative controls bypass context identity, substitute the anchor slot or accept
a wrong receipt generation. Native initial-parent cleanup now includes bounded
group membership scanning; unmatched or nonincremental allocations still block it.
The active release ticket is distinct from the pending group anchor; a failed
attempt retains that active identity and prevents a second submission. This test
does not exercise physical retirement, GPU exclusion or native incremental growth.

These mutation fixtures force recompilation for each variant. Rewriting generated
Ada within the same timestamp interval can otherwise reuse an earlier binary and
invalidate a negative control. `test-retirement-preparation.py` now covers twenty-three
cases, including a context parent absent from the table registry, a same-slot
candidate alias, a changed captured context epoch, context-owned incremental
children, revoked child ranges and child-specific aliases.

`test-retirement-dispatch.py` compiles the actual native request-admission guard
over 96 combinations of retirement, staged VM update and metadata state. Negative
controls remove each transaction guard independently and must fail. This checks
admission exclusion, not hardware invalidation or physical retirement authority.

`buffer_memory_tests.adb` also composes the production retirement dispatcher with
the production `Buffer_Memory` state machine over two retained allocations.
Hosted transport responses cover delayed acknowledgments, stale completion tokens,
wrong generations and send failure. Assertions require references retained before
acknowledgment, finalization only after the sweep, no more than one callback per
step and no replay after a terminal outcome. The transport and memory mappings are
host fixtures; this does not validate GPU cache/TLB retirement on Intel hardware.

## Growable CPU table mirrors

`VM.Initialize` and `Prepare_Update` accept an explicit `Backing_Count` separate
from metadata capacity. The backed prefix is validated, the suffix must be zero,
and offline mapping preflights physical backing before modifying any directory.
The default remains full backing; native bootstrap still uses that default.
`vm_growth_bind_tests.adb` covers 20 full/partial bootstrap and synchronous/stepped
growth combinations, including four initial pages growing into fresh backing,
missing-backing rejection without partial mapping, and partial replacement images.
This enables sparse initialization in the core; native on-demand allocation and
multi-ticket lifetime integration remain separate unfinished work.

The native missing-directory replacement fallback now requests `used + additional`
table pages from the topology plan instead of always requesting 64. It captures
that count across allocation, validates the exact response size, and installs only
that provenance prefix. Trusted binding preparation accepts a zero suffix, not
holes. `vm_update_pipeline_tests.adb` runs all nine lifecycle scenarios with both
full and right-sized backing (18 total), including empty remap, late revocation,
failed invalidation and retirement. This reduces the existing replacement path's
allocation request; it is not incremental directory publication and does not remove
the current native 64-page VM capacity. Hardware performance is unmeasured.

Native context initialization now passes its allocated prefix explicitly, and
initial context publication resolves only used tables into a zeroed mapping array.
The native allocator now supplies four initial table pages, separately from the
64-page CPU mirror/VM quota. `vm_materialize_tests.adb` uses
eight metadata slots but only four backed/mapped pages through the actual volatile
RAM writer, with guard-page, ownership-loss, flush/readback and lineage checks.
Its host CLFLUSH test is not evidence of Intel GPU visibility or cache coherence.

Reducing that initial physical allocation also requires offline growth: initial
application binds precede the first context publication and cannot use the live
directory-update gate. `VM.Append_Offline_Backing` now admits a complete group of
owner-supplied pages into an unsealed image without replacing any prior identity
or mapping. Geometry, duplicate, reserved-table, scratch and mapped-data aliases
are checked before mutation; metadata room alone does not supply backing. The
operation advances the image revision, not its used table count, and does no GPU
IO. The native asynchronous offline allocation adapter is now connected, but
the physical startup reservation is four pages. Ledger and mirror metadata
requests retain room for the 64-page VM quota; they do not allocate 64 physical
GPU table pages. This is not yet an unbounded or fully growable native VM quota.

```sh
nix develop -c bash -c '
  offline_test_dir=$(mktemp -d /tmp/cubit-offline-backing.XXXXXX)
  gprbuild -p -P tests/intel-gpu/vm_growth.gpr \
    -XVM_GROWTH_OBJECT_DIR="$offline_test_dir" vm_offline_backing_tests.adb
  "$offline_test_dir/vm_offline_backing_tests"
'
```

The fixture reproduces failure of a sparse second bind with only four backed
pages, then succeeds after three-page expansion and again after one-page
expansion. It checks preservation on rejected groups, sealed/uninitialized
images, metadata/physical quota distinction and extreme array bounds. This is
an offline CPU image regression, not proof of GPU publication or allocation
ownership; those remain the caller's responsibility.

`Growth.Inspect_Offline` provides the preceding allocation plan without relaxing
the sealed-only live `Inspect` gate. It reports missing directory count separately
from additional physical backing, subtracting already reserved unused pages.
Metadata headroom remains an independent check. The 22-case
`vm_offline_planning_tests` composes plan, append and mapping across one/two/three
directory levels and four through eight initially backed pages, plus invalid,
occupied, frozen and metadata-limited cases. No allocator or GPU writes occur in
the planner; the asynchronous caller must revalidate its captured image revision.

`Binding.Check_Offline_Bind_Request` is the allocation-free authorization step
for that asynchronous path, and is also used by the existing offline bind
handler. It checks the initial-bind envelope (offset in word0 high32, unlike live
VM updates), authenticated BO extent, initialized/unsealed image and captured
revision. Eligibility is deliberately separate from topology/backing availability;
normal mapping still validates aliases and all required pages. After allocation
the caller must recheck identity and revision before appending backing, then
perform normal bind validation against the new revision.

```sh
nix develop -c bash -c '
  bind_test_dir=$(mktemp -d /tmp/cubit-offline-bind.XXXXXX)
  gprbuild -p -P tests/intel-gpu/buffer_requests.gpr \
    -XBUFFER_REQUESTS_OBJECT_DIR="$bind_test_dir" offline_bind_preflight_tests.adb
  "$bind_test_dir/offline_bind_preflight_tests"
'
```

The 21-case fixture uses the actual buffer service, handles, binding and VM:
foreign/revoked/closed handles, malformed fields, offset/extent limits, stale
revision, frozen/uninitialized images, sparse backing expansion, exact offset
mapping and unchanged unbinding. It is hosted integration, not a supervisor or
hardware test.

`test-native-offline-bind.py` extracts the actual native deferred finalizer and
composes it with the real buffer service, binding, VM and provenance ledger.
Its eight cases cover success, owner loss, registry-install failure, page
resolution failure, unavailable backing, stale image revision, consumed ticket
and failed reply delivery. Wrong-ordinal and omitted reply-loss rejection
negative controls must fail. Supervisor allocation/registry, native admission
gate and IPC delivery are modeled; this does not exercise the kernel event loop
or Intel hardware. The native driver also compiles with this adapter.

```sh
nix develop -c python3 tests/intel-gpu/test-native-offline-bind.py
```

`test-native-allocation-routing.py` compiles the actual service-loop allocation
continuation block and the initial VM-finalizer routing branches against modeled
supervisor progress. Fifteen checkpoints verify pending/image-storage/recycling
gates, repeated offline continuation, offline-before-live dispatch, retirement
priority and rejection of a stale phase without a ticket. Three negative controls
remove the allocation-pending gate, offline route or ticket gate and must fail.
This is routing regression coverage, not native IPC execution or an admission
test; kernel completion delivery and allocator ownership remain separate gates.

```sh
nix develop -c python3 tests/intel-gpu/test-native-allocation-routing.py
```

`test-native-offline-admission.py` compiles the actual admission function using
real binding preflight, topology planning and service tickets. Twenty cases
exercise one/two/three-page sizing, malformed/foreign requests, occupied service
state, no additional backing needed, exhausted ledger capacity, published
context and failed owner/reply/allocation handoffs. Render lookup, the post-
reservation owner gate, supervisor start and IPC delivery are modeled. Wrong
size, ignored reply-save failure and wrong ticket-role mutations must fail.

```sh
nix develop -c python3 tests/intel-gpu/test-native-offline-admission.py
```

`test-native-bootstrap-growth.py` extracts the native bootstrap size and exact
context/table/scratch slicing and VM initialization. Real backing views, VM and
provenance then compose a four-page parent with a three-page incremental ticket,
preserving the initial mapping and rejecting scratch as new table backing.
The actual materializer writes all seven used tables and four scratch pages to
volatile host RAM, checks every word and guard pages, and checks scratch-first,
children-before-root flush callback order. Both overlapping scratch and a wrong
backed-count mutation must fail. Physical addresses/ownership and flush callbacks
are modeled; no GPU visibility, scheduling or complete retirement is established.

```sh
nix develop -c python3 tests/intel-gpu/test-native-bootstrap-growth.py
```

```sh
nix develop -c bash -c '
  plan_test_dir=$(mktemp -d /tmp/cubit-offline-plan.XXXXXX)
  gprbuild -p -P tests/intel-gpu/vm_growth.gpr \
    -XVM_GROWTH_OBJECT_DIR="$plan_test_dir" vm_offline_planning_tests.adb
  "$plan_test_dir/vm_offline_planning_tests"
'
```

Native live bind/unbind capture resolves only `VM.Used` table ordinals and clears
unused CPU/DMA mapping positions before each capture. Unallocated capacity need
not have a backing ticket; every live table must still resolve and match the
sealed image. This does not remove insertion's checks against all reserved table
DMA and scratch pages. `nix develop -c python3 tests/intel-gpu/test-live-table-capture.py`
compiles the actual native capture block with real VM metadata and modeled
authority: ten initial/replacement/missing-table cases plus negative controls for
full-capacity lookup and stale unused mappings. It is not a GPU execution test.

```sh
nix develop -c bash -c '
  mirror_test_dir=$(mktemp -d /tmp/cubit-vm-mirrors.XXXXXX)
  cd kernel
  alr exec -- gprbuild -p -P ../tests/intel-gpu/vm_growth.gpr \
    -XVM_GROWTH_OBJECT_DIR="$mirror_test_dir" \
    vm_table_store_tests.adb vm_metadata_growth_tests.adb
  "$mirror_test_dir/vm_table_store_tests"
  "$mirror_test_dir/vm_metadata_growth_tests"
'
```

`Intel_GPU_VM_Image` now stores table words through `VM_Table_Store`, with a
configurable bootstrap and a separate maximum table quota. `Extend_Metadata`
accepts a trusted, already committed, stable CPU reservation in increments up to
64 KiB. It changes neither GPU mappings nor image revision. Old table indices
and contents remain stable; failed capacity checks do not partially map a range.
The store test grows four to 132 mirrors, verifies untouched uncommitted suffixes,
quota/geometry rejection, and independent copying. The actual image test grows
four to 100 mirrors, uses 99 tables for 96 sparse mappings, and exercises cloning,
adoption, retirement and reuse without cross-image aliasing.

`table_metadata_growth_tests` composes the production provenance ledger,
record-growth controller and metadata arena using modeled reserve/commit callbacks
and real typed CPU storage. With the native 1 MiB / 32768-record quotas, its first
64 KiB commit currently yields 2064 records: the bootstrap 64 plus 60 incremental
references need no additional commit. It also crosses the measured capacity,
preserving IDs and generation while rejecting foreign-owner/stale-generation
lookups. The capacity assertion is derived from the real type, not a substitute
for dynamic growth or a new fixed allocation limit.

`test-native-directory-update.py` extracts the native initial-root directory
dispatcher from `main.adb`. Twelve boundary scenarios use the real append-only
provenance ledger with modeled allocation, writer, TLB and reply boundaries.
It checks ownership/allocation/registration/publication/invalidation/commit
failures, backing-size rejection, retained-root mismatch, and the successful
handoff to leaf binding without an early reply or public generation advance.
Negative controls omit directory invalidation and corrupt adopted image IDs.
The separate growth writer/bind/lifecycle tests exercise real CPU table writes
and the production writer; neither suite establishes physical GPU behavior.

```sh
nix develop -c python3 tests/intel-gpu/test-native-directory-update.py
nix develop -c python3 tests/intel-gpu/test-native-directory-io.py
nix develop -c python3 tests/intel-gpu/test-native-directory-admission.py
```

The IO fixture extracts the native exclusion gate, DMA-to-ledger lookup, initial
ordinal lookup and read/write/flush adapters. It uses real provenance and volatile
CPU memory, with modeled ticket resolution and cache flush. It rejects unused
reserved pages, adjacent wrong-ticket records, missing staged registration,
stale generations, foreign sessions, changed DMA, revocation and exclusion loss
inside callbacks. It verifies adopted ordinal5 resolves ledger65, not ledger5.
Two negative controls remove staged-ticket checking and confuse ordinals with IDs.
The admission fixture runs the actual native allocation-selection block against
the real topology planner: eight cases cover one/two/three-page growth, replacement
roots, exhausted ledger headroom, occupied/misaligned/over-quota ranges. Wrong
allocation-size and ticket-role mutations must fail. These are host-side native
adapter tests; supervisor allocation and GPU execution are not emulated here.

The native initial-root path now allocates only missing directory pages,
registers each in its context ledger, publishes one directory operation per
turn, and confirms a fresh TLB invalidation before adopting directory metadata.
It then retains the same held request for the existing stepped leaf-bind path
and its separate invalidation. Growth receipts belong to individual contexts.
Replacement-root contexts still use the replacement-image path; the current
64-table native image quota remains. This is not yet unbounded GPU VM growth.

```sh
nix develop -c bash -c '
  ledger_test_dir=$(mktemp -d /tmp/cubit-table-metadata.XXXXXX)
  gprbuild -p -P tests/intel-gpu/vm_growth.gpr \
    -XVM_GROWTH_OBJECT_DIR="$ledger_test_dir" table_metadata_growth_tests.adb
  "$ledger_test_dir/table_metadata_growth_tests"
'
```

These are CPU metadata tests, not GPU backing allocation or hardware validation.
The caller must authenticate/disjointly reserve the metadata memory and retain
it for the image lifetime. This is an unproved trusted-memory boundary. Native
instantiation now starts with four CPU mirrors. Initial-context and replacement
readiness gates drive asynchronous extension to the existing 64-table quota,
committing at most 64 KiB per allocator turn (last increment 48 KiB).
`context_mirror_growth_tests`, `update_storage_tests`, and
`update_mirror_failure_tests` exercise the allocator compositions, including
commit/ownership failures without replay. These changes compile natively but
are not yet in the v47 image or hardware-tested. GPU physical table backing and
fixed-size DMA/level/receipt arrays remain separate integration work; the physical
64-table reservation has not been reduced or made dynamically growable.

## Growth provenance callback revocation

```sh
nix develop -c bash -c '
  growth_test_dir=$(mktemp -d /tmp/cubit-growth-ownership.XXXXXX)
  cd kernel
  alr exec -- gprbuild -p -P ../tests/intel-gpu/vm_growth.gpr \
    -XVM_GROWTH_OBJECT_DIR="$growth_test_dir" vm_growth_ownership_tests.adb
  "$growth_test_dir/vm_growth_ownership_tests"
'
```

The production incremental directory writer is exercised over host RAM. The
fixture first measures a successful transaction, then revokes exclusion inside
each provenance callback while returning a positive lookup result (9,286
boundaries in the stepped writer). It also revokes ownership between each step
and attempts premature commit at each step (6,174 additional rejection cases).
No read/write/flush may occur after revocation; failed publication cannot be
replayed or adopted even after authority is restored. This exposed a missing
post-callback exclusion check in the original synchronous writer (negative control: "write
after ownership revocation"). It does not prove hardware ordering, native
incremental-growth integration, or general thread safety; callers must still
serialize the transaction and retain all uncertain backing.

The writer now exposes `Start`, `Step`, and `Pending`, replacing the synchronous
publication API. `Start` preflights without memory IO; `Step` performs at most
one read/write/flush callback, checked by the tests. The receipt stores its plan
and cursor across service-loop turns. Source epoch/root and ownership are checked
again on each turn; failure consumes the attempt. Planning and final metadata
adoption still walk the configured capacity; this is bounded hardware IO, not a
claim of constant-time planning or native dispatcher integration.

## Whole-context allocation identity

The application-image integration fixture is also required after changes to
the retirement child: build `submission_buffer.gpr` and run
`build-submission-buffer/submission_buffer_tests` from this directory under Nix.
Its 29 publication/retirement paths assert exact address release and reject
stale image operations after a new claim reuses the same VA. It executes real
Application_Image code over host RAM/mock PTEs, not GPU hardware.

Under Nix, build `gprbuild -p -P tests/intel-gpu/context_tickets.gpr` and run
`tests/intel-gpu/build/context-tickets/context_tickets_tests`. It covers 128
context-parent owner/generation transitions, sixteen combinations of pending
allocation/quarantine/device loss/missing retirement evidence, revoked-session
cleanup, pinned/table-kind separation and stale/duplicate acknowledgment.
This is metadata-only: supervisor release, hardware reference retirement and
reusable native session admission are not established by this fixture.

## Retirement dispatcher ordering

`nix develop -c python3 tests/intel-gpu/test-retirement-dispatch.py` compiles
the actual driver request-poll guard and checks 48 input combinations. While a
supervisor retirement is pending, no client request may be consumed or stale
`Found` flag dispatched. Both metadata gates are covered too. A negative
control removes the pending-retirement guard and must fail. This is hosted
control-flow coverage, not proof of hardware completion or kernel revocation.

## GGTT address reclamation transaction

Run under Nix:

```sh
gprbuild -p -P tests/intel-gpu/ggtt_reclamation.gpr
tests/intel-gpu/build/ggtt-reclamation/ggtt_reclamation_tests
tests/intel-gpu/build/ggtt-reclamation/ggtt_retire_tests
gprbuild -p -P tests/intel-gpu/ggtt_reuse.gpr
tests/intel-gpu/build/ggtt-reuse/ggtt_reuse_tests
```

The new reservations child has no bookkeeping-only release entry point. Its
one-shot transaction scratch-remaps the exact claim, verifies PTE readback,
waits for invalidation, and checks ownership before removing that claim.
The 4,512 hosted cases cover all ledger sizes/removal positions, all ten I/O
failure points and twenty ownership gates at every full-ledger position,
malformed inputs, preservation of other claims/PTEs, and same-address replay.
The original lower-level retirement primitive still retains its claims.

These are mock-PTE regression tests, not hardware validation. The private
ledger transformation called after successful retirement is now separately
SPARK-proved: exact swap removal, preserved other extents/aperture, count
decrement and the complete ledger invariant. The ledger proof reports 65
analysis results with none unproved or justified; `Forget_Detached` has seven
proved checks. Evidence snapshot: `build/reclaim-proof.DDG5pI/gnatprove.out`.
The callback-driven transaction itself remains outside SPARK; this proof
does not establish hardware quiescence or that cleanup authority is valid.
The child is wired into native image retirement and compiles/links natively,
but has not been hardware-tested. The additional reuse fixture runs 1,024
cycles through the actual publisher/reclaimer over mock PTEs, with different
backing, neighboring claims, nonzero scratch entries and stale attempts.
Physical backing,
CPU grants, supervisor tickets and session identity are not released by it.
The exclusive serialized owner and truthful hardware callbacks remain trusted
requirements; tests do not establish those facts on a running machine.

## Native allocation and mapping checks

These separate fixtures boot CuBit under QEMU; they do not emulate Intel GPU
execution. With a current built `kernel/cubit_kernel`, run:

```sh
flock --exclusive coordination/build.lock nix develop -c bash tests/intel-gpu/native/run-demand.sh mappings
```

The `mappings` fixture uses production sharing, metadata reservation/commit and
record-growth code with real kernel self-grants. It fills the initial 64 records
while retiring temporary grants, keeps a reader alive during forced growth to
128 records, uses record 65, then grows to 256 with readers in both storage
tiers. Closing the BO must retain both grants until their readers return; stale
grant access and mapping the closed name must fail. Only two grants are live at
once: this tests stable metadata growth, not removal of the kernel's current
16-grant owner limit or automatic growth under many simultaneous mappings.
The privileged loopback fixture also does not establish cross-process isolation
or GPU retirement. Unique evidence directories contain the serial log and
kernel/fixture binary hashes. Existing `memory`, `ipc` and `views` modes cover
backing allocation, real allocation transport and forwarded-view retention.

## Hosted register and policy checks

Native pipe and primary-plane adapters (2026-09-28): build `native_pipe.gpr`
and `native_plane.gpr`, then run `build-native-pipe/native_pipe_tests` and
`build-native-plane/native_plane_tests` in Nix. These compile the actual native
adapters. The pipe fixture exercises rejected calls without host MMIO and
substitutes mapping readiness and the clock boundary; the plane fixture supplies
a retained-power callback plus anonymous host pages for all twenty plane
register sets. `native_cursor.gpr` similarly exercises all four cursor adapters.
Neither establishes successful physical acquisition.

Current native source requests display pages 0x45000/0x46000/0x44000 in slots
24/25/28, avoiding GGTT slot26 and log observer slot27. After successful reset,
A/B power acquisition uses retained PW1/PW2 references and initial-boot PCI IRQ
disable evidence. C/D additionally require the completed native DC transition.
The collector logs two samples of five planes and one cursor on each held pipe.
`scanout_inventory.gpr` combines all24 observations; missing/changing/unsupported
observations reject the inventory. Its non-overlap contract is SPARK-proved and
tested against65536 interval cases. This is exclusion evidence, not authority
to reclaim or publish GPU addresses. Native four-pipe validation is pending.

ADS storage layout: build `tests/intel-gpu/ads_layout.gpr` in Nix, then run
`tests/intel-gpu/build-ads-layout/ads_layout_tests`. Covers1089 section-size
combinations, exact/one-byte-short backing, unrepresentable sizes and rejection
of1MiB backing for the selected firmware's private area. No native ADS data
initialization or GPU publication is exercised.

Forcewake fallback: `nix develop -c bash -c 'gprbuild -P
tests/intel-gpu/forcewake_fallback.gpr &&
tests/intel-gpu/build-forcewake-fallback/forcewake_fallback_tests'`.
Twenty injected clear/set cases cover missing original ACK, stuck fallback
set/clear, invalid MMIO/time, stalled/regressing clock, late read and original
ACK lost during cleanup. A composed lease test recovers acquire and release.
These are regression tests, not hardware validation or SPARK proof.
Three lease-hook guards additionally reject recovery on bad MMIO/regressing
clock and verify that a failed recovery leaves the lease quarantined.

PCI IRQ snapshot regression: `nix develop -c bash -c 'gprbuild -P
tests/intel-gpu/probe.gpr pci_interrupt_tests.adb &&
tests/intel-gpu/build/pci_interrupt_tests'` (join command lines).
Tests sweep all 256 flag bytes and 256 capability pointers, recognized
record bounds for all MSI formats, duplicate/cyclic/overlapping chains, and
all-ones input. GNATprove level 2 on `intel_gpu_pci_interrupts.adb` proves
runtime checks, dependencies and termination, not PCI hardware quiescence
or full functional decoding correctness. Bootstrap v4 carries this observation
to the native Intel logstore publisher. Tests exhaust all 256 encodings,
round-trip valid ones, reject contradictory/reserved bits and reject old v3.
Both native services compile; no physical-hardware IRQ handoff claim is made.

Run from the repository root in the pinned Nix environment:

```sh
nix develop -c bash -c 'cd kernel && alr exec -- gprbuild -p -P ../tests/intel-gpu/probe.gpr && ../tests/intel-gpu/build/main'
nix develop -c bash -c 'cd kernel && alr exec -- gnatprove -P ../tests/intel-gpu/probe.gpr -u intel_gpu_probe.adb --level=1 --report=all --checks-as-errors=on -j2'
```

The test exhausts all 65,536 device IDs for Intel display, wrong vendor, and
wrong class, plus bounded mapping sizes/offsets and 64-bit boundary cases.
Assertions are enabled in this hosted fixture, not in a kernel/native build.

The package is not yet wired to native device discovery. Its checked address
construction must only be used with an authorized, live mapping and separately
validated register semantics. See [bring-up plan](../../docs/intel-gpu-bringup.md).

The BAR fixtures cover 32/64-bit memory, all flag combinations, I/O rejection,
unsupported encodings, zero/unassigned addresses, missing high words, and
addresses above 4 GiB. BAR size is deliberately not inferred.

2026-09-27 evidence: hosted regression passed; GNATprove level 1 reported
15 obligations discharged (9 flow/initialization/termination, 6 prover), none unproved or
justified, no warnings and no `pragma Assume`. This is only the pure helper,
not a verified GPU driver. Report: `build/gnatprove/gnatprove.out`.

Resource handoff: `resource_tests.adb` exercises `Intel_GPU_Resources`, including
4,896 page-range combinations, unknown extents/platforms, decode-disabled
devices, cache-class rejection, unaligned requests and top-of-address-space
boundaries. No memory mapping or hardware access occurs.

Proof command for this unit:
```sh
nix develop -c bash -c 'cd kernel && alr exec -- gnatprove -P ../tests/intel-gpu/probe.gpr -u intel_gpu_resources.adb --level=2 --report=all --checks-as-errors=on -j2'
```
All reported checks passed, including the containment/rejection postcondition
and three intermediate arithmetic assertions. No Assume or SPARK-Off escape.
This proves containment relative to the supplied resource extent, not that a
caller obtained that extent from trustworthy PCI resource discovery.

The ADLN-specific entry point also checks hardwired BAR encoding/address width
and tests every page in the 16 MiB aperture: only the initial 512 register pages
are admissible. Its functional postcondition passes the same level-2 proof.

`boot_tests.adb` checks the four-word bootstrap format, reserved bits, header
type, version and identity rejection. The decoder's conversions and termination
pass GNATprove level 1. Sender authentication occurs in the native adapter,
not in this pure decoder. Native `main.adb` accepts only the registered devmgr,
maps the admitted region with read-only mode, and remains idle without touching
registers or claiming display ownership.

`check-native.py SERIAL_LOG` checks the private RAM-backed fixture exercising
this real service and the subsequent desktop boot. The fixture is labelled
NOT hardware. It does not validate GPU registers, power domains, scanout,
command submission, or hardware acceleration.

`observation_tests.adb` uses a recording reader to check the staged two-register
snapshot: exact addresses/order, no reads for unknown platforms or unconfirmed
D0, truncated/unaligned/zero/wrapping mappings, and preservation of raw all-ones
and zero values. It does not access MMIO. Native bootstrap still leaves this
capture disconnected pending trusted PCI power-state evidence.

`pci_power_tests.adb` exercises the pure type-0 PCI configuration decoder:
all first-pointer byte values, all PMCSR low-byte values, self-cycle, duplicate
PM records, overlapping PM/header data, truncated PM at 0xFC, and a maximum
48-header chain with/without a cycle. Missing PM is unavailable, not D0.
GNATprove level 2 proves bounds, arithmetic, dependencies and termination of
`intel_gpu_pci_power.adb`; these are not proofs of PCI hardware behavior or of
the returned snapshot remaining current during subsequent MMIO access.

`firmware_tests.adb` covers CSS layout parsing, truncated mandatory payload,
optional absent modulus/exponent data, empty code/key, inconsistent/wrapped
header counts and maximum DWORD code-size arithmetic. The layout contract
passes GNATprove level 2 with `-u intel_gpu_firmware.adb`. These are layout
proofs, not firmware authenticity, compatibility, upload or execution tests.

Real-file hosted fixture (also packaged by the private bring-up image):

```sh
nix eval --raw --file tests/intel-gpu/firmware-source.nix blob
# After building probe.gpr, pass the printed store path:
nix develop -c tests/intel-gpu/build/firmware_file /nix/store/PRINTED-tgl_guc_70.bin
```

The pinned linux-firmware 20250917 TGL GuC file parses as 335360 bytes,
334976 code bytes, signature offset 335104 and 256 signature bytes. This tests
the same Ada decoder as the synthetic cases. The fixture's LICENSE.i915 is
also hash-pinned. The private image packages both separately from the driver.
A successful layout parse
does not authenticate Intel signatures or establish firmware ABI compatibility.

`firmware_reader_tests` exercises the caller-buffer firmware reader with a
1 MiB policy budget and 4 KiB reads: size rejection without I/O, complete and
short reads, failed reads (including after progress), EOF, oversized replies,
malformed CSS, nonzero array origins, and the maximum Natural array index.
`firmware_file` now reads the entire pinned binary through this same generic,
not just its header. Run it and `build/firmware_reader_tests` after building
`probe.gpr`. These are Linux-hosted regressions, not a proof of the reader or
a native filesystem integration test. The future native adapter must bound
IPC waits and finish/revoke buffer loans before returning; the reader cannot
cancel an outstanding IPC request or authenticate a changing file.

`ggtt_tests` exercises the pure GGTT window planner: 4 KiB page rounding,
cross-table-page ranges, last-entry admission, zero/unaligned/oversized inputs,
and exact capacity plus one-byte overflow for every table size from 1..2048
pages. GNATprove level 2 on `intel_gpu_ggtt.adb` proves the returned mapping
stays inside the supplied table size. No actual page-table access occurs.

The pure-reader instance in `observation_proof.ads` makes generic capture code
available for proof, including address preconditions and capture/rejection
postconditions. It does not model physical reads, power stability or faults:

```sh
nix develop -c bash -c 'cd kernel && alr exec -- gnatprove -P ../tests/intel-gpu/probe.gpr -u observation_proof.ads --level=2 --report=all --checks-as-errors=on -j2'
```
# GGTT publication transaction

The display-claim suite also exercises the `PW1_Write_Allowed` predicate used
by the native parent-power adapter: all524288 aligned register offsets with
both permitted masks,192 single-bit mutations, unaligned offsets and invalid
all-ones readbacks. These are hosted tests of write selection, not proof of
fresh hardware reads, serialization, MMIO ordering or physical power behavior.

`build/ggtt_publish_tests` is Linux-hosted callback fault injection, not a
hardware or native CuBit test. It checks occupied entries (including nonzero
non-present entries), preflight read failures, failed preparation, every write
position (including failure after a store), readback failure/corruption, and
invalidation failure. Only the fully successful sequence reports Published.
Out-of-range, unaligned, partial-page and over-budget inputs produce no I/O.
The exact upper DMA boundary is covered. Device ownership, concurrent writer
exclusion and platform visibility/invalidation are external obligations.

The generic `Maximum_Bytes` defaults to 1 MiB for upload staging. ADS callers
can explicitly select 16 MiB; an absolute 16 MiB limit still bounds callbacks.
Tests opt in to a full 4096-page ADS mapping, verify every PTE, and inject
failure at the final preflight read, final write and final readback. Preflight
failure makes no writes; either later failure retains the claim and quarantines
the attempt without invalidation. This is hosted regression evidence, not
native ADS publication or a proof of MMIO ordering.

Publication now consumes a limited, noncopyable `Attempt`. Its state records
whether no writes occurred, writes may have occurred, or publication completed.
Every scenario repeats with the same and a different GPU start address: both
must reject without any callback and preserve the prior phase. There is no
reset/free operation; callers must keep the attempt associated with its retained
allocation. Publication also requires a shared `GGTT_Reservations.Ledger`;
table geometry comes from its one-shot admission, not a second caller argument.
Before any callback, the publisher reserves the exact GPU range. All acquired
claims remain retained, including failures before the first write. Tests create
a fresh attempt with different DMA backing after each reservation-bearing
scenario and verify rejection without callbacks. A default ledger and a range
outside an admitted aperture likewise cannot reach hardware callbacks.
The caller must retain and share the same ledger; constructing a replacement
ledger is not a supported way to bypass quarantine. Firmware/display exclusions
must be established before admission, never inferred from empty PTEs.
This is API misuse resistance, not kernel enforcement or a concurrency proof.

The ledger now has a Ghost `Valid` predicate covering nonempty claims,
containment in the admitted aperture and pairwise nonoverlap. `Admit` and
`Reserve` require and preserve it; their contracts also prove that admission
does not change the claim count and only `Reserved` increments it by one.
Ghost snapshots additionally prove every existing claim is preserved on all
outcomes, and a successful reservation appends exactly the requested extent.
The limited runtime ledger remains noncopyable; snapshots exist only for proof.
GNATprove level 2 discharges both functional contracts, loop invariants and
run-time checks (no unproved checks or `Assume` pragmas). The hosted 1296-pair
regression also passes with contracts enabled. Reproduce the proof with:

```sh
nix develop -c bash -c 'cd kernel && alr exec -- gnatprove -P ../tests/intel-gpu/ggtt_reservations.gpr -u intel_gpu_ggtt_reservations.adb --level=2 --report=all --checks-as-errors=on -j2'
```

This proves a serialized software ledger property, conditional on a valid
incoming ledger. It does not prove platform aperture admission, MMIO/DMA
visibility, native driver concurrency or successful firmware execution.

`Find_Free` proposes a page-sized, power-of-two-aligned range inside an already
admitted aperture, scanning at most 65 passes over 64 unsorted claims. It does
not reserve the proposal: the same serialized owner must subsequently call
`Reserve`/`Publish`, which may still reject descriptor exhaustion. No search
result confers authority over firmware memory or empty GGTT entries.

`Allocate` composes search and reservation in one serialized-owner operation.
It returns an address only with `Reserved`; otherwise the address is zero,
the count is unchanged, and existing claims are preserved. It supplies no
internal lock and does not publish PTEs. Tests cover repeated aligned claims,
descriptor exhaustion, invalid size, a full aperture and a valid zero address
(callers must check status, not use zero as a success sentinel). SPARK checks
the preservation, count and exact successful-claim contracts.

The `Space_Free` return contract proves a successful proposal is nonempty,
contained and disjoint from existing claims; runtime safety and termination
are also proved. This quantified contract currently needs level 3 with
`--timeout=60` using the command above (level 2 did not discharge it).
An independent eight-page occupancy oracle checks 9216 queries covering every
occupancy pattern, four alignments, nine sizes and reverse-order insertion.
Alignment and lowest-fit selection are regression-tested, not part of the
proved return contract. No claim is made about GPU performance from this test.

`GGTT_Publish.Publish_Available` combines that proposal with the existing
reservation-and-publication sequence under caller-held exclusive ownership.
It consumes unsuccessful searches without device callbacks. Hosted tests
exercise aligned selection past retained claims, successful publication and
ambiguous first-store failure, then clear the simulated PTEs and verify a new
attempt still skips the retained claim. Reusing an attempt, including after
no-space failure, performs no device callbacks. This composition is tested,
not SPARK-proved, and does not admit a native firmware aperture by itself.

## Multi-domain forcewake coordination

`build/domain_lease_tests` exercises 128 combinations of three synthetic domain
selections and acquisition/release failures. It checks reverse cleanup order,
continued cleanup after release failure, exact uncertain-domain tracking,
rejection of nested acquisition and repeated release, and non-reuse after
failure. Callback failures are reported as Boolean results, not exceptions.
These Linux-hosted regressions do not validate actual ADL-N domain selection,
register handshakes, reset ordering, or concurrency. No native binding is enabled.

`inventory.gpr` builds the separate ADL-N inventory test in `build-inventory`.
It exhausts all 4,096 media fuse-field combinations and all 65,536 device IDs,
checks invalid vendor/all-ones MMIO rejection, and verifies distinct aligned
domain register pairs. It is a pure hosted decoder test, not hardware discovery.

`forcewake.gpr` runs the existing handshake/deadline/failure-cleanup tests for
each of the five ADL-N register pairs. Request/ack offsets are fixed at generic
instantiation, not selected from untrusted input during a lease. The GT-specific
procedure names were replaced by Acquire/Release, without compatibility aliases.
These are mocked MMIO tests; only GT has been tested on the NUC so far.

`adln_forcewake.gpr` combines the real fuse decoder, five fixed handshake
instances and coordinator. All 288 combinations of media selection and
acquire/release timeout positions are tested, including failed-acquire cleanup,
continued release after failure, exact uncertainty, and invalid identity/fuses.
The MMIO model acknowledges request bits or injects timeouts; it is not a
simulation of Intel silicon or evidence that reset is safe.

`reset_prepare.gpr` tests normal/already-ready/catastrophic preparation,
poll and clock deadlines, invalid MMIO, clock regression, and cancellation
write encoding. No native reset adapter is linked. The model does not establish
engine stopping, cancellation acknowledgment, or hardware-workaround compliance.

`engine_stop.gpr` checks stop/prefetch encodings, all 1,024 combinations of
pending forcewake requests and enables, acknowledgment rejection, settling
with a stalled clock, invalid MMIO, idle timeout and zero-budget no-write.
These callbacks are a register model, not native hardware or SPARK proof.

`gt_reset.gpr` checks two successful full-reset acknowledgments followed by
settling, failure at either cycle, all-ones responses, stalled/regressing
clocks, delayed-read deadline expiry, and rejection of repeated attempts.
It does not issue hardware reset writes or prove safe display preservation.

`handoff.gpr` covers 6,912 combinations of media engine selection, forcewake,
stop, preparation, reset and cleanup outcomes. Stage callbacks assert ordering;
tests reject reset after failed stop/preparation, require all-engine cleanup
after preparation begins, and reject attempt reuse. These abstract callbacks
do not yet exercise the register-level helpers together or native hardware.

## Linear scanout footprint

`scanout_range.gpr` exercises `Intel_GPU_Scanout_Range.Linear`: a pure
calculation from already-decoded GGTT surface address, row pitch, dimensions,
pixel size and source offsets. It retains complete rows including padding and
leading offset rows, rounds outward to pages, and rejects invalid/overflowing
geometry or ranges beyond the table aperture. It is not a register decoder or
an ownership/admission decision. It must not be used for tiled, compressed or
multi-plane formats. The native caller still needs a stable inventory of all
enabled planes/cursors and both live and pending surfaces.

```sh
nix develop -c gprbuild -P tests/intel-gpu/scanout_range.gpr
nix develop -c tests/intel-gpu/build-scanout-range/scanout_range_tests
nix develop -c gnatprove -P tests/intel-gpu/scanout_range.gpr -u intel_gpu_scanout_range.adb --level=2 --report=all
```

The hosted oracle enumerates touched pixel addresses for 5168 layouts and
checks page containment, full-row retention and rounding tightness. Additional
cases cover the final page of a 4GiB aperture, overflow and malformed inputs.
These tests do not establish actual hardware fetch/prefetch behavior or the
correctness of a future register decoder. Evidence belongs under
`tests/mesa-software/target/scanout-range-2.log`. The level-2 proof establishes
runtime checks, termination and the accepted extent's nonempty, page-aligned,
aperture-contained postcondition. Pixel coverage and tight rounding are tested,
not included in that proven postcondition. The first proof attempt is retained
in `scanout-range.log`; it did not prove nonemptiness before explicit rejection
of zero/under-rounded spans was added.

## ADL-N linear plane decoder

`plane_decode.gpr` tests the strict initial RGB8888 plane-register decoder.
It covers a 1920x1080 baseline, all 32 control-bit mutations, all-ones values
and changes in each of six sampled registers, pending/live mismatch, offsets,
stride flags, address flags and aperture overflow. Every rejected result has
an invalid extent. No test drives native MMIO or establishes snapshot atomicity.

```sh
nix develop -c gprbuild -P tests/intel-gpu/plane_decode.gpr
nix develop -c tests/intel-gpu/build-plane-decode/plane_decode_tests
nix develop -c gnatprove -P tests/intel-gpu/plane_decode.gpr -u intel_gpu_plane_decode.adb --level=2 --report=all
```

The contract covers ready/valid agreement and, on success, identical samples,
matching live/programmed addresses, the supported control values, matching
surface origin and a nonempty page-sized extent within the aperture. Hardware
register semantics and the caller's ownership assumptions remain outside that
proof. Evidence: `tests/mesa-software/target/plane-decode-2.log`.

## Read-only plane collection

`plane_collect.gpr` tests the composed collection/decoder boundary with fake
power-reference and register-read callbacks. Collection holds Begin/End access
over exactly two ordered six-field samples. It stops at the first read failure
or all-ones value, calls End once after every successful Begin, and does not
decode partial data or data collected with failed cleanup. A reused output is
cleared before acquisition; failure cannot retain a previously valid extent.

```sh
nix develop -c gprbuild -P tests/intel-gpu/plane_collect.gpr
nix develop -c tests/intel-gpu/build-plane-collect/plane_collect_tests
```

Fault injection covers every read in both passes, callback failure vs all-ones,
successful vs failed End, changes in each second-sample field, unavailable
power and a successful decode. These are hosted regression tests, not a proof
of callback behavior, actual power references, MMIO access or snapshot
atomicity. There is no native binding yet. Evidence:
`tests/mesa-software/target/plane-collect-2.log`.

## Display-power owner claim

`display_claim.gpr` covers the one-shot state used by native devmgr request
0x022F. Tests enumerate designated/caller IDs and badge/device validity, then
reject every retry or replacement after an owner is consumed. GNATprove
checks the exact success condition and unchanged owner on denial. It does
not prove the broker authenticates badges, reads PCI correctly or holds a
hardware power reference. No writable MMIO accompanies this designation.

```sh
nix develop -c gprbuild -P tests/intel-gpu/display_claim.gpr
nix develop -c tests/intel-gpu/build-display-claim/display_claim_tests
nix develop -c gnatprove -P tests/intel-gpu/display_claim.gpr -u intel_gpu_display_claim.adb --level=2 --report=all
```

Passing evidence: `tests/mesa-software/target/display-claim-2.log`.
# Display reference lifecycle

`display_power.gpr` builds `build-display-power/display_power_tests` in Nix.
It composes the real topology, six request-well transactions and lease against
shared simulated MMIO, with 512 full-pipe fault combinations and 32 cross-pipe
reuse transitions. DC-off and IRQ/VGA callbacks remain models, not native code.

`dc_write.gpr` builds `build-dc-write/dc_write_tests` for the low-level
DC_STATE_EN write verifier. It tests seven-consecutive-read stability,
sentinels, write errors, independent budgets and periodic glitches. Run both
build and executable in Nix. Success is not a complete DC-off transition:
DMC/PHY/clock/DBUF integration is still required before native use.

The per-well MMIO enable transaction is tested with `display_enable.gpr` and
`build-display-enable/display_enable_tests` in Nix. Its 156 simulated cases
cover six wells, inherited requests, write/read/clock failures, exact fuse
selection, late acknowledgments and failure quarantine. Another 84 release
cases cover inherited retention, safe request removal, cleanup failures and
successful reuse. Native DC-off/ownership/IRQ/VGA integration remains missing.
The same executable also composes the real transaction and coordinator for
36 acquire/release cycles. This guards against skipping inherited *software*
reference cleanup while still requiring no inherited-release hardware access.

Golden-context reservations use `adln_golden.gpr` and
`build-adln-golden/adln_golden_tests`: eight media inventories, one image per
class, exact addresses/state sizes, short backing and upper-bound/alignment
rejection. GNATprove accepts `-P tests/intel-gpu/adln_golden.gpr
-u intel_gpu_adln_golden.adb --level=2` for runtime checks and the capacity
postcondition. There is no captured context or native GPU mapping in this test.

Complete ADS system info uses `ads_system_info.gpr` and
`build-ads-system-info/ads_system_info_tests`:2048 media/count combinations,
all640 bytes, count256, ignored reserved bits, changed/all-ones observations
and invalid topology. GNATprove accepts `-P tests/intel-gpu/ads_system_info.gpr
-u intel_gpu_ads_system_info.adb --level=2` for runtime/termination checks.
Native sampling occurs under inventory forcewake and admission after release;
no GPU publication or hardware validation is implied by hosted tests.

ADS register-section serialization uses `ads_register_image.gpr` and
`build-ads-register-image/ads_register_image_tests`. Eight media inventories
check packed records, all4096 descriptor bytes, physical VCS2 indexing, zero
tails, exact address ceiling fit, overflow/misalignment and failed admission.
GNATprove accepts `-P tests/intel-gpu/ads_register_image.gpr
-u intel_gpu_ads_register_image.adb --level=2` for runtime-check analysis.
This is host-side byte construction, not native GPU mapping or publication.

ADL-N engine settings use `adln_engine_settings.gpr` and
`build-adln-engine-settings/adln_engine_settings_tests`: all320 engine/MOCS
combinations, exact render settings, masked-write encoding, preserved unrelated
RMW bits and invalid/disabled inventory. GNATprove accepts
`-P tests/intel-gpu/adln_engine_settings.gpr
-u intel_gpu_adln_engine_settings.adb --level=2` for runtime/termination checks.
Platform applicability and masks are audited/tested, not a hardware correctness
proof. This is a pure plan: no MOCS selection, MMIO application or GPU execution.

The native render initialization consumes this plan, including twelve
FORCE_TO_NONPRIV entries assembled from the Intel register-field record.
Four explicit read-only counter DWORDs avoid relying on the range alignment
interpretation; three tuning registers are read/write and the remaining entries
use RING_NOPID. These are the twelve slots managed by i915, not a claim that
all hardware permission mechanisms have been sanitized. Application admission
remains closed. The ADS merge preserves their existing unsteered save entries.
`engine_configure_tests` injects read, write and readback failures at each of
the twelve entries and checks that initialization stops and cannot be retried.
These are hosted mock-MMIO regressions; the native driver compiles and links,
but this permission initialization has not yet been validated on the NUC.

ADL-N common register sets use `adln_regset.gpr` and
`build-adln-regset/adln_regset_tests`: all five engine bases,54 exact entries,
mask/steering flags, sorted offsets, disabled engines, unavailable steering and
insufficient MMIO extent. Run GNATprove with `-P tests/intel-gpu/adln_regset.gpr
-u intel_gpu_adln_regset.adb --level=2` for runtime checks and the successful
entry-count contract. The combined builder also checks315 engine/DSS plans,
63render/55other counts, sorted uniqueness, common-entry preservation and every
workaround flag. Exact upstream equivalence is regression evidence, not proof;
native state initialization/ADS publication remain outstanding.

ADL-N steering selection is exercised with `adln_steering.gpr` and
`build-adln-steering/adln_steering_tests`: all1024 DSS/L3 mask pairs, all256
slice masks, range edges, all-ones reads and ignored reserved bits. Run
GNATprove with `-P tests/intel-gpu/adln_steering.gpr
-u intel_gpu_adln_steering.adb --level=2` for runtime checks, termination and
the returned-index bound. Lowest-enabled selection and ABI/platform agreement
are regression-tested, not a hardware proof. Native fuse reads/MCR writes are
not enabled by this helper. The native driver separately reads the three fuse
registers twice under GT forcewake, then uses `Decode_Stable` after successful
release/identity admission. Tests also reject a change in each sample field
and matching all-ones samples; SPARK proves differing samples cannot be valid.

ADS register-list construction is exercised with `ads_regset.gpr` and
`build-ads-regset/ads_regset_tests`. It checks1024 flag/steering encodings,
full-capacity sorted insertion, exact duplicates, conflicting flags, register
bounds and atomic failure. Run GNATprove with `-P tests/intel-gpu/ads_regset.gpr
-u intel_gpu_ads_regset.adb --level=2` for runtime checks and the nonmutation
failure postcondition. Sorting/ABI equivalence are regression-tested; complete
engine register lists and hardware steering selection are not provided here.

ADS engine serialization is exercised with `ads_engines.gpr` and
`build-ads-engines/ads_engines_tests`. Eight media fuse combinations compare
all576 bytes against an independent expected mapping/mask construction;
the observed NUC fuse and invalid/missing-core inventories are also checked.
Run GNATprove with `-P tests/intel-gpu/ads_engines.gpr
-u intel_gpu_ads_engines.adb --level=2` for runtime checks and the admission
postcondition. Byte-level ABI agreement is regression-tested, not formally
proved equivalent to Linux. The generic system-info tail remains unimplemented.

ADS scheduling policy serialization is exercised with `ads_policies.gpr` and
`build-ads-policies/ads_policies_tests`. Both engine-reset modes check all24
little-endian DWORDs, including zero queue-depth/reserved fields and unchanged
bytes outside the reset flag. Run GNATprove with
`-P tests/intel-gpu/ads_policies.gpr -u intel_gpu_ads_policies.adb --level=2`
for the exact byte postcondition. This is a serializer, not a native ADS
publication, firmware compatibility proof or working recovery implementation.

The ADL-N topology is exercised with `display_topology.gpr` and
`build-display-topology/display_topology_tests`. This checks all 256 low-byte
selections against an independent dependency oracle and composes all four
pipe selections with the display lease callbacks. Run GNATprove with
`-P tests/intel-gpu/display_topology.gpr -u intel_gpu_display_topology.ads --level=2`
for the pure topology contracts; hardware correctness remains outside that proof.

Run `nix develop -c gprbuild -P tests/intel-gpu/display_lease.gpr`, then
`nix develop -c tests/intel-gpu/build-display-lease/display_lease_tests`.
The hosted fault matrix checks ancestor ordering, inherited-request retention,
failure quarantine and successful reuse. This is regression evidence only:
the native power-register backend and its platform prerequisites are not yet
implemented, and no hardware reference is created by running these tests.

Capture-list encoding: build `capture_list.gpr` and run
`build-capture-list/capture_list_tests` under Nix. Prove with
`gnatprove -P tests/intel-gpu/capture_list.gpr -u intel_gpu_capture_list.adb
--level=2 --report=all --checks-as-errors=on -j2`.
Tests check all 255 nonempty supported lengths, empty-list backing bytes,
every descriptor word and padding byte, steering combinations, capacity and
invalid offsets. This is a single-page ADL-N encoder, not platform register
selection, firmware publication or evidence of working hardware capture.

ADL-N platform capture pages: build `adln_capture.gpr`, run
`build-adln-capture/adln_capture_tests`; prove the `intel_gpu_adln_capture.adb`
unit with GNATprove level2/checks-as-errors. The504-case matrix covers all
eight engine inventories and63 nonempty DSS masks, enabled/absent class
selection, relative instance offsets and per-DSS steering. These are hosted
tests and runtime-safety proofs, not native GuC capture validation.

Capture assembly: `ads_capture_image.gpr` builds
`build-ads-capture-image/ads_capture_image_tests`. GNATprove target is
`intel_gpu_ads_capture_image.adb` (level2, checks-as-errors). Tests resolve all
66 pointers across8 inventories, compare each referenced page to its source,
check zero-page/tail content, and exercise capacity/alignment/ceiling rejection.
The allocation contract reserves32KiB even when fewer pages are populated.

Read-only upstream ABI audit (supply the downloaded pinned v6.16 header):
`nix develop -c python3 tests/intel-gpu/check-ads-abi.py /path/to/intel_guc_fwif.h`.
This checks every packed ADS field offset/size and system-info size, rejecting
unknown declarations. It neither modifies shared build outputs nor replaces
native firmware compatibility testing.

ADS composition and CPU materialization:

- `ads_header.gpr`: 256 complete packed-header byte patterns.
- `ads_initialization.gpr`: 504 inventory/topology combinations, section
  pointers, allocation boundaries, malformed inputs and disabled recovery.
- `ads_materialize.gpr`: complete 16 MiB comparison, nonzero array origin,
  zero padding/reserved storage, and unchanged destination on rejection.

Each executable is `build-ads-NAME/ads_NAME_tests` for NAME `header`,
`initialization`, or `materialize`. Use the Nix environment and a 64 MiB
host test stack (`ulimit -s 65536`) for the materialization fixture; production
receives existing backing and does not allocate that host-test array.
Prove `intel_gpu_ads_header.adb`, `intel_gpu_ads_initialization.adb`, and
`intel_gpu_ads_materialize.adb` through their respective projects with
`--level=2 --checks-as-errors=on`.

The writer checks copy bounds, clears supplied CPU backing, and copies the five
initialized sections. Its caller must supply an authentic preparation result
and exclusive writable memory. Neither these checks nor a successful write
establish GGTT ownership, GPU cache visibility, valid golden-context contents,
or permission to publish the ADS to firmware. Native driver binding remains
separate work.
# Native ADS hardware observation

`dma_cache.gpr` / `build-dma-cache/dma_cache_tests` exercise the shared x86
CLFLUSH wrapper on a mapped aligned host page and reject invalid extents.
This requires host CLFLUSH support and does not prove GPU visibility. Native
ADS initialization now materializes into retained DMA backing and uses that
wrapper, but remains uncalled until an owned GGTT extent is available. Numeric
range checks do not replace WOPCM pin-bias, firmware or scanout admission.

The ADS/publication integration regression uses the real ADS composer and
materializer with modeled PTE callbacks. It checks that a dynamically selected,
retained GPU extent (skipping an existing claim) supplies ADS pointers before
any PTE writes. This does not validate native cache visibility or GGTT MMIO.
The 16MiB hosted fixtures need a larger stack:

```
nix develop -c bash -c 'gprbuild -P tests/intel-gpu/ads_materialize.gpr && ulimit -s 65536 && tests/intel-gpu/build-ads-materialize/ads_publish_tests && tests/intel-gpu/build-ads-materialize/ads_materialize_tests'
```

`nix develop -c gprbuild -P tests/intel-gpu/ads_observe.gpr` builds the
`build-ads-observe/ads_observe_tests` hosted regression. It checks admission
without MMIO, exactly two ordered samples of topology and doorbell registers,
all 256 encoded doorbell capacities, invalid reads, sample changes and invalid
topology. Native reset captures the same observation only after completion,
with its forcewake reference retained. Captured values feed the future ADS
initialization path; this is not ADS publication or evidence of working GuC.
Tests verify callback behavior, not hardware power, MMIO ordering or atomicity.
