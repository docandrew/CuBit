# Native DMA-retention fixture

## Demand-backing oracle (native QEMU pass, 2026-10-02)

`demand_backing_check.adb` is a separate disposable-VM supervisor using the
production extent allocator, record growth and actual CuBit DMA/owned-memory
syscalls. It is installed as `devmgr.svc` **only in a private minimal initrd**;
the kernel's existing supervisor authority is used without weakening checks.
No normal stage-1 files or user disks are modified. Run under Nix and the shared
build lock:

```sh
flock --exclusive --nonblock coordination/build.lock nix develop -c \
  bash tests/intel-gpu/native/run-demand.sh
```

The runner records the existing kernel binary's hash rather than rebuilding it.
It creates isolated build/image/log outputs in `tests/intel-gpu/demand-backing.*`.
It checks 17 allocations, at most one new 2 MiB block per step, 18 MiB committed
backing, 4,112 page sentinels in separate write/read passes, metadata extension,
slice reuse, stale retirement rejection and owner-loss denial. The VM receives
no GPU mappings or external buffer loans; this is CPU mapping/lifetime evidence,
not GPU DMA, allocation IPC, Mesa rendering or display retirement evidence.
Native run `tests/intel-gpu/demand-backing.PnS7R8/serial.log` passed on four-CPU
QEMU with 512 MiB RAM. Kernel SHA256:
`e5918f700edb9f26609db7f81c139716408937f5678d706e94336ffd0ec7f98c`.
Fixture SHA256:
`405a59057e5dc2a93c3ef08af3df7015271e636881c447bc1fd3bc7812fd8c7e`.
The owner-loss branch changes the allocator callback to unavailable; it does
not kill/revoke a real process. The slice's preserved contents test same-owner
reuse, not clearing before cross-client exposure. Explicit large-page DMA mode
is requested; the oracle checks CPU access, not page-table leaf encoding.
Initial attempt compiled but failed to link because the test project omitted
the native Builder `-nostdlib` flag; the corrected project passed compile,
link, image creation and the complete runtime oracle.

## Saved-capability allocation loopback (native QEMU pass, 2026-10-02)

Run the same command with an additional `ipc` argument. This selects
`allocation_ipc_check.adb`, composing production `Buffer_Memory`,
`Allocation_Growth`, `Extent_Allocator`, metadata growth and extent decoding
with actual kernel async submit/receive/completion syscalls. The supervisor
side uses saved reply slot 58, as devmgr does. A second request is received and
answered while the first allocation remains pending; its reply must not replace
the saved allocation reply. Each saved reply is consumed exactly once, and an
immediate attempt to reuse it must fail.

Four-CPU QEMU passed 17 saved allocation replies, three extent-address queries
(only the missing suffix is fetched), one interleaved request, metadata growth
on both sides, zero/flush/readback, and preservation of earlier buffer sentinels.
The driver observed backing snapshots growing from 4 MiB to 6 MiB. Evidence:
`tests/intel-gpu/demand-backing.1CNi90/serial.log`. Kernel hash is the same as
the memory-only run above; fixture SHA256:
`cb480c0cb1978f9fe5cf3d64519198bb040211843a6434d2175c6279ef9bb3d6`.

This is real IPC but **one privileged process with a self endpoint**. The test
router mirrors the relevant devmgr branches; it is not the complete devmgr
binary. It does not prove cross-process isolation, real endpoint death/revocation,
malicious-client admission, or hardware execution. No production capabilities,
staging files or disks are changed.

The current IPC fixture extends this gate to **18 physical extents (36 MiB)**,
17 saved replies and one interleaved request. Its explicitly configured 64 MiB
test policy forces both supervisor and driver extent directories past their
sixteen-entry bootstrap storage. It asserts supervisor capacity growth and
observes the driver's metadata-wait state, in addition to bounded physical
steps, clearing/readback and earlier-buffer preservation. Production devmgr
uses the same `Extent_Growth` adapter; its live policy is still 32 MiB.
Four-CPU/512 MiB QEMU passed in `demand-backing.dlureW`; exact kernel and fixture
hashes are in its `input.sha256`, with the full oracle in `serial.log`. The
runner now requires the eighteen-extent marker, not the historical three-extent
marker above. This remains allocation/IPC evidence, not Intel rendering.

## Historical DMA-retention and grant fixture

Root-grant large-leaf resolution has now been promoted to the main kernel.
Historical references below to private-only kernel behavior describe the
staged validation runs. Event `4D55` is an optional sacrificial permission test:
grant the owner a self endpoint at slot6, then send that event after regrant.
It acquires a read-only alias, verifies the sentinel, and writes through it.
Require the read marker followed by a user write-protection fault; reject
`FAIL readonly large alias write returned`. This run intentionally stops before
the full lifecycle/quota PASS and must use a separate runner completion gate.

Current extension (private kernel, not yet promoted): Borrow also receives a
`Large` Boolean. Send label `4D53` for that child; it verifies all 8192 CPU
offsets, then grants the last 4 KiB of its first 2 MiB mapping read-only.
The supervisor must verify the sentinel and hold the acquisition through owner
exit, then return it. Run `/tmp/cubit-usb-live.npfh_jcq` passed this with a
private `createGrant` root-walker `Allow_Big => True` change. Require
`dma-retention: large CPU8192 PASS` and two deferred-loan-return markers.
The main kernel still rejects large-leaf root grants; this extension requires
that experimental kernel until read-only fault/revocation gates are complete.
Live-owner revocation now passes in `/tmp/cubit-usb-live.42nzc743`: after
verifying the first large-page loan, Borrow returns its acquisition and sends
event label `4D54`. The owner revokes, confirms retirement, checks all 8192
offsets remain intact, and publishes a new reference using reply `4D52`.
Borrow acquires and verifies this second reference before returning it to the
outer owner-exit test. Require `dma-retention: live revoke owner8192 intact PASS`.
Read-only write-fault testing remains outstanding.
The rejected-grant run described below is historical, not the current fixture.

DMA_Retention_Check is a generic procedure compiled with the CuBit runtime,
not a Linux test. Instantiate it inside a privileged test supervisor with a
Spawn callback returning a fresh suspended child. The supervisor needs process
read/write/grant authority. Borrow must resume the first child, obtain a
generation-checked grant of its DMA page, acquire it read-only and verify a
sentinel before returning success. The test kills that owner while the loan
is held, checks that a new child cannot reuse its PID, and returns the loan
before continuing allocation checks. The second allocation owner is also
resumed after all sixteen blocks have been mapped in explicit large-page mode.
Borrow returns failure for its deliberately invalid grant reply; the runner
must separately require the child's large-page PASS marker, so a timeout or
unrelated grant failure cannot count as success.
The test kills every successfully tested
child; it consumes the entire 64MiB retained-DMA boot budget. Use a fresh test
VM, never run it as normal startup policy.

The private devmgr fixture instantiates this with a map-check.app spawn and
runs it only when that test binary is in the bootstrap archive. QEMU runner
requires the PASS marker and rejects FAIL markers. Thirty-two 2MiB retained ranges
must be disjoint after each owner disappears from the process list. Failed
reservations, unknown modes/orders, exhaustion across owner exits, and an
ordinary allocation outside retained ranges are checked.

The current fixture allocates sixteen order9 blocks per owner, for two owners,
at adjacent CPU virtual addresses. It does not require physical adjacency.
The first owner uses mode1 (4 KiB CPU leaves), the second mode3 (2 MiB CPU
leaves). In run `/tmp/cubit-usb-live.zowru7oa`, the second child wrote distinct
values to all 8192 constituent 4 KiB offsets, read them back in a separate pass,
and attempted 512 actual one-page grant syscalls, all rejected. Required marker:
`dma-retention: large CPU8192 and rejected grants512 PASS`.
Both owners then passed the existing exit/retention/quota checks. This verifies
native user CPU access and grant rejection, not GPU page tables or GPU DMA.
It rejects order14 and Unsigned_64'Last. This multi-block revision passed native
four-CPU UEFI QEMU in private workspace `dma-extents-test-qq5k9br0`, run
`/tmp/cubit-usb-live.j2f7_bg6`: both owners held sixteen blocks concurrently,
retention survived exit/deferred CPU loan, and the 64MiB quota remained spent.
The allocations need not be physically scattered, so this does not establish
fragmented-pool behavior or GPU DMA correctness. The
historical run below tested order12. Kernel admission was corrected from >=maximum to >maximum
after verifying both allocator paths include the maximum free list.
This revision passed four-CPU UEFI QEMU in isolated workspace
`.build-workspaces/dma-max-order-r37_25jh`, run `cubit-usb-live.yxpwf9ka`.
It verifies four16MiB allocations below4GiB, disjoint retained backing across
owner exit, the deferred CPU loan, failed-request quota rollback and exhausted
64MiB quota with ordinary allocation still available. No GPU DMA is exercised.
The older runs below used eight8MiB allocations.

This is bounded regression evidence, not an exhaustive allocator proof or a
GPU DMA test. The deferred CPU-loan extension passed in private native QEMU
run `cubit-usb-live.xiopwdp2`; concurrent allocation/exit remains a separate test.
No device is pointed at these allocations.

Constrained extension passed native4GiB QEMU run `cubit-usb-live.ya3c1fm0`:
before every8MiB retained allocation, ceilings1,4095,8MiB-1 fail. All eight
successful blocks are aligned and fit entirely below4GiB; the first still
exercises deferred CPU-loan return. Reaching the full64MiB quota establishes
that these failures did not leak that quota. Ordinary constrained allocation
also rejects4095 and succeeds below4GiB after retained quota exhaustion.
These impossible limits exercise early rejection, not fragmented-pool search
exhaustion or concurrent free/reallocation; those remain separate coverage.

`child-main.adb` is the test child's main (install as `main.adb` in the
private map-check app). Its start event has no sender identity; the response
uses endpoint slot 5 with capSubmit, not a reply to the event's NO_PROCESS
sender. The supervisor must grant slot 5 to itself, authenticate the received
request's caller against the spawned PID, validate the encoded reference,
then acquire and check the sentinel. No completion token is requested: the
fixture deliberately kills the child while the acquisition remains held.
## Native CPU-export retention

Run under Nix and the shared build lock:

```sh
flock --exclusive --nonblock coordination/build.lock nix develop -c bash tests/intel-gpu/native/run-demand.sh views
```

This boots a privileged disposable fixture, not the desktop. Production buffer
allocation, handles and views use real kernel self-grants. Three cycles check
read-only access, shared sentinel contents, two reader pins after name closure,
delayed retirement until acquisition return, final release and stale-reference
rejection. No GPU mappings or cross-process isolation are tested. The existing
kernel and fixture hashes are recorded; production staging remains untouched.
Passing evidence: `../demand-backing.kZQEfo/serial.log` and `input.sha256`.
The later `../demand-backing.Uq6uOa/` run additionally holds a forwarded terminal
child across root revocation and parent return. Write escalation/re-forwarding
are rejected; the child's return is required before the root's BO pin can drop.
The later `../demand-backing.XJMKqq/` run additionally drives retirement through
the production pending-only FIFO. Eight polls retain the acquired parent/child;
returning the parent still waits for the child. Returning the child completes
exactly once, and 32 subsequent polls do not replay the callback. The second
independent view still pins the backing. This passes across three cycles with
real kernel grants, not GPU completion or cross-process isolation.
The runner requires the stronger queued-retirement completion marker.
