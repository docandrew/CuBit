# Native DMA-retention fixture

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
