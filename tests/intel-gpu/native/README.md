# Native DMA-retention fixture

DMA_Retention_Check is a generic procedure compiled with the CuBit runtime,
not a Linux test. Instantiate it inside a privileged test supervisor with a
Spawn callback returning a fresh suspended child. The supervisor needs process
read/write/grant authority. Borrow must resume the first child, obtain a
generation-checked grant of its DMA page, acquire it read-only and verify a
sentinel before returning success. The test kills that owner while the loan
is held, checks that a new child cannot reuse its PID, and returns the loan
before continuing allocation checks. Other children remain suspended.
The test kills every successfully tested
child; it consumes the entire 64MiB retained-DMA boot budget. Use a fresh test
VM, never run it as normal startup policy.

The private devmgr fixture instantiates this with a map-check.app spawn and
runs it only when that test binary is in the bootstrap archive. QEMU runner
requires the PASS marker and rejects FAIL markers. Four 16MiB retained ranges
must be disjoint after each owner disappears from the process list. Failed
reservations, unknown modes/orders, exhaustion across owner exits, and an
ordinary allocation outside retained ranges are checked.

The current fixture targets maximum allocator order12 and rejects order13 and
Unsigned_64'Last. Kernel admission was corrected from >=maximum to >maximum
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
