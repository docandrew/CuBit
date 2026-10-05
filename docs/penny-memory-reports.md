# Penny allocation diagnostics

Provision an empty `/servo/memory-check` marker in a disposable test image to
request Servo memory reports. The default browser does not enable this marker.
No additional writable directory or capability is granted to Penny.

The embedder keeps one outstanding request and uses a capacity-one channel with
nonblocking delivery and polling. Requests are spaced 30 seconds after a response.
A 60-second timeout disables further requests. The UI does not wait for reporters.
The underlying Servo reporter still controls its own collection work.

`PENNY-MEMORY-REPORT` lists the number of categories. `PENNY-MEMORY-ENTRY` prints
the largest 20, with labels limited to 180 characters and control characters removed.
These categories can overlap. CuBit currently lacks the reporter's global RSS and
system-heap totals; they are not substitutes for the separate owned-memory syscall.
Engine “decommitted” categories do not prove backing was released: the libc
MADV_DONTNEED path currently remains advisory. Collection can perturb memory use.

The initial native test exposed an interior-pointer error: weak-referenceable DOM
objects use Rc, but codegen selected the Box sizing callback. `patch_servo.py` now
selects an Rc-aware callback, borrowing the existing reference with ManuallyDrop
and delegating to the existing Rc sizing trait. Plain Box objects retain their
original callback. `tests/servo/test_rc_dom_size.py` checks the helper with an
instrumented allocator/trait adapter, including alignment, reference lifetime and
a negative control. Native validation separately exercised the complete engine
with YouTube loading, resize/menu interactions, four reports, tab closure and
process/network retirement. This does not establish a fix for the original
unprofiled user click abort.
