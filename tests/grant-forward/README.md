# Native three-party grant forwarding

Run under the shared build lock, using Nix:

```
make -C kernel grant-forward cubit_kernel
bash tests/headless/run.sh --test grant-forward --accel tcg,thread=multi --cpus 4 --timeout 60 --keep-logs
bash tests/headless/run.sh --test grant-forward-intermediary-exit --accel tcg,thread=multi --cpus 4 --timeout 60 --keep-logs
bash tests/headless/run.sh --test grant-forward-owner-exit --accel tcg,thread=multi --cpus 4 --timeout 60 --keep-logs
bash tests/headless/run.sh --test grant-forward-desktop --accel tcg,thread=multi --cpus 4 --timeout 60 --keep-logs
```

The fixture boots actual CuBit owner, intermediary and reader processes. Its
owner and reader reuse the existing **test-only** IPC/CCL-host registration
identities in an isolated startup profile; neither ordinary async-ipc nor the
real CCL test host is started in this profile. Production service authority
policy is unchanged.

The intermediary acquires an owner-forwardable root, derives its second page
read-only to the reader, then returns its root acquisition. The first page has
a different pattern to detect wrong offsets. Root revocation must
remain pending while the reader retains a readable child. New acquisitions must
fail after revocation. The owner changes the backing bytes after revocation;
the reader must see the new bytes through its retained mapping, not a snapshot.
Returning the reader acquisition, or exiting the reader,
must retire the child and finally release the root hold.

Two additional isolated profiles exercise exit with a retained reader:

All three profiles passed native CuBit in four-CPU QEMU TCG on 2026-09-30.

- `grant-forward-intermediary-exit`: the intermediary exits with its root
  acquisition and child grant still live. The reader waits for admission to
  close, verifies its retained page, returns it, then asks the surviving owner
  to confirm root retirement.
- `grant-forward-owner-exit`: the owner changes its backing page and exits.
  The reader waits for admission to close, sees the changed bytes, and returns
  its acquisition. The surviving intermediary confirms child retirement. This
  checks retained backing and child cleanup, not subsequent owner PID reuse or
  physical allocator reuse.

Admission probes return every temporary successful acquisition; the original
reader acquisition stays held throughout. Polls are bounded and do not treat
an exit reply alone as proof that teardown has happened. Each scenario emits
its own PASS marker only after these checks.

Negative cases include ordinary nonforwardable roots, missing acquisition,
write/range escalation, stale parent generations, unknown syscall flags,
missing recipient authority, a full-width invalid offset, overlong child
acquisitions, and attempting to forward a terminal child. The pass marker is emitted only after
all checks complete. This is CPU shared-memory integration, not Intel GPU
execution, hardware DMA quiescence, or a proof of the native kernel adapter.

Remaining coverage: deferred owner PID/frame reuse, stale recipient
capabilities, slot exhaustion, injected native allocation failures,
and concurrent derivation/revocation stress. Hosted mapping failure tests are
in `tests/grant-loans`; their callbacks do not emulate real CPU TLBs.

`grant-forward-desktop` replaces the synthetic reader with the real Desktop
and display services, using the native Mesa presentation bridge. A synthetic
owner supplies completed linear RAM pixels (not Intel-rendered output). The
app verifies that a foreign-owned root cannot be attached directly, derives a
read-only child, attaches it, and returns its root acquisition. Root revocation
and child retirement must remain pending across attach/present acknowledgments.
Destroying the surface must release Desktop's acquisition and allow both child
and root retirement. This checks actual compositor attachment/lifetime behavior,
not rendered pixel readback, Mesa WSI integration, GPU execution or a GPU fence.
This fourth profile passed four-CPU native CuBit/QEMU TCG on 2026-09-30.

The fixture also links the production C presenter lifecycle adapter. Its
synthetic owner implements the `0A23` presentation-map/retire wire exchange,
then the C adapter runs real acquire/derive/attach/return/revoke calls. Cleanup
must stay pending while Desktop retains the surface, complete after destroy,
and remain idempotent. This exercises the C ABI and orchestration, not Intel
buffer allocation or GPU synchronization.
