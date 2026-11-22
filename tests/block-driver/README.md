# Block driver contract (DEV-002)

Linux-hosted regression tests for `CuBit.Block_Driver`, the asynchronous
generic every Block.Device.V1 provider instantiates (docs/device-classes.md):

```sh
nix develop -c bash -c 'cd kernel && alr exec -- gprbuild -p -P ../tests/block-driver/block_driver_tests.gpr && ../tests/block-driver/build/main'
```

The production generic and `CuBit.Block_Requests` are compiled unchanged. The
fixtures replace only the kernel grant API (several grants held at once) and
`CuBit.Block_Driver_Runtime` (a scripted request queue, a settable clock, a
log of every answer). A fake four-slot device lets the test decide when each
command finishes and how.

Covered:
- refusal before Start;
- the writer's and the reader's Describe;
- counted refusal of unauthorised writes;
- write-then-read through kept replies, exact block extents;
- four requests in flight with a fifth waiting for a slot;
- a slot answering its own request;
- short and failed transfers reporting zero bytes;
- deadline expiry with cancel;
- refusing at once, with the grant returned, when the reply can't be kept;
- unadmitted and foreign grants;
- passing and failing flushes;
- OP_IDENTIFY_DEVICE;
- a synchronous device answered at once without a kept reply, through
  Poll and through the blocking `Wait_And_Serve`.

Mutation checks (each made the run fail, 2026-10-30):
- dropping the grant return when the reply can't be kept;
- reporting a short transfer's bytes;
- ignoring write authority;
- disabling the synchronous fast path.

These are regression tests, not SPARK proofs, and not native IPC; the UFS
headless and laptop runs cover the native path.
