# Grant lifecycle events and control messages

Guest test for docs/data-plane.md ("The unit is the grant", "Control
messages"): `tests/headless/run.sh --test control-events`.

`control-check.app` lends `control-child.app` a one-page outlet ring. The
grant is created with the notify flag (`CuBit.Launching.Lend_Ring`), and
procmgr derives the child's grant from it, so the derived grant inherits the
flag. The launcher then:

1. sends a Stop to PID 1, which it did not launch: refused by the kernel;
2. revokes the ring: the kernel revokes the derived grant with it and posts
   EVENT_GRANT_REVOKED to the child, whose runtime (`CuBit.Process_Events`,
   `CuBit.Streams.Return_Revoked`) returns the mapping and closes the outlet;
3. sends the child a Stop, which it accepts as the child's launcher;
4. checks the child exited 0 (it saw the revoke, then the Stop), and waits
   for EVENT_GRANT_RETURNED for its ring, posted when procmgr returns its
   hold after the child ends.

Before that, the child attacks `control-producer.app` (the ipc-test service,
an outlet producer reached through the manifest's `ipc_test` endpoint):

- **Flood:** it opens all 8 reader channels, fills the producer's mailbox
  with junk until the kernel refuses, then closes all 8. Their `OP_CLOSE`
  messages cannot get in; the kernel keeps each reader's end on its grant
  for the producer until read (docs/ipc-delivery.md), so all 8 slots must
  open again. Mutation check: with the owner's notice not kept
  (`setOwnerNoticeLocked` a no-op), this step fails.
- **Forgeries and bad requests:** grant-returned, grant-revoked, control and
  child-exit events sent with `SEND_EVENT` are refused by the kernel;
  malformed and mismatched opens are refused by the producer; closes of
  channels the child does not hold change nothing (its own still reads).
- **Reads** the producer's outlet through a consuming channel, then closes
  and reopens it more times than there are reader slots.

Exit codes of the child: 0 both seen, 2 the ring stayed open, 3 no Stop,
4 no ring lent, 5 the outlet could not be read, 6 an attack succeeded (or
the flood left slots taken).

Build: `tests/control-events/build.sh [out-dir]` in the Nix shell, under the
build lock, after `make -C kernel user_runtime ccl-manifest`.
