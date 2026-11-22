# Filesystem change events

`CuBit.Filesystem_Events` (the event-ring record codec, userspace/runtime/gnat)
and `Watch_Reserve` (filesystem.svc's room rule for a client's event ring,
userspace/services/filesystem): docs/filesystem-protocol-v2.md step 4.
Hosted, Linux:

```sh
export TMPDIR=/home/doc/cubit-build-tmp
nix develop -c bash -c 'cd kernel && alr exec -- gprbuild -p -P ../tests/filesystem-events/events.gpr && ../tests/filesystem-events/build/main'
nix develop -c bash -c 'cd kernel && alr exec -- gnatprove -P ../tests/filesystem-events/events.gpr -u cubit-filesystem_events.adb watch_reserve.adb --level=2 --report=all -j4'
```

The tests check relative-path validation, 20,000 encode/decode round trips
with truncated and damaged records refused (garbage never faults), and a
200,000-step model of watches, events and a slow reader: the reserve
invariant (room for one Rescan_Needed per Watching watch) always holds,
a watch whose Rescan_Needed was read never stays behind when it could
watch again, and an owed record (Rescan_Needed or Watch_Ended) goes in as
soon as there is room. The proof (level 2) covers no run-time errors in both units,
Decode accepting only well-formed records, and every Decide, Settle,
Force_Rescan and End_Watch keeping the invariant (77 checks). The guest test
is storage-check's `QUEUE-EVENTS-CHECK` (headless storage-grants).
