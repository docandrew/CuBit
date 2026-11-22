# GuC CT transport coverage

Run in the Nix development environment:

```sh
gprbuild -p -P tests/intel-gpu/guc_ct_send.gpr
tests/intel-gpu/build-guc-ct-send/guc_ct_send_tests
gprbuild -p -P tests/intel-gpu/guc_ct_receive.gpr
tests/intel-gpu/build-guc-ct-receive/guc_ct_receive_tests
```

These are hosted callback tests, not firmware execution tests. Both projects
enable assertions and overflow checks. The separate CTB planner has SPARK
contracts; that does not prove the callback implementations or device ordering.

## Sending

The driver checks the descriptor against its retained producer cursor, writes
the complete frame, makes those writes visible, publishes the tail, makes that
publication visible, then notifies firmware. Tests cover wraparound, every
callback failure (including callbacks that mutate memory before reporting a
failure), full-ring retry, malformed descriptors, local input rejection, and
twenty consecutive sends. No uncertain message is automatically replayed.

## Receiving

The driver validates a descriptor snapshot before reading the header. It copies
the complete frame into local storage before ordering its reads and publishing
the new head. Only a successful final visibility callback exposes the copied
message. Empty rings can be polled again; a truncated published frame breaks
the channel. Tests overwrite ring storage during head publication to check
that the returned message remains independent. Each callback failure, invalid
descriptor, invalid format/reserved bits, zero length and truncated frame is
also checked. Failure returns a zeroed message, never a partial one.

## Native integration requirements still outstanding

- Bind callbacks to retained, registered DMA backing with the correct memory
  attributes, cache maintenance and acquire/release ordering. A compiler fence
  alone is not the implementation of these contracts.
- Validate all descriptor reserved words. Firmware owns H2G head and G2H tail;
  the driver retains and checks H2G tail and G2H head.
- Establish authenticated firmware readiness and register both rings before
  initializing either channel. The `Registered` argument represents this
  trusted local evidence, not data an app can supply.
- Serialize channel access and prohibit callback reentry. Callbacks are bounded
  and nonraising; exception recovery is not implemented by these wrappers.
- Validate HXG origin/type/action/length and match response fences before
  dispatch. CT framing alone does not authorize or validate a GuC action.
- Track request fences, response credits and outstanding request lifetimes.
  `Queued` means publication, not acknowledgement or GPU execution.
- Treat `Broken` as a transport failure requiring coordinated recovery. Retain
  backing until hardware quiescence is established; do not reinitialize the
  same object or silently reissue uncertain requests.

These helpers are not yet connected to native GuC registration or submission.
They do not establish working hardware-accelerated 3D.
