# intel-gpu session and GuC context recycling

Hosted tests, proof and mutation checks for recycling intel-gpu's render
sessions and GuC context IDs, and for growing its per-session tables on
demand instead of a fixed 16 slots. Linux-hosted: none of this is Intel
hardware evidence.

Run in the Nix shell, from the repository root:

    tests/intel-gpu-recycling/run.sh          # hosted tests (and the changed intel-gpu ones)
    systemd-run --user --scope -p MemoryMax=24G -p MemorySwapMax=0 \
      nix develop -c gnatprove -P tests/intel-gpu-recycling/prove.gpr -j2 \
      --level=2 --report=fail --checks-as-errors=on
    tests/intel-gpu-recycling/mutations.sh    # every mutation must be caught

## Design in brief

- A session ends by `Close_Own` (Mesa's device teardown), by its queue
  channel closing, or by its client dying: the kernel's grant-end notice
  for the queue channel (`Handle_Grant_Event`). The queue channel is the
  session (docs/capability-handles.md section 4). `Close_Own` and the
  retirement query are idempotent for retired and recycled sessions,
  because Mesa closes its queue before it calls `Close_Own`.
- The driver's existing retirement then runs (buffer and map sweep,
  scheduling disable and GuC deregistration, context image GGTT release,
  context parent and table retirement, teardown of the session's BOs).
- `Recycle_Retired_Sessions` frees a slot once all of that is done: the
  session table (`Intel_GPU_Render_Sessions.Recycle`) requires a retired
  session that holds nothing (its GuC context hold ends only with the
  GuC's DEREGISTER_DONE; its image hold only when the GGTT range is
  released) and the release facts (no allocation charged to it, no CPU
  grant outstanding, queue forgotten, no operation in progress names it).
  Then the context ID and record are released (`Intel_GPU_Context_Table.Release`,
  which also requires the acknowledged deregistration), the queue forgotten,
  the session's metadata reservations and client account returned, and
  every per-session table resets the slot. Tags never repeat.
- A session slot's GuC context ID is fixed (`Context_ID (Slot)`), so slot
  recycling is guc_id recycling.
- Capacity: every per-session table reserves address space for the whole
  GuC context-ID space (65,527 sessions) and commits pages only as the
  table grows (`Intel_GPU_Slot_Storage`, owned reservations), by
  `Growth_Step` slots when admission finds no free slot. Memory, not a
  table size, is what runs out; then admission is refused.
- G2H reply credits are taken per request from one pool (the G2H ring less
  i915's unsolicited reserve): a MODE_SET or DEREGISTER that finds no room
  waits with back pressure (`Intel_GPU_G2H_Credits`), replacing the
  compile-time bound of 16 contexts.

## What is checked

- `session_lifecycle_tests`: 20,000 random launches (at most 40 live) with
  activation, aborts, `Close_Own`, transport end and lost replies, the GuC
  and GGTT acknowledgements arriving late in any order, recycling tried at
  every step. Live sessions never share a slot or GuC context ID; a slot is
  never reused before its previous session's context was acknowledged
  deregistered; recycling succeeds exactly when everything is released;
  tags never repeat; stale tags never resolve and close as finished; the
  table grows only when full (to 40 slots); nothing leaks at the end.
- `context_recycling_tests`: the production context table, GuC session and
  lifecycle against a modelled GuC answering MODE_SET and DEREGISTER late:
  IDs are released only after DEREGISTER_DONE (or when the GuC never saw
  them) and reused; answers in flight never exceed a three-answer pool,
  requests wait with back pressure, and the pool accounts exactly the
  answers owed.
- `slot_storage_tests`: commit on demand, defaults on new and reset slots
  (including fields without defaults), refused commits grow nothing,
  committed slots never move.
- `client_account_tests`: finished sessions' budget accounts are forgotten
  only when closed and charged nothing; every other account stays exact.
- `test-recycle-glue.py`: the native driver's `End_Client_Session`,
  `Handle_Channel_Close`, `Handle_Grant_Event`, `Try_Recycle` and
  `Recycle_Retired_Sessions`, extracted from main.adb, against the
  production session table with every hardware observation modelled.
- `render_sessions_tests`, `render_control_tests` (tests/intel-gpu):
  exhaustive five-step model sequences with recycling, authority and close
  semantics.
- Mutations (25): each removes one rule (recycling guards, holdings, tag
  issue, context release, credit checks and accounting, account forget,
  storage initialization and growth, close semantics, the driver glue).

## Proved

Level 2, 309 checks, 0 unproved: `Intel_GPU_Render_Sessions`,
`Intel_GPU_Render_Control`, `Intel_GPU_G2H_Credits`. Over a ghost model of
the table, every operation's frame is stated: only `Acknowledge_Deregistration`
ends a slot's GuC context hold, `Recycle` frees only a retired slot that
holds nothing and whose release facts all hold, and `Reserve` takes only a
free slot, changing no other. Since a slot's GuC context ID is fixed, no
GuC context ID is reused before the GuC acknowledged its deregistration.
Free slots hold nothing (type invariant). The pool never accounts more
than its capacity. Not proved: the driver glue that feeds the table its
facts (tested above), the context table (tested), the storage layer.
