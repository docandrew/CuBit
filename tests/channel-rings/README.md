# Channel rings

`CuBit.Channel_Rings` (`userspace/runtime/gnat/cubit-channel_rings.ad[sb]`)
keeps the index bookkeeping for one direction of a shared byte ring. It is
used by netstack and by network channel clients (see "Async channels" in
`docs/netstack-redesign.md`). Each side keeps its own index privately and
accepts the peer's index only if it moves forward within the ring.

```sh
nix develop -c bash tests/channel-rings/run.sh            # host tests
nix develop -c bash tests/channel-rings/run.sh --prove    # plus gnatprove, level 1
nix develop -c bash tests/channel-rings/mutations.sh      # mutation check
```

- **Proved (level 1, no unproved checks):**
  - every slice lies inside the ring, and every copy stays within its
    buffers;
  - the fill never exceeds the ring's size;
  - committing or consuming moves the private index by exactly N;
  - a peer index is accepted exactly when it moves forward without
    overfilling (producer: releasing no more than was committed), and a
    rejected index changes nothing.
- **Tested (host):**
  - byte streams arrive in order and intact, across 32-bit index
    wrap-around;
  - copies and zero-copy slices are mixed, with random sizes, for ring
    sizes of 4 KiB, 64 KiB and 1 MiB;
  - 200,000 hostile peer indices per size are judged against an
    independent formulation of the rules.
- **Layout mirror (`layout-check.py`, run by `run.sh`):** every constant
  of `CuBit.Net_Channel_Layout` has the same value in
  `userspace/c/cubit_net_channel.h`, and C has no extra ones.
- **Datagram rings (`CuBit.Datagram_Rings`, proved):** every access stays
  in the ring and the caller's buffer; a datagram is put whole or not at
  all; a take returns at most the caller's buffer and reports truncation;
  malformed headers from the peer are reported. Tested: random datagrams
  across wrap-around arrive intact and in order; 50,000 random rings of
  hostile headers never read out of bounds. The check that a pad record is
  wholly published before it is skipped is defensive (a partial pad cannot
  cause an out-of-bounds access), so no mutant covers it.
- **Slot rings (`CuBit.Slot_Rings`, generic, proved through the
  instances `slot_ring_small.ads` and `slot_ring_frames.ads`):**
  - every slot is inside the ring and the fill never exceeds the slot
    count;
  - a peer index is accepted exactly when it moves forward without
    overfilling, and a rejected one changes nothing;
  - pushes and takes move the private index by one and touch only their
    slot;
  - indices less than a ring apart never share a slot (`Lemma_Distinct`,
    `Lemma_Free_Slot`), so an element in flight is never overwritten.

  Tested: a 4-slot ring carries a value sequence in order across 32-bit
  wrap-around, with random batches and hostile indices judged against an
  independent formulation. The 128-slot frame ring's slots are distinct
  around the wrap.
- **Frame rings (`CuBit.Frame_Rings`, proved):** the driver <-> netstack
  packet grant (header page, then 128 receive and 128 transmit slots):
  every slot lies inside its area.
- **Queue pairs (`CuBit.Submission_Queues`, generic, proved through
  `queue_small.ads`):**
  - the service owes at most as many answers as the completion ring has
    room for (`Valid`), so an answer never waits and is never dropped;
  - the client never has more requests outstanding than completion
    slots;
  - an answer with no pending request is refused;
  - each answer carries its request's token.

  Tested: 400,000 random submit, take, complete and reap steps pair every
  answer with its request.
- **Mutation check:** 32 plausible bugs (13 byte-ring, 4 datagram, 9 slot
  ring, 6 queue pair), all fail to prove, plus a control that proves.
- **Results (2026-09-28, clean run):** 496 checks proved at level 1.
- **Not covered:** SPARK does not model memory ordering between the
  processes. Each index must be read once from shared memory into a local
  value, and fences must be placed around the indices and notification
  flags. That code belongs to the callers.
