# Grant bells (CAP-002 step 3)

Hosted tests, proof and mutation checks for the kernel's channel wake
bookkeeping: `kernel/src/grant_doorbells.ad[sb]` (SPARK: a grant's bell,
pending and ended) and the bell list `kernel/src/grant_bell_lists.ad[sb]`
(generic over the grant records' link fields). A channel wake is a
labelled one-way `SEND_EVENT` (no new system call; label and words in
`shared/capabilities/channel_wake_abi.ads`). Design:
docs/capability-handles.md section 4.

Run in the Nix shell, from the repository root:

    tests/grant-doorbells/run.sh            # hosted, random steps
    systemd-run --user --scope -p MemoryMax=24G -p MemorySwapMax=0 \
      tests/grant-doorbells/run.sh --prove  # plus gnatprove level 2
    tests/grant-doorbells/mutations.sh      # each mutation must be caught

What the hosted test checks after every step (create, ring, take, end)
against a model: each process's bell list is the FIFO of its rung grants
(order, reciprocal links, tail, count); a bell is pending exactly while its
grant is listed; repeats coalesce; a ring after the end is refused.

Proved (level 2, 21 checks, 0 unproved, 2026-10-17): `Ring` (links
exactly when newly pending, refused after `Finish`), `Take` and `Finish`
(unlink exactly when it was pending); repeats between takes are taken once
(`Lemma_Ring_Taken_Once`), never lost and never doubled.

Not covered here (kernel glue in Process.IPC, tested in the guest by
async-ipc): the owner check under grantLock, unlinking at every grant end,
setting the process's bell bit and waking its waiter. Mutations (6) all
caught: see mutations.sh.
