# Grant list walks without allocation

Hosted test, proof and mutation checks for `kernel/src/grant_sweeps.ad[sb]`:
how the kernel walks a process's owned or received grant list
(`Process.IPC.Sweep_Grants`) while each visit may unlink grants, others on
the same list included, link new ones, or walk again.

Process teardown walks these lists, and an out-of-memory kill reaches
teardown exactly when no page is free. The old walk took a page of scratch
memory for a snapshot of the list and panicked the kernel ("No memory to walk
a grant list") when there was none. Each walk now keeps a cursor in its own
stack frame; unlinking a grant (`unlinkReceived`, `releaseOwnedSlot`) first
moves every cursor resting on it to its successor (`Pass`). Nothing is
allocated and no depth is fixed.

Run in the Nix shell, from the repository root:

    tests/grant-sweeps/run.sh            # hosted, random walks
    systemd-run --user --scope -p MemoryMax=24G -p MemorySwapMax=0 \
      tests/grant-sweeps/run.sh --prove  # plus gnatprove level 2 (model)
    tests/grant-sweeps/mutations.sh      # each mutation must be caught

Hosted (`main.adb`, the production unit): grants on two lists at once
(owned and received), visits that unlink the visited grant, its successor,
or any other grant, link new grants, and start nested walks on the same or
another list. Every walk is checked: each visited grant is on the walked list
and was there when the walk began, none is visited twice, and every grant on
the list throughout is visited.

Proved (`sweep_model.ad[sb]`, level 2, 26 checks, 0 unproved, 2026-10-18):
the protocol on a model list linked through arrays, with ghost-style ranks
increasing along the list. A cursor always rests on a listed grant or at the
end; a step visits a grant the walk has not visited, even one unlinked and
linked again meanwhile. Not proved: termination (each walk visits at most
once each of finitely many grants, and grants linked meanwhile go behind
it), and that the model matches `Grant_Sweeps` (tested above).

Mutations (4) all caught: see mutations.sh. The guest regression is the
headless `oom-reclaim` test (`userspace/apps/oom-check`): a process holding
1100 grants exhausts memory, is killed for it, and is reclaimed with no page
free.
