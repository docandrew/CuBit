# Advisory file locks

`File_Locks` (userspace/services/filesystem): which open handles hold a
file shared or exclusively, which wait, and in what order they are granted
(`Queue_Lock`, flock(2) semantics; `Fs_Server.Locks` applies it to the
handle table). Hosted, Linux:

```sh
export TMPDIR=/home/doc/cubit-build-tmp
nix develop -c bash -c 'cd tests/filesystem-locks && gprbuild -q -p -P locks.gpr && build/main'
nix develop -c bash -c 'cd kernel && systemd-run --user --scope -p MemoryMax=24G -p MemorySwapMax=0 \
  alr exec -- gnatprove -P ../tests/filesystem-locks/locks.gpr -u locks_proof.ads --level=2 --report=all -j2'
```

The tests run fixed cases (shared beside shared, exclusive conflicts, an
exclusive waiter not overtaken by later shared requests, conversion
releasing first, deadlines, a waiter's handle going) and 2,000 random
sequences of 300 requests, releases, closes, cancelled waits and expiries
over 8 handles and 3 files, checked after every step against properties
computed independently of the unit: holders never conflict, the waiter
count is exact, a request is granted at once exactly when it is compatible
and nobody waits for the file, Settle grants only the oldest waiter and
leaves none that could be granted, Expire ends only passed deadlines and
leaves none, Cancel_Wait keeps the lock.

2026-11-02: `FILE-LOCKS: 2601631 checks PASS`. Mutants (each must fail):
dropping the "nobody waits" condition (19,323 failures), granting any
waiter instead of the oldest (6,530), not counting a wait's end (444,345),
expiring 1 ms early (1,342). GNATprove level 2 on the service's instance
(2,048 slots): 30 checks, 0 unproved (no run-time errors; every operation
keeps `Consistent`: two handles hold one file only when both hold it
shared).

The guest tests are the fs-coherence `lock-*` cases (tests/fs-coherence),
checked against Linux's own flock first (`host.sh`).
