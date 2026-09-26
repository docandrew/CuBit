# ID ledger for the dynamic process and thread tables

`nix develop -c make -C kernel test-id-ledger prove-id-ledger`

`Id_Ledger` (`kernel/src/id_ledger.ads`) is the pure SPARK bookkeeping behind
the two-level process and thread tables (docs/threads.md): which IDs are in
use, each ID's generation, and when a page of records must be mapped or may
be returned. Generations are kept here rather than in the records, so they
survive page release.

**Proved (GNATprove, on the 64-ID instance in `test_ledger.ads`):** 54
checks, none unproved or justified, no `Assume`.
- `Well_Formed` (an ID in use is never retired) holds initially and is
  preserved by every operation. The first proof attempt found this invariant
  missing from the specification: `Release` promised a non-retired result
  without it.
- `Allocate` returns the lowest ID that is free, not reserved and not
  retired, or 0 only when there is none. It changes nothing else, and
  reports whether the ID's page was otherwise empty (map it first).
- `Allocate_Specific` succeeds exactly for an unused, unretired ID.
- `Release` makes the ID unused and advances its generation. At the
  `Generation_Limit` it retires the ID forever instead, like
  `Capabilities.Operations.advanceGeneration`. It changes nothing else, and
  reports whether the page is now empty (it may be returned).
- `Invalidate` advances a still-reserved ID's generation, for teardown
  while grants are outstanding. At the limit it changes nothing and reports
  saturation; the later `Release` then retires the ID.
- `Prove_Release_Invalidates`: after release, a reference made for the old
  object is not `Current`, including after the ID is reused.

**Hosted test:** 200,000 random allocate, release, invalidate and
specific-allocate operations, checked against an independent model after
every step: lowest-first choice, page empty/first signals, generations,
stale references and retirement. The test instance sets
`Generation_Limit => 40`, so retirement at the limit happens during the run
(the process table will use 65,535).

**Not proved:** the adapter that maps and unmaps pages and the directory
lookup (next step), and locking (the adapter holds the table lock around
ledger calls). Retirement at the limit is both proved and exercised.
