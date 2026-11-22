# Authority model (CAP-012)

The authority delegation model of docs/authority-model.md, executable.

- `authority_model.ads/.adb`: SPARK model of the delegation graph. Every
  operation (issue a root, spawn, derive, install, introduce, transfer,
  carry, deliver, close, revoke, exercise, call) has its rule as a
  precondition and keeps `Invariant` (no amplification, provenance to a
  declared request, an approved plan and a root, inert brokers, calls down
  the layering); `Revoke` reaches every derived copy and nothing else.
  Lemmas: every valid node traces to a root; the synchronous call graph has
  no cycle. Since spawngate (CAP-012 fixes): SPAWN's three rules (wildcard
  only; installable only with Grant; a parent only under Grant:
  `Spawns_Admitted`) and Launch-into-S (docs/authority-model.md 7.5):
  whatever is installed or introduced into a launched program lies within
  a node of its session's envelope (held by its supervisor) or one its
  requester introduces (`Within_Envelope`), and a launch leaves every node
  of the requester unchanged and gives it none (`Gains_Nothing`, the Post
  of Spawn, Install and Introduce). Since lhandle (the launch handle as a
  capability, CAP-012 7.5 item 5, and DBG-001): Launch-into-S parents the
  child under its supervisor (the spawner) and gives the requester one
  launch handle, not a node (Spawn's Post); handles narrow but never widen
  and revoking one reaches every handle narrowed from it and every debug
  session minted through them (`Handles_Kept`, `Revoke_Handle`); a debug
  session (kind `Debugging`) is minted only by the target's supervisor from
  the target's own control, scoped to it, without Grant or Spawn, never on a
  never-debuggable program, and only for a live handle with Debug on it or
  the session's debug authority (`Debug_Within_Envelope`, `Mint_Debug`).
  **Proved** by gnatprove at level 1 (368 checks, none unproved).
- `exhaustive.adb`: every operation sequence up to depth 5 (617 332 states,
  43.3 M steps) or 6 (`--deep`, state bound 40 M) from the booted state of a
  four-program configuration (devmgr, procmgr, a driver, an app), checking
  the model's invariant and independent re-computations after every step.
  It then runs fifteen mutants, each a rule CuBit lacked at k3w 5adc8f71b or
  lacks today (SPAWN through a child's control, SPAWN handing a CSPACE to an
  authority without Grant, a parent named without Grant, a launch
  attenuated by kind alone, a launch that gives its requester a copy,
  revocation without a derivation record, a
  broker keeping a usable copy, calls up the layering, minting after boot,
  an install the plan never approved; and for handles and debug sessions:
  a mint on a never-debuggable program, through a handle without Debug, or
  carrying Grant, a revocation that leaves narrowed handles and their
  sessions, a narrowing that adds Debug), and requires a shortest
  counterexample for each. It replays spawn placements (a) and (b) as scenarios.
- `rules.ccl`, `inventory.ccl`: the model's vocabulary and classification
  rule as typed CCL, and the 80 real delegation paths (k3w 5adc8f71b; K-05
  to K-07, P-02, P-03 and P-06 as fixed on branch spawngate), 29
  root issuers and 17 inter-process synchronous calls.
- `inventory_check.adb`: on the CCL VM, checks that the CCL `classify`
  equals the SPARK `Classify` on all 1024 fact combinations, that every
  recorded verdict follows from its facts (and that a wrong one is caught),
  ids are unique, every kind has one root issuer, a misspelt kind is
  refused, and every recorded call goes down the layering.

What is not proved: that the model matches the kernel and services. The
inventory's facts are read from the code (Observed) or rest on kernel
proofs (Enforced); the model checks only that they are classified
consistently.

Run (Nix shell, memory-capped):

    systemd-run --user --scope -p MemoryMax=24G -p MemorySwapMax=0 \
      nix develop -c bash tests/authority-model/run.sh --prove
