# Package Store regression

Opt-in hosted suite: `tests/acpi-hosted/run.sh --group package-store --mode release` (or `checked`). No skip-main option. Seven mains cover exact 96 ACPICA-observed controls, package-store rejection/atomicity, integer origin, package references, ordinary Store, current owned GC and retained old/new package values. Builds have 180-second limits; each runtime has a 30-second limit, 64 MiB stack and 1 MiB output cap, one compiler worker at nice 19.

ACPICA external EvaluateObject resolves Index results before serialization. CuBit intentionally exposes retained references: the comparator requires exact Package_Slot, explicitly dereferences through its public API, checks raw integer type/value, and releases both handles. No AML DerefOf or width normalization is added to the comparison. The original 96 expectations and eight table payloads remain exact. Raw oracle logs include cache teardown warnings; no forced AML compilation was used.

The production change only preserves raw integer payload and origin when copying into a package slot. It does not implement remaining Store completion/owner-count behavior. The 96-control replay passed 797 checks/profile; four supported regressions passed 6503/profile using split receipts; owned GC passed 260/profile; retained-value independence passed 99/profile. These are prior private runs on exact selected source dependencies, not fresh runs of this portable runner. See evidence.json for closure comparison and assumptions. No new proof or native boot claim.

The older copy_store_root_tests fixture could not compile because historical instrumentation is absent. Its failure is retained in the evidence; current GC and public retained-value checks do not claim equivalent coverage. Capture manifests are repinned for the one-line production change; their previous runtime receipts remain predecessor evidence, not a new run.

`python3 tests/aml-package-store/run.py --validate-only` verifies all pins and seven exact directory inventories without a compiler. Default/focused/all dispatcher selections remain unchanged.

The integer ownership/completion successor refreshes this suite's source inventory without rerunning this whole selection. Its earlier receipts remain predecessor-scoped; see `../aml-integer-ownership/README.md` for the separately tested current subset.

## Runtime package/capacity source update

The runtime package and continuation-capacity update repins this registration to changed interpreter sources and explicit rejecting generic adapters. Earlier runtime/proof results above are predecessor-source evidence; refreshing manifests is not a rerun of this whole selection. Separately, the private package gates passed 2,182 plus 584 checks per profile, and the current composed service smoke passed 106 per profile across startup policy, startup metrics, bootstrap devices and request boundaries. The portable runtime-package registration is a new entrypoint; its inventory checks do not claim a fresh all-suite runtime result. Full ASLTS and whole-interpreter proof remain incomplete.
