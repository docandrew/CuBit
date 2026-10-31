# Stopped EC receive drain

Run with TMPDIR set before Nix:

    export TMPDIR=/home/doc/cubit-build-tmp
    nix develop --command bash tests/acpi-ec-drain/run.sh
    nix develop --command bash tests/acpi-ec-drain/semantic.sh

The hosted runner supports --source-root DIR and --validate-only. It pins36sources and audits the exact22compiled units, strict checked/release switches and matching case inventories. 2056checks/64processes cover32cases/profile: each read/write prefix, query prefixes, idle stop, clock/ownership loss, unknown write, lost saved answer, adapter exception/reentry, pre-existing serving faults and an unowned transaction. Mock queues retain later requests/events; draining never receives them, runs event/control hooks, starts a transaction or performs an idle wait. Repeated drains do not replay hardware, replies or notice publication.

Controller.With_Receive.Drain is for a serialized trusted lifecycle owner after serving stops. It requests stop and gives the existing owner at most one bounded advance, including an eligible bounded active-hardware pause. A precommand exchange may stop without hardware writes; an already submitted exchange may finish under its original deadline/work/pause budgets. No reset, recovery release or restart is provided. Faulted/reentered/uncorrelated ownership quarantines this route; backing, saved replies and recovery remain retained. Advanced reports a bounded attempt, not completion or safe process exit. No pending saved reply does not establish quiescence. Original stop reason and independent reply/notice/hardware observations remain visible.

Callbacks, kernel queues, clock, admission and command-wide exclusive ownership or held Global Lock are modeled/trusted obligations. All callbacks must be bounded, normally returning and nonreentrant; fault tests check conservative quarantine, not correctness of arbitrary failing hardware callbacks. No actual hardware authority, process/launcher or AML startup selection is supplied. Source admission, actual native launch, retained recovery lifecycle and full service/interpreter SPARK verification remain unfinished.

The existing receive suite separately passes544checks/36processes against this body. Semantic-only actual-runtime checks select252sources/222runtime specs with zero objects in both profiles and instantiate default saved reply/queue callbacks plus Drain. The semantic main must never link or execute. No new SPARK proof, native/hardware execution or fresh ACPICA comparison is claimed; AML semantics are unchanged. Historical proof/semantic records elsewhere remain historical.
