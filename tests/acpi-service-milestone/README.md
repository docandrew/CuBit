# Service milestone registration candidate

Hosted registration candidate. Includes 33 explicit mains for Event, field-backed named evaluation, device traversal, startup/metrics, retention/public collection, and pure policy. Each closure pins all Ada files in its selected source directories; no native/runtime/kernel directories are used.

Run from the repository in Nix:

    TMPDIR=/home/doc/cubit-build-tmp nice -n 19 nix develop -c python3 tests/acpi-service-milestone/run.py --validate-only
    TMPDIR=/home/doc/cubit-build-tmp nice -n 19 nix develop -c python3 tests/acpi-hosted/run.py --group service-milestone --mode release
    TMPDIR=/home/doc/cubit-build-tmp nice -n 19 nix develop -c python3 tests/acpi-hosted/run.py --group service-milestone --mode checked

180 seconds per build, 30 per runtime, 64 MiB stack, 1 MiB capture log, one compiler job. Compiler/tool/runtime sources, actual compiler invocations, ALI dependencies and executables are checked/pinned. Python optimization is rejected. No oracle regeneration. Category reference logs/ASL/AML remain pinned; hosted-constructed fixtures are not labeled ACPICA evidence.

Pure policy proof: exact four policy files passed 10 VCs plus six flow checks (Decide contract cases, modulo normalization and unavailable timeout), and 22 hosted checks per profile. This does not prove device coordination, region availability, total charge, retention, Event concurrency, native behavior or whole interpreter. The service region phase remains unavailable; snapshot Complete is separate from device readiness. Continuation capacity changes are excluded.

The broader hosted gate passed 76 selected jobs: 16 semantic adapter compilations and 22 runtime mains per profile, with 7,710 explicit checks in each profile. Its final continuation ran 47 jobs; the other 29 jobs retain exact dependency, log and binary provenance from successful earlier runs. Eight separately verified focused executions contribute 314 checks per profile and remain prior-execution evidence. Failed fixture builds are preserved; none is counted as a pass. The public collection fixture replaces an unsupported historical instrumented fixture, so the old exact internal root-bitset assertions are not claimed.

The portable three-main smoke passed 463 checks per profile (policy 22, combined field/Event 57, collection 384) on the predecessor runner with a lower stack limit. The final runner restores the documented 64 MiB stack limit and has passed inventory/hash validation only; it has not been rerun for runtime evidence. All 23 portable mains have not been executed together through this entrypoint. Broad-gate, smoke, proof, and manifest-validation evidence are separate; refreshing manifests is not a historical test rerun.

## Explicit smoke selection

Without --selection, runner executes all labels in closure.json. `--selection` accepts one or more unique exact closure labels, rejects empty/unknown/duplicate labels, and records the selection in the audit. Every registered source inventory and source/reference hash is verified even for a subset.

    TMPDIR=/home/doc/cubit-build-tmp nice -n 19 nix develop -c python3 tests/acpi-service-milestone/run.py --mode release --selection startup_policy_tests field_event_combined_tests copy_store_collecting_tests
    TMPDIR=/home/doc/cubit-build-tmp nice -n 19 nix develop -c python3 tests/acpi-service-milestone/run.py --mode checked --selection startup_policy_tests field_event_combined_tests copy_store_collecting_tests

These three smoke mains cover the proved policy fixture, combined field/Event behavior, and public Copy/Store collection fixture. A smoke result must not be described as a portable rerun of the complete selection. Broad-gate and source-equivalence evidence remain separately scoped.

The historical injected-hook `copy_store_root_tests` remains source/evidence only and is not registered as runnable. The normal public `copy_store_collecting_tests` is the supported collection regression.

## Runtime package/capacity source update

The runtime package and continuation-capacity update repins this registration to changed interpreter sources and explicit rejecting generic adapters. Earlier runtime/proof results above are predecessor-source evidence; refreshing manifests is not a rerun of this whole selection. Separately, the private package gates passed 2,182 plus 584 checks per profile, and the current composed service smoke passed 106 per profile across startup policy, startup metrics, bootstrap devices and request boundaries. The portable runtime-package registration is a new entrypoint; its inventory checks do not claim a fresh all-suite runtime result. Full ASLTS and whole-interpreter proof remain incomplete.

## Retained startup completion observations

Pages 13–16 expose the retained finish/module report without executing startup.
The new `completion_metrics_tests` label lives in its own source directory;
the default selection now contains 24 labels. Existing labels and bounds are
unchanged. Use `--selection completion_metrics_tests` for the focused case.
The private release gate passed completion 732, startup 57 and endpoint 129 checks.
Supplementary checked completion compiled but timed out at 30 seconds; the other
two checked mains subsequently passed separately (startup 57 and endpoint 129).
No failure is counted as a pass. The identical new
fixture body was relocated for this registration and separately passed all 732
checks through the portable release entrypoint. This is one selection, not an
all 24 runtime rerun.
The earlier whole-selection evidence above remains predecessor-source evidence;
repinning Requests and the endpoint fixture does not rerun other selections.


Mutex stage 1 (hosted): `--selection mutex_object_tests mutex_lifetime_tests` covers real leaf objects, canonical flags, rejected copying, authenticated lifetime and actual collection. Production/fixtures match private focused worker50916:307+162 checks/profile; Event/retention regressions add460/profile. Acquire/Release remain unsupported; no A18 completion or proof claim. The separate unchanged137-byte reference replay retains fatal original expectations and is not registered as a passing or xfail selection. Prior compile/runtime failures remain archived in private receipts. Fresh portable worker47304 passed both selections in release and checked:469 checks/profile (object307+lifetime162), with all8 build/runtime stages0. Earlier focused worker50916 passed929/profile; its460 Event/retention checks were not rerun by this portable gate. The separate normal137-byte replay90747 passed admission and member initialization, then TEST returned Unsupported charged2 and the unchanged assertion failed fatally. This is not Acquire/Release support, a full proof, or a native-build claim. All prior failures remain preserved.


## Current Mutex stage2 registration

This registration now contains 33 labels and 66 projects. Added focused
selections cover invocation lifetime, identity, module behavior, request error
mapping, the exact 24-table/144-observation matrix, table sessions, and the
unchanged original 137-byte replay (`mutex_uncontended_tests`). Original AML,
expected observations and provenance are preserved. Earlier registration counts
and stage1 paragraphs above describe historical checkpoints.

The initial private integration passed 2,375 checks/profile. Maintained-session
release continuation passed 17 mains plus Requests semantic compilation;
original call_tests release 5,018 passed separately. Segmented checked call_tests
covered 5,018 checks in fresh processes, while the unsplit 30-second checked
timeout remains unresolved. Table sessions passed 415 checks/profile, and
worker 18201 passed the original replay's 40 checks/profile in release and checked.
The earlier 90747 failure is preserved. None of these results is an all 33
portable rerun, a completed original 75-stage gate, whole locking proof or native
execution claim. Metadata refresh is not historical test re-execution.

Ordinary uncontended Acquire/Release and serialized-method ownership are now
implemented. Contended waits and the FACS Global Lock remain unsupported.
Invoke_Read_Only, Invoke_Mutable and Invoke_With_Tables on raw State require
caller-exclusive synchronous access throughout invocation and callbacks;
callbacks must not reenter the same State. Per-call lock state does not serialize
separate callers. The negative Package-Mutex IPKG remains Unsupported_Value
with recovery; no positive temporary-package ReleaseIndex coverage is claimed.

## Startup without hardware regions

The default bootstrap permits device initialization when its namespace scan
finds no hardware OperationRegion declarations. A declared hardware region
still reports unavailable even when no Field uses it. This does not install a
handler or grant hardware access. Snapshot completion remains separate from the
device outcome.

Focused `bootstrap_device_tests` and `startup_metrics_tests` passed 49 and 57
checks in both release and assertion-enabled profiles. They cover root/system
bus ordering, device `_STA`/`_INI`, ordinary errors, retained device Fatal
diagnostics, no replay, startup fuel, metrics and unconfigured memory/IO/PCI/EC
declarations. This is hosted regression evidence, with no new proof or native
execution claim. The compatibility selections are recorded separately.

Release compatibility passed completion metrics 732, bootstrap catalog 115,
bootstrap migration 44, provider orchestration 165 and notification polling 379.
The checked completion-metrics program compiled but exceeded its unchanged
30-second runtime cap. It includes the public 100,000-fuel exhaustion loop;
the timed-out run does not establish checked completion-metrics coverage.
The timeout is retained independently of the focused 106-check/profile pass.
