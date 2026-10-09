# Startup denial regression

Run in the repository Nix environment:

```sh
python3 tests/compositor/startup-denial/run.py --toolchain-root "$PWD"
```

The runner copies the current production Vulkan startup body, checked layout,
capability interface and readiness policy into an isolated build. External
operations and the startup declaration are mocked; production SPARK contracts
are checked separately by desktop_policy_proof.gpr.

Thirteen cases cover successful setup, denial at every stage, disabled/invalid
configuration, zero epoch, dimension overflow, absent capability and failed
inspection. Stage call counts reject work after failure, especially readback
after upload denial. A historical body without that guard fails this test.

This is hosted control-flow evidence, not native allocator-denial, GPU fence,
physical memory reclamation or hardware rendering evidence. Failed build and
test directories are retained.


The same thirteen cases now require exactly one `setup unavailable stage=...`
record for the first unmet startup gate, and none on success. Device creation
and health checks are distinguished; missing authority is reported as admission,
not as an inferred allocator failure. Pipeline-specific Vulkan diagnostics remain
separate. The unmodified adapter fails the new assertions (negative control).
The modified SPARK adapter passes 31 checks, none unproved; this does not prove
Mesa, hardware readiness, log transport or native allocation behavior.

`native_log_observer.adb` is the source of the native channel observer (compiled
as Main). It authenticates Desktop's publisher, requires the exact admission
record once between INIT_AFTER and software selection, rejects stream gaps,
and closes its reader. The recorded native run also covers software Mesa,
menus, pointer/keyboard progress, cursor damage at 100%/125% and metrics.
Its frozen platform/runtime/Mesa inputs are explicit in the evidence. It proves
the missing-render-authority fallback, not native quota, metadata exhaustion or
failures at hardware target/pipeline/upload/readback allocation stages. Those
G1–G4 integration gates remain open pending an agreed real driver test hook.
