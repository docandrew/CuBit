# Same-Core GPE event bridge

Run through the pinned Nix development environment with TMPDIR=/home/doc/cubit-build-tmp: bash tests/acpi-gpe-local-runtime/run.sh. This is a Linux-hosted suite, not a CuBit build.

Fourteen processes cover seven cases at both AML integer widths: normal handler/Notify/result cleanup, missing handler, conflicting edge and level handlers, zero fuel, another retained CRS result, stopped runtime, and Fatal. The normal path also fences queued and claimed Notify receipts before another invocation and rejects replay after acknowledgement. The ambiguous fixture is intentionally invalid ASL; iasl -f emits it solely to check refusal, and it is never executed. Repair-disabled ACPICA independently executes the normal fixtures and checks the returned package and Notify.

The actual request/Core, local runtime, GPE resolver/invoker and HID retained-result owner execute. Clock, provider and routing callbacks are modeled. The test route has no other enrolled consumers; production must route every consumer before receipt acknowledgement. Hosted Core capacity is 64 nodes / 16 retained roots, not native capacity. Whole-Core hosted code generation uses -O0 -gnatp; checked native SDK semantic checks are separate.

This suite proves no SPARK obligations and validates no interrupt source, mask/clear/enable, EC port transaction, hardware admission, native loop or laptop. Callback serialization and normally returning provisioned evaluators remain assumptions. Source catalogs reject stale and unlisted inputs and all accepted artifacts are hashed. Native component enrollment and a runtime-bound generic instantiation do not enable any hardware source.
