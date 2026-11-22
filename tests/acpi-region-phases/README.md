# Scoped ACPI registration phases

Run in the Nix development environment, with TMPDIR=/home/doc/cubit-build-tmp. Hosted only: no kernel/image/QEMU build. run.sh --validate-only checks all pinned inputs.

Exercises the actual Core, ancestor planner and device walk against the captured laptop EC _REG body with scalar field models. Includes AML32/64, renamed ancestors, consumer-before-provider declaration order, unavailable/failed ancestors, handler changes, budgets, retained results, coalescing, partial registration and no replay. ACPICA comparisons check explicit method sequences; they do not validate hardware.

The cleanup suite exercises the actual Bootstrap driver and shutdown admission. A deliberately unvisited second group contains a Fatal _REG: cleanup must disconnect only the successful first group. Failed or uncertain region sessions refuse disconnect. An explicit hosted result-owner release helper is included solely for regression setup; native EC63 integration does not gain that helper. Runtime maintenance admission stays unchanged.

Full Core hosted execution uses -O0 -gnatp due the known checked Core compiler issue; the separate small admission product runs assertions enabled and suppressed. Native checked/release SDK semantics are separate evidence. No new SPARK proof or hardware validation is claimed.
