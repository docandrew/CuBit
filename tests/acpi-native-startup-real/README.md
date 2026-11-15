# Actual-root startup regressions

Run tests/acpi-native-startup-real/run.sh in the pinned Nix environment, after exporting TMPDIR=/home/doc/cubit-build-tmp. --validate-only checks source/tool pins without compilation. --source-root DIR selects a relocated repository. All build products stay in a unique TMPDIR directory.

The suite retains all88 original startup cases (both AML widths and both logger modes), plus two frozen-clock initializer Stall cases:1,726checks. It builds the actual startup coordinator, interpreter and physical root with explicit fixtures for guest kernel, hardware, heap, clocks, launch data, logger, server entry and scripted HID/power discovery. Native callbacks are selected; namespace/result capacities are smaller. owner_tests is a compiled legacy main, not an executed acceptance case.

Scripted discovery flags retain no real probe handles; these tests do not prove their lifetime or real pending cleanup. Hosted terminal exceptions are not native last-chance execution. Whole-Core functionality uses O0/gnatp with warnings retained. No native execution, hardware validation or whole-service proof is claimed.

ACPICA comparisons use the exact exported DSDTs and copied Fatal records. Initializer aborts and low32 OSL argument projection are explicit; cache-tracking exit diagnostics remain recorded. Module-level delays are an open loading-effects limitation.
