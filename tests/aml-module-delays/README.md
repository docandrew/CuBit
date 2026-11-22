# Module loading delays

Export TMPDIR=/home/doc/cubit-build-tmp before the pinned Nix environment, then run tests/aml-module-delays/run.sh. --validate-only checks exact source/tool pins; --source-root DIR selects a relocated repository. Build outputs use a unique TMPDIR directory.

48 fresh processes/640 checks cover both AML widths, Stall/Sleep, sequential and nested calls, provider refusal/failure, invalid Stall, zero operation budget, no replay and retained timer/debug loading restrictions. The delay callback is modeled; its normalized request and continuation/failure behavior are checked. Eight ACPICA runs consume the exact exported DSDTs and compare successful module continuation. Durations are not compared.

No hardware or native scheduling is executed, and no new formal proof is claimed. All non-test code is selected from repository paths; allocator fixtures are explicit.
