# Local module-startup failure handling

Run with TMPDIR=/home/doc/cubit-build-tmp in the pinned Nix shell:

    bash tests/acpi-local-module-startup/run.sh

The actual Core, copied FADT selection, AML module execution and generic
startup hook execute. Initializer/close and exception injection callbacks
are modeled. Fifteen cases run at both AML integer widths. All ten previous
startup cases remain, with initialization/execution/post-execution/closure
exceptions added. Failure reports remain terminal; a later call cannot replay
initialization, execution or closure, even if AML already executed.

Close_Completed reports a normally returning callback, not physical cleanup.
An exception before closure can leave local initialized state retained; this
never establishes retirement permission. Both failure origin flags and the
attempted/completed closure distinction are checked.

Compilation is release-only -O0 -gnatp with warnings-as-errors and an exact
ALI inventory. The test instantiates Core directly; assertion-enabled Core
code generation has a documented GNAT failure and is deliberately excluded.
Two repair-disabled ACPICA runs check four typed module-fixture results.
No new proof, native/hardware execution or callback/IPC equivalence is claimed.

Sources, tools, generated fixtures and projects are pinned around commands.
One low-priority worker, 180-second build, 30-second cases, 24-GiB address space,
64-MiB Core stack and 1-MiB output. ACPICA: five CPU/wall seconds,128 MiB,2-MiB
thread stack. Logs and RESULT.json stay in a unique TMPDIR directory.

The complete service/AML verification and native Lenovo bring-up remain open.
