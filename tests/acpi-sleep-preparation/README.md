# Root PTS invocation regression

In the pinned Nix environment with TMPDIR=/home/doc/cubit-build-tmp:

    bash tests/acpi-sleep-preparation/run.sh
    node tests/acpi-sleep-preparation/oracle.mjs /absolute/output/RESULT.json

--validate-only verifies inputs without compiling. --reuse-build ABS_RESULT reuses
a successful binary only after exact selected-source, project intent, tool and
prior-input checks; unselected old sources must have no compiled ALI. All current
ASL fixtures and tests execute again. Default behavior compiles a fresh binary.
Outputs remain in unique temporary directories. Core uses O0/gnatp; explicit
checks validate the new generic driver. No full assertion-enabled Core or proof.

20executions /682checks across AML32/64. Covers exact S1..S5 arguments, root and
alias lookup, zero budget, ignored scalar/package/reference/metadata returns,
ordinary failure with prior effects, exhaustion without retry, absent root and
wrong-kind declarations. Every request leaves no retained result. Invalid reserved
name fixtures use one four-byte name replacement and a corrected checksum after
compilation; positive ASL is compiled without forcing rejected compiler input.

18independent ACPICA runs compare effects and exact errors. The host operation
budget case is omitted. A caller ignores the unwanted device-reference return
internally; direct acpiexec export rejects that reference class with AE_TYPE.
No native runtime window, power-manager wire, register IO or hardware execution.

The generic's trusted callback must preserve Core/catalog/retained ownership and
the budget, without reset/reentry or deadline renewal. Native activation requires
an admitted finite owner runtime window. Method completion is not power-off readiness.
