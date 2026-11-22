# Status-gated runtime caches

    export TMPDIR=/home/doc/cubit-build-tmp
    nix develop -c bash tests/acpi-status-cache-runtime/run.sh

Requires pinned GNAT/GPRbuild, Node and ACPICA on PATH. Hash-checked source copies compile with -O0 -gnatp, 180-second build/30-second runtime limits and 24 GiB address space. Full assertion-enabled Core generation remains excluded due to the known toolchain failure.

308 hosted checks: seven scenarios in both AML integer widths. Actual request startup observes usable devices, then cached lid/AC refreshes read a fresh _STA before invoking _LID/_PSR. Valid/missing-status devices sample once; absent, disabled, broken, malformed or zero-budget status suppresses the member query. Status and query share each total budget. Known absence publishes Missing, while disabled/failed status publishes Failure, with no invented closed/offline value. Retained objects are cleaned before the finite runtime window closes. Provider/delay callbacks are modeled; this is not native or hardware execution and adds no proof claim.

Empty _INI methods ensure the startup walk actually observes _STA before the later refresh. A malformed runtime response uses the repository's existing CopyObject type-change pattern. ACPICA references disable repair, explicitly reset fixture counters after loader initialization and verify 36 integer plus four Buffer results. They validate fixture semantics, not cache policy. Physical state, freshness clocks and all-consumer notification publication remain separate integration obligations.
