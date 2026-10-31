# EC clock policy proof

Set `TMPDIR=/home/doc/cubit-build-tmp` before entering Nix. Run with the
project's GNATprove/CVC5 toolchain:

    bash tests/acpi-ec-clock-proof/run.sh
    bash tests/acpi-ec-clock-proof/run.sh --validate-only

`--source-root DIR` selects a matching source tree. The normal run copies the
three hash-pinned sources into a private output directory. Each SPARK level
1/2 has a 180-second process bound, 5-second solver budget and one worker.
The cached-validation mode rechecks the retained evidence without running a
compiler or prover.

Both functions are analyzed completely. Four flow checks establish termination
and absence of global dependencies. Ten solver checks establish safe arithmetic
and conversion, plus the exact functional postconditions: unavailable samples
stay unavailable; out-of-range samples are refused; accepted samples preserve
their value; a positive slice advances by the smaller of the remaining distance
and the configured maximum. Slice requires Target > Now.

The report auditor requires the exact three entities and all fourteen check
locations/rules. It rejects skipped analysis, pragma Assume statements, external
claim assumptions and incomplete proof branches. Twenty deliberately altered
reports per level must be refused. This checks the report gate, not GNATprove's
soundness; the compiler, prover and platform arithmetic model remain trusted.

The separate `tests/acpi-ec-clock` behavioral suite exercises the actual policy
and adapter with modeled clock/pause callbacks in checked and release builds.
No proof of physical time, monotonic hardware, suspend behavior, callback
truthfulness, native execution or the complete AML interpreter is claimed.
