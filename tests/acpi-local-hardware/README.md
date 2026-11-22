Hosted local ACPI hardware regressions

Run from the Nix development environment with GNAT, gprbuild, ACPICA and Node:
`bash tests/acpi-local-hardware/run.sh`. `--validate-only` checks the frozen
source catalog without compiling. `--source-root DIR` selects a review tree.

The suite exercises the real named-register/NVS/PCI/platform-pool mux with
modeled physical callbacks. Small authority tests run checked and release;
actual Core AML32/64 tests use -O0 -gnatp and explicit checks. It validates
eleven combined modes per width and 22 ACPICA typed results. Existing
tests/acpi-local-fields retains its scope tests and selected proof milestone.

Inputs and suite helpers are pinned in sources.json, copied into a fresh
private TMPDIR directory and checked around every command. Build timeout is
180 seconds; execution timeout 30 seconds; ACPICA has five CPU/wall seconds
and 128 MiB address space. Other commands have 24 GiB address space and
1 MiB captured-output limits. Builds use one compiler worker. Logs and
RESULT.json remain in the reported output directory, including failures.

This suite does not run native/kernel/image/QEMU builds or proofs. It does
not establish actual hardware behavior, complete AML coverage, fresh composed
runtime Field graph support or whole-service correctness. ACPICA shutdown
allocation diagnostics are preserved, not claimed clean.

Runtime composed-Field successor (2026-11-11): first-use bank/index graphs
resolve their full dependencies before selector/data operations, using the
same charged module resolver when region resolution is explicitly enabled.
The cached direct binding path remains unchanged; an attempted bound IO is
never retried. New bank/index cases use previously unbound pool datum offsets,
with selector writes and actual data bytes checked. Legacy provider behavior
and all existing refusal/unknown/read-only cases remain. Namespace/codegen is
release-only -O0 -gnatp; small policies run both profiles. Source candidates
are separated from audit-only target-runtime witnesses; exact Core ALIs are
checked. No new proof, native or hardware evidence is claimed.

Method-created Field lifecycle regression (2026-11-12): twenty invocations
each of a method-created banked and indexed Field per successful fixture mode,
at both AML widths. Each method creates and uses its Field, returns a value
and leaves its temporary name absent under both root and method namespace.
Selector/data transfers must share the current lease, mappings remain reused,
leases balance and retained results return to zero on every iteration. The
bank selector varies with Arg0. Repeated ACPICA calls independently validate
four additional values per width. These are tested declaration-lifetime cases,
not a proof for all method-created graph shapes or native hardware behavior.

Method-created completion-unknown cases (2026-11-12): normal module startup
precedes an uncertain write in either a method-created banked or indexed Field.
The modeled datum write occurs once, its original uncertain request remains
recorded, and a repeated method call/later scalar access cannot replay it.
Named/NVS provider requests stay blocked, mappings and backing remain retained,
physical leases balance and Core remains valid with no retained result roots.
These are modeled completion failures, not proof of hardware error recovery;
no reconciliation/reset or physical unmapping is attempted.

Required I/O-window follow-up: every Field, including unlocked data, now has a separate mandatory finite owner-window inspector. Lock availability remains independent; lock deadlines clamp to that owner window. Model callbacks explicitly supply the same stable deadline. Scope tests cover closure/replacement/refusal/clamping and cleanup after closure; actual-Core window-stop variants close admission after the first completed byte write and require no further effects, no replay and balanced ownership. The full completion4 composed/unknown-completion corpus is retained. No new proof/native/hardware claim.

The run.sh wrapper works inside nix develop even when ACPICA is absent from PATH: it resolves the repository flake-pinned acpica-tools, offline, without a global install, lockfile write or native build. TMPDIR is exported before Nix. Existing tool paths remain pinned by the test receipt; cached dependency resolution must be available for offline use.
