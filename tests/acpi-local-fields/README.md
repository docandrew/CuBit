Hosted local ACPI Field regression

Run inside the repository's Nix development environment. Set TMPDIR before
entering Nix:

```sh
export TMPDIR=/home/doc/cubit-build-tmp
nix develop -c bash tests/acpi-local-fields/run.sh
```

The default run tests the small scope and geometry policy with assertions and
overflow checks enabled, then with checks suppressed. It also compiles the
actual AML Core/Field/device-initialization path in release mode, exercising
locked and unlocked direct, wide, Preserve read/modify/write, BankField and
IndexField operations under both AML integer widths. Every modeled hardware
effect checks whether the physical lease is held. A selector-write uncertainty
stops further effects. The real device-region barrier stays incomplete because
this test does not install region handlers or manufacture _REG readiness.

ACPICA compiles the shared startup ASL fixture with Lock/NoLock variants and
DSDT revision 1/2. Twenty-four typed results are checked against the same
explicit values asserted in the Core test, including AML32's Buffer result for
a 64-bit Field. Raw ACPICA shutdown allocation diagnostics remain in the logs;
an exit status and matching values do not establish a diagnostic-clean run.

For a quick iteration, use `--scope-only` (21 cases in each small build profile).
For a milestone, add `--with-proof`: GNATprove levels 1 and 2 check the concrete
scope instance and Field_Layout/Register_Views/Field_Plans with real CVC5/Z3.
The runner refuses unresolved checks, skipped entities or pragma Assume.
`--validate-only` checks the catalog without compiling. `--source-root DIR`
allows testing an independent source tree against the same exact catalog.

The source catalog pins repository inputs and test helpers. Builds use copied
inputs in a unique TMPDIR directory, rechecking originals and copies around
every command. Each child has a 24 GiB address-space ceiling, one compiler/prover
worker, a 180-second build/proof limit, a 30-second execution limit and a 1 MiB
captured-output ceiling. ACPICA has 5 seconds, 5 CPU seconds and 128 MiB. The
shell wrapper selects nice 19, a 64 MiB stack and no core dumps. No whole
Namespace/Core assertion-enabled build is attempted.

RESULT.json, source/tool hashes, exact commands, raw logs and selected .spark
reports are written only below the printed TMPDIR directory. There is no shared
build output, kernel/image/QEMU build, Git operation or automatic source edit.

This is hosted evidence with modeled physical callbacks. The selected proofs
cover runtime checks and existing pure contracts; they do not prove physical
FACS access, native wrappers, full AML semantics, the service event loop or
hardware behavior. No native execution or usable-driver claim is made here.

Current-Field registration refresh (2026-11-12): Namespace source hash now
selects the tested runtime bank/index resolver. Current Core dependencies and
the hosted allocator closure are explicit, with audit witnesses separated
from compiled candidates. This refresh reruns functional hosted tests only;
no --with-proof run or fresh proof claim. Earlier proof artifacts retain their
original exact source scope. Core remains -O0 -gnatp; small policy tests run
checked/release. No native/hardware validation is established by this suite.

Required I/O-window follow-up: every Field, including unlocked data, now has a separate mandatory finite owner-window inspector. Lock availability remains independent; lock deadlines clamp to that owner window. Model callbacks explicitly supply the same stable deadline. Scope tests cover closure/replacement/refusal/clamping and cleanup after closure; actual-Core window-stop variants close admission after the first completed byte write and require no further effects, no replay and balanced ownership. The full completion4 composed/unknown-completion corpus is retained. No new proof/native/hardware claim.

The run.sh wrapper works inside nix develop even when ACPICA is absent from PATH: it resolves the repository flake-pinned acpica-tools, offline, without a global install, lockfile write or native build. TMPDIR is exported before Nix. Existing tool paths remain pinned by the test receipt; cached dependency resolution must be available for offline use.
