# GPIO hosted and proof gates

Run in the CuBit Nix shell, setting TMPDIR before entering it:

```sh
export TMPDIR=/home/doc/cubit-build-tmp
nix develop --command bash tests/gpio-native/run.sh
nix develop --command bash tests/gpio-native/run.sh --prove-only
```

`--validate-only` checks catalog inputs without compiling, executing or proving.
`--source-root DIR` selects an explicit checkout/package root. Inputs have exact
hashes in `sources.json`; changed inputs require deliberate review and catalog
refresh. Outputs use unique directories below TMPDIR; logs and terminal command
results are retained. The shell sets a 64 MiB stack and disables core dumps;
engines use nice19, one compiler/prover job, 180-second build/proof and 30-second
per-case runtime bounds, 5-second solver calls, and 1 MiB log limits.

The hosted gate uses the actual production sources when their bytes match, and
explicit per-suite models for kernel/native boundaries. It runs checked and
release profiles and audits compiler ALI switches. Expected checks per profile:

| Suite | Checks | Scope |
| --- | ---: | --- |
| main | 13,880 | Actual IRQ-driven Main; 26 startup/lifecycle/failure scenarios |
| registry | 14,414 | Dynamic admission/retirement and joined IRQ turn; 45 cases |
| slots | 5,129 | Controller arbitration and released-slot retarget/reuse |
| park | 2,580,955 | Whole GPIO pin-domain enumeration, parking and 27 I/O fault cases |
| irq | 388 | Notice authentication/hold, absent-parent dispatch, bounded turn |
| total | 2,614,766 | Checked and release profiles; explicit modeled hardware/kernel boundaries |

Large counts include repeated enumeration assertions and are not a percentage of
hardware, AML or independent test coverage. The Main model uses ordinary register
arrays, symbolic handles and modeled transport. Quarantine cases stop through a
test-only observation exception after bounded retained waits, not a clean native
shutdown. Generated parameter type declarations come from the prior genuine CCL
signature; startup/parameter values in the Main fixture are explicit test models.

The proof gate checks geometry/register policy and the exact IRQ/parking owner
instantiations at levels1 and2. Each level has108 proof diagnostics and60 flow
checks,153 Valid CVC5 goals and5 trivially proved leaves. It rejects unresolved
checks, explicit Assume pragmas and skipped proof/flow. Dependencies are retained
by name in DEPENDENCIES.json, so resolved entity IDs are not mistaken for proof
of imported contracts.

Proved properties are runtime safety and the stated contracts: active owners
retain valid register plans; retarget preserves the event ticket; parking work,
access/write counts and pin classifications satisfy the state invariant; Locate
agrees with Mapped_Pin, and returned search pins are mapped and at/after the
requested pin. Search minimality/completeness is regression-tested, not claimed
as a stronger proved contract. Table geometry must still match the chipset.

Proof-only GPIO_Proof_Boundary imports arbitrary callback values/statuses. Their
declared global effects, termination and absence of owner-state aliasing are
trusted. Mapping readiness is immutable during parking. Compiler Interfaces
shift intrinsics use tool models. Stable ownership, serialization and absence of
concurrent enable-register writers are integration obligations. No proof-only
boundary belongs in the production component.

No end-to-end controller/registry/IPC/Main/AML proof, native kernel/MMIO execution,
SCI/GPE/parent-IRQ delivery or real laptop acceptance is established here. No new
ACPICA result is claimed: these gates test GPIO code, not AML conformance.

The actual component/manifest now live in userspace/services/gpio. Main uses
required typed memory handles, the described devmgr endpoint identity and
compiled named policy; the obsolete ACPI-memory/process/typed-parameter draft
is removed. Launch preflight reads the actual Generic_ACPI_Configuration and
checks all four handles before any map/probe/register access. Missing or
mismatched later windows refuse the whole startup. The hosted kernel and
launch-block boundaries remain models. Actual generated Startup/native SDK
semantic checks are in tests/gpio-native-entry; no native execution or hardware
acceptance is established by either hosted suite.

The IRQ additions are hosted-tested, not newly proved. The existing scoped proof gate covers its listed low-level units; it does not establish the new parent-line handoff, native queue/ABI/clock/ACK behavior, or whole-service correctness. Native preflight in tests/gpio-native-entry regenerates the real optional parent Startup and checks the component against the coordinator schema; it does not link or boot a guest.
