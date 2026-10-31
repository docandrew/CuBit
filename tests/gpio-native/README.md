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
| main | 9,904 | Actual Main; 11 startup/lifecycle/failure scenarios |
| registry | 10,775 | Dynamic admission/retirement; 35 cases |
| slots | 5,129 | Controller arbitration and released-slot retarget/reuse |
| park | 2,580,955 | Whole GPIO pin-domain enumeration, parking and 27 I/O fault cases |
| total | 2,606,763 | 182 build/runtime commands across both profiles |

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

`integration-draft/` contains the pending CCL component/manifest/policy proposals.
They are not build-registered declarations and depend on launcher/runtime work.
Native adapter semantic evidence is carried separately by exact source hashes;
its Startup fixture and pending Close(Carried) runtime overlay are not a working
native executable.
