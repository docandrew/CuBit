# EC native deferred-call owner — hosted integration

Run `TMPDIR=/home/doc/cubit-build-tmp nix develop --command bash tests/acpi-ec-native-calls/run.sh`.
`--validate-only` checks source/test pins without compilation; `--source-root DIR`
selects an explicitly pinned source tree. The suite writes disjoint temporary
outputs, uses one compiler worker, 180-second build and 30-second runtime limits,
and caps each captured log at 1 MiB. Both checked and release profiles are run.

Geometry tests cover every logical EC cell, span/width/access rejection,
partial multi-datum plans and remote descriptors, plus the existing Field
plan/datum regressions. Joined tests compile ordinary ByteAcc AML with iASL,
then execute it through the actual Core, typed lid reader, Field IO, remote
provider, deferred endpoint, named EC cell catalog and transaction engine.
The mock models hardware registers, authenticated transport metadata, clock
and continuous dedicated ownership. It exercises partial writes, denied
writes, full-width sender identity, locked binding refusal and retained
uncertainty without replay after an unknown hardware access.

This is hosted evidence, not a CuBit boot or Lenovo hardware test. Compilation
with iASL is not an independent execution oracle for the modeled EC hardware.
Whole-Field Lock scope, real reservations/GlobalLock, native admission,
interrupts, clock conversion and startup/provider orchestration remain pending.
The suite supplies no new formal proof; geometry proof evidence is separate.


## Trusted EC cell/provider association

ACPI_EC_Field_Bindings builds a remote descriptor from the same independently
admitted cell description used by the concrete EC catalog. It checks full64-bit
expected owner,nonzero ID/epoch,rights/mask consistency,bounded EC region and
cell containment. Returned IDs,epoch,permissions and mask remain exact; region
base plus byte offset equals the cell. No builder call installs admission,
contacts a peer or accesses hardware. Native code must independently establish
admission currency,matching controller/endpoint contexts and protocol ownership.

The joined EC Field test uses this builder for actual AML/lid/read/write flow.
It also covers all256cells in full and singleton regions,maximum64-bit IDs and
epochs,upper-owner-bit mismatch,read-only/masked rights and invalid extents or
cell association. Hosted checked/release1487joined checks/profile plus the
existing1063geometry and datum/plan regressions pass. Function postconditions
are exercised in checked builds; no new SPARK proof is claimed for this helper.


## Saved reply and native API scope

The joined path now includes ACPI_EC_Native_Calls:save before submission,exact
full-width token/owner/register/epoch correlation,one-step progress,and one
answer attempt. Tests cover save refusal before effects,busy requests without
replacing the retained reply,whole IPC flag rejection,lost reply without replay,
actual AML/lid/partial-write flow,and independent retained recovery after all
saved replies are consumed. Hardware callbacks require a live modeled reply.

CuBit.Handles,Process_IDs and Capability_ABI use actual production types. Small
Messages/IPC fixture specs model only the needed surface; callbacks replace
kernel operations. The test-only Handles.Testing child creates modeled handles;
production code never constructs a handle from a word. This test is not a
validation of actual kernel reply-handle consumption or process death.

Run semantic.sh in Nix with TMPDIR set to check the generic default callbacks
against actual runtime headers in both profiles using -gnatc. It creates no
objects or executable. --source-root selects the runtime/repository source tree;
--unit-root optionally supplies the new production units separately. Each
compile remains bounded to180seconds/j1,nice19,64MiB stack and1MiB logs.

The adapter belongs in a separate privileged EC backend. Native admission and
protocol ownership,launch/endpoint publication,clock conversion,event scheduling,
GlobalLock,whole-Field Lock and SCI/GPE/_REG integration remain unfinished.
Neither hosted tests nor semantic checks are a SPARK proof or native execution.
