# Userspace GPE event lifecycle

Run in the pinned Nix environment, exporting TMPDIR before nix develop:

    bash tests/acpi-gpe-lifecycle/run.sh --output-root <private-directory>

Select ACPICA_BIN or PATH. The runner pins/copies its explicit source manifest,
compiles fresh AML revisions1/2, compares actual method counters/lid state with
ACPICA, and builds/runs checked (-gnata -gnato) and release (-gnatp) hosted tests.
506 checks/process, 1012/profile across both revisions. One compiler
job, 180s build, 30s runtime, 5s ACPICA, 64MiB stack, no core dumps,1MiB captured
logs. Invoke nice -n 19. No native/kernel/image/QEMU build or hardware I/O runs.

ACPI_GPE_Lifecycle.Driver follows ACPI6.6 section5.6.4 for admitted runtime
events: mask source, clear edge status, run handler, clear level status, enable.
Normative reference: https://uefi.org/sites/default/files/resources/ACPI_Spec_6.6.pdf
Start, Service and Finish separate provider preparation, bounded queued AML and
completion. Zero budget defers at Ready. The handler is consumed before entry;
duplicate/reentrant calls cannot replay it. Failure/uncertainty never retries or
automatically enables. Unknown enable completion explicitly loses masked-state
evidence. Refused enable preserves confirmed masking only under the provider's
no-effect guarantee. Handler/clear failures retain confirmed masking under the
documented control-state preservation requirements.

Hosted coverage includes exact edge/level callback traces, zero-budget deferral,
rejected/uncertain/raised mask/clear/enable callbacks, handler errors/uncertainty/
budget exhaustion/overcharge/exceptions/no invocation, invalid sources,
reentry and terminal duplicates. Normal callbacks invoke actual Core _E01/_L02
methods and verify one queued Notify for the current lid; a real AML divide error
stays masked. ACPICA checks method effects/counters/lid query only, not the
modeled hardware callback order or its provenance. Hardware ordering expectations
come from the specification; this is not native interrupt delivery evidence.

Trust/ownership: a serialized local controller supplies independently admitted
opaque source IDs, epochs/capabilities and a bound method/driver callback. Neither
AML nor a message may register arbitrary addresses. The real backend independently
checks authority/epoch on each operation and reports attributable completion.
Refused must mean no effect; transport uncertainty is Completion_Unknown. Clear
must not change enable state; the handler must preserve source control ownership
and clean retained AML results. Callbacks must not reset this session. Resource
backing/authority must remain retained until terminal completion is reconciled.
There is no generic reset or retry; allocating another session is not recovery
evidence for an uncertain delivery. Keep the source quarantined until the real
provider supplies terminal evidence. This library creates no hardware authority.

This is the runtime event lifecycle only. Actual SCI registration/routing,
active-bit selection, GPE method discovery/binding, named-register/backend and
native event-loop integration, interrupt coalescing/storm policy, EC/device-driver
dispatch, fixed events, wake/_PRW and sleep coordination remain separate work.
This code adds no wire role/endpoint/capability, mutable firmware mapping, native
log sink or policy decisions. Existing native integration must use logstore plus
serial stage/summary lines per coordination/release-stack.md when wired.

For the bounded, separate controller proof, run in the same Nix environment:

    bash tests/acpi-gpe-lifecycle/prove.sh --output-root <private-directory>

The runner copies and hashes the actual two production units and concrete
generic instance, uses level 2 with a five-second prover limit and a 180-second
outer limit, and fails on unproved checks. It preserves the report, command,
tool identity, exit status and source-baseline verification in its output.
The concrete backend has imported, Global-null callbacks with arbitrary outcome
and charge outputs. Callback bodies, termination, exceptions, reentry and real
hardware effects are assumptions or outside this proof. Exception and reentry
behavior remain covered by the separate hosted tests.

The controller run discharges eight functional contract checks and eight flow
checks (six initialization, two termination), with zero unproved or justified
checks. Four callback termination assumptions remain explicit. Contracts cover
phase admission, unchanged reports on rejected operations, bounded accepted
handler accounting, masking during handler service, and successful-handler
evidence before completion. Ready sessions carry no prior accepted charge;
the private I/O contract preserves handler accounting across acknowledgments.
This is a proof of the instantiated controller under the stated backend model,
not of the AML interpreter, method binding, native provider or event loop.

Evidence: controller contracts proved under those assumptions; hosted behavior
regression-tested. No syscall, native boot or hardware validation. Full ACPI/AML/
service correctness remains incomplete.
