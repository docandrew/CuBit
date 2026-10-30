# Region availability notifications

`ACPI_Region_Initialization` connects the trusted handler/binding preparation
phase to `_REG` evaluation. It supplies no address-space handler or hardware
authority. The owning controller installs every required handler and Field
binding before calling Connect, keeps one Session for the service lifetime,
and serializes service/controller access. Handler_Ready is a trusted observation
of that completed work; a firmware OperationRegion declaration cannot establish it.

Connect snapshots initialized OperationRegion metadata and local `_REG` methods,
then checks the complete inventory before any notification. It rechecks service,
region/method identity, geometry and readiness before and after each invocation.
New or deferred regions prevent completion. Each region invokes its local
method with the address-space ID and connect code 1; a missing method is optional.
Local method aliases resolve canonically, including alias chains. Parent-scope
methods are not inherited. Regions sharing a method are separate
notifications. Disconnect uses code 0 while handlers are still available.

Memory, I/O and DataTable spaces suppress `_REG`. PCI root-bus suppression
requires a trusted platform assertion and a local `_BBN`; metadata alone cannot
establish root-bus identity. These rules follow
[ACPI _REG](https://uefi.org/htmlspecs/ACPI_Spec_6_4_html/06_Device_Configuration/Device_Configuration.html#reg-region)
and [ACPICA region handling](https://github.com/acpica/acpica/blob/master/source/components/events/evregion.c).
The device initializer retains ACPICA's root/`_SB._INI` prepass before this barrier,
then performs the ordinary device `_STA`/`_INI` walk.

Fuel is shared within each notification direction. Ignored compound/reference
returns are released, and ordinary failures, Fatal and unknown Field completion
stop the sequence. Earlier firmware/hardware effects remain. A consumed session
cannot repeat Connect or retry a failed direction. An exception leaves an
in-progress phase that those entry points cannot reenter. The controller must
resolve attributable completion and backing lifetime separately; this module
never replays an operation, reinstalls authority or releases backend backing.

The copied region metadata reader requires a current service-owned descriptor,
rejects wrong phase/kind/identity and clears output on failure. Copies are
descriptive data, never capabilities.

Hosted tests exercise real Core/AML/Field/provider code and the device barrier,
including exact normal iASL images and 28 acpiexec getter observations. EC handler
availability is modeled; effects go through an independently admitted memory
register. This does not implement an EC driver. The native-provider instantiation
is checked by compilation only against exact runtime declarations.

Default startup still has no hardware provider. It permits the device walk only
when the namespace contains no hardware OperationRegion declarations; a hardware
declaration keeps startup's device phase incomplete. This empty-requirement
check does not invoke this coordinator or grant access. Live provisioning, additional
address-space backends, qualified notification arguments, automatic orphan EC/GPIO provider admission, dynamic registration and dependency ordering
remain integration work. Providers must not report readiness for unsupported
semantics. No new SPARK proof or native/QEMU/hardware acceptance is claimed.

The opt-in tests/acpi-region-alias suite checks local _REG aliases with actual
Core/AML and fresh ACPICA comparisons, plus the unchanged existing region matrix
in both hosted profiles. Nonmethod aliases, ancestor fallback, unavailable
providers and foreign-owner disconnect remain refused. No new proof or native
provider/initialization acceptance is implied by those hosted tests.

The child library in region-init/orphans provides explicit owner-only orphan
_REG notifications for an independently admitted EC/GPIO Device. Instantiate
ACPI_Region_Initialization.Orphans with a real provider-readiness observation;
include that source directory when integrating the child. It selects only the
device's canonical local _REG and refuses unresolved region metadata. A matching
direct OperationRegion is ordinary-coordinator work; other spaces do not block
the orphan notice. Readiness and identity are checked before/after AML.

Each binding connects once and may disconnect once while the provider remains
installed. Rejected/uncertain/failed attempts do not replay; pending result
cleanup remains owned until release succeeds under the same Core lifetime.
The child neither supplies hardware authority nor certifies the global region
barrier. Automatic provider admission/dispatch and native integration remain
separate owner work. tests/acpi-orphan-reg covers886checks/profile with actual
Core/AML and modeled provider readiness, plus explicit ACPICA comparisons; no
new proof, native delivery or hardware acceptance is claimed.
