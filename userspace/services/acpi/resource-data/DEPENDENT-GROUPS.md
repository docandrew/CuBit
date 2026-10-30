# Dependent resource layout

Dependent_Groups.Analyze describes one copied resource template. It validates
full framing/checksum policy and the item quota first, then marker grammar,
priority fields and caller-provided output capacity in one grammar pass.
An error leaves every slot defaulted. Unused success slots are also defaulted.
There is no hidden maximum number of alternatives, allocation or truncation.

Repeated StartDependentFn markers delimit sibling alternatives; one
EndDependentFn closes their set. An end without a start, repeated end, a new
start after the end, missing end, bad marker lengths or reserved priority values
report explicit errors. Empty alternatives are retained. A template without
groups is valid and can use an empty output array.

Each alternative retains source order, marker offset, half-open body span,
item count and the optional raw priority byte. Missing priority means Acceptable
for both compatibility and performance; explicit values Good/Acceptable/Suboptimal
are retained separately. No ranking, tie breaking or selection policy is applied.
Common items before and after the set are counted. Layout.Group_First/Group_Limit
bound the marker-inclusive set; Alternative.First/Limit exclude its markers.
Layout.End_Tag and successful Result.Position identify the end-tag offset, not
the full template extent. All offsets refer to this exact copied input.

Unknown ordinary descriptors remain opaque counted items. Structural success
does not validate their payloads, resource combinations, device ownership or
cross-template policy. It confers no hardware rights and does not compose a
chosen _SRS buffer. No callback or external effect exposes partial output;
each error clears all slots before returning. The result argument must be an
unconstrained Analysis_Result variable so its status can change. This requirement
is explicit in Analyze's precondition.

Tests enumerate token sequences and compare with an independent positional
model, check all priority bytes, exact spans/counts/order, failure atomicity,
capacity/quota boundaries and high input/output array bounds. Normal iASL and
actual Core replay cover common prefix/suffix items and two alternatives, plus
an earlier default-priority example. A scoped SPARK proof now discharges 101
checks/194 goals plus six flow checks. It proves runtime safety, error output
defaulting, unused slot defaulting, alternative capacity, bounded half-open spans,
item counts bounded by span length, end-tag/group bounds and loop progress.
It does not prove grammar acceptance completeness or exact source-to-layout
equivalence; the independent positional model remains regression evidence for
those behaviors. See tests/acpi-resource-layout-proof/README.md.

Specification: ACPI 6.6 sections 6.4.2.3 and 6.4.2.4:
https://uefi.org/specs/ACPI/6.6/06_Device_Configuration.html
