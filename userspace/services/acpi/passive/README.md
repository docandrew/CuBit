# Passive discovery owned by the service

ACPI_Requests.Passive_Queries.Handle operates on the service's existing owned
Requests.State. It derives the generation from that exact server. The Bootstrap
child derives readiness from completed catalog/member initialization and the
runtime namespace state. ACPI_Service_Core.Runtime_Status is a read-only generic
child; its runtime predicate is contracted against the existing Ghost accessor.
Neither bridge exposes the mutable Core or lets request words select readiness.

The owner supplies its full process-lifetime instance identity and kernel-stamped
observer role. Inputs and every scratch/output array are distinct private stable
storage. Static discovery copies HID/CID/UID/ADR/STA/CRS facts. Method-valued fields
remain explicitly Method_Valued; this API never invokes AML or materializes a
Field. Hardware regions and device initialization may still be unavailable.
Copied identities, defaults and resource descriptions are not hardware authority
or proof of device presence/readiness. Active fixed-method observations and
resource admission remain owner operations with their own prerequisites.

The native adapter must classify the generated typed request before acquisition,
acquire the loan with the actual sender as expected owner, copy the exact bounded
request, and use the existing loan/session/pump lifecycle. Return acquisition
before replying and preserve uncertain cleanup rather than replaying work.
Numeric selectors/generated request and reply codecs are not allocated here.
The native entry remains observer/table-only until that binding is integrated.
No new hardware capability, raw Core access, allocation policy or clock epoch is
introduced. The bridge is hosted-tested and native-semantic-checked, not newly
SPARK proved or hardware validated.
