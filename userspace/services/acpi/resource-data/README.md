# Copied ACPI resource-data framing

The child [basic descriptor decoder](BASIC-DESCRIPTORS.md) now parses IRQ, I/O,
FixedIO and Memory32Fixed fields from these copied frames. That child is a
separate functionality increment; the proof scope below remains the parent.

ACPI_Resource_Data walks copied small/large resource-item frames and reports
bounded payload offsets, lengths and forward progress. It does not return borrowed
slices or hardware addresses. Unknown and reserved names remain raw metadata;
framing does not validate descriptor semantics or admit any resource.

Scan is a full-buffer policy: an exact EndTag must end the supplied copy, with no
trailing bytes. A zero checksum disables checksum validation as ACPI specifies;
a nonzero checksum requires a zero sum over the supplied template. Item limits
bound traversal, and failures retain the failing offset and prior item count.
Empty input reports Missing_End_Tag. Read_Frame is separately available to
consumers with another explicit extent policy. Existing AML concatenation's
pinned ACPICA compatibility profile is unchanged.

Hosted tests cover truncation, maximum large payloads, zero-length items, item
limits, checksum policies and high array bounds. Four normal iASL _CRS buffers
also pass through actual service evaluation, retained copies and release. These
observations do not authorize IO/memory/interrupt access or allocate resources.

The selected unit's bounds, geometry/progress and quota/termination contracts
have 51 proved checks (93 valid goals) and 10 flow checks, with no unresolved/skipped
checks. Full checksum functional equivalence and descriptor-specific semantics
are tested or unfinished, not established by those postconditions. See the
[hosted tests](../../../../tests/acpi-resource-data) and
[proof recipe](../../../../tests/acpi-resource-proof).
