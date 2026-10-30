# Copied GPIO connections

GPIO_Connections decodes revision-one large-item 0x0C into named interrupt or
I/O connection types, consumer/producer role, sharing, pin configuration, raw
hundredths-of-milliampere drive strength and hundredths-of-millisecond debounce.
Interrupt fields retain mode, polarity and wake capability. ActiveBoth requires
Edge; reserved encodings fail. I/O restrictions retain the configuration-
preservation distinctions. Standard configuration values 0..3 and vendor values
128..255 are preserved; reserved flags, configuration and source index fail.

Pin, source and vendor offsets are relative to the descriptor on the wire and
returned as absolute spans in the exact copied input. Checked ordered offsets,
even/nonempty pin extent and exact terminal vendor length prevent one descriptor
from borrowing following bytes. Pin tables are not required to be machine-word
aligned. Padding before the pin table is tolerated through the explicit offset.
Interrupt connections require one pin; I/O lists use the descriptor's own count,
without a fixed output array or allocation. Read_Pin revalidates the descriptor
and index instead of trusting a caller-constructed span. The 0xFFFF No_Pin value
is preserved in order and must be ignored by consumers. Duplicate raw pin values
are not silently deduplicated; the normal ASL compiler rejects duplicate lists.

Resource Source is an opaque span, including a trailing NUL when present. Empty
or unterminated byte spans are structurally retained: Decode does not claim a
valid controller name, perform namespace search, or establish a binding. A later
consumer must validate/resolve the name and check controller-relative pins,
configuration combinations and ownership. Vendor bytes are opaque. No descriptor
value creates GPIO, interrupt or memory authority or changes hardware state.

Tests use independent numeric partitions over every 16-bit flag and each offset
field, all pin and drive/debounce values, all revisions/kinds/configurations and
source indices, truncation, high array bounds and maximum-length pin tables.
Normal iASL/ACPICA data covers GpioInt Edge/ActiveBoth/wake, shared output I/O,
No_Pin, vendor configuration/data, producer role and preservation restriction.
Actual AML Core replay compares all retained bytes and typed fields and releases
owned results. The initial ASL duplicate-pin compiler rejection is retained;
normal reference compilation uses distinct pins without force or suppression.

This is hosted functionality evidence, not a GPIO backend, capability grant,
native execution or a new proof milestone. Existing scoped proofs for the other
resource units retain their exact source scopes; this child awaits the next
major proof batch.

Specification: ACPI 6.6 section 6.4.3.8.1 and the GpioInt/GpioIo ASL macros:
https://uefi.org/specs/ACPI/6.6/06_Device_Configuration.html#gpio-connection-descriptor

A subsequent scoped proof now discharges 99 checks/176 goals plus 17 flow for this
unit, including safety, existing span/count/extent guarantees, and pin-read
availability/value relative to the decoded span/count. The ghost model reads
the actual little-endian bytes at that reported location. Complete descriptor
field-to-wire mapping, all error semantics and hardware admission remain outside
the proof. This supersedes the initial unproved status above; see
 tests/acpi-gpio-routing-proof/README.md for immutable evidence and exact scope.

The later wire-correspondence proof strengthens successful Decode contracts:
header, relative-to-absolute offsets, span lengths/count and common/connection
fields match the encoded bytes. Read_Pin retains exact availability/value
contracts at that wire-derived pin span. The GPIO unit discharges238 proof
checks/405 goals plus30 flow. This does not establish acceptance completeness,
exact error classification or controller/source validity. The only operational
refinement is a bounded offset-addition helper with an inverse-offset contract;
parser decisions are unchanged. See tests/acpi-gpio-routing-proof/README.md.
