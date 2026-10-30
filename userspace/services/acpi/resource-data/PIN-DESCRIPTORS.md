# Pin function, configuration and group descriptions

ACPI_Resource_Data.Pin_Descriptors decodes the five revision-one pin-resource
forms: PinFunction(0x0D),PinConfig(0x0F),PinGroup(0x10),PinGroupFunction(0x11)
and PinGroupConfig(0x12). These are descriptions in a copied AML resource Buffer,
not permission to configure a pin or invoke a controller. Nothing is mapped,
resolved or written by decoding.

Named kinds and field types preserve pull/function IDs and raw32-bit
configuration values. A single per-kind layout defines fixed fields; offsets
remain relative to this descriptor on the wire, and returned spans are absolute
in the exact caller copy. Bounds, ordered partitions, exact terminal vendor
extent, reserved flags, revision and zero source index are checked. Pin lists
must contain a positive whole number of16-bit entries. No fixed output array is
allocated. Pin order, duplicates and all raw pin numbers are retained; GPIO's
No_Pin convention is not silently applied to these other descriptors.

Direct function/configuration descriptions return pin and source spans;
PinGroup returns pin and label spans; group references return source and label
spans with Count0/Pins(0,0). Absent spans are (0,0). Read_Pin revalidates Decode
and its index instead of accepting a caller-fabricated span. No pin exists for
a group reference until a separate consumer authenticates the provider and
resolves the label against its current resource template.

Labels must be nonempty and exactly NUL-terminated, matching the resource-group
contract. Source names remain opaque bounded spans, as for the GPIO decoder;
source syntax/termination, namespace identity and provider ownership are later
consumer checks. PinFunction is implicitly a consumer; PinGroup encodes its
producer/consumer bit separately from other forms and has no sharing bit.
This difference is explicit in the result instead of interpreting all flags alike.

Pull codes0..3 and128..255 are accepted. Configuration type codes0..13 and128..255
are retained;14..127 are reserved. Values remain raw, including N/A and vendor
values; type-specific constraints/units, controller limits, consistency with
other descriptors, allocation and application ordering are not established.
In particular successful structural decoding does not make an arbitrary Schmitt
trigger value or controller-specific function valid for hardware.

The pure matrix enumerates every flag word, pin/source/label/vendor offset,
vendor length, raw pin and function ID, plus byte revisions/source indices/
pull/configuration types, value bits, high array bounds and truncated prefixes.
Its independent partition expectations cover odd/empty pins, overlapping spans,
labels containing early NULs and invalid endpoints. Source-byte opacity is tested
separately from label validity. The subsequent clock batch closes the original
maximum-sized pin-list execution gate: full wire payload, large count/last pin,
padding, nonzero origins/high array bounds and malformed terminal extents are
covered in both profiles. Configuration-value/controller admission stays open.

Normal iASL emits all five descriptor forms in PIN0._CRS, including a producer
PinGroup and matching group references, provider-relative pins, a standard
configuration and an opaque vendor configuration. All eighteen prior resource
buffers remain byte-identical. Actual Core tests evaluate/retain/copy every byte,
decode settings and spans, read pins and release results. No label/provider
resolution, native GPIO work or new serial/pin proof is implied. The six older
proof registrations retain their exact selected inputs/scopes.

Sources: ACPI6.6 sections6.4.3.9 through6.4.3.13 and19.6 pin macros:
https://uefi.org/specs/ACPI/6.6/06_Device_Configuration.html#pin-function-descriptor
https://uefi.org/sites/default/files/resources/ACPI_Spec_6.6.pdf
The retained ACPICA resource layouts and normal compiler output provide an
independent encoding comparison; no force or diagnostic suppression was used.

The original pin-milestone source pin-resource-replay-3lfzN9S2/source passes3,029,640 focused
checks/profile (1969547pin/795976serial/263543PCI/574actualCore) in release-1v8mif1s
and checked-9zikjbz7. All four mains/eight stages pass/profile; independent audits
verify182ALI/178strict commands each and all source/reference/tool/runtime/
compiler/binary/artifact hashes. That publication carries221production/1056AdaGPR
inputs unchanged; newer clock/boundary test receipts are separate. Not all fourteen
default mains or a new proof. Initial missing
explicit Byte_Count conversions were compile errors, preserved before correction;
the normal ACPICA fixture and expected prior vectors were not changed to pass.

The later connection proof covers this actual unit:114checks/197goals+21flow,
seven all-SPARK entities/no assumptions/skips/unresolved. Decode's span/count
contract bounds pin reads; Read_Pin's ghost model proves availability/value
relative to those reported fields. This is not full wire-offset/field decoding,
acceptance/error classification, label/provider resolution or value admission.
Five ineffective local initializations are removed, with parser decisions
unchanged. See tests/acpi-connections-proof/README.md for exact proof scope.
