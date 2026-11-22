# Associating copied HID-I2C resources

`ACPI_HID_I2C_Resources.Analyze` observes one exact copied resource template.
It first checks the full framing, EndTag, checksum and caller item quota using
`ACPI_Resource_Data.Scan`, then uses the existing serial/GPIO decoders. It
reports whether exactly one I2C serial connection and one GPIO interrupt were
found. The first decoded descriptions and their zero-based offsets are kept
along with counts; duplicates return `Ambiguous_Connections`. Other framed
items are counted, not treated as admitted resources. Dependent alternatives
return an explicit outcome; this routine never picks an alternative.

The existing decoders preserve bus address/mode/speed, vendor bytes, source
spans, interrupt mode/polarity/sharing/wake, and the pin span. Spans refer only
to the caller's exact copy. `GPIO_Connections.Read_Pin` revalidates the descriptor
when reading its pin. `No_Pin`, controller-specific restrictions, controller
path resolution, source syntax, ownership, dependency ordering and access
rights remain the controller/registration owner's responsibility. Finding
descriptions does not establish that they are suitable for hardware use.
`Position` identifies the failing connection or unsupported context; the caller
can use the specific decoder there for its precise diagnostic.

The hosted composition uses the actual Core to observe compatible ID and
`_STA`, retain `_CRS`, discover the required copy length, retry the copy with
the same retained result, analyze it, and query the HID descriptor register.
`ACPI_Resource_Reader.Copy_Template` does not re-evaluate AML during resizing.
The owner releases the retained `_CRS` result after consuming the copy. A
disabled device can still return a valid `_CRS`, so resource framing or a
found connection pair cannot substitute for `_STA`/initialization policy.
This is a hosted composition test, not a new native service endpoint or a
complete production probe coordinator.

References:

- [ACPI current resource settings and resource descriptors](https://uefi.org/specs/ACPI/6.6/06_Device_Configuration.html)
- [HID-I2C discovery and enumeration](https://learn.microsoft.com/en-us/windows-hardware/drivers/hid/plug-and-play-support-and-power-management)

Proof acceptance and hosted counts belong to this milestone's immutable
evidence; SPARK mode alone is not a proof result. The existing upstream decoder
proof scopes are documented in the resource-data directory. Native service,
I2C/GPIO/HID driver and real laptop integration remain unfinished.
