# HID over I2C discovery observations

`ACPI_HID_I2C` is a service-owner consumer of `ACPI_Service_Core`. It observes
fixed ACPI members through statically supplied invocation/evaluation adapters.
It does not operate an I2C controller, route GPIO interrupts or deliver input.

`Observe_Status` selects only the device's canonical direct `_STA`, supporting
named values and aliases. Missing status uses `ACPI_Device_Policy`'s default
without ancestor fallback. It preserves all64 flags and reports presence,
enablement and functioning independently. Returned objects are released before
return, including wrong-type and budget/provider error paths. Execution failures
and wrong return types remain distinct with their original execution reason.
Its accepted charge can be subtracted from a discovery budget. Status observation
does not establish initialization, dependency completion or hardware readiness.

`Identify` evaluates only the selected device's direct `_CID`. It recognizes
`PNP0C50` as an exact seven-byte string or its compressed integer, either alone
or in a flat package of integer/string IDs. A nonmatching ID does not stop the
walk; malformed element types after a match still reject the observation.
An empty package does not match. No 64-bit integer is narrowed to find a match.
One operation budget covers the AML execution and one host-work unit for each
examined ID. `AML_Charged` records the evaluator's charge (including a rejected
overcharge); `Charged` records only accepted budget consumption. Package
elements are retained individually while read and released before advancing.

`Query_Descriptor` evaluates only the selected device's direct `_DSM` with the
HID-I2C UUID and revision 1. Function 0 must return a buffer whose low two bits
advertise function 1. Function 1 must return an integer descriptor-register
number in the 16-bit range. Zero is representable, so callers must inspect the
outcome rather than treating zero as failure. Each call receives a fresh UUID
buffer and empty package: AML arguments can be mutated. Both calls share the
AML operation budget, and neither is retried automatically.

Neither observation falls back to an ancestor's member. The selected node must
be a current device descriptor. Direct member selection uses `Find_Object`,
which resolves a valid Alias binding to its canonical target without searching
parent scopes. This supports device-local aliased `_CID` values and `_DSM`
methods; unsuccessful canonical lookups remain explicit failures.
Compound results and temporary arguments are
released, including failure paths. Cleanup/provider failures remain explicit.
The trusted adapters must preserve Core validity, catalog/retained ownership,
obey the budget, and avoid reentry/reset. These obligations need a milestone
proof against the actual adapters; SPARK mode alone is not proof evidence.

The device owner separately handles `_HID`, `_STA`, `_CRS`, `_DEP`, power and
registration. A compatible ID is a candidate identity, not readiness. Copied
resource descriptions and the descriptor-register number grant no hardware
authority. Actual evaluation can have effects and belongs to the controller
context, not an arbitrary observer API.

References:

- [ACPI device configuration and compatible IDs](https://uefi.org/specs/ACPI/6.6/06_Device_Configuration.html)
- [HID-I2C ACPI discovery and enumeration](https://learn.microsoft.com/en-us/windows-hardware/drivers/hid/plug-and-play-support-and-power-management)

Current evidence is private Linux-hosted regression testing. It is not a
registered native component, a new proof result, or validation of any laptop.
