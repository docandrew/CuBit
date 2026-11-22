# Common address-range geometry

Address_Spaces.Geometry checks the common fields of Word/DWord/QWord address
items. Check accepts named geometry parameters; Assess adapts a decoded copied
descriptor and rejects a non-decoded result. Feasible includes an inclusive
First/Last witness inside the declared bounds. It neither allocates that range
nor authorizes access to it.

The validator checks encoded width, bound ordering and a contiguous low-bit
granularity mask. It distinguishes assigned use (_CRS/_SRS) from possible use
(_PRS). Fixed assignments require both fixed flags, nonzero length, zero
granularity and matching endpoints. Possible forms support fixed size with
variable location, or variable size with at most one fixed endpoint. Fixed
endpoint alignment and fixed-size length alignment are checked. The validator
also establishes that at least one nonempty aligned range fits the window.

A witness is an example, not an allocator's preferred choice. Variable-size
forms can describe the full 64-bit range without storing its unrepresentable
length. The implementation checks room before advancing an unaligned minimum,
uses length-minus-one/differences for fixed ranges and avoids computing a
full-width granularity successor. Empty or inconsistent fixed forms fail with
explicit statuses. Inputs are never silently normalized.

Bridge role, subtractive-decode restrictions, translation (including sparse I/O),
source resolution, resource-specific meaning and independent platform ownership
remain separate policies. A feasible raw or vendor description grants no hardware
capability. The pure interface also accepts constructed inputs; width checks do
not rely on prior decoder execution.

Tests compare feasibility with an independent small-domain endpoint enumerator
using integer division, across all encoded widths and both uses. Additional
cases cover every power-of-two granularity through bit 63, full-width endpoints,
nonrepresentable spans and out-of-width constructed inputs. Actual Core tests
adapt six normal iASL descriptors. At its initial functionality milestone this grandchild was tested, not yet proved;
the earlier four-unit decoder proof retains its original scope.

Specification: ACPI 6.6 table 6.44 and address-space granularity definitions:
https://uefi.org/specs/ACPI/6.6/06_Device_Configuration.html

A separate subsequent milestone now proves 21 checks/33 goals and eight flow
checks for this grandchild, including the feasible-witness postcondition. Full
feasibility completeness and platform resource policy remain outside that proof.
See tests/acpi-resource-policy-proof/README.md; the old four-unit receipt is still
separate and does not include this grandchild.
