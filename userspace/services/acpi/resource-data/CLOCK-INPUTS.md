# Copied clock-input descriptions

ACPI_Resource_Data.Clock_Inputs decodes revision-one ClockInput resources
(large name0x13). Named types preserve fixed/variable mode, Hz/kHz/MHz scale,
32-bit numerator, nonzero16-bit divisor and the optional source/index. In_Hertz
returns an exact fraction using a64-bit scaled numerator; it never rounds
100/3MHz down to an integer. The largest scaled numerator fits its named type.
Neither decoding nor this arithmetic requests a frequency change or establishes
clock-source ownership, output count, physical feasibility or hardware authority.

The fixed wire prefix has13bytes: three framing bytes, revision/flags, divisor,
numerator and the source-index octet. Normal ACPICA ClockInput(100,3,MHz,Fixed,,)
emits payload length10 and index0 even without a source. ACPI6.6 table6.65's
length-row minimum does not match its own field offsets or this normal macro
output; the decoder follows those field offsets and normal encoding rather than
rejecting it based on the inconsistent minimum. Header length/actual extent,
revision, reserved flags/scale and divisor0 are checked explicitly.

A present source must have exactly one final NUL. Variable clocks require a
nonempty source. Fixed clocks accept absent or explicitly empty sources only
with index0; named sources retain the controller-relative index. Names remain
unresolved bytes; source path syntax, clock-output count and source capability
must be checked by an authenticated consumer. Source spans are absolute within
the exact caller copy and include the NUL. There is no fixed name array or new
capacity ceiling beyond the descriptor's16-bit wire length.

Tests enumerate all16-bit flags/scales/divisors, every byte revision/index,
full numerator bits and unit scaling, zero/full-width numerators, fractional
rates, source termination, fixed/variable missing-source cases, truncated fields,
nonzero offsets/high array bounds and maximum-size source data. Normal ACPICA
adds fixed fractional, variable named and full-width named clock examples to the
resource fixture, preserving every earlier resource Buffer byte-for-byte. Actual
Core checks AML evaluation, retained Buffer/copy, typed settings/exact fraction
and result cleanup.

The same batch closes the previously open maximum-pin-list execution gate: all
five pin-resource forms are tested at maximum wire payload, with padding,
nonzero origin/high array bounds, last-pin read, out-of-range pin, truncation
and exact terminal vendor extent. This expands tests without changing pin or
serial production code. It does not establish controller/value admission.

The original runtime batch preceded the accepted three-unit proof described
below. The six earlier proof registrations retain their selected inputs/scopes.
Resource-family decoding is not completion of native
resource delivery, controller integration, AML semantics or the full goal.

Sources: ACPI6.6 section6.4.3.14/ClockInput macro and normal iASL20260408 output:
https://uefi.org/specs/ACPI/6.6/06_Device_Configuration.html#clock-input-resource-descriptor
https://uefi.org/sites/default/files/resources/ACPI_Spec_6.6.pdf

Accepted immutable source clock-resource-replay-GV9Zkbz3/source, focused suite
results/release-zf5oef1v and checked-qzpabpax under tests/acpi-resource-data:
3227583checks/profile (clock197879/pin1969581/serial795976/PCI263543/Core604).
Five mains/ten stages/profile,188ALI/183strict commands independently audited
alongside all source/reference/tool/runtime/compiler/binary/artifact hashes.
Publication carries223production/1061AdaGPR inputs unchanged. This is not an
all-fifteen rerun, native execution or a proof of these three new units.

The later connection proof covers this actual unit:58checks/95goals+12flow,
six all-SPARK entities/no assumptions/skips/unresolved. In_Hertz's postcondition
establishes exact numerator scaling and unchanged divisor, alongside decoder
runtime safety/extent contracts. Complete raw decoding/acceptance/error behavior
and source/controller admission are separate. See the proof suite README.
