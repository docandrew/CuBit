# Explicit legacy battery information

`ACPI_Battery_Legacy_Information` decodes the separate 13-element `_BIF` format:
nine DWORD fields and four complete bounded ASCII strings. It preserves unknown
versus zero, unit/technology identity and raw granularity values; it rejects
oversized integers, reserved unit/technology values, invalid capacities and
invalid text without returning partial information. It has no extended BIX
revision, sampling, accuracy, cycle or swapping fields to fabricate.

The reader invokes canonical direct `_BIF` once, under one supplied budget and
the owning Core context. It checks device/member identity, exact package shape,
element types and text capacity. Every retained element/root release is attempted
once; cleanup/contract failures are explicit and do not establish quiescence.
Owners must establish battery identity/presence, initialization and admitted
region context independently. This reader does not add automatic fallback after
a failed `_BIX`, reset a service or grant hardware authority.

Run `node tests/acpi-battery-legacy/run.mjs` in the pinned Nix hosted environment
with TMPDIR=/home/doc/cubit-build-tmp. `ACPI_RESULT_DIR` may name an existing
private output directory; otherwise one is created under TMPDIR. `ACPI_CORE_ROOT`
can explicitly select a coherent private Core tree. The pure format runs in
checked/release modes; actual whole-Core reader tests use release code generation
only. Normal iASL/acpiexec fixtures run at both AML widths with repairs disabled.
Run `node tests/acpi-battery-legacy/reference.mjs ABS_RESULT_DIRECTORY` to compare
the returned package type/count and all nine integers/four full strings exactly.
All compiler/runtime/reference commands are bounded and outputs remain private.

Specification: ACPI 6.6 section 10.2.2.2 and table 10.4,
https://uefi.org/specs/ACPI/6.6/10_Power_Source_and_Power_Meter_Devices.html#bif-battery-information
The existing `_BIX` implementation is unchanged. ACPICA's recorded outstanding-cache
allocation diagnostics remain visible; successful typed observations do not
claim a clean ACPICA shutdown or cover malformed-package semantics.

The pure decoder now has proved acceptance/value contracts and runtime safety,
flow and termination checks for the configured text capacities 8, 16 and 128.
Run `node tests/acpi-battery-legacy/prove.mjs` in the pinned Nix environment.
The portable runner checks nonvacuous coverage at SPARK levels 1 and 2: 111
proof checks and 36 flow/termination checks per level, across the three ordinary
library instances. It rejects unresolved checks, skipped proofs and `pragma
Assume`, and checks that modular postcondition dependencies have local proofs.
Both native instances use capacity 128; other generic capacities are not proved.
The trusted basis includes the compiler, proof translation, standard library
contracts and SMT solvers.

The contract says decoding succeeds exactly for the declared numeric/text
schema, preserves units, technology, known/unknown values and exact text, and
returns empty information on rejection. Error diagnostic priority, raw AML
package extraction, ownership cleanup and service integration remain tested
properties rather than claims of this proof. The functional decoder statements
are unchanged; the additions are ghost predicates, contracts and invariants.
The final source passes 1,055 pure checks in each checked/release profile,
443 reader checks at each AML width and 26 typed ACPICA reference elements.
These results do not establish native execution, hardware acceptance or complete
ACPI battery/service verification.
