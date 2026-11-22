# Pinned ACPICA opcode inventory

Run node tests/aml-opcode-inventory/check.mjs to rederive and compare the complete
recorded inventory. No compiler,interpreter or network is used. --write regenerates
inventory.json after deliberate reference/policy updates. The three unmodified
reference files are byte-identical to entries in the pinned ACPICA source archive;
commit/archive/file hashes are recorded in reference.json. Original license headers
are retained.

The short256-entry and extended137-entry parser dispatch arrays are joined to
all131opcode-info entries and amlcode.h definitions. Every instruction definition
must have a dispatch,and every non-placeholder dispatch must be classified.
There are113instruction encodings:84short and29extended. Package and VarPackage
remain distinct encodings although ACPICA displays both as Package. Local/Arg
forms count as distinct encodings. Fourteen parser-internal dispatches and one
Comment tooling extension are separated; field-list subencodings(4),logical
compound aliases(3),prefixes,calculation aliases and2obsolete undispatched bit
operators are classified separately. Reserved/ASCII/prefix dispatch routes are
retained explicitly. Unknown or ambiguous definitions/mappings fail.

Each instruction maps to an existing docs/acpi-coverage.md feature family. This
is bookkeeping,not evidence that an opcode or its operand/type/target/lifetime/
failure/width combinations are implemented,tested or proved. A11,A16,A22,A23
cover cross-cutting behavior and grammar rather than separate instruction bytes.
Normative ACPI grammar reconciliation and per-opcode semantic evidence remain
unfinished; A-AUDIT is not complete. No support percentage follows from113.
A17 LoadTable/Load remain implementation gaps. Modern ACPI6.6 specifies Boolean
results; Unload was deprecated in6.3 and its encoded compatibility/refusal policy
remains unresolved. The113encoded-instruction inventory still includes it.
Dynamic loading requires module/namespace/backing ownership; immutable bootstrap
import is insufficient. See docs/acpi-dynamic-tables.md and the separate
self-authored ACPICA reference suite; neither establishes CuBit opcode support.

Three isolated negative checks reject changed inventory,missing Unload mapping
and modified reference bytes. Evidence: /home/doc/git/.acpi-proof-work/opcode-inventory-work/VALIDATED.json.
No new AML execution,SPARK proof,native build,boot or hardware validation.
