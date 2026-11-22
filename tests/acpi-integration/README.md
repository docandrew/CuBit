# Hosted ACPI integration

These projects exercise the same production AML, firmware and service sources
as the original `tests/aml-core/aml.gpr`, with separate object/output directories.
`integration_release.gpr` uses `-gnatp`; `integration_checked.gpr` uses
`-gnata -gnato`. Ordinary Check oracles remain active in release. Full Ghost
state-preservation assertions execute only in checked mode. Neither is a proof.

The fourteen suites cover authenticated retained MCFG/SLIT/MADT queries, bootstrap
package-member initialization/diagnostics, requests/endpoints, shared package
members, owned copying/references, and string comparisons. The `mocks` directory
contains hosted CuBit grant primitives, never production implementations. Native
block dispatch is exercised, but real process identities, kernel authentication,
acquisition, revocation, teardown and live startup are not established here.

The owned Timer_Verification fixture and its matching specification are private
to this project. Original timer/region/read-only fixtures in `tests/aml-core`
retain their unowned contexts and explicitly reject object/reference/comparison
callbacks; Keep_Value preserves their pre-existing descriptors without access
to an owned arena. Do not mix those fixtures' specifications or claim object ownership
from their successful integer tests.

Table request labels0–7 retain their original meanings;8 is reserved for native
grant import. Read-only metadata labels9–12 cover MCFG/SLIT;13–15 cover MADT.
All generic read predicates exclude8. MADT typed decoding supports wire kinds
0–5,9,10; unknown kinds retain bounded record metadata and reject typed queries.
Descriptions confer no hardware authority. SRAT labels 16–18 add authenticated info/record/typed queries. Their domain,
address and handle metadata confer no authority.

Read_Metrics page7 reports package-member Bound/Missing/Unsupported, page8 reports
Initialized/Pending/zero. Bootstrap initializes only after all advertised tables
arrive, retains the report and publishes Complete. Missing/unsupported members
remain uninitialized and are not retried. This is not general ACPI device `_INI`
or `_REG` execution. Observer/provider authority and revision rules remain.

The original table_runner now explicitly initializes its single installed table
before reading/invoking. Successful output remains compatible; nonzero missing/
unsupported counts go to stderr. `check_runner_initialization.py` compares a
forward repeated-buffer alias against ACPICA in both widths and checks stable
missing/unsupported reports and uninitialized package slots. Its output is
written below runner-fixtures; those generated artifacts are not source files.


String replacement and owner Store suites cover quota failure, identity,
retained aliases, descriptor refresh and valid/invalidated Index references.
Old byte extents are append-only, with no garbage collection. Full mixed-type
Store and CopyObject ARGR/LOCR/INXR remain unsupported. The SRAT suite checks
record/page combinations and unchanged diagnostics through hosted native dispatch.

Run the GPR projects from this directory under the repository Nix shell with
TMPDIR exported first and one compiler job. Checked tests use a 64 MiB stack
budget. The runner initialization harness accepts `--tools DIRECTORY` pointing
to pinned ACPICA executables and sets that hosted stack budget explicitly.


Pre-quota integration checkpoint: all 52 original hosted mains compile. Fourteen
integration suites pass 72,308 explicit checks per release/checked mode; checked
also runs contracts and Ghost assertions. Ten affected original checked suites
pass 42,238 checks. The original typed suite keeps every bytecode/fuel scenario,
now using Owned.Load/Invoke and snapshots: string captures require independent
IDs and equal data; nonallocating paths retain exact state equality, allocating
paths preserve all original nodes, methods, bindings and object payloads. Each
fuel trial starts with fresh owned storage. The original Store target sweep now
expects Truncated for recognized Index opcode 0x88 without operands.

Two fresh ACPICA alias comparisons and six initialization-diagnostic runs pass.
Store's prior 58 comparisons plus four local identity comparisons, and CopyObject's
36/42 matches with six explicit unsupported cases, are inherited evidence:
all 44 production AML source files at that checkpoint matched the audited
candidate. They were not rerun in this service tree. The subsequent quota follow-up
changes three AML units solely to preserve Value_Limit across local/argument and
named write adapters; its focused validation is recorded separately.
No new proof, native build or live service test was performed.

Quota follow-up validation: session26911 exited zero. Three focused suites pass
372 explicit checks per release/checked mode (quota256, owner Store39,
replacement77); checked includes assertions and overflow checks. Four original
generic caller mains compile/link with the appended Write_Status value. The
parent's broader suites were not repeated. Entire invocation used nice19/-j1.
