# Hosted AML remote Field provider

In the pinned Nix environment with TMPDIR set according to coordination/acpi.md,
run each gate/profile separately and inspect the result:

```sh
nice -n19 python3 tests/acpi-field-remote/run.py --mode release --selection field_provider_api
nice -n19 python3 tests/acpi-field-remote/run.py --mode release --selection remote_field_tests
nice -n19 python3 tests/acpi-field-remote/run.py --mode checked --selection field_provider_api
nice -n19 python3 tests/acpi-field-remote/run.py --mode checked --selection remote_field_tests
```

The API gate compiles the native provider and actual Core.Field_IO instantiation
against exact mirrored runtime declarations; it does not link or run. Runtime
tests exercise actual Core/AML/Field plans, the remote provider/client and the
Region_IO dispatcher through a hosted packet transport and modeled hardware.
There are 11,930 checks per profile, including all 90 retained normal ACPICA Qword
store images (all 16 bytes compared), integer-width reads, raw Buffer/String
stores, per-operation permission changes, unknown completion and no replay.

Additional cases cover descriptor rejection, bounded layered confirmation
history, history capacity before effects, facts surviving later uncertainty,
revocation during pending completion, strict epoch replacement, independent
backend denial despite permissive metadata, and completed out-of-width reads.
Native provisioning, actual IPC provenance/deadline enforcement, descriptor
provenance, hardware semantics/backing and terminal evidence are modeled or
trusted boundaries, not native acceptance results. The underlying CuBit adapter
has its own separate API and hosted gates in tests/acpi-register-transport.

Source/mirror/tool/runtime hashes, compiled ALI dependencies and fatal Ada warning
flags are recorded. Builds are single-worker/180 seconds, runtime30seconds,
stack64MiB and logs bounded; use at most two heavy workers. Historical exact
receipts are retained in EVIDENCE.json, with no private executable fallback.
Selected codec/client/provider contracts have a separate reproducible proof gate
in tests/acpi-client-proof. Those selected proofs do not establish complete
provider or interpreter correctness. `--validate-only` checks registration only.

Exact single-register `Bind` and `Add_Binding` now admit explicit aligned memory
Qword fields through the common datum engine. The expanded matrix adds 5,045
checks across both APIs, both AML integer widths, every existing source/update
kind, the aligned ACPICA store images, layout rejection, revocation and uncertain
reads/writes. A Qword read returns an eight-byte Buffer in 32-bit AML and an
Integer in 64-bit AML. Reconciliation leaves backend authority revoked; only an
explicit newer-epoch regrant, metadata replacement and fresh binding restores
access. I/O-port Qword, selector fields and locking remain unsupported.

The exact-Qword functionality is regression-tested, without a new SPARK proof.
The earlier layout admission proof excluded Qword and is historical for the
modified Admitted body/postcondition. Historical 6,885-check receipts remain in
EVIDENCE.json; the expanded runtime receipts are in EXACT-QWORD-EVIDENCE.json.

The API gate also instantiates the region notification coordinator through the
actual native provider and Core.Field_IO types. This is compile-only evidence;
the separate hosted `_REG` matrix is in tests/aml-field-initializer. Registration
updates do not imply a fresh rerun of the older remote runtime matrix.
