# Hosted AML Field I/O

Run `python3 tests/aml-field-io/run.py --mode release` or `--mode checked` in the pinned Nix environment with TMPDIR set as required by coordination/acpi.md.

The six selections cover 516 Core checks, 186 unknown-completion checks,
482 partial/Buffer-read checks, 440 AnyAcc/SystemIO-read checks and 6,313
partial-write checks, plus 6,663 Qword checks. Each passed release and assertion-enabled profiles.
The preceding production composition passed 17,294 checks/profile across
24 mains; exact source and carried dependency scopes are recorded in
`MULTIDATUM-EVIDENCE.json`. This is not a rerun of all service selections.

Ordinary NoLock SystemMemory/SystemIO Fields up to 64 bits use up to nine
explicit named Byte/Word/Dword targets (and Memory Qword targets); AnyAcc uses bytes. Default invocation
does not acquire hardware authority. Read bindings never enable writes.
Trusted `Add_Write_Binding` requires existing complete-register write authority
and safe exact-write admission for every target. Preserve also needs read
permission on partial datums. Geometry, issuer, endpoint, epoch and direction
permissions are checked before effects and again at each operation.

Writes are sequential and may leave a completed prefix on later failure.
Buffer/String sources use raw bytes, including zero padding of short/empty
sources; Integer sources follow the AML integer width. Read results wider
than that width use a Buffer. The nine-datum boundary and SystemIO callback
space are tested in a hosted model, not on physical hardware.

Unknown completion stops subsequent AML effects and retains a receipt outside namespace rollback. A trusted controller must supply exact terminal completion evidence; reconciliation does not replay the operation or restore revoked authority.

Run `bash tests/aml-field-io/check_partial_write_reference.sh` in the pinned
environment to verify the portable cached ACPICA evidence. It extracts byte
observations from the original raw logs and reproduces `field_write_oracle.ads`:
48 normal ACPICA tables, 120 stores and 1,440 compared bytes. It does not run
ACPICA or measure bus access ordering. Original tool paths are provenance only.

Fields larger than 64 bits, SystemIO QwordAcc/BufferAcc, BankField, IndexField,
connections, LockRule/FACS and native transport remain unfinished. Bare
returned DerefOf(RefOf(Field)) still differs from ACPICA's external AE_TYPE
result; arithmetic-consumed cases match. The whole interpreter is not proved.
The read-plan helper proof is historical after Qword admission changed; datum
helper safety evidence is separate. No new end-to-end proof or native correctness is claimed.

The six current selections total 14,600 checks per profile. Memory Qword
reads cover the full 64-bit range and one/two-register fields. Qword tests
also cover AML integer widths, raw Buffer/String stores, permission changes
during read-modify-write, partial completion and unknown receipts without replay.
Run `bash tests/aml-field-io/check_qword_reference.sh` to re-extract retained
ACPICA byte observations and reproduce all 90 store goldens. This uses only
the local reference corpus and shell tools; original tool paths are provenance.
`QWORD-EVIDENCE.json` records the exact hosted source and result scopes.
