# Hosted AML Field I/O

Run `python3 tests/aml-field-io/run.py --mode release` or `--mode checked` in the pinned Nix environment with TMPDIR set as required by coordination/acpi.md.

The two selections exercise 516 Core checks and 186 unknown-completion checks. They cover one explicitly bound, named whole Byte/Word/Dword NoLock register per invocation through a trusted hosted provider. Default invocation does not acquire hardware access. Binding revalidates copied geometry and issuer, endpoint, epoch and named-register identity before dispatch.

Unknown completion stops subsequent AML effects and retains a receipt outside namespace rollback. A trusted controller must supply exact terminal completion evidence; reconciliation does not replay the operation or restore revoked authority.

These are hosted tests. Partial or multiple-datum fields, Qword, BankField, IndexField, LockRule/FACS and native transport are not implemented by this path. The separate bare-Namespace reset tests do not fix the GNAT 15.3 crash when instantiating Field_IO through an external Core.Values generic. The whole interpreter is not yet SPARK-proved.
