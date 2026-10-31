# Field references through Local and Arg

These hosted regressions require a table field read through CondRefOf and DerefOf to agree with the direct field read. Cases cover32/64-bit AML integers,8-bit integer and72-bit Buffer fields, unavailable owned backing, zero-budget refusal, and explicit reference identity. The native driver and hardware are not exercised.

Export TMPDIR=/home/doc/cubit-build-tmp before entering Nix. Run with the development environment's Node,GNAT,gprbuild and ACPICA tools:

```sh
export TMPDIR=/home/doc/cubit-build-tmp
nix develop -c node tests/aml-field-dereference/run.mjs --mode checked
nix develop -c node tests/aml-field-dereference/run.mjs --mode release
```

The runner verifies exact source inputs before and after execution, bounds compiler/runtime output and deadlines, checks98 assertions/profile, and compares the direct/Local/Arg integer results with fresh ACPICA fixtures for both table revisions. ACPICA20260408 reports four outstanding cache allocations on shutdown; this is retained in the logs and is not a clean-shutdown claim. Explicit RefOf is covered as identity preservation in hosted code, not claimed as ACPICA external-result equivalence.

The separate tests/aml-field-reset suite covers named-register provider access and reset/reconciliation. No SPARK proof is added here.

The callback-contract fixture checks10 explicit failure/success classifications through the real generic executor, plus a positive field-read control in each AML width. A failed status-only lookup cannot fabricate a successful result or a fatal request payload. The two executables total165 assertions/profile; named-register compatibility is checked separately in aml-field-reset.
