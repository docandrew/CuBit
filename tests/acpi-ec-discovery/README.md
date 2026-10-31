# One-shot EC namespace discovery owner

```sh
export TMPDIR=/home/doc/cubit-build-tmp
nix develop --command bash tests/acpi-ec-discovery/run.sh
```

Set TMPDIR before Nix. Builds/runtime use private outputs,nice19/j1,180s/30s,
5s ACPICA commands,stack64MiB/core0 and logs≤1MiB. Both profiles reject Ada
warnings; checked enables assertions/overflow and release suppresses checks.
The runner checks source hashes and actual ALI switches. `--validate-only`
verifies metadata only; `--source-root DIR` selects an explicit matching tree.

ACPI_EC_Discovery sequences local canonical _HID,_GLK,_GPE,_CRS lookups. Its
four evaluations share one finite AML budget. PNP0C09 accepts integer/string
forms. Missing _GLK defaults to no firmware lock request,not a reservation.
Classic _GPE accepts an integer or two-element Device-reference/integer package.
The trusted HW-reduced mode permits missing _GPE,requires a GPIO interrupt and
refuses a required Global Lock. Resource claims are not admission or permissions.

A capacity failure retains the exact _CRS result for Copy_Resources; retries
perform no AML evaluation or budget reset. Failed releases retain parent/child
handles privately. Close only operates in the owning Core and repeats cleanup,
never AML. No partial description escapes; cleanup failure clears output bytes.
An unrelated retained result survives discovery. One Session cannot restart.
The report's Field_Uncertainty is independent of Pending (result handles only):
no retained roots does not establish lower provider/hardware quiescence. The
flag remains visible even when an overcharge produces Provider_Contract_Error.
Native owners must keep/reconcile lower Field and backend obligations separately.

Is_Current checks saved device/member/block namespace bindings. It does not
establish unchanged firmware values,atomic observation of the tuple,physical
ownership or authority. Description is a historical snapshot. The caller keeps
the copied Data prefix of Observe.Required bytes for GPIO-offset revalidation.
Actual EC device eligibility/_STA/_INI order,provider readiness,boot UID matching,
resource backing/alias admission,Global Lock acquisition and GPE-block identity
are outer-controller responsibilities. No hardware or interrupt capability is
created. Do not make a blocking upward call from an EC backend into AML.

The309checks/profile use the actual AML service core,resource reader/projection
and discovery owner. They cover defaults,integer/string IDs,scalar/package GPE,
HW-reduced GPIO constraints,budget exhaustion,capacity retry,foreign contexts,
parent/child cleanup failure,malformed evaluator responses,latched uncertainty,
consumed-session refusal,invalid devices and preservation of unrelated roots.
Only charge/empty-handle/unknown-result and release-failure cases inject trusted
callback faults; normal cases evaluate the requested member directly.

Normal pinned ACPICA20260408 compilation accepts Alias(GPES,_GPE) and repair-
disabled execution confirms the actual package/device/17 value. No package
callback substitution,forced AML or diagnostic suppression is used. Direct
package Name/Method forms remain rejected by this compiler as recorded in the
preceding resource milestone; alias coverage does not erase that discrepancy.
The empty-template warning and shutdown cache allocations remain in raw logs.

SPARK_Mode/checked contracts are not a formal proof of this new owner. Native
startup,provider attachment,admission/locking,SCI/GPE delivery/rearm,hardware
acceptance and complete service/AML verification remain unfinished.
