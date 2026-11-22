# Hosted HID-I2C discovery milestone

This opt-in suite exercises the actual userspace ACPI Core and its retained
argument constructors, compatible-ID and descriptor queries, copied resource
association, and the existing exhaustive GPIO matrix. It runs on Linux with
fake or absent hardware callbacks. It does not build or boot CuBit or operate
the laptop, I2C controller, GPIO interrupt or HID input driver.

Use the pinned development shell with `TMPDIR=/home/doc/cubit-build-tmp` set
before entering Nix and inside it:

```
python3 tests/acpi-hid-i2c/run.py --validate-only
python3 tests/acpi-hid-i2c/run.py --mode release
python3 tests/acpi-hid-i2c/run.py --mode checked
```

An explicit `--selection arguments identity status descriptor resources discovery power sleep gpio` chooses
unique labels; the default runs all nine. Each selection has its own private
objects, binary and bounded log. The existing maintained capture and inventory
guards are reused. A fixture argument is accepted only when named in the
pinned reference manifest. Compiler/runtime/binary/ALI hashes are recorded.
Compile180s, run30s, stack64MiB, log1MiB, j1, nice19. Checked uses gnata+gnato;
release uses gnatp. These are hosted test GPR projects, not native component
declarations or a native build-system change.

Expected counts per profile:

| Selection | Checks | Scope |
| --- | ---: | --- |
| arguments | 101 | Constructed UUID/package arguments and retained ownership |
| identity | 203 | Exact PNP0C50 scalar/flat/named/aliased IDs, budgets and cleanup |
| status | 117 | Direct/named/aliased _STA, defaults, independent flags, budgets and cleanup |
| descriptor | 327 | Device-local direct/aliased _DSM capability/address protocol |
| resources | 628 | Core _CID/typed _STA/_CRS copy/association/_DSM composition |
| discovery | 203 | Retained _CRS sizing/cancel/context/shared-budget discovery session |
| power | 132 | ACPI0003 identity and per-source _PSR online/offline/unknown |
| sleep | 424 | Root _Sx declaration/used-type observations and unused-return handling |
| gpio | 590956 | Existing GPIO decoder/typed settings/pin matrix |

The total is 593091 checks/profile. Counts are not AML coverage percentages.
The status selection preserves all64 status bits and separately observes
presence, enablement and functioning. Missing status uses the existing device
policy default without ancestor evaluation. Noninteger returns and execution
failures remain distinct; temporary results are released. Observed status is
not device initialization or hardware readiness. The resources selection now
uses this helper with the remaining shared budget, replacing its two direct
member-lookup assertions with the separately tested status observation.
The nine-selection aggregate is an expected count, not a claim that every
selection was freshly run after every change; consult the selected run records.

The discovery session retains the same _CRS across sizing requests. It requires
owner serialization while pending: retaining an AML object does not freeze it.
Foreign-context requests preserve the true owner's session; cancellation and
completion release only its own result. These are ordered observations, not an
atomic readiness snapshot. The power selection distinguishes source use from
physical cable presence. The sleep selection separately records declaration,
mandatory S0/S5 indication and usable type fields. It evaluates present objects
even when their return is unused, and ignores B when the register model does.
No power transition, native service/driver, controller or register access is
established by these tests. Pure resource/GPIO proof scope below does not cover
the new discovery/power/sleep generic bodies or their trusted callbacks.
The resources main sizes and resizes a copy using the same retained _CRS
result; it verifies one AML evaluation. Disabled _STA remains distinct from
valid resources. Missing/duplicate connections, dependent alternatives,
truncation, checksums, item quotas and high array bounds are covered. The
controller owner still establishes _HID, initialization, dependencies, power,
controller identity and hardware rights. A compatible ID or copied span grants
no authority. A production probe coordinator remains unfinished.

The reference directory contains pinned ASL/AML/hex fixtures and captured
ACPICA20260408 logs. `hid_aliases.log` records the updated alias fixtures:
aliased `_CID` returns the expected two IDs, and aliased `_DSM` returns support
mask 03 and descriptor register 1234. Earlier direct-fixture logs are retained
as historical observations. Normal tests never regenerate the oracle. The
resource fixture intentionally produces an empty-ResourceTemplate warning.
acpiexec logs retain outstanding-allocation teardown diagnostics (four for
the updated query fixture, 28 for updated identity, fourteen for earlier
identity/resources); this is not a clean allocation audit. ACPICA confirms valid
return forms/settings, not every malformed-input consumer policy assertion.

The independent selected proof is opt-in:

```
bash tests/acpi-hid-i2c/run-proof.sh --validate-only
nice -n19 bash tests/acpi-hid-i2c/run-proof.sh prove
bash tests/acpi-hid-i2c/check-proof.sh "$PWD" "$PROOF_RESULT" "$RECEIPT_JSON"
```

The selected bodies are resource association and GPIO. The predecessor private
scope discharged association35 checks/66 goals plus GPIO238/405, 32 flow checks,
14 all-SPARK entities and no unresolved/skipped checks or pragma Assume.
The GPIO change moves unchanged ghost expression completions/constants from
its body to the private specification for client proof visibility. Public
declarations/contracts and operational body are preserved. The selected proof
covers runtime safety, initialization/dependencies/termination, declared result
shape and existing GPIO correspondence, with upstream framing/serial contracts
as modular assumptions. It is not dependency-closed acceptance completeness,
exact error classification, generic service ownership, hardware admission or
whole AML/native correctness. Constructor/identity/query service proofs remain
unfinished; checked tests do not substitute for them.

Current input manifests identify this source; they do not rewrite historical
proof receipts or make their old input hashes current. The changed Core,
Namespace and GPIO units invalidate any blanket carry claim for earlier proof
closures. Publication needs fresh registered runs and audited evidence. The
full userspace-service and verified-interpreter goal remains incomplete.
