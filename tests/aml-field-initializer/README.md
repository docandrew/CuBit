# Hosted aml-field-initializer

Field initializer execution, region notifications, named evaluation, adversarial
evaluation and device initialization. Checks split 1195/88/85/60.

Run `python3 tests/aml-field-initializer/run.py --mode release` or `--mode checked`
in the pinned Nix environment, with the required TMPDIR. Both profiles passed
1,309 checks on the current production sources. `--validate-only` checks
registration and hashes without compilation.

The combined Field composition is recorded in
`../aml-field-io/MULTIDATUM-EVIDENCE.json`; receipt source scopes remain distinct.
Bounds remain 180 seconds/build, 30 seconds/runtime, 64 MiB stack and 1 MiB/log.
No physical hardware, native deployment or whole-interpreter proof is claimed.

The new region coordinator contributes 776 checks to the first main. Cases cover
local per-region `_REG`, connect/disconnect arguments, always-available-space
suppression, trusted PCI-root/local-BBN handling, readiness preflight, service
identity, copied metadata failure outputs, fuel, ignored compound returns,
ordinary failure, Fatal, uncertainty and no replay. A real device barrier runs
notifications after the root/system-bus prepass and before the device walk.

Two normal iASL images (revisions 1/2) are loaded byte-for-byte and compared with
28 getter values extracted from ordinary acpiexec output. Its automatic startup
logs are preserved separately from the explicit reset/notification sequence.
Both reference processes exited zero, but both logs end with a four-allocation
cache diagnostic; that diagnostic is retained and is not represented as a clean
full-startup result. No force, slack, disabled initialization or custom handler
was used. Verify the retained data without executing ACPICA:

```sh
bash tests/aml-field-initializer/check_region_reference.sh
```

The metadata API checks descriptor currentness; wrong-service rejection and
cleared failure outputs are exercised here. Deferred-region
rejection is an explicit guard, not an independently constructed deferred-region
runtime case. EC availability is modeled; actual effects use admitted memory
Fields. Native provisioning, qualified/orphan notices, full lifecycle integration
and current-source proofs remain unfinished. REGION-EVIDENCE.json records exact
source scopes; private receipt paths are never executable fallbacks.

The lid regression adds 61 checks to `field_initializer_tests` (1195 total after typed-query and identity cases).
`lid-reference/lid.asl` is ordinary ASL compiled by ACPICA 20260408. Its complete
156-byte DSDT is embedded in the existing test unit. `_HID` identifies PNP0C0D;
`_LID` reads the admitted byte Field. Three test-only methods write simulated
closed/open/nonzero-open state and issue Notify(LID0, 0x80). They are fixture
controls, not production interfaces exposed to observers.

Run `bash tests/aml-field-initializer/check_lid_reference.sh` before the hosted
runner. It checks exact embedded bytes/checksum, seven typed ACPICA results,
and three device notifications against retained output. The transcript retains
ACPICA's known four-cache-allocation teardown diagnostic; it is not a claim of
clean ACPICA teardown. The existing runner authenticates all reference hashes.
Both profiles exercise the real Field I/O adapter and modeled named-register
provider, copied notification target/path/value, repeated Notify, read-only
queries and zero-budget transitions without effects. The initial simulated
value 15 and repeated final transition are additional local regressions rather
than additional ACPICA observations. Nonzero values are preserved for later
lid policy; no power-manager policy or native CCL event transport is tested.

This does not establish physical lid/SCI/GPE/EC operation, firmware quirk handling,
native service startup or hardware backlight support. No production code or
SPARK proof scope changes in this fixture milestone.

`ACPI_Lid.Query` is now the internal typed lid adapter exercised by these cases.
It accepts a trusted owner's current Device descriptor, resolves only `_LID`,
and returns Unknown/Closed/Open plus explicit lookup/evaluation/type/cleanup
outcome, charged budget and raw integer sample. Zero means closed; every nonzero
integer means open. Missing, no-return and compound returns remain Unknown;
compound results are released. Zero budget, non-device and foreign-arena
bindings are rejected without invocation. The device owner must separately
identify PNP0C0D; this helper does not discover devices or establish authority.
The generic evaluator must obey Core validity/catalog/result contracts.
The two new production units are SPARK-mode source with executable contracts,
but no fresh SPARK proof claim is made. CCL observer/controller integration,
notification-to-sample policy and native SCI/GPE/EC support remain unfinished.

`ACPI_Lid.Identify` provides registration-time `_HID` identification through the
same trusted evaluator. It matches exact PNP0C0D integer/string forms, preserves
the caller's retained roots, and releases temporary strings/other objects on
success, mismatch or type failure. Its 39 additional checks bring this selection
to1195. Cases include shortened/suffixed strings, a high-word integer that must
not be narrowed, buffer/package/no-return/missing forms, a foreign arena,
zero budget and a full retained-root table. String/negative identity fixtures
are constructed local AML, not additional ACPICA comparison observations.

The accepted runtime source is the immutable lid-identification-pmy1ThND/source
checkpoint; only docs/hash metadata change here. Consumed Ada and GPR hashes must
match that origin before carrying its release/checked receipts. The helper is
not a discovery walk, registration store, `_STA` policy or CCL controller gate.
It makes the previously documented identity check implementable by the owner;
registration and native event/IPC integration still remain.

The `_HID` integer/string forms follow ACPI Device Configuration:
https://uefi.org/specs/ACPI/6.5/06_Device_Configuration.html
No new SPARK proof is claimed for these two adapter units.
