# Named read-only ACPI PM timer probe

Hosted: export TMPDIR=/home/doc/cubit-build-tmp before entering Nix, then run bash tests/acpi-register-probe/run.sh. Tests use modeled metadata/read/clock callbacks, not kernel IPC or hardware. All declared inputs are hash checked before and after.

The native/ component is an application for coordinator-controlled testing after acpi-backend.svc registers role35. It requests logstore and acpi-registers only: no firmware or hardware grant. Trusted startup must explicitly approve the declared register scope. There is no image/launcher/build-catalog edit in this test addition. Use ccl-build; there is no production .gpr or per-program Makefile rule.

One context, one finite2000ms budget, at most8successful named read-only Dword PM-timer reads. Full metadata/unique IDs and epoch are checked. Failure/unknown completion stops with no replay. Compare low24bits only; changing ticks do not prove timer frequency or elapsed-time accuracy. This does not validate AML, EC, lid, battery, GPIO or I2C hardware.

Require stage timer ok COUNTER_CHANGED and TEST: PASS acpi-register-probe. Failure reasons are fixed explicit strings even when native Enum'Image is numeric. Initialization failures print their typed reason, bring-up summary and TEST: FAIL. Native logging lifetime/IPC/hardware remain unvalidated until the coordinator runs it. The default manifest endpoint grant is an explicitly trusted test scope; do not treat this app as a general untrusted hardware capability.
