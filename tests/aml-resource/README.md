# Hosted resource-template concatenation checks

Run in the repository Nix shell:

```
python3 tests/acpi-hosted/run.py --group resource-templates --mode release
python3 tests/acpi-hosted/run.py --group resource-templates --mode checked
```

The explicit group builds six functional mains and resource_runner, then checks 72 cached ACPICA observations with exact status, payload and MARK gates. Existing all-group selection is unchanged.

The canonical six-main group passed 19,575 checks plus 72 cached comparisons per strict profile. This total includes the pure helper (14,999), ordinary owner (4,282), resource collecting (150), resource Core (22), Match collecting (100), and Match Core (22). The separate 12-check whitebox maximum-String fixture is not part of this group and does not establish that AML literals can load strings beyond the existing parser limit.

Resource parsing follows the documented pinned ACPICA structural profile, including its large-descriptor remaining-length precheck and zero-length small-vendor compatibility behavior. This is not full normative descriptor validation and grants no hardware access. Conversion uses bounded virtual input views without temporary AML object allocation; equivalent ACPICA out-of-memory behavior is not claimed. No resource helper, owner or interpreter proof is claimed here.
