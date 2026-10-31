# Controlled module Field startup

    export TMPDIR=/home/doc/cubit-build-tmp
    nix develop --command bash tests/acpi-module-startup/run.sh checked
    nix develop --command bash tests/acpi-module-startup/run.sh release

--validate-only checks the closed source inventory and pins. Each profile runs
1030 checks in45 isolated Ada processes, including both AML integer widths.
The actual Core, namespace and Field execution paths are used; hardware/provider
callbacks and the region barrier remain hosted models.

Trusted startup targets may cover direct ordinary NoLock Fields through64bits,
including partial bytes and up to9 datums. The entire named association must be
present and unambiguous before dispatch. Targets do not come from firmware or
wire input. Existing write-rights preflight, current geometry/epoch checks,
update rules, finite request limits and unknown-operation receipts remain in
the shared execution engine. Named geometry alone creates no authority.
Bank/index/connected or otherwise unsupported module layouts remain refused.
The default module executor stays hardware-free. Completed/failed startup is
one-shot; partial known effects and unknown receipts never authorize replay.

Besides the existing binding/module/copied-startup cases, tests cover complete
and partial two-cell stores/readback, write-only versus required RMW rights,
missing/duplicate/stale/forged cells, later geometry drift, uncertain completion
and no replay. The64-bit field starting at bit4 exercises all9byte cells,
Buffer input and eight-byte result preservation at both AML widths, terminal
root cleanup, last-cell permission/geometry refusal before effects, and an
unknown last write retaining its exact receipt.

ACPICA comparisons are recorded explicitly:

- Original module/read fixtures compare their expected scalar values.
- Wide/partial fixtures compare final bytes and scalar values after explicit
  initialization matching the modeled provider's initial contents.
- Tight SystemMemory and padded16-byte EC versions compare all eight result
  bytes and edge preservation at both AML widths.
- ACPICA20260408 rejects the tight9-byte EC version with AE_AML_REGION_LIMIT;
  this is a recorded reference discrepancy, not a passing equivalence claim.
  All references retain their known cache-cleanup diagnostics. None establishes
  physical EC protocol/access counts, automatic startup ordering or hardware.

The tight EC difference is consistent with the upstream EC-specific widening
of multi-byte field access in [exprep.c](https://github.com/acpica/acpica/blob/master/source/components/executer/exprep.c),
followed by the region-width check in [exfldio.c](https://github.com/acpica/acpica/blob/master/source/components/executer/exfldio.c).
Those links describe current upstream source, not the pinned tool source tree.
The observed pinned-tool error is Base+Offset+Width0+8+8 beyond length9. Our
named-cell engine uses byte accesses within the admitted nine cells; it does
not gain permissions for extra bytes to match the reference's internal width.
The ACPI [Field rules, table19.34](https://uefi.org/htmlspecs/ACPI_Spec_6_4_html/19_ASL_Reference/ACPI_Source_Language_Reference.html)
list byte access for EmbeddedControl. The independent ordinary-memory/padded
controls validate values without erasing the tight-region difference.

These are hosted functional/reference tests, not new SPARK proofs, native
CuBit execution or hardware validation. Independent backend admission,
reservation/held Global Lock, provider/region enrollment, process lifetime and
recovery supervision remain required. Full verified service/interpreter and
native/hardware acceptance remain unfinished.


## Optional composed startup selection

Bank/index/nested/wide module startup now has a separate opt-in runner:

    bash tests/acpi-module-startup/composed/run.sh --selection all

See composed/README.md for the15596checks/716processes, selectors, modeled-provider assumptions, explicit ACPICA discrepancy and remaining proof/native gaps. The original runner above remains1030checks/profile. Its standalone direct-only description is historical scope of that matrix; general startup behavior is described by the composed suite.
