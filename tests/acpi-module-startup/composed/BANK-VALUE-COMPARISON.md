# Wide bank selection value comparison

The installed ACPICA20260408 control writes only bytes10 32 54 76 followed by
zeros into the80-bit selector at either table revision. The original literal is
FEDCBA9876543210. The revision2 observation therefore differs from CuBit's
existing full AML-width Integer policy. The runner records this as a discrepancy;
it must not count that control as equivalence or silently truncate CuBit's value.

The [ACPI6.6 AML grammar](https://uefi.org/sites/default/files/resources/ACPI_Spec_6.6.pdf)
section20.2.5.2 describes BankValue as TermArg yielding Integer. The accessible
[ACPI6.4 BankField reference](https://uefi.org/htmlspecs/ACPI_Spec_6_4_html/19_ASL_Reference/ACPI_Source_Language_Reference.html#bankfield-declare-bank-data-field)
also describes an Integer; the general integer definition follows table width.
Preserving that width is our interpretation, not a newly proved conformance fact
or a settled compatibility decision for all firmware.

Current [upstream AcpiDsEvalBankFieldOperands](https://github.com/open-acpica/acpica/blob/master/source/components/dispatcher/dsopcode.c)
casts the evaluated value to UINT32 before saving it. That is consistent with the
installed-tool observation; current master is not pinned source provenance for
the installed binary. The tool source/hash and raw run logs remain in the test
evidence. No external bug report, tool patch or implementation policy change was
made. Known cache-cleanup diagnostics remain qualified separately.
