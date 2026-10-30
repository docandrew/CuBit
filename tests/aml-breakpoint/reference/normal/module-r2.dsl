/*
 * Intel ACPI Component Architecture
 * AML/ASL+ Disassembler version 20260408 (64-bit version)
 * Copyright (c) 2000 - 2026 Intel Corporation
 * 
 * Disassembling to symbolic ASL+ operators
 *
 * Disassembly of /home/doc/git/.acpi-proof-work/aml-breakpoint-disassembly-oracle-fdss8jb1/results/module-r2.aml
 *
 * Original Table Header:
 *     Signature        "DSDT"
 *     Length           0x0000004D (77)
 *     Revision         0x02
 *     Checksum         0x1A
 *     OEM ID           "CUBIT"
 *     OEM Table ID     "BRKPOINT"
 *     OEM Revision     0x00000001 (1)
 *     Compiler ID      "INTL"
 *     Compiler Version 0x20260408 (539362312)
 */
DefinitionBlock ("", "DSDT", 2, "CUBIT", "BRKPOINT", 0x00000001)
{
    Name (MARK, Zero)
    If (One)
    {
        BreakPoint
        MARK = 0x77
    }

    Method (TEST, 0, NotSerialized)
    {
        Return (MARK) /* \MARK */
    }

    Method (READ, 0, NotSerialized)
    {
        Return (MARK) /* \MARK */
    }
}

