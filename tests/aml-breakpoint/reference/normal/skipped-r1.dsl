/*
 * Intel ACPI Component Architecture
 * AML/ASL+ Disassembler version 20260408 (64-bit version)
 * Copyright (c) 2000 - 2026 Intel Corporation
 * 
 * Disassembling to symbolic ASL+ operators
 *
 * Disassembly of /home/doc/git/.acpi-proof-work/aml-breakpoint-disassembly-oracle-fdss8jb1/results/skipped-r1.aml
 *
 * Original Table Header:
 *     Signature        "DSDT"
 *     Length           0x0000004B (75)
 *     Revision         0x01 **** 32-bit table (V1), no 64-bit math support
 *     Checksum         0xF2
 *     OEM ID           "CUBIT"
 *     OEM Table ID     "BRKPOINT"
 *     OEM Revision     0x00000001 (1)
 *     Compiler ID      "INTL"
 *     Compiler Version 0x20260408 (539362312)
 */
DefinitionBlock ("", "DSDT", 1, "CUBIT", "BRKPOINT", 0x00000001)
{
    Name (MARK, Zero)
    Method (TEST, 0, NotSerialized)
    {
        If (Zero)
        {
            BreakPoint
            MARK = 0x55
        }

        Return (0x66)
    }

    Method (READ, 0, NotSerialized)
    {
        Return (MARK) /* \MARK */
    }
}

