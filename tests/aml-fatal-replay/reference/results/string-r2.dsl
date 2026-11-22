/*
 * Intel ACPI Component Architecture
 * AML/ASL+ Disassembler version 20260408 (64-bit version)
 * Copyright (c) 2000 - 2026 Intel Corporation
 * 
 * Disassembling to symbolic ASL+ operators
 *
 * Disassembly of /home/doc/git/.acpi-proof-work/aml-fatal-normal-names-yk5xpkv8/results/string-r2.aml
 *
 * Original Table Header:
 *     Signature        "DSDT"
 *     Length           0x00000072 (114)
 *     Revision         0x02
 *     Checksum         0xEA
 *     OEM ID           "CUBIT"
 *     OEM Table ID     "FATALREF"
 *     OEM Revision     0x00000001 (1)
 *     Compiler ID      "INTL"
 *     Compiler Version 0x20260408 (539362312)
 */
DefinitionBlock ("", "DSDT", 2, "CUBIT", "FATALREF", 0x00000001)
{
    Name (MARK, Zero)
    Name (CNT0, Zero)
    Method (TEST, 0, NotSerialized)
    {
        Fatal (0xA5, 0x12345678, "123456789ABCDEF0")
        MARK = 0x77
        Return (0x88)
    }

    Method (READ, 0, NotSerialized)
    {
        Return (MARK) /* \MARK */
    }

    Method (CNTR, 0, NotSerialized)
    {
        Return (CNT0) /* \CNT0 */
    }
}

