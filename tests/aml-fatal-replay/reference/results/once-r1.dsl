/*
 * Intel ACPI Component Architecture
 * AML/ASL+ Disassembler version 20260408 (64-bit version)
 * Copyright (c) 2000 - 2026 Intel Corporation
 * 
 * Disassembling to symbolic ASL+ operators
 *
 * Disassembly of /home/doc/git/.acpi-proof-work/aml-fatal-normal-names-yk5xpkv8/results/once-r1.aml
 *
 * Original Table Header:
 *     Signature        "DSDT"
 *     Length           0x00000073 (115)
 *     Revision         0x01 **** 32-bit table (V1), no 64-bit math support
 *     Checksum         0xAA
 *     OEM ID           "CUBIT"
 *     OEM Table ID     "FATALREF"
 *     OEM Revision     0x00000001 (1)
 *     Compiler ID      "INTL"
 *     Compiler Version 0x20260408 (539362312)
 */
DefinitionBlock ("", "DSDT", 1, "CUBIT", "FATALREF", 0x00000001)
{
    Name (MARK, Zero)
    Name (CNT0, Zero)
    Method (MARG, 0, NotSerialized)
    {
        CNT0++
        Return (0x55)
    }

    Method (TEST, 0, NotSerialized)
    {
        Fatal (0xA5, 0x12345678, MARG ())
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

