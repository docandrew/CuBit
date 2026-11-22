/*
 * Intel ACPI Component Architecture
 * AML/ASL+ Disassembler version 20260408 (64-bit version)
 * Copyright (c) 2000 - 2026 Intel Corporation
 * 
 * Disassembling to symbolic ASL+ operators
 *
 * Disassembly of /home/doc/git/.acpi-proof-work/mutex-oracle-identifier-validation-j8q69sjp/results/r1-unowned-release/table.aml
 *
 * Original Table Header:
 *     Signature        "DSDT"
 *     Length           0x00000086 (134)
 *     Revision         0x01 **** 32-bit table (V1), no 64-bit math support
 *     Checksum         0x0B
 *     OEM ID           "CUBIT"
 *     OEM Table ID     "MUTEX"
 *     OEM Revision     0x00000001 (1)
 *     Compiler ID      "INTL"
 *     Compiler Version 0x20260408 (539362312)
 */
DefinitionBlock ("", "DSDT", 1, "CUBIT", "MUTEX", 0x00000001)
{
    Mutex (M000, 0x00)
    Mutex (M001, 0x01)
    Mutex (M002, 0x02)
    Mutex (M012, 0x02)
    Name (MARK, Zero)
    Name (ZVAL, Zero)
    Method (PROB, 0, NotSerialized)
    {
        Local0 = Acquire (M000, 0x0000)
        If ((Local0 == Zero))
        {
            Release (M000)
        }

        Return (Local0)
    }

    Method (TEST, 0, NotSerialized)
    {
        MARK = One
        Release (M000)
        MARK = 0x02
        Return (Zero)
    }
}

