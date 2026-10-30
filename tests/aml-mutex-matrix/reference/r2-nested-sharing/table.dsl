/*
 * Intel ACPI Component Architecture
 * AML/ASL+ Disassembler version 20260408 (64-bit version)
 * Copyright (c) 2000 - 2026 Intel Corporation
 * 
 * Disassembling to symbolic ASL+ operators
 *
 * Disassembly of /home/doc/git/.acpi-proof-work/mutex-oracle-identifier-validation-j8q69sjp/results/r2-nested-sharing/table.aml
 *
 * Original Table Header:
 *     Signature        "DSDT"
 *     Length           0x000000A1 (161)
 *     Revision         0x02
 *     Checksum         0x10
 *     OEM ID           "CUBIT"
 *     OEM Table ID     "MUTEX"
 *     OEM Revision     0x00000001 (1)
 *     Compiler ID      "INTL"
 *     Compiler Version 0x20260408 (539362312)
 */
DefinitionBlock ("", "DSDT", 2, "CUBIT", "MUTEX", 0x00000001)
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

    Method (GET0, 0, NotSerialized)
    {
        Return (Acquire (M000, 0x0000))
    }

    Method (PUT0, 0, NotSerialized)
    {
        Release (M000)
        Return (One)
    }

    Method (TEST, 0, NotSerialized)
    {
        Local0 = GET0 ()
        MARK = PUT0 ()
        Return (Local0)
    }
}

