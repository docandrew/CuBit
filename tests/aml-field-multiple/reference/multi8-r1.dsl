/*
 * Intel ACPI Component Architecture
 * AML/ASL+ Disassembler version 20260408 (64-bit version)
 * Copyright (c) 2000 - 2026 Intel Corporation
 * 
 * Disassembling to symbolic ASL+ operators
 *
 * Disassembly of /home/doc/git/.acpi-proof-work/field-multiple-normal-oracle-ilbufegh/results/multi8-r1.aml
 *
 * Original Table Header:
 *     Signature        "DSDT"
 *     Length           0x00000140 (320)
 *     Revision         0x01 **** 32-bit table (V1), no 64-bit math support
 *     Checksum         0x58
 *     OEM ID           "CUBIT"
 *     OEM Table ID     "MULTIFLD"
 *     OEM Revision     0x00000001 (1)
 *     Compiler ID      "INTL"
 *     Compiler Version 0x20260408 (539362312)
 */
DefinitionBlock ("", "DSDT", 1, "CUBIT", "MULTIFLD", 0x00000001)
{
    Name (MARK, Zero)
    OperationRegion (RGN0, SystemMemory, 0x4000, 0x10)
    Field (RGN0, ByteAcc, NoLock, Preserve)
    {
        F001,   8, 
        F002,   8, 
        F003,   8
    }

    Method (SRC0, 0, NotSerialized)
    {
        MARK *= 0x0A
        MARK += One
        Return (0x12)
    }

    Method (SRC1, 0, NotSerialized)
    {
        MARK *= 0x0A
        MARK += 0x02
        Return ((F001 + One))
    }

    Method (BAD0, 0, NotSerialized)
    {
        MARK *= 0x0A
        MARK += 0x03
        Local0 = Zero
        Divide (One, Local0, Local1, Local2)
        Return (One)
    }

    Method (TEST, 0, NotSerialized)
    {
        MARK = Zero
        F001 = SRC0 ()
        F002 = SRC1 ()
        F003 = 0x77
        Return ((F001 + F002))
    }

    Method (FAIL, 0, NotSerialized)
    {
        MARK = One
        F001 = 0x44
        F002 = BAD0 ()
        F003 = 0x55
        Return (0x99)
    }

    Method (OBS0, 0, NotSerialized)
    {
        Return (MARK) /* \MARK */
    }

    Method (PR01, 0, NotSerialized)
    {
        Return (F001) /* \F001 */
    }

    Method (PR02, 0, NotSerialized)
    {
        Return (F002) /* \F002 */
    }

    Method (PR03, 0, NotSerialized)
    {
        Return (F003) /* \F003 */
    }
}

