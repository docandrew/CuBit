/*
 * Intel ACPI Component Architecture
 * AML/ASL+ Disassembler version 20260408 (64-bit version)
 * Copyright (c) 2000 - 2026 Intel Corporation
 * 
 * Disassembling to symbolic ASL+ operators
 *
 * Disassembly of /home/doc/git/.acpi-proof-work/field-core-normal-oracle-proposal-mhs6804e/results/whole16-r1.aml
 *
 * Original Table Header:
 *     Signature        "DSDT"
 *     Length           0x000000EC (236)
 *     Revision         0x01 **** 32-bit table (V1), no 64-bit math support
 *     Checksum         0x30
 *     OEM ID           "CUBIT"
 *     OEM Table ID     "FLDCORE"
 *     OEM Revision     0x00000001 (1)
 *     Compiler ID      "INTL"
 *     Compiler Version 0x20260408 (539362312)
 */
DefinitionBlock ("", "DSDT", 1, "CUBIT", "FLDCORE", 0x00000001)
{
    Name (MARK, Zero)
    OperationRegion (RGN0, SystemMemory, 0x4000, 0x02)
    Field (RGN0, WordAcc, NoLock, Preserve)
    {
        FLD0,   16
    }

    Method (SRC0, 0, NotSerialized)
    {
        MARK *= 0x0A
        MARK += One
        Return (0x9ABCDEF0)
    }

    Method (BAD0, 0, NotSerialized)
    {
        MARK *= 0x0A
        MARK += 0x03
        Local0 = Zero
        Divide (One, Local0, Local1, Local2)
        Return (0x55)
    }

    Method (TEST, 0, NotSerialized)
    {
        MARK = Zero
        FLD0 = SRC0 ()
        Local0 = FLD0 /* \FLD0 */
        MARK *= 0x0A
        MARK += 0x02
        Return (Local0)
    }

    Method (FAIL, 0, NotSerialized)
    {
        FLD0 = BAD0 ()
        Return (0x66)
    }

    Method (OBS0, 0, NotSerialized)
    {
        Return (MARK) /* \MARK */
    }

    Method (PROB, 0, NotSerialized)
    {
        Return (FLD0) /* \FLD0 */
    }
}

