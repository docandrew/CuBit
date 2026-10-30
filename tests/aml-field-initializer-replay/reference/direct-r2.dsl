/*
 * Intel ACPI Component Architecture
 * AML/ASL+ Disassembler version 20260408 (64-bit version)
 * Copyright (c) 2000 - 2026 Intel Corporation
 * 
 * Disassembling to symbolic ASL+ operators
 *
 * Disassembly of /home/doc/git/.acpi-proof-work/field-initializer-normal-runtime-feqwecy7/results/direct-r2.aml
 *
 * Original Table Header:
 *     Signature        "DSDT"
 *     Length           0x000000F3 (243)
 *     Revision         0x02
 *     Checksum         0xB3
 *     OEM ID           "CUBIT"
 *     OEM Table ID     "INITIO"
 *     OEM Revision     0x00000001 (1)
 *     Compiler ID      "INTL"
 *     Compiler Version 0x20260408 (539362312)
 */
DefinitionBlock ("", "DSDT", 2, "CUBIT", "INITIO", 0x00000001)
{
    Name (MARK, Zero)
    OperationRegion (RGN0, SystemMemory, 0x4000, 0x10)
    Field (RGN0, ByteAcc, NoLock, Preserve)
    {
        F001,   8, 
        F002,   8, 
        F003,   8
    }

    Method (RSET, 0, NotSerialized)
    {
        F001 = 0x0F
        F002 = Zero
        F003 = Zero
        MARK = Zero
        Return (Zero)
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

    Scope (_SB)
    {
        Device (DVA0)
        {
            Name (_HID, "CUB0001")  // _HID: Hardware ID
            Field (RGN0, ByteAcc, NoLock, Preserve)
            {
                _STA,   8
            }

            Method (_INI, 0, NotSerialized)  // _INI: Initialize
            {
                MARK = One
                F002 = 0x22
                MARK = 0x12
                F003 = 0x33
                MARK = 0x0123
            }
        }
    }
}

