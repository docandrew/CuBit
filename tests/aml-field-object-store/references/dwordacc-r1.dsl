/*
 * Intel ACPI Component Architecture
 * AML/ASL+ Disassembler version 20260408 (64-bit version)
 * Copyright (c) 2000 - 2026 Intel Corporation
 * 
 * Disassembling to symbolic ASL+ operators
 *
 * Disassembly of /home/doc/git/.acpi-proof-work/field-whole-object-reference-vzz5bqba/results/dwordacc-r1.aml
 *
 * Original Table Header:
 *     Signature        "DSDT"
 *     Length           0x0000010F (271)
 *     Revision         0x01 **** 32-bit table (V1), no 64-bit math support
 *     Checksum         0xD1
 *     OEM ID           "CUBIT"
 *     OEM Table ID     "WHOLEOBJ"
 *     OEM Revision     0x00000001 (1)
 *     Compiler ID      "INTL"
 *     Compiler Version 0x20260408 (539362312)
 */
DefinitionBlock ("", "DSDT", 1, "CUBIT", "WHOLEOBJ", 0x00000001)
{
    OperationRegion (RGN0, SystemMemory, 0x4000, 0x10)
    Field (RGN0, DWordAcc, NoLock, Preserve)
    {
        F001,   32, 
        F002,   32, 
        F003,   32
    }

    Method (RSET, 0, NotSerialized)
    {
        F001 = 0xA5
        F002 = 0x5A
        F003 = 0x3C
        Return (Zero)
    }

    Method (BUFR, 0, NotSerialized)
    {
        RSET ()
        F001 = Buffer (0x08)
            {
                 0xC7, 0x34, 0x12, 0x5A, 0x96, 0x69, 0xA5, 0x3C   // .4.Z.i.<
            }
        Return (F001) /* \F001 */
    }

    Method (STRG, 0, NotSerialized)
    {
        RSET ()
        F001 = "Az2!"
        Return (F001) /* \F001 */
    }

    Method (SDEC, 0, NotSerialized)
    {
        RSET ()
        F001 = "1234"
        Return (F001) /* \F001 */
    }

    Method (SMP0, 0, NotSerialized)
    {
        RSET ()
        F001 = ""
        Return (F001) /* \F001 */
    }

    Method (BMP0, 0, NotSerialized)
    {
        RSET ()
        F001 = Buffer (Zero){}
        Return (F001) /* \F001 */
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

