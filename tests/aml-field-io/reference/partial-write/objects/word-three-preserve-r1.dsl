/*
 * Intel ACPI Component Architecture
 * AML/ASL+ Disassembler version 20260408 (64-bit version)
 * Copyright (c) 2000 - 2026 Intel Corporation
 * 
 * Disassembling to symbolic ASL+ operators
 *
 * Disassembly of /home/doc/git/.acpi-proof-work/field-partial-object-stores-wbb7nw0o/results/word-three-preserve-r1.aml
 *
 * Original Table Header:
 *     Signature        "DSDT"
 *     Length           0x000001AE (430)
 *     Revision         0x01 **** 32-bit table (V1), no 64-bit math support
 *     Checksum         0xE1
 *     OEM ID           "CUBIT"
 *     OEM Table ID     "PARTIO"
 *     OEM Revision     0x00000001 (1)
 *     Compiler ID      "INTL"
 *     Compiler Version 0x20260408 (539362312)
 */
DefinitionBlock ("", "DSDT", 1, "CUBIT", "PARTIO", 0x00000001)
{
    OperationRegion (RGN0, SystemMemory, 0x4000, 0x20)
    Field (RGN0, ByteAcc, NoLock, Preserve)
    {
        B000,   8, 
        B001,   8, 
        B002,   8, 
        B003,   8, 
        B004,   8, 
        B005,   8, 
        B006,   8, 
        B007,   8, 
        B008,   8, 
        B009,   8, 
        B010,   8, 
        B011,   8
    }

    Field (RGN0, WordAcc, NoLock, Preserve)
    {
            ,   11, 
        VAL0,   23
    }

    Method (RSET, 0, NotSerialized)
    {
        B000 = 0xA5
        B001 = 0x5A
        B002 = 0x3C
        B003 = 0xC3
        B004 = 0x96
        B005 = 0x69
        B006 = 0xF0
        B007 = 0x0F
        B008 = 0x33
        B009 = 0xCC
        B010 = 0x87
        B011 = 0x78
        Return (Zero)
    }

    Method (BUFR, 0, NotSerialized)
    {
        RSET ()
        VAL0 = Buffer (0x08)
            {
                 0xC7, 0x34, 0x12, 0x5A, 0x96, 0x69, 0xA5, 0x3C   // .4.Z.i.<
            }
        Return (VAL0) /* \VAL0 */
    }

    Method (STRG, 0, NotSerialized)
    {
        RSET ()
        VAL0 = "Az2!"
        Return (VAL0) /* \VAL0 */
    }

    Method (P000, 0, NotSerialized)
    {
        Return (B000) /* \B000 */
    }

    Method (P001, 0, NotSerialized)
    {
        Return (B001) /* \B001 */
    }

    Method (P002, 0, NotSerialized)
    {
        Return (B002) /* \B002 */
    }

    Method (P003, 0, NotSerialized)
    {
        Return (B003) /* \B003 */
    }

    Method (P004, 0, NotSerialized)
    {
        Return (B004) /* \B004 */
    }

    Method (P005, 0, NotSerialized)
    {
        Return (B005) /* \B005 */
    }

    Method (P006, 0, NotSerialized)
    {
        Return (B006) /* \B006 */
    }

    Method (P007, 0, NotSerialized)
    {
        Return (B007) /* \B007 */
    }

    Method (P008, 0, NotSerialized)
    {
        Return (B008) /* \B008 */
    }

    Method (P009, 0, NotSerialized)
    {
        Return (B009) /* \B009 */
    }

    Method (P010, 0, NotSerialized)
    {
        Return (B010) /* \B010 */
    }

    Method (P011, 0, NotSerialized)
    {
        Return (B011) /* \B011 */
    }
}

