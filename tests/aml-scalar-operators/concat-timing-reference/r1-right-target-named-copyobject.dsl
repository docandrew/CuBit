/*
 * Intel ACPI Component Architecture
 * AML/ASL+ Disassembler version 20260408 (64-bit version)
 * Copyright (c) 2000 - 2026 Intel Corporation
 * 
 * Disassembling to symbolic ASL+ operators
 *
 * Disassembly of /home/doc/git/.acpi-proof-work/concat-source-timing-oracle-3pyqjc0w/r1-right-target-named-copyobject.aml
 *
 * Original Table Header:
 *     Signature        "DSDT"
 *     Length           0x000000A4 (164)
 *     Revision         0x01 **** 32-bit table (V1), no 64-bit math support
 *     Checksum         0x16
 *     OEM ID           "CUBIT"
 *     OEM Table ID     "CATTIME "
 *     OEM Revision     0x00000001 (1)
 *     Compiler ID      "INTL"
 *     Compiler Version 0x20260408 (539362312)
 */
DefinitionBlock ("", "DSDT", 1, "CUBIT", "CATTIME ", 0x00000001)
{
    Name (SRC0, 0x11)
    Name (PKG0, Package (0x01)
    {
        Zero
    })
    Method (CTRL, 0, NotSerialized)
    {
        Return (0xA5)
    }

    Method (WIDT, 0, NotSerialized)
    {
        Return (Ones)
    }

    Method (SRCV, 0, NotSerialized)
    {
        Return (SRC0) /* \SRC0 */
    }

    Method (TVAL, 0, NotSerialized)
    {
        Return (DerefOf (PKG0 [Zero]))
    }

    Method (MUTR, 0, NotSerialized)
    {
        CopyObject (0x22, SRC0) /* \SRC0 */
        Return (0x33)
    }

    Method (MUTT, 0, NotSerialized)
    {
        CopyObject (0x22, SRC0) /* \SRC0 */
        Return (Zero)
    }

    Method (TEST, 0, NotSerialized)
    {
        CopyObject (0x11, SRC0) /* \SRC0 */
        Return (Concatenate (0x33, SRC0, PKG0 [MUTT ()]))
    }
}

