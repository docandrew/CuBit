/*
 * Intel ACPI Component Architecture
 * AML/ASL+ Disassembler version 20260408 (64-bit version)
 * Copyright (c) 2000 - 2026 Intel Corporation
 * 
 * Disassembling to symbolic ASL+ operators
 *
 * Disassembly of /home/doc/git/.acpi-proof-work/numeric-source-timing-oracle-scok9jqu/r1-named-add-left-copyobject.aml
 *
 * Original Table Header:
 *     Signature        "DSDT"
 *     Length           0x00000097 (151)
 *     Revision         0x01 **** 32-bit table (V1), no 64-bit math support
 *     Checksum         0x51
 *     OEM ID           "CUBIT"
 *     OEM Table ID     "NUMTIME "
 *     OEM Revision     0x00000001 (1)
 *     Compiler ID      "INTL"
 *     Compiler Version 0x20260408 (539362312)
 */
DefinitionBlock ("", "DSDT", 1, "CUBIT", "NUMTIME ", 0x00000001)
{
    Name (SRC0, 0x11)
    Name (BUF0, Buffer (0x02)
    {
         0x00, 0x00                                       // ..
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

    Method (BUFV, 0, NotSerialized)
    {
        Return (DerefOf (BUF0 [One]))
    }

    Method (MUT0, 0, NotSerialized)
    {
        CopyObject (0x22, SRC0) /* \SRC0 */
        Return (One)
    }

    Method (TEST, 0, NotSerialized)
    {
        CopyObject (0x11, SRC0) /* \SRC0 */
        BUF0 [One] = Zero
        Return ((SRC0 + MUT0 ()))
    }
}

