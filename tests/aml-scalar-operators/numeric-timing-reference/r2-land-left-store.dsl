/*
 * Intel ACPI Component Architecture
 * AML/ASL+ Disassembler version 20260408 (64-bit version)
 * Copyright (c) 2000 - 2026 Intel Corporation
 * 
 * Disassembling to symbolic ASL+ operators
 *
 * Disassembly of /home/doc/git/.acpi-proof-work/numeric-source-timing-oracle-scok9jqu/r2-land-left-store.aml
 *
 * Original Table Header:
 *     Signature        "DSDT"
 *     Length           0x00000095 (149)
 *     Revision         0x02
 *     Checksum         0xCF
 *     OEM ID           "CUBIT"
 *     OEM Table ID     "NUMTIME "
 *     OEM Revision     0x00000001 (1)
 *     Compiler ID      "INTL"
 *     Compiler Version 0x20260408 (539362312)
 */
DefinitionBlock ("", "DSDT", 2, "CUBIT", "NUMTIME ", 0x00000001)
{
    Name (SRC0, Zero)
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
        SRC0 = One
        Return (One)
    }

    Method (TEST, 0, NotSerialized)
    {
        CopyObject (Zero, SRC0) /* \SRC0 */
        BUF0 [One] = Zero
        Return ((DerefOf (RefOf (SRC0)) && MUT0 ()))
    }
}

