/*
 * Intel ACPI Component Architecture
 * AML/ASL+ Disassembler version 20260408 (64-bit version)
 * Copyright (c) 2000 - 2026 Intel Corporation
 * 
 * Disassembling to symbolic ASL+ operators
 *
 * Disassembly of /home/doc/git/.acpi-proof-work/resource-integer-conversion-oracle-aj50y5tf/r2-buffer-right-target-copyobject.aml
 *
 * Original Table Header:
 *     Signature        "DSDT"
 *     Length           0x00000111 (273)
 *     Revision         0x02
 *     Checksum         0xB5
 *     OEM ID           "CUBIT"
 *     OEM Table ID     "RESTIME "
 *     OEM Revision     0x00000001 (1)
 *     Compiler ID      "INTL"
 *     Compiler Version 0x20260408 (539362312)
 */
DefinitionBlock ("", "DSDT", 2, "CUBIT", "RESTIME ", 0x00000001)
{
    Name (SRC0, Buffer (0x04)
    {
         0x71, 0x11, 0x79, 0x00                           // q.y.
    })
    Name (PKG0, Package (0x01)
    {
        Zero
    })
    Name (SEQN, Zero)
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

    Method (MRKV, 0, NotSerialized)
    {
        Return (SEQN) /* \SEQN */
    }

    Method (MUTR, 0, NotSerialized)
    {
        Debug = 0xA1
        SEQN = ((SEQN * 0x0A) + One)
        CopyObject (Buffer (0x04)
            {
                "q\"y"
            }, SRC0) /* \SRC0 */
        Return (Buffer (0x06)
        {
             0x84, 0x01, 0x00, 0x33, 0x79, 0x00               // ...3y.
        })
    }

    Method (MUTT, 0, NotSerialized)
    {
        Debug = 0xA2
        SEQN = ((SEQN * 0x0A) + 0x02)
        CopyObject (Buffer (0x04)
            {
                "q\"y"
            }, SRC0) /* \SRC0 */
        Return (Zero)
    }

    Method (TEST, 0, NotSerialized)
    {
        CopyObject (Buffer (0x04)
            {
                 0x71, 0x11, 0x79, 0x00                           // q.y.
            }, SRC0) /* \SRC0 */
        SEQN = Zero
        Return (ConcatenateResTemplate (Buffer (0x06)
            {
                 0x84, 0x01, 0x00, 0x33, 0x79, 0x00               // ...3y.
            }, DerefOf (RefOf (SRC0)), PKG0 [MUTT ()]))
    }
}

