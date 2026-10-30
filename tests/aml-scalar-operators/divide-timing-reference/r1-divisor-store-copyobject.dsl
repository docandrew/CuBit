/*
 * Intel ACPI Component Architecture
 * AML/ASL+ Disassembler version 20260408 (64-bit version)
 * Copyright (c) 2000 - 2026 Intel Corporation
 * 
 * Disassembling to symbolic ASL+ operators
 *
 * Disassembly of /home/doc/git/.acpi-proof-work/divide-target-timing-oracle-6yxs83q4/r1-divisor-store-copyobject.aml
 *
 * Original Table Header:
 *     Signature        "DSDT"
 *     Length           0x00000117 (279)
 *     Revision         0x01 **** 32-bit table (V1), no 64-bit math support
 *     Checksum         0xE2
 *     OEM ID           "CUBIT"
 *     OEM Table ID     "DIVTIME "
 *     OEM Revision     0x00000001 (1)
 *     Compiler ID      "INTL"
 *     Compiler Version 0x20260408 (539362312)
 */
DefinitionBlock ("", "DSDT", 1, "CUBIT", "DIVTIME ", 0x00000001)
{
    Name (SRC0, 0x03)
    Name (BUF0, Buffer (0x02)
    {
         0x00, 0x00                                       // ..
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

    Method (RVAL, 0, NotSerialized)
    {
        Return (DerefOf (BUF0 [Zero]))
    }

    Method (QVAL, 0, NotSerialized)
    {
        Return (DerefOf (BUF0 [One]))
    }

    Method (MRKV, 0, NotSerialized)
    {
        Return (SEQN) /* \SEQN */
    }

    Method (RMUT, 0, NotSerialized)
    {
        Debug = 0xA1
        SEQN = ((SEQN * 0x0A) + One)
        SRC0 = 0x04
        Return (Zero)
    }

    Method (QMUT, 0, NotSerialized)
    {
        Debug = 0xA2
        SEQN = ((SEQN * 0x0A) + 0x02)
        CopyObject (0x05, SRC0) /* \SRC0 */
        Return (One)
    }

    Method (TEST, 0, NotSerialized)
    {
        CopyObject (0x03, SRC0) /* \SRC0 */
        SEQN = Zero
        BUF0 [Zero] = Zero
        BUF0 [One] = Zero
        Return (Divide (0x39, DerefOf (RefOf (SRC0)), BUF0 [RMUT ()], BUF0 [
            QMUT ()]))
    }
}

