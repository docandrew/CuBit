/*
 * Intel ACPI Component Architecture
 * AML/ASL+ Disassembler version 20260408 (64-bit version)
 * Copyright (c) 2000 - 2026 Intel Corporation
 * 
 * Disassembling to symbolic ASL+ operators
 *
 * Disassembly of /home/doc/git/.acpi-proof-work/runtime-package-retype-fixed-failure-oracle-0j70eenq/results/r1-success.aml
 *
 * Original Table Header:
 *     Signature        "DSDT"
 *     Length           0x000000DE (222)
 *     Revision         0x01 **** 32-bit table (V1), no 64-bit math support
 *     Checksum         0x72
 *     OEM ID           "CUBIT"
 *     OEM Table ID     "RTFAIL"
 *     OEM Revision     0x00000001 (1)
 *     Compiler ID      "INTL"
 *     Compiler Version 0x20260408 (539362312)
 */
DefinitionBlock ("", "DSDT", 1, "CUBIT", "RTFAIL", 0x00000001)
{
    Name (MARK, Zero)
    Name (SEEN, Zero)
    Name (HOLD, Zero)
    Name (DIV0, Zero)
    Name (DONE, Zero)
    Method (CNT0, 1, NotSerialized)
    {
        MARK++
        Arg0 = 0x55
        SEEN = DerefOf (Arg0)
        CopyObject (Arg0, HOLD) /* \HOLD */
        Return (0x02)
    }

    Method (LATE, 0, NotSerialized)
    {
        MARK++
        Return (One)
    }

    Method (TEST, 0, NotSerialized)
    {
        MARK = Zero
        SEEN = Zero
        DONE = Zero
        Name (P000, Package (0x02)
        {
            Buffer (CNT0 (RefOf (P000))){}, 
            Buffer (LATE ()){}
        })
        DONE = One
        Return (P000) /* \TEST.P000 */
    }

    Method (OBS0, 0, NotSerialized)
    {
        Return (MARK) /* \MARK */
    }

    Method (OBS1, 0, NotSerialized)
    {
        Return (SEEN) /* \SEEN */
    }

    Method (OBS2, 0, NotSerialized)
    {
        Return (DONE) /* \DONE */
    }

    Method (READ, 0, NotSerialized)
    {
        Return (DerefOf (HOLD))
    }
}

