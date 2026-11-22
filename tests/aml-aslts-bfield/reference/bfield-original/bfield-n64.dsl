/*
 * Intel ACPI Component Architecture
 * AML/ASL+ Disassembler version 20260408 (64-bit version)
 * Copyright (c) 2000 - 2026 Intel Corporation
 * 
 * Disassembling to symbolic ASL+ operators
 *
 * Disassembly of /home/doc/git/.acpi-proof-work/aslts-bfield-normal-oracle-vhxcnk87/results/bfield-n64.aml
 *
 * Original Table Header:
 *     Signature        "DSDT"
 *     Length           0x00006479 (25721)
 *     Revision         0x02
 *     Checksum         0xA7
 *     OEM ID           "Intel"
 *     OEM Table ID     "Many"
 *     OEM Revision     0x00000001 (1)
 *     Compiler ID      "INTL"
 *     Compiler Version 0x20260408 (539362312)
 */
DefinitionBlock ("", "DSDT", 2, "Intel", "Many", 0x00000001)
{
    Name (Z062, 0x3E)
    Name (FF32, 0xFFFFFFFF)
    Name (FF64, Ones)
    Name (TRCF, Zero)
    Name (TRCH, "ASLTS")
    Name (STST, "STST")
    Name (CTST, "CTST")
    Name (PR01, One)
    Name (PR02, One)
    Name (TMT0, Zero)
    Name (MTHR, Zero)
    Method (SET3, 1, NotSerialized)
    {
        MTHR = Arg0
    }

    Name (C000, 0x0A)
    Name (C001, 0x05)
    Name (C002, 0x0D)
    Name (C003, 0x0C)
    Name (C004, 0x06)
    Name (C005, 0x04)
    Name (C006, 0x1F)
    Name (C007, 0x33)
    Name (C008, Zero)
    Name (C009, One)
    Name (C00A, 0x02)
    Name (C00B, 0x03)
    Name (C00C, 0x04)
    Name (C00D, 0x05)
    Name (C00E, 0x06)
    Name (C00F, 0x07)
    Name (C010, 0x08)
    Name (C011, 0x09)
    Name (C012, 0x0A)
    Name (C013, 0x0B)
    Name (C014, 0x0C)
    Name (C015, 0x0D)
    Name (C016, 0x0E)
    Name (C017, 0x0F)
    Name (C018, 0x10)
    Name (C019, 0x11)
    Name (C01A, 0x12)
    Name (C01B, 0x13)
    Name (C01C, 0x14)
    Name (C01D, 0x15)
    Name (C01E, 0x16)
    Name (C01F, 0x17)
    Name (C020, 0x18)
    Name (C021, 0x19)
    Name (C022, 0x1A)
    Name (C023, 0x1B)
    Name (C024, 0x1C)
    Name (C025, 0x1D)
    Name (C027, 0x1E)
    Name (C028, Zero)
    Name (NMTP, Package (0x20)
    {
        "Uninitialized", 
        "Integer", 
        "String", 
        "Buffer", 
        "Package", 
        "Field Unit", 
        "Device", 
        "Event", 
        "Method", 
        "Mutex", 
        "Operation Region", 
        "Power Resource", 
        "Processor", 
        "Thermal Zone", 
        "Buffer Field", 
        "DDB Handle", 
        "Debug Object", 
        "LOCAL_REGION_FIELD", 
        "LOCAL_BANK_FIELD", 
        "LOCAL_INDEX_FIELD", 
        "LOCAL_REFERENCE", 
        "LOCAL_ALIAS", 
        "LOCAL_METHOD_ALIAS", 
        "LOCAL_NOTIFY", 
        "LOCAL_ADDRESS_HANDLER", 
        "LOCAL_RESOURCE", 
        "LOCAL_RESOURCE_FIELD", 
        "LOCAL_SCOPE", 
        "LOCAL_EXTRA", 
        "LOCAL_DATA", 
        "--", 
        "--"
    })
    Name (C080, Zero)
    Name (C081, Zero)
    Name (C082, Zero)
    Name (C083, Zero)
    Name (C084, Zero)
    Name (C085, Zero)
    Name (C086, Zero)
    Name (C087, Zero)
    Name (C088, Zero)
    Name (C089, Zero)
    Name (C08A, Zero)
    Name (C08B, Zero)
    Name (C08C, 0x00788B60)
    Name (EXCV, Zero)
    Name (ZFFF, 0x07FF)
    Name (F64, Zero)
    Name (ISZ0, Zero)
    Name (ISZC, Zero)
    Name (ETR0, 0x04B0)
    Name (ETR1, 0x0190)
    Name (ERRP, Package (ETR0, ){})
    Name (RP0P, Package (ETR0, ){})
    Name (RMRC, Zero)
    Name (ERRS, Zero)
    Name (ERRB, Zero)
    Name (ERR0, Zero)
    Name (ERR1, Zero)
    Name (ERR2, Zero)
    Name (ERR3, Zero)
    Name (ERR4, Zero)
    Name (ERR5, Zero)
    Name (ERR6, Zero)
    Name (ERR7, Zero)
    Name (FNAM, Zero)
    Method (SET0, 3, NotSerialized)
    {
        If (ERR0)
        {
            ERR ("SET0", Z062, 0x010A, Zero, Zero, ERR0, Zero)
        }
        Else
        {
            CopyObject (Arg0, ERR0) /* \ERR0 */
            CopyObject (Arg1, ERR1) /* \ERR1 */
            CopyObject (Arg2, ERR2) /* \ERR2 */
        }
    }

    Method (RST0, 0, NotSerialized)
    {
        CopyObject (Zero, ERR0) /* \ERR0 */
        CopyObject (Zero, ERR1) /* \ERR1 */
        CopyObject (Zero, ERR2) /* \ERR2 */
        CopyObject (Zero, FNAM) /* \FNAM */
    }

    Method (RST2, 0, NotSerialized)
    {
        ERR3 = Zero
    }

    Method (GET2, 0, NotSerialized)
    {
        Return (ERR3) /* \ERR3 */
    }

    Name (TCLA, Zero)
    Name (TCLF, One)
    Name (TCLC, 0x02)
    Name (TCLE, 0x03)
    Name (TCLD, 0x04)
    Name (TCLS, 0x05)
    Name (TCLM, 0x06)
    Name (TCLT, 0x07)
    Name (TCLI, 0x08)
    Name (MAXC, 0x08)
    Name (TCLL, Zero)
    Name (TIND, 0x12345678)
    Name (TSNM, "NAME_OF_TEST")
    Name (NRMT, "")
    Name (FLG5, Zero)
    Name (FLG6, Zero)
    Name (ABUU, Zero)
    Method (SETF, 1, NotSerialized)
    {
        CopyObject (Arg0, FNAM) /* \FNAM */
    }

    Method (THDR, 3, NotSerialized)
    {
        SETF (Arg2)
        Concatenate ("TEST: ", Arg0, Local1)
        Concatenate (Local1, ", ", Local2)
        Concatenate (Local2, Arg1, Local3)
        Concatenate (Local3, " (", Local4)
        Concatenate (Local4, Arg2, Local5)
        Concatenate (Local5, ")", Local6)
        Debug = Local6
    }

    Method (RPT0, 0, NotSerialized)
    {
        Name (B000, Buffer (0x04){})
        If (SizeOf (NRMT))
        {
            Concatenate (":", TCN0 (TCLL), Local1)
            Concatenate (Local1, ":", Local0)
            Concatenate (Local0, TNIC (TCLL, TIND), Local1)
            Concatenate (Local1, ":", Local0)
            Concatenate (Local0, NRMT, Local1)
            Concatenate (Local1, ":", Local0)
            Local7 = (ERRS - ERR5) /* \ERR5 */
            If (FLG5)
            {
                Concatenate (Local0, "SKIPPED:", Local1)
            }
            ElseIf (FLG6)
            {
                Concatenate (Local0, "BLOCKED:", Local1)
            }
            ElseIf (Local7)
            {
                Concatenate (Local0, "FAIL:Errors # ", Local2)
                B000 = Local7
                Concatenate (Local2, B000, Local0)
                Concatenate (Local0, ":", Local1)
                ERR6++
            }
            Else
            {
                Concatenate (Local0, "PASS:", Local1)
            }

            Concatenate (":", CTST, Local0)
            Concatenate (Local0, Local1, Local2)
            Debug = Local2
            If ((RMRC < ETR0))
            {
                Concatenate (":", STST, Local2)
                Concatenate (Local2, Local1, Local0)
                RP0P [RMRC] = Local0
            }

            RMRC++
        }

        ERR5 = Zero
        FLG5 = Zero
        FLG6 = Zero
    }

    Method (SRMT, 1, NotSerialized)
    {
        RPT0 ()
        ERR5 = ERRS /* \ERRS */
        If (One)
        {
            Concatenate (Arg0, " test started", Debug)
        }

        CopyObject (Arg0, NRMT) /* \NRMT */
    }

    Method (SKIP, 0, NotSerialized)
    {
        FLG5 = One
    }

    Method (BLCK, 0, NotSerialized)
    {
        FLG6 = One
    }

    Method (BEG0, 2, NotSerialized)
    {
        SET0 (Arg0, Arg1, Zero)
    }

    Method (END0, 0, NotSerialized)
    {
        RST0 ()
    }

    Method (STTT, 4, NotSerialized)
    {
        TSNM = Arg0
        TCLL = Arg1
        TIND = Arg2
        NRMT = ""
        FLG5 = Zero
        FLG6 = Zero
        ERR5 = Zero
        ERRB = PK00 (Arg1, Arg2)
        Concatenate ("TEST (", TCN0 (TCLL), Local1)
        Concatenate (Local1, "), ", Local0)
        Concatenate (Local0, TSNM, Local1)
        If (RTPT)
        {
            Local7 = Zero
            If ((RUN0 == Zero))
            {
                Local7 = One
            }
            ElseIf ((RUN0 == One))
            {
                If (Arg3)
                {
                    Local7 = One
                }
            }
            ElseIf ((RUN0 == 0x02))
            {
                If ((Arg3 == Zero))
                {
                    Local7 = One
                }
            }
            ElseIf ((RUN0 == 0x03))
            {
                If ((Arg3 == RUN1))
                {
                    Local7 = One
                }
            }
            ElseIf ((RUN0 == 0x04))
            {
                If ((Arg1 == RUN2))
                {
                    If ((Arg2 == RUN3))
                    {
                        Local7 = One
                    }
                }
            }
        }
        Else
        {
            Local7 = One
        }

        If (!Local7)
        {
            Concatenate (Local1, ", SKIPPED", Local0)
            Local1 = Local0
        }

        Debug = Local1
        Return (Local7)
    }

    Method (FTTT, 0, NotSerialized)
    {
        CH03 ("FTTT", Zero, 0x0249, Zero, Zero)
        RPT0 ()
        TSNM = "NAME_OF_TEST"
        TCLL = Zero
        TIND = 0x12345678
        NRMT = ""
        FLG5 = Zero
        FLG6 = Zero
        ERR5 = Zero
    }

    Method (PK00, 2, NotSerialized)
    {
        Local0 = (Arg0 & 0x0F)
        Local1 = (Arg1 & 0x1F)
        Local2 = (Local0 << 0x05)
        Local0 = (Local2 | Local1)
        Local7 = (Local0 << 0x17)
        Return (Local7)
    }

    Method (PK01, 2, NotSerialized)
    {
        Local0 = (Arg0 & 0x07FF)
        Local1 = (Arg1 & 0x0FFF)
        Local2 = (Local0 << 0x0C)
        Local7 = (Local2 | Local1)
        Return (Local7)
    }

    Method (PK02, 1, NotSerialized)
    {
        Local0 = (Arg0 & 0x01FF)
        Local7 = (Local0 << 0x17)
        Return (Local7)
    }

    Method (PK03, 2, NotSerialized)
    {
        Local0 = (Arg0 & 0x07FF)
        Local1 = (Arg1 & 0x0FFF)
        Local2 = (Local0 << 0x0C)
        Local7 = (Local2 | Local1)
        Return (Local7)
    }

    Method (ERR, 7, NotSerialized)
    {
        Local3 = Zero
        Local6 = Zero
        If (ERR0)
        {
            Local4 = ERR0 /* \ERR0 */
            Local3 = ERR1 /* \ERR1 */
            If (ERR2)
            {
                Local5 = ERR2 /* \ERR2 */
            }
            Else
            {
                Local5 = Arg4
            }
        }
        Else
        {
            Local4 = Zero
            Local5 = Arg4
            If ((TCLL == TCLD))
            {
                If (Local5)
                {
                    Local4 = ZFFF /* \ZFFF */
                }
            }
            Else
            {
                Local4 = Arg3
            }

            If ((ObjectType (Arg0) == C00A))
            {
                Local3 = Arg0
            }
        }

        If (Local4)
        {
            Local6 = PK01 (Local4, Local5)
        }

        If ((TCLL == TCLD))
        {
            Local0 = PK02 (TIND)
            Local6 |= Local0
        }

        Local0 = PK03 (Arg1, Arg2)
        Local7 = (ERRB | Local0)
        Local1 = "---------- ERROR    : "
        Concatenate (Local1, Arg0, Local0)
        Debug = Local0
        ERP0 (Arg1, Arg2, Local4, Local3, Local5)
        If ((ObjectType (Arg5) == One))
        {
            ToHexString (Arg6, Local0)
            ToDecimalString (Arg6, Local1)
            Concatenate ("**** Expected Result: 0x", Local0, Local0)
            Concatenate (Local0, ", (", Local0)
            Concatenate (Local0, Local1, Local0)
            Concatenate (Local0, ")", Local0)
            Debug = Local0
            ToHexString (Arg5, Local0)
            ToDecimalString (Arg5, Local1)
            Concatenate ("**** Actual Result  : 0x", Local0, Local0)
            Concatenate (Local0, ", (", Local0)
            Concatenate (Local0, Local1, Local0)
            Concatenate (Local0, ")", Local0)
            Debug = Local0
        }
        Else
        {
            Debug = "**** Actual Result:"
            Debug = Arg5
            Debug = "**** Expected Result:"
            Debug = Arg6
        }

        Debug = "---------- END\n"
        If ((ERRS < ETR1))
        {
            Local0 = (ERRS * 0x03)
            ERRP [Local0] = Local7
            Local0++
            ERRP [Local0] = Local6
            Local0++
            ERRP [Local0] = Local3
        }

        ERRS++
        ERR3 = One
    }

    Method (ERP0, 5, NotSerialized)
    {
        Concatenate ("TITLE               : ", TSNM, Local0)
        Debug = Local0
        Concatenate ("COLLECTION          : ", TCN0 (TCLL), Local0)
        Local1 = TNIC (TCLL, TIND)
        Debug = Local0
        Concatenate ("TEST CASE           : ", Local1, Local0)
        Debug = Local0
        Concatenate ("TEST                : ", NRMT, Local0)
        Debug = Local0
        If ((FNAM != Zero))
        {
            Local1 = FNAM /* \FNAM */
        }
        ElseIf ((Arg0 == ZFFF))
        {
            Local1 = SB00 (TIND, Zero)
        }
        Else
        {
            Local1 = DerefOf (TFN0 [Arg0])
        }

        Concatenate ("ERROR,    File      : ", Local1, Local0)
        Debug = Local0
        Concatenate ("          Line      : ", ToDecimalString (Arg1), Local0)
        Debug = Local0
        If (Arg2)
        {
            If ((Arg2 == ZFFF))
            {
                Local1 = SB00 (TIND, Zero)
            }
            Else
            {
                Local1 = DerefOf (TFN0 [Arg2])
            }

            Concatenate ("CHECKING, File      : ", Local1, Local0)
            Debug = Local0
            If ((ObjectType (Arg3) == C00A))
            {
                Concatenate ("             Method : ", Arg3, Local0)
                Debug = Local0
            }

            Concatenate ("             Line   : ", ToDecimalString (Arg4), Local0)
            Debug = Local0
        }
    }

    Method (SB00, 2, NotSerialized)
    {
        Local7 = "?"
        If ((Arg1 == Zero))
        {
            ToDecimalString (Arg0, Local0)
            Concatenate ("*", Local0, Local1)
            Concatenate (Local1, ".asl", Local7)
        }
        ElseIf ((Arg1 == One))
        {
            ToDecimalString (Arg0, Local0)
            Concatenate ("Demo of bug ", Local0, Local7)
        }

        Return (Local7)
    }

    Method (PRN0, 1, Serialized)
    {
        Name (LPN0, Zero)
        Name (LPC0, Zero)
        LPN0 = SizeOf (Arg0)
        LPC0 = Zero
        While (LPN0)
        {
            Local0 = DerefOf (Arg0 [LPC0])
            Debug = Local0
            LPN0--
            LPC0++
        }
    }

    Method (CH00, 4, NotSerialized)
    {
        If ((Arg3 != Zero))
        {
            ERR (Arg0, Z062, 0x03AD, Zero, Zero, Arg1, Arg2)
        }
    }

    Method (CH01, 4, NotSerialized)
    {
        If ((Arg3 != Ones))
        {
            ERR (Arg0, Z062, 0x03BC, Zero, Zero, Arg1, Arg2)
        }
    }

    Method (RNG0, 3, NotSerialized)
    {
        If ((Arg1 > Arg2))
        {
            Debug = "RNG0: RangeMin greater than RangeMax"
            Fatal (0x00, 0x00000000, Zero)
        }

        If ((Arg1 > Arg0))
        {
            Return (Zero)
        }
        ElseIf ((Arg0 > Arg2))
        {
            Return (Zero)
        }

        Return (Ones)
    }

    Name (BIG0, "qwertyuiopasdfghjklzxcvbnm1234567890QWERTYUIOPASDFGHJKLZXCVBNMqwertyuiopasdfghjklzxcvbnm1234567890QWERTYUIOPASDFGHJKLZXCVBNMqwertyuiopasdfghjklzxcvbnm1234567890QWERTYUIOPASDFGHJKLZXCVBNMqwertyuiopasdf")
    Name (ALL0, "`1234567890-=qwertyuiop[]\\asdfghjkl;\'zxcvbnm,./~!@#$%^&*()_+QWERTYUIOP{}|ASDFGHJKL:\"ZXCVBNM<>?")
    Method (CST0, 0, NotSerialized)
    {
        If ((C000 != 0x0A))
        {
            ERR ("c000 corrupted", Z062, 0x03E7, Zero, Zero, Zero, Zero)
        }

        If ((C001 != 0x05))
        {
            ERR ("c001 corrupted", Z062, 0x03EC, Zero, Zero, Zero, Zero)
        }

        If ((C002 != 0x0D))
        {
            ERR ("c002 corrupted", Z062, 0x03F1, Zero, Zero, Zero, Zero)
        }

        If ((C003 != 0x0C))
        {
            ERR ("c003 corrupted", Z062, 0x03F6, Zero, Zero, Zero, Zero)
        }

        If ((C004 != 0x06))
        {
            ERR ("c004 corrupted", Z062, 0x03FB, Zero, Zero, Zero, Zero)
        }

        If ((C005 != 0x04))
        {
            ERR ("c005 corrupted", Z062, 0x0400, Zero, Zero, Zero, Zero)
        }

        If ((C006 != 0x1F))
        {
            ERR ("c006 corrupted", Z062, 0x0405, Zero, Zero, Zero, Zero)
        }

        If ((C007 != 0x33))
        {
            ERR ("c007 corrupted", Z062, 0x040A, Zero, Zero, Zero, Zero)
        }

        If ((C008 != Zero))
        {
            ERR ("c008 corrupted", Z062, 0x040F, Zero, Zero, Zero, Zero)
        }

        If ((C009 != One))
        {
            ERR ("c009 corrupted", Z062, 0x0414, Zero, Zero, Zero, Zero)
        }

        If ((C00A != 0x02))
        {
            ERR ("c00a corrupted", Z062, 0x0419, Zero, Zero, Zero, Zero)
        }

        If ((C00B != 0x03))
        {
            ERR ("c00b corrupted", Z062, 0x041E, Zero, Zero, Zero, Zero)
        }

        If ((C00C != 0x04))
        {
            ERR ("c00c corrupted", Z062, 0x0423, Zero, Zero, Zero, Zero)
        }

        If ((C00D != 0x05))
        {
            ERR ("c00d corrupted", Z062, 0x0428, Zero, Zero, Zero, Zero)
        }

        If ((C00E != 0x06))
        {
            ERR ("c00e corrupted", Z062, 0x042D, Zero, Zero, Zero, Zero)
        }

        If ((C00F != 0x07))
        {
            ERR ("c00f corrupted", Z062, 0x0432, Zero, Zero, Zero, Zero)
        }

        If ((C010 != 0x08))
        {
            ERR ("c010 corrupted", Z062, 0x0437, Zero, Zero, Zero, Zero)
        }

        If ((C011 != 0x09))
        {
            ERR ("c011 corrupted", Z062, 0x043C, Zero, Zero, Zero, Zero)
        }

        If ((C012 != 0x0A))
        {
            ERR ("c012 corrupted", Z062, 0x0441, Zero, Zero, Zero, Zero)
        }

        If ((C013 != 0x0B))
        {
            ERR ("c013 corrupted", Z062, 0x0446, Zero, Zero, Zero, Zero)
        }

        If ((C014 != 0x0C))
        {
            ERR ("c014 corrupted", Z062, 0x044B, Zero, Zero, Zero, Zero)
        }

        If ((C015 != 0x0D))
        {
            ERR ("c015 corrupted", Z062, 0x0450, Zero, Zero, Zero, Zero)
        }

        If ((C016 != 0x0E))
        {
            ERR ("c016 corrupted", Z062, 0x0455, Zero, Zero, Zero, Zero)
        }

        If ((C017 != 0x0F))
        {
            ERR ("c017 corrupted", Z062, 0x045A, Zero, Zero, Zero, Zero)
        }

        If ((C018 != 0x10))
        {
            ERR ("c018 corrupted", Z062, 0x045F, Zero, Zero, Zero, Zero)
        }

        If ((C019 != 0x11))
        {
            ERR ("c019 corrupted", Z062, 0x0464, Zero, Zero, Zero, Zero)
        }
    }

    Method (SFT0, 4, Serialized)
    {
        Name (N000, Zero)
        Name (NCUR, Zero)
        N000 = Arg1
        NCUR = Zero
        Local6 = Zero
        If (Arg2)
        {
            Local3 = Arg3
            Local5 = (0x08 - Local3)
        }
        Else
        {
            Local5 = Arg3
            Local3 = (0x08 - Local5)
        }

        Local0 = Arg1
        Local0++
        Name (B000, Buffer (Local0){})
        While (N000)
        {
            Local0 = DerefOf (Arg0 [NCUR])
            Local1 = (Local0 >> Local3)
            Local2 = (Local1 & 0xFF)
            Local1 = (Local2 | Local6)
            Local4 = (Local0 << Local5)
            Local6 = (Local4 & 0xFF)
            B000 [NCUR] = Local1
            N000--
            NCUR++
        }

        B000 [NCUR] = Local6
        Return (B000) /* \SFT0.B000 */
    }

    Method (MBS0, 2, NotSerialized)
    {
        Local0 = (Arg0 + Arg1)
        Local1 = (Local0 + 0x07)
        Divide (Local1, 0x08, Local2, Local0)
        Return (Local0)
    }

    Method (SFT1, 5, Serialized)
    {
        Name (PREV, Zero)
        Name (MS00, Zero)
        Name (MS01, Zero)
        Name (MS02, Zero)
        Name (MS03, Zero)
        Name (TAIL, Zero)
        Name (LBT0, Zero)
        Name (LPN0, Zero)
        Name (LPC0, Zero)
        Name (NB01, Zero)
        Name (NREG, Zero)
        Name (NB08, Zero)
        Name (NB09, Zero)
        Name (REST, Zero)
        If ((Arg2 < One))
        {
            ERR ("sft", Z062, 0x04CD, Zero, Zero, Arg2, One)
            Return (Ones)
        }

        If ((Arg1 > 0x07))
        {
            ERR ("sft", Z062, 0x04D3, Zero, Zero, Arg1, 0x07)
            Return (Ones)
        }

        NB01 = MBS0 (Arg1, Arg2)
        Name (B000, Buffer (NB01){})
        NB08 = Arg1
        NB09 = (0x08 - NB08) /* \SFT1.NB08 */
        Local0 = (0xFF >> NB08) /* \SFT1.NB08 */
        MS01 = (Local0 << NB08) /* \SFT1.NB08 */
        MS00 = ~MS01 /* \SFT1.MS01 */
        Local7 = (Arg1 + Arg2)
        REST = (Local7 % 0x08)
        If ((REST == Zero))
        {
            REST = 0x08
        }

        PREV = (Arg3 & MS00) /* \SFT1.MS00 */
        If ((Arg2 >= NB09))
        {
            NREG = One
            Local7 = (Arg2 - NB09) /* \SFT1.NB09 */
            Divide (Local7, 0x08, Local1, Local0)
            NREG += Local0
        }

        LPN0 = NREG /* \SFT1.NREG */
        LPC0 = Zero
        While (LPN0)
        {
            Local7 = DerefOf (Arg0 [LPC0])
            Local0 = (Local7 << NB08) /* \SFT1.NB08 */
            Local1 = (Local0 | PREV) /* \SFT1.PREV */
            B000 [LPC0] = Local1
            PREV = (Local7 >> NB09) /* \SFT1.NB09 */
            LPN0--
            LPC0++
        }

        If ((REST == 0x08))
        {
            TAIL = Zero
        }
        ElseIf ((REST <= NB08))
        {
            TAIL = One
        }
        Else
        {
            TAIL = 0x02
            LBT0 = DerefOf (Arg0 [LPC0])
        }

        If ((TAIL == One))
        {
            Local0 = (0xFF >> REST) /* \SFT1.REST */
            MS03 = (Local0 << REST) /* \SFT1.REST */
            MS02 = ~MS03 /* \SFT1.MS03 */
            Local0 = (PREV & MS02) /* \SFT1.MS02 */
            Local1 = (Arg4 & MS03) /* \SFT1.MS03 */
            Local2 = (Local0 | Local1)
            B000 [LPC0] = Local2
        }
        ElseIf ((TAIL == 0x02))
        {
            Local0 = (PREV & MS00) /* \SFT1.MS00 */
            Local1 = (LBT0 << NB08) /* \SFT1.NB08 */
            Local7 = (Local0 | Local1)
            Local2 = (0xFF >> REST) /* \SFT1.REST */
            Local0 = (Local2 << REST) /* \SFT1.REST */
            Local1 = ~Local0
            Local2 = (Local7 & Local1)
            Local3 = (Arg4 & Local0)
            Local0 = (Local2 | Local3)
            B000 [LPC0] = Local0
        }

        Return (B000) /* \SFT1.B000 */
    }

    Method (M4C0, 4, Serialized)
    {
        Name (TMP0, Zero)
        Name (TMP1, Zero)
        Local7 = Zero
        TMP0 = ObjectType (Arg1)
        If (F64)
        {
            TMP1 = ObjectType (Arg2)
            If ((TMP0 != TMP1))
            {
                ERR (Arg0, Z062, 0x055B, Zero, Zero, TMP0, TMP1)
                Local7 = One
            }
            ElseIf ((Arg1 != Arg2))
            {
                ERR (Arg0, Z062, 0x0560, Zero, Zero, Arg1, Arg2)
                Local7 = One
            }
        }
        Else
        {
            TMP1 = ObjectType (Arg3)
            If ((TMP0 != TMP1))
            {
                ERR (Arg0, Z062, 0x0569, Zero, Zero, TMP0, TMP1)
                Local7 = One
            }
            ElseIf ((Arg1 != Arg3))
            {
                ERR (Arg0, Z062, 0x056E, Zero, Zero, Arg1, Arg3)
                Local7 = One
            }
        }

        Return (Local7)
    }

    Method (M4A1, 2, Serialized)
    {
        Name (S000, " ")
        Local0 = DerefOf (Arg0 [Arg1])
        S000 [Zero] = Local0
        Return (S000) /* \M4A1.S000 */
    }

    Method (STRT, 1, Serialized)
    {
        Method (M555, 0, NotSerialized)
        {
        }

        DataTableRegion (HDR, "DSDT", "", "")
        Field (HDR, AnyAcc, NoLock, Preserve)
        {
            SIG,    32, 
            LENG,   32, 
            REV,    8, 
            SUM,    8, 
            OID,    48, 
            OTID,   64, 
            OREV,   32, 
            CID,    32, 
            CREV,   32
        }

        M555 ()
        TMT0 = Timer
        If ((REV < 0x02))
        {
            F64 = Zero
            ISZ0 = 0x04
            ISZC = 0x08
            Debug = "32-bit mode"
        }
        Else
        {
            F64 = One
            ISZ0 = 0x08
            ISZC = 0x10
            Debug = "64-bit mode"
        }

        If (CH02 ())
        {
            ERR7++
            CH03 ("STRT", Zero, 0x05BD, Zero, Zero)
            EXC0 = Zero
            EXC1 = Zero
        }

        SRTP (Arg0)
        RTPI ()
        RST0 ()
        RST2 ()
        SET2 (SETN)
    }

    Name (TCNP, Package (0x09)
    {
        "compilation", 
        "functional", 
        "complex", 
        "exceptions", 
        "bdemo", 
        "service", 
        "mt", 
        "Identity2MS", 
        "IMPL"
    })
    Method (TCN0, 1, NotSerialized)
    {
        Local7 = "?"
        If ((Arg0 <= MAXC))
        {
            Local7 = DerefOf (TCNP [Arg0])
        }

        Return (Local7)
    }

    Method (TNIC, 2, Serialized)
    {
        Local7 = "?"
        Switch (ToInteger (Arg0))
        {
            Case (One)
            {
                Local7 = DerefOf (TNF0 [Arg1])
            }
            Case (0x02)
            {
                Local7 = DerefOf (TNC0 [Arg1])
            }
            Case (0x03)
            {
                Local7 = DerefOf (TNE0 [Arg1])
            }
            Case (0x04)
            {
                Local7 = SB00 (Arg1, One)
            }
            Case (0x05)
            {
                Local7 = DerefOf (TNS0 [Arg1])
            }
            Case (0x06)
            {
                Local7 = DerefOf (TNM0 [Arg1])
            }
            Case (0x07)
            {
                Local7 = DerefOf (TNT0 [Arg1])
            }
            Case (0x08)
            {
                Local7 = DerefOf (TNI0 [Arg1])
            }

        }

        Return (Local7)
    }

    Name (TNF0, Package (0x0F)
    {
        "arithmetic", 
        "bfield", 
        "constant", 
        "control", 
        "descriptor", 
        "external", 
        "local", 
        "logic", 
        "manipulation", 
        "name", 
        "reference", 
        "region", 
        "synchronization", 
        "table", 
        "module"
    })
    Name (TNC0, Package (0x14)
    {
        "misc", 
        "provoke", 
        "oarg", 
        "oconst", 
        "olocal", 
        "oreturn", 
        "onamedloc", 
        "onamedglob", 
        "opackageel", 
        "oreftonamed", 
        "oconversion", 
        "oreftopackageel", 
        "rstore", 
        "roptional", 
        "rconversion", 
        "rcopyobject", 
        "rindecrement", 
        "rexplicitconv", 
        "badasl", 
        "namespace"
    })
    Name (TNE0, Package (0x07)
    {
        "exc", 
        "exc_operand1", 
        "exc_operand2", 
        "exc_result1", 
        "exc_result2", 
        "exc_ref", 
        "exc_tbl"
    })
    Name (TNS0, Package (0x01)
    {
        "condbranches"
    })
    Name (TNM0, Package (0x01)
    {
        "mt-mutex"
    })
    Name (TNT0, Package (0x01)
    {
        "abbu"
    })
    Name (TNI0, Package (0x01)
    {
        "dynobj"
    })
    Name (TFN0, Package (0xCD)
    {
        "UNDEF", 
        "crbuffield.asl", 
        "constants.asl", 
        "ctl0.asl", 
        "ctl1.asl", 
        "ctl2.asl", 
        "timing.asl", 
        "concatenaterestemplate.asl", 
        "dependentfn.asl", 
        "dma.asl", 
        "dwordio.asl", 
        "dwordmemory.asl", 
        "dwordspace.asl", 
        "extendedio.asl", 
        "extendedmemory.asl", 
        "extendedspace.asl", 
        "fixedio.asl", 
        "interrupt.asl", 
        "io.asl", 
        "irq.asl", 
        "irqnoflags.asl", 
        "memory24.asl", 
        "memory32.asl", 
        "memory32fixed.asl", 
        "qwordio.asl", 
        "qwordmemory.asl", 
        "qwordspace.asl", 
        "register.asl", 
        "resourcetemplate.asl", 
        "rtemplate.asl", 
        "vendorlong.asl", 
        "vendorshort.asl", 
        "wordbusnumber.asl", 
        "wordio.asl", 
        "wordspace.asl", 
        "logical.asl", 
        "concatenate.asl", 
        "eisaid.asl", 
        "match1.asl", 
        "mid.asl", 
        "objecttype.asl", 
        "sizeof.asl", 
        "store.asl", 
        "tobuffer.asl", 
        "todecimalstring.asl", 
        "tofrombcd.asl", 
        "tohexstring.asl", 
        "tointeger.asl", 
        "tostring.asl", 
        "touuid.asl", 
        "unicode.asl", 
        "package.asl", 
        "event.asl", 
        "mutex.asl", 
        "misc.asl", 
        "provoke.asl", 
        "oconversion.asl", 
        "rconversion.asl", 
        "exc.asl", 
        "exc_operand1.asl", 
        "exc_result.asl", 
        "XXXXXX.asl", 
        "common.asl", 
        "ehandle.asl", 
        "oproc.asl", 
        "otest.asl", 
        "rproc.asl", 
        "rtest.asl", 
        "switch1.asl", 
        "switch2.asl", 
        "switch3.asl", 
        "switch4.asl", 
        "switch5.asl", 
        "switch6.asl", 
        "while.asl", 
        "match2.asl", 
        "ref00.asl", 
        "ref01.asl", 
        "ref02.asl", 
        "ref03.asl", 
        "ref04.asl", 
        "ref70.asl", 
        "operations.asl", 
        "arithmetic.asl", 
        "ocommon.asl", 
        "oconst.asl", 
        "onamedglob1.asl", 
        "onamedglob2.asl", 
        "onamedloc1.asl", 
        "onamedloc2.asl", 
        "opackageel.asl", 
        "oreftonamed1.asl", 
        "exc_00_undef.asl", 
        "exc_01_int.asl", 
        "exc_02_str.asl", 
        "exc_03_buf.asl", 
        "exc_04_pckg.asl", 
        "exc_05_funit.asl", 
        "exc_06_dev.asl", 
        "exc_07_event.asl", 
        "exc_08_method.asl", 
        "exc_09_mux.asl", 
        "exc_10_oreg.asl", 
        "exc_11_pwr.asl", 
        "exc_12_proc.asl", 
        "exc_13_tzone.asl", 
        "exc_14_bfield.asl", 
        "exc_operand2.asl", 
        "ref05.asl", 
        "ref71.asl", 
        "ref06.asl", 
        "ref50.asl", 
        "name.asl", 
        "data.asl", 
        "dataproc.asl", 
        "datastproc.asl", 
        "ref07.asl", 
        "olocal.asl", 
        "oreturn.asl", 
        "oreftopackageel.asl", 
        "oreftonamed2.asl", 
        "oarg.asl", 
        "rcommon.asl", 
        "rstore.asl", 
        "rcopyobject.asl", 
        "rindecrement.asl", 
        "rexplicitconv.asl", 
        "roptional.asl", 
        "tcicmd.asl", 
        "dobexec.asl", 
        "dobdecl.asl", 
        "dobctl.asl", 
        "dobexceptions.asl", 
        "method.asl", 
        "function.asl", 
        "condbranches.asl", 
        "add.asl", 
        "standaloneRet.asl", 
        "store.asl", 
        "return.asl", 
        "dobmisc.asl", 
        "opregions.asl", 
        "dtregions.asl", 
        "regionfield.asl", 
        "indexfield.asl", 
        "bankfield.asl", 
        "badasl.asl", 
        "mt-common.asl", 
        "mt-mutex.asl", 
        "mt-mxs.asl", 
        "mutex2.asl", 
        "mutex_proc.asl", 
        "mt-tests.asl", 
        "mt-service.asl", 
        "ns0.asl", 
        "ns1.asl", 
        "ns2.asl", 
        "ns3.asl", 
        "ns4.asl", 
        "ns5.asl", 
        "ns6.asl", 
        "I2MS_msfail0.asl", 
        "I2MS_st0.asl", 
        "I2MS_ns_in00.asl", 
        "I2MS_ns_in10.asl", 
        "I2MS_ns_in20.asl", 
        "I2MS_ns_in30.asl", 
        "I2MS_ns_in40.asl", 
        "I2MS_ns_in50.asl", 
        "I2MS_mt0_abbu.asl", 
        "I2MS_mt0_aslts.asl", 
        "I2MS_recursion_abbu.asl", 
        "I2MS_recursion_aslts.asl", 
        "serialized.asl", 
        "load.asl", 
        "unload.asl", 
        "loadtable.asl", 
        "recursion.asl", 
        "ns-scope.asl", 
        "ns-fullpath.asl", 
        "scope.asl", 
        "object.asl", 
        "order.asl", 
        "I2MS_ns_dv00.asl", 
        "I2MS_ns_dv10.asl", 
        "I2MS_ns_dv20.asl", 
        "I2MS_ns_dv30.asl", 
        "I2MS_ns_device.asl", 
        "I2MS_ns_device_abbu.asl", 
        "I2MS_ns_device_aslts.asl", 
        "I2MS_ns4.asl", 
        "I2MS_ns5.asl", 
        "I2MS_ns6.asl", 
        "fixeddma.asl", 
        "gpioint.asl", 
        "gpioio.asl", 
        "i2cserialbus.asl", 
        "spiserialbus.asl", 
        "uartserialbus.asl", 
        "pinfunction.asl", 
        "pinconfig.asl", 
        "pingroup.asl", 
        "pingroupfunction.asl", 
        "pingroupconfig.asl", 
        "external.asl"
    })
    Method (UNP0, 3, Serialized)
    {
        Local7 = (Arg0 >> 0x1C)
        Local0 = (Local7 & 0x0F)
        Local7 = (Arg0 >> 0x17)
        Local1 = (Local7 & 0x1F)
        Local7 = (Arg0 >> 0x0C)
        Local2 = (Local7 & 0x07FF)
        Local3 = (Arg0 & 0x0FFF)
        Local6 = ""
        Local7 = ""
        Switch (ToInteger (Local0))
        {
            Case (One)
            {
                Local6 = DerefOf (TNF0 [Local1])
                If (ERR4)
                {
                    Local7 = ", functional, "
                }
            }
            Case (0x02)
            {
                Local6 = DerefOf (TNC0 [Local1])
                If (ERR4)
                {
                    Local7 = ", complex, "
                }
            }
            Case (0x03)
            {
                Local6 = DerefOf (TNE0 [Local1])
                If (ERR4)
                {
                    Local7 = ", exceptions, "
                }
            }
            Case (0x04)
            {
                Local0 = (Arg1 >> 0x17)
                Local1 = (Local0 & 0x01FF)
                Local6 = SB00 (Local1, One)
                If (ERR4)
                {
                    Local7 = ", bug-demo, "
                }
            }
            Case (0x05)
            {
                Local6 = DerefOf (TNS0 [Local1])
                If (ERR4)
                {
                    Local7 = ", service, "
                }
            }
            Case (0x06)
            {
                Local6 = DerefOf (TNM0 [Local1])
                If (ERR4)
                {
                    Local7 = ", mt, "
                }
            }
            Case (0x07)
            {
                Local6 = DerefOf (TNT0 [Local1])
                If (ERR4)
                {
                    Local7 = ", Identity2MS, "
                }
            }
            Case (0x08)
            {
                Local6 = DerefOf (TNI0 [Local1])
                If (ERR4)
                {
                    Local7 = ", IMPL, "
                }
            }

        }

        Concatenate (Local7, Local6, Local5)
        Concatenate (Local5, ", ", Local1)
        If ((Local2 == ZFFF))
        {
            Local0 = (Arg1 >> 0x17)
            Local2 = (Local0 & 0x01FF)
            Local6 = SB00 (Local2, Zero)
        }
        Else
        {
            Local6 = DerefOf (TFN0 [Local2])
        }

        Concatenate (Local1, Local6, Local7)
        Concatenate (Local7, ", ", Local1)
        Concatenate (Local1, Local3, Local7)
        Local5 = (Arg1 & 0x007FFFFF)
        If (Local5)
        {
            Local5 = (Arg1 >> 0x0C)
            Local2 = (Local5 & 0x07FF)
            Local3 = (Arg1 & 0x0FFF)
            If ((Local2 == ZFFF))
            {
                Local0 = (Arg1 >> 0x17)
                Local2 = (Local0 & 0x01FF)
                Local6 = SB00 (Local2, Zero)
            }
            Else
            {
                Local6 = DerefOf (TFN0 [Local2])
            }

            Concatenate (Local7, ", ", Local1)
            Concatenate (Local1, Local6, Local5)
            Concatenate (Local5, ", ", Local1)
            Concatenate (Local1, Local3, Local7)
            If ((ObjectType (Arg2) == C00A))
            {
                Concatenate (Local7, ", ", Local1)
                Concatenate (Local1, Arg2, Local7)
            }
        }

        Return (Local7)
    }

    Method (RERR, 0, Serialized)
    {
        Name (LPN0, Zero)
        Name (LPC0, Zero)
        LPN0 = ETR1 /* \ETR1 */
        If ((ERRS < LPN0))
        {
            LPN0 = ERRS /* \ERRS */
        }

        Local0 = Zero
        Debug = "========= ERRORS SUMMARY (max 400):"
        While (LPN0)
        {
            Local7 = DerefOf (ERRP [Local0])
            Local0++
            Local6 = DerefOf (ERRP [Local0])
            Local0++
            Local4 = DerefOf (ERRP [Local0])
            Local0++
            Local1 = UNP0 (Local7, Local6, Local4)
            If (ERR4)
            {
                Concatenate ("", Local7, Local2)
                Concatenate (Local2, ", ", Local5)
                Concatenate (Local5, Local6, Local2)
                Concatenate (Local2, Local1, Local7)
            }
            Else
            {
                Concatenate ("", Local1, Local7)
            }

            Debug = Local7
            LPN0--
            LPC0++
        }

        If ((ERRS > ETR1))
        {
            Debug = "********* Not all errors were traced, maximum exceeded!"
        }

        Debug = "========= END."
    }

    Method (RRM0, 0, Serialized, 3)
    {
        Name (LPN0, Zero)
        Name (LPC0, Zero)
        LPN0 = ETR0 /* \ETR0 */
        If ((RMRC < LPN0))
        {
            LPN0 = RMRC /* \RMRC */
        }

        Debug = "========= ROOT METHODS SUMMARY (max 600):"
        While (LPN0)
        {
            Local7 = DerefOf (RP0P [LPC0])
            Debug = Local7
            LPN0--
            LPC0++
        }

        If ((RMRC > ETR0))
        {
            Debug = "********* Not all root Methods were traced, maximum exceeded!"
        }

        Debug = "========= END."
    }

    Method (FNSH, 0, NotSerialized)
    {
        CH03 ("FNSH", Zero, 0x0834, Zero, Zero)
        CST0 ()
        Local7 = Timer
        Local6 = (Local7 - TMT0) /* \TMT0 */
        Divide (Local6, 0x0A, Local1, Local2)
        Divide (Local2, 0x000F4240, Local1, Local0)
        Debug = Concatenate ("Run time (in seconds): 0x", Local0)
        Debug = Concatenate ("The total number of exceptions handled: 0x", EXC1)
        If (ERRS)
        {
            RERR ()
        }

        RRM0 ()
        If (F64)
        {
            Local0 = "TEST ACPICA: 64-bit :"
        }
        Else
        {
            Local0 = "TEST ACPICA: 32-bit :"
        }

        If (ERR7)
        {
            Concatenate ("!!!! ERRORS were detected during the loading stage, # 0x", ERR7, Debug)
        }

        EXC1 = Zero
        If ((ERRS || ERR7))
        {
            Concatenate (Local0, " FAIL : Errors # 0x", Local1)
            Concatenate (Local1, ERRS, Local2)
            Concatenate (Local2, ", Failed tests # 0x", Local1)
            Debug = Concatenate (Local1, ERR6)
            Return (One)
        }

        Debug = Concatenate (Local0, " PASS")
        Return (Zero)
    }

    Method (TRC0, 3, NotSerialized)
    {
        If (TRCF)
        {
            Concatenate (TRCH, ", WRITE: where ", Local0)
            Concatenate (Local0, Arg1, Local1)
            Concatenate (Local1, ", ", Local0)
            Concatenate (Local0, Arg2, Local1)
            Debug = Local1
        }
    }

    Method (TRC1, 3, NotSerialized)
    {
        If (TRCF)
        {
            Concatenate (TRCH, ", READ: where ", Local0)
            Concatenate (Local0, Arg1, Local1)
            Concatenate (Local1, ", ", Local0)
            Concatenate (Local0, Arg2, Local1)
            Debug = Local1
        }
    }

    Method (TRC2, 1, NotSerialized)
    {
        If (TRCF)
        {
            Concatenate (TRCH, ", ", Local0)
            Concatenate (Local0, Arg0, Local1)
            Debug = Local1
        }
    }

    Method (TRC8, 0, NotSerialized)
    {
        TRCF = One
    }

    Method (TRC9, 0, NotSerialized)
    {
        TRCF = Zero
    }

    Method (TS00, 1, NotSerialized)
    {
        If (PR01)
        {
            Concatenate ("Test ", Arg0, Local0)
            Concatenate (Local0, " started", Local1)
            Debug = Local1
        }
    }

    Method (TMR0, 1, NotSerialized)
    {
        Divide (Arg0, 0x0A, Local0, Local1)
        Divide (Local1, 0x000F4240, Local0, Local2)
        Return (Local2)
    }

    Name (SLCK, Zero)
    Name (MLVL, Zero)
    Method (MN00, 0, NotSerialized)
    {
        SLCK = Zero
        MLVL = One
        Local7 = MAIN ()
        Return (Local7)
    }

    Method (MN01, 0, NotSerialized)
    {
        SLCK = One
        MLVL = One
        Local7 = MAIN ()
        Return (Local7)
    }

    Name (RUN0, Zero)
    Name (RUN1, Zero)
    Name (RUN2, Zero)
    Name (RUN3, Zero)
    Name (RTPT, Zero)
    Name (W000, Zero)
    Name (W001, Zero)
    Name (W002, Zero)
    Name (W003, Zero)
    Name (W004, Zero)
    Name (W005, Zero)
    Name (W006, Zero)
    Name (W007, Zero)
    Name (W008, Zero)
    Name (W009, Zero)
    Name (W00A, Zero)
    Name (W00B, Zero)
    Name (W00C, Zero)
    Name (W00D, Zero)
    Name (W01A, Zero)
    Name (W00E, Zero)
    Name (W00F, Zero)
    Name (W010, Zero)
    Name (W011, Zero)
    Name (W012, Zero)
    Name (W022, Zero)
    Name (W013, Zero)
    Name (W014, Zero)
    Name (W015, Zero)
    Name (W016, Zero)
    Name (W017, Zero)
    Name (W021, Zero)
    Name (W018, Zero)
    Name (W019, Zero)
    Name (W020, Zero)
    Method (SRTP, 1, NotSerialized)
    {
        RTPT = Arg0
    }

    Method (RTPI, 0, NotSerialized)
    {
        RUN0 = Zero
        RUN1 = Zero
        RUN2 = One
        RUN3 = 0x03
        W000 = One
        W001 = One
        W002 = One
        W003 = One
        W004 = One
        W005 = One
        W006 = One
        W007 = One
        W008 = One
        W009 = One
        W00A = One
        W00B = One
        W00C = One
        W00D = One
        W00E = One
        W00F = One
        W010 = One
        W011 = One
        W021 = One
        W012 = One
        W013 = One
        W014 = One
        W015 = One
        W016 = One
        W019 = One
        W017 = One
        W018 = One
    }

    Name (Y078, Zero)
    Name (Y083, Zero)
    Name (Y084, One)
    Name (Y098, One)
    Name (Y100, Zero)
    Name (Y103, One)
    Name (Y104, One)
    Name (Y105, One)
    Name (Y106, Zero)
    Name (Y111, One)
    Name (Y113, Zero)
    Name (Y114, Zero)
    Name (Y118, Zero)
    Name (Y119, Zero)
    Name (Y120, Zero)
    Name (Y121, Zero)
    Name (Y126, Zero)
    Name (Y127, Zero)
    Name (Y128, One)
    Name (Y132, Zero)
    Name (Y133, Zero)
    Name (Y134, Zero)
    Name (Y135, Zero)
    Name (Y136, One)
    Name (Y157, One)
    Name (Y164, One)
    Name (Y176, Zero)
    Name (Y178, One)
    Name (Y182, One)
    Name (Y192, One)
    Name (Y200, Zero)
    Name (Y203, Zero)
    Name (Y204, Zero)
    Name (Y205, Zero)
    Name (Y206, Zero)
    Name (Y207, Zero)
    Name (Y208, Zero)
    Name (Y213, Zero)
    Name (Y214, Zero)
    Name (Y215, Zero)
    Name (Y216, Zero)
    Name (Y217, Zero)
    Name (Y220, Zero)
    Name (Y221, One)
    Name (Y222, Zero)
    Name (Y223, One)
    Name (Y224, Zero)
    Name (Y238, Zero)
    Name (Y242, Zero)
    Name (Y243, Zero)
    Name (Y248, Zero)
    Name (Y251, Zero)
    Name (Y260, Zero)
    Name (Y261, Zero)
    Name (Y262, Zero)
    Name (Y263, Zero)
    Name (Y264, Zero)
    Name (Y275, Zero)
    Name (Y276, Zero)
    Name (Y281, Zero)
    Name (Y282, Zero)
    Name (Y283, One)
    Name (Y284, One)
    Name (Y286, One)
    Name (Y287, Zero)
    Name (Y288, Zero)
    Name (Y289, Zero)
    Name (Y290, Zero)
    Name (Y292, Zero)
    Name (Y293, Zero)
    Name (Y294, Zero)
    Name (Y296, Zero)
    Name (Y297, Zero)
    Name (Y300, Zero)
    Name (Y301, Zero)
    Name (Y302, Zero)
    Name (Y349, Zero)
    Name (Y350, Zero)
    Name (Y361, Zero)
    Name (Y362, Zero)
    Name (Y364, Zero)
    Name (Y365, Zero)
    Name (Y366, Zero)
    Name (Y367, Zero)
    Name (Y500, Zero)
    Name (Y501, Zero)
    Name (Y502, Zero)
    Name (Y503, Zero)
    Name (Y504, Zero)
    Name (Y505, Zero)
    Name (Y506, Zero)
    Name (Y507, Zero)
    Name (Y508, Zero)
    Name (Y509, Zero)
    Name (Y510, Zero)
    Name (Y511, Zero)
    Name (Y512, Zero)
    Name (Y513, Zero)
    Name (Y514, Zero)
    Name (Y516, Zero)
    Name (Y517, Zero)
    Name (Y518, Zero)
    Name (Y519, Zero)
    Name (Y520, Zero)
    Name (Y521, Zero)
    Name (Y522, One)
    Name (Y523, Zero)
    Name (Y524, Zero)
    Name (Y525, Zero)
    Name (Y526, Zero)
    Name (Y527, Zero)
    Name (Y600, Zero)
    Name (Y601, Zero)
    Name (Y602, One)
    Name (Y603, Zero)
    Name (Y900, Zero)
    Name (Y901, One)
    Name (Y902, One)
    Name (Q001, One)
    Name (Q002, Zero)
    Name (Q003, Zero)
    Name (Q004, Zero)
    Name (Q005, Zero)
    Name (Q006, Zero)
    Name (Q007, Zero)
    Name (Q008, Zero)
    Name (Q009, Zero)
    Name (Q00A, Zero)
    Name (Q00B, Zero)
    Name (RN00, One)
    Name (RN01, Zero)
    Name (RN02, Zero)
    Name (RN03, Zero)
    Name (RN04, Zero)
    Name (RN05, Zero)
    Name (RN06, Zero)
    Name (X104, One)
    Name (X114, One)
    Name (X127, One)
    Name (X128, One)
    Name (X131, One)
    Name (X132, One)
    Name (X133, One)
    Name (X153, One)
    Name (X170, One)
    Name (X191, One)
    Name (X192, One)
    Name (X193, One)
    Name (X194, One)
    Name (X195, Zero)
    Name (FLG9, Zero)
    Name (RUN4, Zero)
    Name (REL0, 0x16)
    Name (SETN, 0x05)
    Method (SET2, 1, Serialized)
    {
        Local0 = Arg0
        Switch (ToInteger (Local0))
        {
            Case (Zero)
            {
                Y135 = Zero
                Y900 = One
                Y901 = Zero
                FLG9 = One
                Y263 = Zero
                Y275 = Zero
                Y276 = Zero
            }
            Case (One)
            {
                Y135 = One
                Y900 = Zero
                Y901 = Zero
                FLG9 = One
                Y263 = Zero
                Y275 = Zero
                Y276 = Zero
            }
            Case (0x02)
            {
                Y135 = Zero
                Y900 = Zero
                Y901 = One
                FLG9 = Zero
                Y263 = Zero
                Y275 = Zero
                Y276 = Zero
            }
            Case (0x03)
            {
                Y135 = Zero
                Y900 = One
                Y901 = Zero
                FLG9 = One
                Y263 = One
                Y275 = Zero
                Y276 = Zero
                Y262 = Zero
            }
            Case (0x04)
            {
                Y135 = Zero
                Y900 = One
                Y901 = Zero
                FLG9 = One
                Y263 = One
                Y275 = One
                Y276 = One
                Y262 = Zero
                Y251 = Zero
                Y300 = Zero
            }
            Case (0x05)
            {
                Y135 = Zero
                Y900 = One
                Y901 = One
                FLG9 = One
                Y263 = One
                Y275 = One
                Y276 = One
                Y262 = One
                Y251 = Zero
                Y300 = Zero
            }
            Case (0x06)
            {
                Y135 = Zero
                Y900 = One
                Y901 = Zero
                FLG9 = One
                Y263 = One
                Y275 = One
                Y276 = One
                Y262 = One
                Y251 = One
                Y300 = One
                Y902 = Zero
            }

        }

        If (!RUN4)
        {
            Concatenate ("Release of parent ACPICA code 0x", Revision, Debug)
            Concatenate ("Release of ASLTS test suite  0x", REL0, Debug)
            Concatenate ("Settings of ASLTS test suite 0x", Arg0, Debug)
        }
    }

    Name (Z063, 0x3F)
    Name (EXC0, Zero)
    Name (EXC1, Zero)
    Name (EX00, Zero)
    Name (EX01, "")
    Name (EX04, Zero)
    Name (EX05, "")
    Name (EX0D, 0xFD)
    Name (EX0E, 0xFE)
    Name (EX0F, 0xFF)
    Name (PF00, Package (0x57)
    {
        Package (0x03)
        {
            Zero, 
            Zero, 
            "AE_OK"
        }, 

        Package (0x03)
        {
            One, 
            One, 
            "AE_ERROR"
        }, 

        Package (0x03)
        {
            0x02, 
            0x02, 
            "AE_NO_ACPI_TABLES"
        }, 

        Package (0x03)
        {
            0x03, 
            0x03, 
            "AE_NO_NAMESPACE"
        }, 

        Package (0x03)
        {
            0x04, 
            0x04, 
            "AE_NO_MEMORY"
        }, 

        Package (0x03)
        {
            0x05, 
            0x05, 
            "AE_NOT_FOUND"
        }, 

        Package (0x03)
        {
            0x06, 
            0x06, 
            "AE_NOT_EXIST"
        }, 

        Package (0x03)
        {
            0x07, 
            0x07, 
            "AE_ALREADY_EXISTS"
        }, 

        Package (0x03)
        {
            0x08, 
            0x08, 
            "AE_TYPE"
        }, 

        Package (0x03)
        {
            0x09, 
            0x09, 
            "AE_NULL_OBJECT"
        }, 

        Package (0x03)
        {
            0x0A, 
            0x0A, 
            "AE_NULL_ENTRY"
        }, 

        Package (0x03)
        {
            0x0B, 
            0x0B, 
            "AE_BUFFER_OVERFLOW"
        }, 

        Package (0x03)
        {
            0x0C, 
            0x0C, 
            "AE_STACK_OVERFLOW"
        }, 

        Package (0x03)
        {
            0x0D, 
            0x0D, 
            "AE_STACK_UNDERFLOW"
        }, 

        Package (0x03)
        {
            0x0E, 
            0x0E, 
            "AE_NOT_IMPLEMENTED"
        }, 

        Package (0x03)
        {
            0x0F, 
            0x0F, 
            "AE_VERSION_MISMATCH"
        }, 

        Package (0x03)
        {
            0x10, 
            0x0F, 
            "AE_SUPPORT"
        }, 

        Package (0x03)
        {
            0x11, 
            0x11, 
            "AE_SHARE"
        }, 

        Package (0x03)
        {
            0x12, 
            0x10, 
            "AE_LIMIT"
        }, 

        Package (0x03)
        {
            0x13, 
            0x11, 
            "AE_TIME"
        }, 

        Package (0x03)
        {
            0x14, 
            0x14, 
            "AE_UNKNOWN_STATUS"
        }, 

        Package (0x03)
        {
            0x15, 
            0x12, 
            "AE_ACQUIRE_DEADLOCK"
        }, 

        Package (0x03)
        {
            0x16, 
            0x13, 
            "AE_RELEASE_DEADLOCK"
        }, 

        Package (0x03)
        {
            0x17, 
            0x14, 
            "AE_NOT_ACQUIRED"
        }, 

        Package (0x03)
        {
            0x18, 
            0x15, 
            "AE_ALREADY_ACQUIRED"
        }, 

        Package (0x03)
        {
            0x19, 
            0x16, 
            "AE_NO_HARDWARE_RESPONSE"
        }, 

        Package (0x03)
        {
            0x1A, 
            0x17, 
            "AE_NO_GLOBAL_LOCK"
        }, 

        Package (0x03)
        {
            0x1B, 
            0x18, 
            "AE_ABORT_METHOD"
        }, 

        Package (0x03)
        {
            0x1C, 
            0x1001, 
            "AE_BAD_PARAMETER"
        }, 

        Package (0x03)
        {
            0x1D, 
            0x1002, 
            "AE_BAD_CHARACTER"
        }, 

        Package (0x03)
        {
            0x1E, 
            0x1003, 
            "AE_BAD_PATHNAME"
        }, 

        Package (0x03)
        {
            0x1F, 
            0x1004, 
            "AE_BAD_DATA"
        }, 

        Package (0x03)
        {
            0x20, 
            0x1005, 
            "AE_BAD_ADDRESS"
        }, 

        Package (0x03)
        {
            0x21, 
            0x1006, 
            "AE_ALIGNMENT"
        }, 

        Package (0x03)
        {
            0x22, 
            0x1005, 
            "AE_BAD_HEX_CONSTANT"
        }, 

        Package (0x03)
        {
            0x23, 
            0x1006, 
            "AE_BAD_OCTAL_CONSTANT"
        }, 

        Package (0x03)
        {
            0x24, 
            0x1007, 
            "AE_BAD_DECIMAL_CONSTANT"
        }, 

        Package (0x03)
        {
            0x25, 
            0x2001, 
            "AE_BAD_SIGNATURE"
        }, 

        Package (0x03)
        {
            0x26, 
            0x2002, 
            "AE_BAD_HEADER"
        }, 

        Package (0x03)
        {
            0x27, 
            0x2003, 
            "AE_BAD_CHECKSUM"
        }, 

        Package (0x03)
        {
            0x28, 
            0x2004, 
            "AE_BAD_VALUE"
        }, 

        Package (0x03)
        {
            0x29, 
            0x2005, 
            "AE_TABLE_NOT_SUPPORTED"
        }, 

        Package (0x03)
        {
            0x2A, 
            0x2005, 
            "AE_INVALID_TABLE_LENGTH"
        }, 

        Package (0x03)
        {
            0x2B, 
            0x3001, 
            "AE_AML_ERROR"
        }, 

        Package (0x03)
        {
            0x2C, 
            0x3002, 
            "AE_AML_PARSE"
        }, 

        Package (0x03)
        {
            0x2D, 
            0x3001, 
            "AE_AML_BAD_OPCODE"
        }, 

        Package (0x03)
        {
            0x2E, 
            0x3002, 
            "AE_AML_NO_OPERAND"
        }, 

        Package (0x03)
        {
            0x2F, 
            0x3003, 
            "AE_AML_OPERAND_TYPE"
        }, 

        Package (0x03)
        {
            0x30, 
            0x3004, 
            "AE_AML_OPERAND_VALUE"
        }, 

        Package (0x03)
        {
            0x31, 
            0x3005, 
            "AE_AML_UNINITIALIZED_LOCAL"
        }, 

        Package (0x03)
        {
            0x32, 
            0x3006, 
            "AE_AML_UNINITIALIZED_ARG"
        }, 

        Package (0x03)
        {
            0x33, 
            0x3007, 
            "AE_AML_UNINITIALIZED_ELEMENT"
        }, 

        Package (0x03)
        {
            0x34, 
            0x3008, 
            "AE_AML_NUMERIC_OVERFLOW"
        }, 

        Package (0x03)
        {
            0x35, 
            0x3009, 
            "AE_AML_REGION_LIMIT"
        }, 

        Package (0x03)
        {
            0x36, 
            0x300A, 
            "AE_AML_BUFFER_LIMIT"
        }, 

        Package (0x03)
        {
            0x37, 
            0x300B, 
            "AE_AML_PACKAGE_LIMIT"
        }, 

        Package (0x03)
        {
            0x38, 
            0x300C, 
            "AE_AML_DIVIDE_BY_ZERO"
        }, 

        Package (0x03)
        {
            0x39, 
            0x300D, 
            "AE_AML_BAD_NAME"
        }, 

        Package (0x03)
        {
            0x3A, 
            0x300E, 
            "AE_AML_NAME_NOT_FOUND"
        }, 

        Package (0x03)
        {
            0x3B, 
            0x300F, 
            "AE_AML_INTERNAL"
        }, 

        Package (0x03)
        {
            0x3C, 
            0x3010, 
            "AE_AML_INVALID_SPACE_ID"
        }, 

        Package (0x03)
        {
            0x3D, 
            0x3011, 
            "AE_AML_STRING_LIMIT"
        }, 

        Package (0x03)
        {
            0x3E, 
            0x3012, 
            "AE_AML_NO_RETURN_VALUE"
        }, 

        Package (0x03)
        {
            0x3F, 
            0x3014, 
            "AE_AML_NOT_OWNER"
        }, 

        Package (0x03)
        {
            0x40, 
            0x3015, 
            "AE_AML_MUTEX_ORDER"
        }, 

        Package (0x03)
        {
            0x41, 
            0x3016, 
            "AE_AML_MUTEX_NOT_ACQUIRED"
        }, 

        Package (0x03)
        {
            0x42, 
            0x3017, 
            "AE_AML_INVALID_RESOURCE_TYPE"
        }, 

        Package (0x03)
        {
            0x43, 
            0x3018, 
            "AE_AML_INVALID_INDEX"
        }, 

        Package (0x03)
        {
            0x44, 
            0x3019, 
            "AE_AML_REGISTER_LIMIT"
        }, 

        Package (0x03)
        {
            0x45, 
            0x301A, 
            "AE_AML_NO_WHILE"
        }, 

        Package (0x03)
        {
            0x46, 
            0x301B, 
            "AE_AML_ALIGNMENT"
        }, 

        Package (0x03)
        {
            0x47, 
            0x301C, 
            "AE_AML_NO_RESOURCE_END_TAG"
        }, 

        Package (0x03)
        {
            0x48, 
            0x301D, 
            "AE_AML_BAD_RESOURCE_VALUE"
        }, 

        Package (0x03)
        {
            0x49, 
            0x301E, 
            "AE_AML_CIRCULAR_REFERENCE"
        }, 

        Package (0x03)
        {
            0x4A, 
            0x4001, 
            "AE_CTRL_RETURN_VALUE"
        }, 

        Package (0x03)
        {
            0x4B, 
            0x4002, 
            "AE_CTRL_PENDING"
        }, 

        Package (0x03)
        {
            0x4C, 
            0x4003, 
            "AE_CTRL_TERMINATE"
        }, 

        Package (0x03)
        {
            0x4D, 
            0x4004, 
            "AE_CTRL_TRUE"
        }, 

        Package (0x03)
        {
            0x4E, 
            0x4005, 
            "AE_CTRL_FALSE"
        }, 

        Package (0x03)
        {
            0x4F, 
            0x4006, 
            "AE_CTRL_DEPTH"
        }, 

        Package (0x03)
        {
            0x50, 
            0x4007, 
            "AE_CTRL_END"
        }, 

        Package (0x03)
        {
            0x51, 
            0x4008, 
            "AE_CTRL_TRANSFER"
        }, 

        Package (0x03)
        {
            0x52, 
            0x4009, 
            "AE_CTRL_BREAK"
        }, 

        Package (0x03)
        {
            0x53, 
            0x400A, 
            "AE_CTRL_CONTINUE"
        }, 

        Package (0x03)
        {
            0x54, 
            0x3013, 
            "AE_AML_METHOD_LIMIT"
        }, 

        Package (0x03)
        {
            0x55, 
            0x100B, 
            "AE_INDEX_TO_NOT_ATTACHED"
        }, 

        Package (0x03)
        {
            0x56, 
            0x1B, 
            "AE_OWNER_ID_LIMIT"
        }
    })
    Name (EXC2, 0xC8)
    Name (EX02, Package (EXC2, ){})
    Name (EX03, Package (EXC2, ){})
    Method (_ERR, 3, NotSerialized)
    {
        EX00 = Arg0
        EX01 = Arg1
        If ((EX04 == Zero))
        {
            EX04 = Arg0
            EX05 = Arg1
        }

        If (MTHR)
        {
            If ((EXC0 < EXC2))
            {
                EX02 [EXC0] = Arg2
                EX03 [EXC0] = Arg0
            }
            Else
            {
                Debug = "Maximal number of exceptions exceeded"
                ERR ("_ERR", Z063, 0x02DF, Zero, Zero, EXC0, EXC2)
            }
        }

        EXC0++
        EXC1++
        Return (Zero)
    }

    Method (CH02, 0, NotSerialized)
    {
        If (EXC1)
        {
            Concatenate ("Some unexpected exceptions were handled, 0x", EXC1, Local0)
            ERR ("CH02", Z063, 0x02F3, Zero, Zero, Local0, Zero)
        }

        Return (EXC1) /* \EXC1 */
    }

    Method (CH03, 5, NotSerialized)
    {
        Local7 = Zero
        If (EXC0)
        {
            Concatenate ("Unexpected exceptions (count ", EXC0, Local0)
            Concatenate (Local0, "), the last is ", Local1)
            Concatenate (Local1, EX01, Local0)
            Concatenate (Local0, ", ", Local1)
            Concatenate (Local1, EX00, Debug)
            ERR (Arg0, Z063, 0x030B, Arg1, Arg2, Arg3, Arg4)
            Local7 = EXC0 /* \EXC0 */
        }

        EXC0 = Zero
        EX04 = Zero
        Return (Local7)
    }

    Method (ST16, 1, Serialized)
    {
        Name (EBUF, Buffer (ISZC){})
        Name (RBUF, Buffer (0x04){})
        EBUF = ToHexString (Arg0)
        Mid (EBUF, (ISZC - 0x04), 0x04, RBUF) /* \ST16.RBUF */
        Return (Concatenate ("0x", ToString (RBUF, Ones)))
    }

    Method (CH04, 7, NotSerialized)
    {
        Local5 = Zero
        If ((Arg2 == 0xFF))
        {
            If ((EXC0 == Zero))
            {
                Local5 = One
                Debug = "ERROR: No ANY exception has arisen."
            }
        }
        Else
        {
            Local2 = DerefOf (PF00 [Arg2])
            Local3 = DerefOf (Local2 [One])
            Local4 = DerefOf (Local2 [0x02])
            If ((EXC0 == Zero))
            {
                Local5 = One
                Concatenate ("No exception - expected: ", Local4, Local0)
                Concatenate (Local0, "-", Local0)
                Concatenate (Local0, ST16 (Local3), Local0)
                Debug = Local0
            }
            ElseIf ((!Arg1 && (EXC0 > One)))
            {
                Local5 = One
                Concatenate ("More than one exception: 0x", EXC0, Local0)
                Debug = Local0
            }
            Else
            {
                If ((Arg1 == One))
                {
                    Local6 = EX04 /* \EX04 */
                    Local7 = EX05 /* \EX05 */
                }
                Else
                {
                    Local6 = EX00 /* \EX00 */
                    Local7 = EX01 /* \EX01 */
                }

                If ((Local3 != Local6))
                {
                    Local5 = One
                    Concatenate ("Exception: ", Local7, Local0)
                    Concatenate (Local0, "-", Local0)
                    Concatenate (Local0, ST16 (Local6), Local0)
                    Concatenate (" differs from expected: ", Local4, Local1)
                    Concatenate (Local0, Local1, Local0)
                    Concatenate (Local0, "-", Local0)
                    Concatenate (Local0, ST16 (Local3), Local0)
                    Debug = Local0
                }

                If ((Local4 != Local7))
                {
                    Local5 = One
                    Debug = "Unexpected exception:"
                    Debug = Concatenate ("Expected: ", Local4)
                    Debug = Concatenate ("Received: ", Local7)
                }
            }
        }

        EXC0 = Zero
        EX04 = Zero
        If (Local5)
        {
            ERR (Arg0, Z063, 0x038A, Arg3, Arg4, Arg5, Arg6)
        }

        Return (Local5)
    }

    Method (CH05, 0, NotSerialized)
    {
        Return (CH03 ("CH05", Zero, 0x0392, Zero, Zero))
    }

    Method (CH06, 3, NotSerialized)
    {
        If (EXCV)
        {
            Return (CH04 (Arg0, Zero, Arg2, Zero, 0x0399, Zero, Zero))
        }
        Else
        {
            Return (CH04 (Arg0, Zero, 0xFF, Zero, 0x039F, Zero, Zero))
        }
    }

    Method (CH07, 7, NotSerialized)
    {
        If (SLCK)
        {
            CH03 (Arg0, Arg3, 0x03AA, Zero, Arg6)
        }
        Else
        {
            CH04 (Arg0, Arg1, Arg2, Arg3, 0x03AE, Arg5, Arg6)
        }
    }

    Method (MSG0, 2, NotSerialized)
    {
        Concatenate ("THREAD ID ", Arg0, Local0)
        Concatenate (Local0, ": ", Local1)
        Concatenate (Local1, Arg1, Local0)
        Debug = Local0
    }

    Method (MTEX, 1, NotSerialized)
    {
        Local2 = Package (0x02)
            {
                Zero, 
                Zero
            }
        Local3 = Zero
        Local4 = EXC0 /* \EXC0 */
        Local5 = Zero
        While (Local4)
        {
            Local0 = DerefOf (EX02 [Local5])
            If ((Local0 == Arg0))
            {
                Local1 = DerefOf (EX03 [Local5])
                If ((Local3 == Zero))
                {
                    Local2 [Zero] = Local1
                }

                Local3++
                EX02 [Local5] = Zero
            }

            Local4--
            Local5++
        }

        Local2 [One] = Local3
        Return (Local2)
    }

    Method (CH08, 6, NotSerialized)
    {
        Local2 = MTEX (Arg1)
        Local3 = DerefOf (Local2 [Zero])
        Local4 = DerefOf (Local2 [One])
        Local7 = Zero
        If (Local4)
        {
            Concatenate ("Unexpected exception 0x", Local3, Local0)
            Concatenate (Local0, ", number of exceptions 0x", Local1)
            Concatenate (Local1, Local4, Local0)
            MSG0 (Arg1, Local0)
            ERR (Arg0, Z063, 0x040D, Arg2, Arg3, Arg4, Arg5)
            Local7 = One
        }

        Return (Local4)
    }

    Method (CH09, 6, NotSerialized)
    {
        Local7 = MTEX (Arg1)
        Local6 = DerefOf (Local7 [Zero])
        Local7 = DerefOf (Local7 [One])
        Local5 = Zero
        If ((Arg2 == 0xFF))
        {
            If ((Local7 == Zero))
            {
                Local5 = One
                MSG0 (Arg1, "ERROR: No ANY exception has arisen.")
            }
        }
        Else
        {
            Local2 = DerefOf (PF00 [Arg2])
            Local3 = DerefOf (Local2 [One])
            Local4 = DerefOf (Local2 [0x02])
            If ((Local7 == Zero))
            {
                Local5 = One
                Concatenate ("No exception has arisen, expected: ", Local4, Local0)
                Concatenate (", opcode 0x", Local3, Local1)
                Concatenate (Local0, Local1, Local0)
                MSG0 (Arg1, Local0)
            }
            ElseIf ((Arg0 && (Local7 > One)))
            {
                Local5 = One
                Concatenate ("More than one exception has arisen: 0x", Local7, Local0)
                MSG0 (Arg1, Local0)
            }
            ElseIf ((Local3 != Local6))
            {
                Local5 = One
                Concatenate ("The exception 0x", Local6, Local0)
                Concatenate (Local0, " differs from expected ", Local1)
                Concatenate (Local1, ST16 (Local3), Local0)
                MSG0 (Arg1, Local0)
            }
        }

        If (Local5)
        {
            ERR ("CH09", Z063, 0x045E, Arg3, Arg4, Zero, Zero)
        }

        Arg5 = Local7
        Return (Local5)
    }

    Method (CH0A, 0, NotSerialized)
    {
        EXC0 = Zero
    }

    Name (Z001, One)
    Name (BS00, 0x0100)
    Name (B000, Buffer (BS00){})
    Name (B0FF, Buffer (BS00)
    {
        /* 0000 */  0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,  // ........
        /* 0008 */  0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,  // ........
        /* 0010 */  0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,  // ........
        /* 0018 */  0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,  // ........
        /* 0020 */  0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,  // ........
        /* 0028 */  0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,  // ........
        /* 0030 */  0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,  // ........
        /* 0038 */  0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,  // ........
        /* 0040 */  0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,  // ........
        /* 0048 */  0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,  // ........
        /* 0050 */  0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,  // ........
        /* 0058 */  0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,  // ........
        /* 0060 */  0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,  // ........
        /* 0068 */  0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,  // ........
        /* 0070 */  0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,  // ........
        /* 0078 */  0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,  // ........
        /* 0080 */  0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,  // ........
        /* 0088 */  0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,  // ........
        /* 0090 */  0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,  // ........
        /* 0098 */  0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,  // ........
        /* 00A0 */  0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,  // ........
        /* 00A8 */  0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,  // ........
        /* 00B0 */  0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,  // ........
        /* 00B8 */  0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,  // ........
        /* 00C0 */  0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,  // ........
        /* 00C8 */  0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,  // ........
        /* 00D0 */  0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,  // ........
        /* 00D8 */  0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,  // ........
        /* 00E0 */  0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,  // ........
        /* 00E8 */  0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,  // ........
        /* 00F0 */  0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,  // ........
        /* 00F8 */  0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF   // ........
    })
    Name (B256, Buffer (BS00)
    {
        /* 0000 */  0x00, 0x00, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07,  // ........
        /* 0008 */  0x08, 0x09, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F,  // ........
        /* 0010 */  0x00, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17,  // ........
        /* 0018 */  0x18, 0x19, 0x1A, 0x1B, 0x1C, 0x1D, 0x1E, 0x1F,  // ........
        /* 0020 */  0x10, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27,  // .!"#$%&'
        /* 0028 */  0x28, 0x29, 0x2A, 0x2B, 0x2C, 0x2D, 0x2E, 0x2F,  // ()*+,-./
        /* 0030 */  0x20, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37,  //  1234567
        /* 0038 */  0x38, 0x39, 0x3A, 0x3B, 0x3C, 0x3D, 0x3E, 0x3F,  // 89:;<=>?
        /* 0040 */  0x30, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47,  // 0ABCDEFG
        /* 0048 */  0x48, 0x49, 0x4A, 0x4B, 0x4C, 0x4D, 0x4E, 0x4F,  // HIJKLMNO
        /* 0050 */  0x40, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57,  // @QRSTUVW
        /* 0058 */  0x58, 0x59, 0x5A, 0x5B, 0x5C, 0x5D, 0x5E, 0x5F,  // XYZ[\]^_
        /* 0060 */  0x50, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67,  // Pabcdefg
        /* 0068 */  0x68, 0x69, 0x6A, 0x6B, 0x6C, 0x6D, 0x6E, 0x6F,  // hijklmno
        /* 0070 */  0x60, 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77,  // `qrstuvw
        /* 0078 */  0x78, 0x79, 0x7A, 0x7B, 0x7C, 0x7D, 0x7E, 0x7F,  // xyz{|}~.
        /* 0080 */  0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87,  // ........
        /* 0088 */  0x88, 0x89, 0x8A, 0x8B, 0x8C, 0x8D, 0x8E, 0x8F,  // ........
        /* 0090 */  0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97,  // ........
        /* 0098 */  0x98, 0x99, 0x9A, 0x9B, 0x9C, 0x9D, 0x9E, 0x9F,  // ........
        /* 00A0 */  0xA0, 0xA1, 0xA2, 0xA3, 0xA4, 0xA5, 0xA6, 0xA7,  // ........
        /* 00A8 */  0xA8, 0xA9, 0xAA, 0xAB, 0xAC, 0xAD, 0xAE, 0xAF,  // ........
        /* 00B0 */  0xB0, 0xB1, 0xB2, 0xB3, 0xB4, 0xB5, 0xB6, 0xB7,  // ........
        /* 00B8 */  0xB8, 0xB9, 0xBA, 0xBB, 0xBC, 0xBD, 0xBE, 0xBF,  // ........
        /* 00C0 */  0xC0, 0xC1, 0xC2, 0xC3, 0xC4, 0xC5, 0xC6, 0xC7,  // ........
        /* 00C8 */  0xC8, 0xC9, 0xCA, 0xCB, 0xCC, 0xCD, 0xCE, 0xCF,  // ........
        /* 00D0 */  0xD0, 0xD1, 0xD2, 0xD3, 0xD4, 0xD5, 0xD6, 0xD7,  // ........
        /* 00D8 */  0xD8, 0xD9, 0xDA, 0xDB, 0xDC, 0xDD, 0xDE, 0xDF,  // ........
        /* 00E0 */  0xE0, 0xE1, 0xE2, 0xE3, 0xE4, 0xE5, 0xE6, 0xE7,  // ........
        /* 00E8 */  0xE8, 0xE9, 0xEA, 0xEB, 0xEC, 0xED, 0xEE, 0xEF,  // ........
        /* 00F0 */  0xF0, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7,  // ........
        /* 00F8 */  0xF8, 0xF9, 0xFA, 0xFB, 0xFC, 0xFD, 0xFE, 0xFF   // ........
    })
    Name (B010, Buffer (BS00){})
    Name (B101, Buffer (BS00){})
    Name (B0B0, Buffer (BS00){})
    Name (B0G0, Buffer (BS00){})
    Name (B0F0, Buffer (BS00){})
    Method (M210, 7, Serialized)
    {
        Name (PR00, Zero)
        If (PR00)
        {
            Debug = "========:"
        }

        Name (B001, Buffer (Arg6){})
        Name (B002, Buffer (Arg6){})
        CreateBitField (B001, Arg1, F001)
        B001 = Arg4
        If (PR00)
        {
            Debug = B001 /* \M210.B001 */
        }

        F001 = 0xFFFFFFFFFFFFFFFE
        Local0 = F001 /* \M210.F001 */
        If ((Local0 != Zero))
        {
            ERR (Arg0, Z001, 0x9E, Zero, Zero, Local0, Zero)
        }

        B002 = Arg2
        If ((B001 != B002))
        {
            ERR (Arg0, Z001, 0xA4, Zero, Zero, Zero, Zero)
        }

        If (PR00)
        {
            Debug = B001 /* \M210.B001 */
        }

        B001 = Arg3
        If (PR00)
        {
            Debug = B001 /* \M210.B001 */
        }

        F001 = Zero
        Local0 = F001 /* \M210.F001 */
        If ((Local0 != Zero))
        {
            ERR (Arg0, Z001, 0xB8, Zero, Zero, Local0, Zero)
        }

        B002 = Arg5
        If ((B001 != B002))
        {
            ERR (Arg0, Z001, 0xBE, Zero, Zero, Zero, Zero)
        }

        If (PR00)
        {
            Debug = B001 /* \M210.B001 */
        }

        B001 = Arg5
        If (PR00)
        {
            Debug = B001 /* \M210.B001 */
        }

        F001 = One
        Local0 = F001 /* \M210.F001 */
        If ((Local0 != One))
        {
            ERR (Arg0, Z001, 0xD2, Zero, Zero, Local0, One)
        }

        B002 = Arg3
        If ((B001 != B002))
        {
            ERR (Arg0, Z001, 0xD8, Zero, Zero, Zero, Zero)
        }

        If (PR00)
        {
            Debug = B001 /* \M210.B001 */
        }

        B001 = Arg2
        If (PR00)
        {
            Debug = B001 /* \M210.B001 */
        }

        F001 = Ones
        Local0 = F001 /* \M210.F001 */
        If ((Local0 != One))
        {
            ERR (Arg0, Z001, 0xEC, Zero, Zero, Local0, One)
        }

        B002 = Arg4
        If ((B001 != B002))
        {
            ERR (Arg0, Z001, 0xF2, Zero, Zero, Zero, Zero)
        }

        If (PR00)
        {
            Debug = B001 /* \M210.B001 */
        }

        Local0 = SizeOf (B001)
        If ((Local0 != Arg6))
        {
            ERR (Arg0, Z001, 0xFF, Zero, Zero, Local0, Arg6)
        }

        Local0 = SizeOf (B002)
        If ((Local0 != Arg6))
        {
            ERR (Arg0, Z001, 0x0105, Zero, Zero, Local0, Arg6)
        }

        Local0 = ObjectType (F001)
        If ((Local0 != C016))
        {
            ERR (Arg0, Z001, 0x010B, Zero, Zero, Local0, C016)
        }

        If (PR00)
        {
            Debug = Local0
        }
    }

    Method (M211, 0, Serialized)
    {
        Debug = "TEST: m211, Create 1-Bit Buffer Field:"
        Name (BSZ0, Zero)
        BSZ0 = BS00 /* \BS00 */
        Name (BSZ1, Zero)
        Name (N000, Zero)
        Name (NCUR, Zero)
        N000 = (BSZ0 * 0x08)
        BSZ1 = N000 /* \M211.N000 */
        While (N000)
        {
            If ((NCUR >= BSZ1))
            {
                ERR ("M211", Z001, 0x0128, Zero, Zero, NCUR, BSZ1)
                Break
            }

            B010 = B000 /* \B000 */
            B101 = B0FF /* \B0FF */
            Divide (NCUR, 0x08, Local1, Local0)
            Local2 = (One << Local1)
            B010 [Local0] = Local2
            Local3 = ~Local2
            B101 [Local0] = Local3
            M210 ("M211", NCUR, B000, B0FF, B010, B101, BSZ0)
            N000--
            NCUR++
        }
    }

    Method (M212, 3, Serialized)
    {
        Name (PR00, Zero)
        If (PR00)
        {
            Debug = "========:"
        }

        Name (B001, Buffer (Arg2){})
        Name (B002, Buffer (Arg2){})
        CreateByteField (B001, Arg1, F001)
        B001 = B010 /* \B010 */
        If (PR00)
        {
            Debug = B001 /* \M212.B001 */
        }

        F001 = 0xFFFFFFFFFFFFFF00
        Local0 = F001 /* \M212.F001 */
        If ((Local0 != Zero))
        {
            ERR (Arg0, Z001, 0x0155, Zero, Zero, Local0, Zero)
        }

        B002 = B000 /* \B000 */
        If ((B001 != B002))
        {
            ERR (Arg0, Z001, 0x015B, Zero, Zero, Zero, Zero)
        }

        If (PR00)
        {
            Debug = B001 /* \M212.B001 */
        }

        B001 = B0FF /* \B0FF */
        If (PR00)
        {
            Debug = B001 /* \M212.B001 */
        }

        F001 = Zero
        Local0 = F001 /* \M212.F001 */
        If ((Local0 != Zero))
        {
            ERR (Arg0, Z001, 0x016F, Zero, Zero, Local0, Zero)
        }

        B002 = B101 /* \B101 */
        If ((B001 != B002))
        {
            ERR (Arg0, Z001, 0x0175, Zero, Zero, Zero, Zero)
        }

        If (PR00)
        {
            Debug = B001 /* \M212.B001 */
        }

        B001 = B101 /* \B101 */
        If (PR00)
        {
            Debug = B001 /* \M212.B001 */
        }

        F001 = 0xFF
        Local0 = F001 /* \M212.F001 */
        If ((Local0 != 0xFF))
        {
            ERR (Arg0, Z001, 0x0189, Zero, Zero, Local0, 0xFF)
        }

        B002 = B0FF /* \B0FF */
        If ((B001 != B002))
        {
            ERR (Arg0, Z001, 0x018F, Zero, Zero, Zero, Zero)
        }

        If (PR00)
        {
            Debug = B001 /* \M212.B001 */
        }

        B001 = B000 /* \B000 */
        If (PR00)
        {
            Debug = B001 /* \M212.B001 */
        }

        F001 = Ones
        Local0 = F001 /* \M212.F001 */
        If ((Local0 != 0xFF))
        {
            ERR (Arg0, Z001, 0x01A3, Zero, Zero, Local0, 0xFF)
        }

        B002 = B010 /* \B010 */
        If ((B001 != B002))
        {
            ERR (Arg0, Z001, 0x01A9, Zero, Zero, Zero, Zero)
        }

        If (PR00)
        {
            Debug = B001 /* \M212.B001 */
        }

        B001 = B000 /* \B000 */
        If (PR00)
        {
            Debug = B001 /* \M212.B001 */
        }

        F001 = 0xFFFFFFFFFFFFFF96
        Local0 = F001 /* \M212.F001 */
        If ((Local0 != 0x96))
        {
            ERR (Arg0, Z001, 0x01BD, Zero, Zero, Local0, 0x96)
        }

        B002 = B0B0 /* \B0B0 */
        If ((B001 != B002))
        {
            ERR (Arg0, Z001, 0x01C3, Zero, Zero, Zero, Zero)
        }

        If (PR00)
        {
            Debug = B001 /* \M212.B001 */
        }

        Local0 = SizeOf (B001)
        If ((Local0 != Arg2))
        {
            ERR (Arg0, Z001, 0x01D0, Zero, Zero, Local0, Arg2)
        }

        Local0 = SizeOf (B002)
        If ((Local0 != Arg2))
        {
            ERR (Arg0, Z001, 0x01D6, Zero, Zero, Local0, Arg2)
        }

        Local0 = ObjectType (F001)
        If ((Local0 != C016))
        {
            ERR (Arg0, Z001, 0x01DC, Zero, Zero, Local0, C016)
        }

        If (PR00)
        {
            Debug = Local0
        }
    }

    Method (M213, 0, Serialized)
    {
        Debug = "TEST: m213, Create 8-Bit Buffer Field:"
        Name (BSZ0, Zero)
        BSZ0 = BS00 /* \BS00 */
        Name (BSZ1, Zero)
        Name (N000, Zero)
        Name (NCUR, Zero)
        N000 = BSZ0 /* \M213.BSZ0 */
        BSZ1 = N000 /* \M213.N000 */
        While (N000)
        {
            If ((NCUR >= BSZ1))
            {
                ERR ("M213", Z001, 0x01F9, Zero, Zero, NCUR, BSZ1)
                Break
            }

            B010 = B000 /* \B000 */
            B0B0 = B000 /* \B000 */
            B101 = B0FF /* \B0FF */
            B010 [NCUR] = 0xFF
            B0B0 [NCUR] = 0x96
            B101 [NCUR] = Zero
            M212 ("M213", NCUR, BSZ0)
            N000--
            NCUR++
        }
    }

    Method (M214, 3, Serialized)
    {
        Name (PR00, Zero)
        If (PR00)
        {
            Debug = "========:"
            Debug = Arg1
            Debug = Arg2
        }

        Name (B001, Buffer (Arg2){})
        Name (B002, Buffer (Arg2){})
        CreateWordField (B001, Arg1, F001)
        B001 = B010 /* \B010 */
        If (PR00)
        {
            Debug = B001 /* \M214.B001 */
        }

        F001 = 0xFFFFFFFFFFFF0000
        Local0 = F001 /* \M214.F001 */
        If ((Local0 != Zero))
        {
            ERR (Arg0, Z001, 0x0227, Zero, Zero, Local0, Zero)
        }

        B002 = B000 /* \B000 */
        If ((B001 != B002))
        {
            ERR (Arg0, Z001, 0x022D, Zero, Zero, Zero, Zero)
        }

        If (PR00)
        {
            Debug = B001 /* \M214.B001 */
        }

        B001 = B0FF /* \B0FF */
        If (PR00)
        {
            Debug = B001 /* \M214.B001 */
        }

        F001 = Zero
        Local0 = F001 /* \M214.F001 */
        If ((Local0 != Zero))
        {
            ERR (Arg0, Z001, 0x0241, Zero, Zero, Local0, Zero)
        }

        B002 = B101 /* \B101 */
        If ((B001 != B002))
        {
            ERR (Arg0, Z001, 0x0247, Zero, Zero, Zero, Zero)
        }

        If (PR00)
        {
            Debug = B001 /* \M214.B001 */
        }

        B001 = B101 /* \B101 */
        If (PR00)
        {
            Debug = B001 /* \M214.B001 */
        }

        F001 = 0xFFFF
        Local0 = F001 /* \M214.F001 */
        If ((Local0 != 0xFFFF))
        {
            ERR (Arg0, Z001, 0x025B, Zero, Zero, Local0, 0xFFFF)
        }

        B002 = B0FF /* \B0FF */
        If ((B001 != B002))
        {
            ERR (Arg0, Z001, 0x0261, Zero, Zero, Zero, Zero)
        }

        If (PR00)
        {
            Debug = B001 /* \M214.B001 */
        }

        B001 = B000 /* \B000 */
        If (PR00)
        {
            Debug = B001 /* \M214.B001 */
        }

        F001 = Ones
        Local0 = F001 /* \M214.F001 */
        If ((Local0 != 0xFFFF))
        {
            ERR (Arg0, Z001, 0x0275, Zero, Zero, Local0, 0xFFFF)
        }

        B002 = B010 /* \B010 */
        If ((B001 != B002))
        {
            ERR (Arg0, Z001, 0x027B, Zero, Zero, Zero, Zero)
        }

        If (PR00)
        {
            Debug = B001 /* \M214.B001 */
        }

        Local0 = SizeOf (B001)
        If ((Local0 != Arg2))
        {
            ERR (Arg0, Z001, 0x0286, Zero, Zero, Local0, Arg2)
        }

        Local0 = SizeOf (B002)
        If ((Local0 != Arg2))
        {
            ERR (Arg0, Z001, 0x028C, Zero, Zero, Local0, Arg2)
        }

        B001 = B000 /* \B000 */
        If (PR00)
        {
            Debug = B001 /* \M214.B001 */
        }

        F001 = 0xFFFFFFFFFFFF7698
        Local0 = F001 /* \M214.F001 */
        If ((Local0 != 0x7698))
        {
            ERR (Arg0, Z001, 0x029B, Zero, Zero, Local0, 0x7698)
        }

        B002 = B0B0 /* \B0B0 */
        If ((B001 != B002))
        {
            ERR (Arg0, Z001, 0x02A1, Zero, Zero, Zero, Zero)
        }

        If (PR00)
        {
            Debug = B001 /* \M214.B001 */
        }

        Local0 = SizeOf (B001)
        If ((Local0 != Arg2))
        {
            ERR (Arg0, Z001, 0x02AE, Zero, Zero, Local0, Arg2)
        }

        Local0 = SizeOf (B002)
        If ((Local0 != Arg2))
        {
            ERR (Arg0, Z001, 0x02B4, Zero, Zero, Local0, Arg2)
        }

        Local0 = ObjectType (F001)
        If ((Local0 != C016))
        {
            ERR (Arg0, Z001, 0x02BA, Zero, Zero, Local0, C016)
        }

        If (PR00)
        {
            Debug = Local0
        }
    }

    Method (M215, 0, Serialized)
    {
        Debug = "TEST: m215, Create 16-Bit Buffer Field:"
        Name (BSZ0, Zero)
        BSZ0 = BS00 /* \BS00 */
        Name (BSZ1, Zero)
        Name (N000, Zero)
        Name (NCUR, Zero)
        N000 = (BSZ0 - One)
        BSZ1 = N000 /* \M215.N000 */
        While (N000)
        {
            If ((NCUR >= BSZ1))
            {
                ERR ("M215", Z001, 0x02D7, Zero, Zero, NCUR, BSZ1)
                Break
            }

            B010 = B000 /* \B000 */
            B0B0 = B000 /* \B000 */
            B101 = B0FF /* \B0FF */
            Local0 = NCUR /* \M215.NCUR */
            B010 [Local0] = 0xFF
            B0B0 [Local0] = 0x98
            B101 [Local0] = Zero
            Local0++
            B010 [Local0] = 0xFF
            B0B0 [Local0] = 0x76
            B101 [Local0] = Zero
            M214 ("M215", NCUR, BSZ0)
            N000--
            NCUR++
        }
    }

    Method (M216, 3, Serialized)
    {
        Name (PR00, Zero)
        If (PR00)
        {
            Debug = "========:"
            Debug = Arg1
            Debug = Arg2
        }

        Name (B001, Buffer (Arg2){})
        Name (B002, Buffer (Arg2){})
        CreateDWordField (B001, Arg1, F001)
        B001 = B010 /* \B010 */
        If (PR00)
        {
            Debug = B001 /* \M216.B001 */
        }

        F001 = 0xFFFFFFFF00000000
        Local0 = F001 /* \M216.F001 */
        If ((Local0 != Zero))
        {
            ERR (Arg0, Z001, 0x030A, Zero, Zero, Local0, Zero)
        }

        B002 = B000 /* \B000 */
        If ((B001 != B002))
        {
            ERR (Arg0, Z001, 0x0310, Zero, Zero, Zero, Zero)
        }

        If (PR00)
        {
            Debug = B001 /* \M216.B001 */
        }

        B001 = B0FF /* \B0FF */
        If (PR00)
        {
            Debug = B001 /* \M216.B001 */
        }

        F001 = Zero
        Local0 = F001 /* \M216.F001 */
        If ((Local0 != Zero))
        {
            ERR (Arg0, Z001, 0x0324, Zero, Zero, Local0, Zero)
        }

        B002 = B101 /* \B101 */
        If ((B001 != B002))
        {
            ERR (Arg0, Z001, 0x032A, Zero, Zero, Zero, Zero)
        }

        If (PR00)
        {
            Debug = B001 /* \M216.B001 */
        }

        B001 = B101 /* \B101 */
        If (PR00)
        {
            Debug = B001 /* \M216.B001 */
        }

        F001 = 0xFFFFFFFF
        Local0 = F001 /* \M216.F001 */
        If ((Local0 != 0xFFFFFFFF))
        {
            ERR (Arg0, Z001, 0x033E, Zero, Zero, Local0, 0xFFFFFFFF)
        }

        B002 = B0FF /* \B0FF */
        If ((B001 != B002))
        {
            ERR (Arg0, Z001, 0x0344, Zero, Zero, Zero, Zero)
        }

        If (PR00)
        {
            Debug = B001 /* \M216.B001 */
        }

        B001 = B000 /* \B000 */
        If (PR00)
        {
            Debug = B001 /* \M216.B001 */
        }

        F001 = Ones
        Local0 = F001 /* \M216.F001 */
        If ((Local0 != 0xFFFFFFFF))
        {
            ERR (Arg0, Z001, 0x0358, Zero, Zero, Local0, 0xFFFFFFFF)
        }

        B002 = B010 /* \B010 */
        If ((B001 != B002))
        {
            ERR (Arg0, Z001, 0x035E, Zero, Zero, Zero, Zero)
        }

        If (PR00)
        {
            Debug = B001 /* \M216.B001 */
        }

        Local0 = SizeOf (B001)
        If ((Local0 != Arg2))
        {
            ERR (Arg0, Z001, 0x0369, Zero, Zero, Local0, Arg2)
        }

        Local0 = SizeOf (B002)
        If ((Local0 != Arg2))
        {
            ERR (Arg0, Z001, 0x036F, Zero, Zero, Local0, Arg2)
        }

        B001 = B000 /* \B000 */
        If (PR00)
        {
            Debug = B001 /* \M216.B001 */
        }

        F001 = 0xFFFFFFFF32547698
        Local0 = F001 /* \M216.F001 */
        If ((Local0 != 0x32547698))
        {
            ERR (Arg0, Z001, 0x037E, Zero, Zero, Local0, 0x32547698)
        }

        B002 = B0B0 /* \B0B0 */
        If ((B001 != B002))
        {
            ERR (Arg0, Z001, 0x0384, Zero, Zero, Zero, Zero)
        }

        If (PR00)
        {
            Debug = B001 /* \M216.B001 */
        }

        Local0 = SizeOf (B001)
        If ((Local0 != Arg2))
        {
            ERR (Arg0, Z001, 0x0391, Zero, Zero, Local0, Arg2)
        }

        Local0 = SizeOf (B002)
        If ((Local0 != Arg2))
        {
            ERR (Arg0, Z001, 0x0397, Zero, Zero, Local0, Arg2)
        }

        Local0 = ObjectType (F001)
        If ((Local0 != C016))
        {
            ERR (Arg0, Z001, 0x039D, Zero, Zero, Local0, C016)
        }

        If (PR00)
        {
            Debug = Local0
        }
    }

    Method (M217, 0, Serialized)
    {
        Debug = "TEST: m217, Create 32-Bit Buffer Field:"
        Name (BSZ0, Zero)
        BSZ0 = BS00 /* \BS00 */
        Name (BSZ1, Zero)
        Name (N000, Zero)
        Name (NCUR, Zero)
        N000 = (BSZ0 - 0x03)
        BSZ1 = N000 /* \M217.N000 */
        While (N000)
        {
            If ((NCUR >= BSZ1))
            {
                ERR ("M217", Z001, 0x03BA, Zero, Zero, NCUR, BSZ1)
                Break
            }

            B010 = B000 /* \B000 */
            B0B0 = B000 /* \B000 */
            B101 = B0FF /* \B0FF */
            Local0 = NCUR /* \M217.NCUR */
            B010 [Local0] = 0xFF
            B0B0 [Local0] = 0x98
            B101 [Local0] = Zero
            Local0++
            B010 [Local0] = 0xFF
            B0B0 [Local0] = 0x76
            B101 [Local0] = Zero
            Local0++
            B010 [Local0] = 0xFF
            B0B0 [Local0] = 0x54
            B101 [Local0] = Zero
            Local0++
            B010 [Local0] = 0xFF
            B0B0 [Local0] = 0x32
            B101 [Local0] = Zero
            M216 ("M217", NCUR, BSZ0)
            N000--
            NCUR++
        }
    }

    Method (M218, 3, Serialized)
    {
        Name (PR00, Zero)
        Name (ERR0, Zero)
        ERR0 = ERRS /* \ERRS */
        Name (BB00, Buffer (0x08){})
        Name (BB01, Buffer (0x08)
        {
             0xFF, 0xFF, 0xFF, 0xFF, 0x00, 0x00, 0x00, 0x00   // ........
        })
        Name (BB02, Buffer (0x08)
        {
             0x98, 0x76, 0x54, 0x32, 0x00, 0x00, 0x00, 0x00   // .vT2....
        })
        If (PR00)
        {
            Debug = "========:"
            Debug = Arg1
            Debug = Arg2
        }

        Name (B001, Buffer (Arg2){})
        Name (B002, Buffer (Arg2){})
        CreateQWordField (B001, Arg1, F001)
        CreateDWordField (B002, (Arg1 + 0x04), F321)
        B001 = B010 /* \B010 */
        If (PR00)
        {
            Debug = "======== 1:"
            Debug = B001 /* \M218.B001 */
        }

        F001 = Zero
        Local0 = F001 /* \M218.F001 */
        If ((F64 == One))
        {
            If ((Local0 != Zero))
            {
                ERR (Arg0, Z001, 0x040C, Zero, Zero, Local0, Zero)
            }
        }
        ElseIf ((Local0 != BB00))
        {
            ERR (Arg0, Z001, 0x0411, Zero, Zero, Zero, Zero)
        }

        B002 = B000 /* \B000 */
        If ((B001 != B002))
        {
            ERR (Arg0, Z001, 0x0417, Zero, Zero, Zero, Zero)
        }

        If (PR00)
        {
            Debug = "======== 2:"
            Debug = B001 /* \M218.B001 */
        }

        B001 = B0FF /* \B0FF */
        If (PR00)
        {
            Debug = "======== 3:"
            Debug = B001 /* \M218.B001 */
        }

        F001 = Zero
        Local0 = F001 /* \M218.F001 */
        If ((F64 == One))
        {
            If ((Local0 != Zero))
            {
                ERR (Arg0, Z001, 0x042F, Zero, Zero, Local0, Zero)
            }
        }
        ElseIf ((Local0 != BB00))
        {
            ERR (Arg0, Z001, 0x0434, Zero, Zero, Zero, Zero)
        }

        B002 = B101 /* \B101 */
        If ((B001 != B002))
        {
            ERR (Arg0, Z001, 0x043A, Zero, Zero, Zero, Zero)
        }

        If (PR00)
        {
            Debug = "======== 4:"
            Debug = B001 /* \M218.B001 */
        }

        B001 = B101 /* \B101 */
        If (PR00)
        {
            Debug = "======== 5:"
            Debug = B001 /* \M218.B001 */
        }

        F001 = Ones
        Local0 = F001 /* \M218.F001 */
        If ((F64 == One))
        {
            If ((Local0 != Ones))
            {
                ERR (Arg0, Z001, 0x0454, Zero, Zero, Local0, Ones)
            }
        }
        ElseIf ((Local0 != BB01))
        {
            If (PR00)
            {
                Debug = "=========================:"
                Debug = Local0
                Debug = BB01 /* \M218.BB01 */
                Debug = "=========================."
            }

            ERR (Arg0, Z001, 0x0461, Zero, Zero, Zero, Zero)
        }

        B002 = B0FF /* \B0FF */
        If ((F64 == Zero))
        {
            F321 = Zero
        }

        If ((B001 != B002))
        {
            If (PR00)
            {
                Debug = "=========================:"
                Debug = B001 /* \M218.B001 */
                Debug = B002 /* \M218.B002 */
                Debug = "=========================."
            }

            ERR (Arg0, Z001, 0x0476, Zero, Zero, Zero, Zero)
        }

        If (PR00)
        {
            Debug = "======== 6:"
            Debug = B001 /* \M218.B001 */
        }

        B001 = B000 /* \B000 */
        If (PR00)
        {
            Debug = "======== 7:"
            Debug = F001 /* \M218.F001 */
            Debug = B001 /* \M218.B001 */
            Debug = BB01 /* \M218.BB01 */
        }

        F001 = Ones
        If (PR00)
        {
            Debug = "======== 8:"
            Debug = Local0
            Debug = F001 /* \M218.F001 */
            Debug = B001 /* \M218.B001 */
            Debug = BB01 /* \M218.BB01 */
        }

        Local0 = F001 /* \M218.F001 */
        If ((F64 == One))
        {
            If ((Local0 != Ones))
            {
                ERR (Arg0, Z001, 0x04A0, Zero, Zero, Local0, Ones)
            }
        }
        ElseIf ((Local0 != BB01))
        {
            If (PR00)
            {
                Debug = "=========================:"
                Debug = Local0
                Debug = BB01 /* \M218.BB01 */
                Debug = "=========================."
            }

            ERR (Arg0, Z001, 0x04AD, Zero, Zero, Zero, Zero)
        }

        B002 = B010 /* \B010 */
        If ((F64 == Zero))
        {
            F321 = Zero
        }

        If ((B001 != B002))
        {
            If (PR00)
            {
                Debug = "=========================:"
                Debug = B001 /* \M218.B001 */
                Debug = B002 /* \M218.B002 */
                Debug = "=========================."
            }

            ERR (Arg0, Z001, 0x04C2, Zero, Zero, Zero, Zero)
        }

        If (PR00)
        {
            Debug = "======== 9:"
            Debug = B001 /* \M218.B001 */
        }

        Local0 = SizeOf (B001)
        If ((Local0 != Arg2))
        {
            ERR (Arg0, Z001, 0x04CE, Zero, Zero, Local0, Arg2)
        }

        Local0 = SizeOf (B002)
        If ((Local0 != Arg2))
        {
            ERR (Arg0, Z001, 0x04D4, Zero, Zero, Local0, Arg2)
        }

        B001 = B000 /* \B000 */
        If (PR00)
        {
            Debug = "======== 10:"
            Debug = F001 /* \M218.F001 */
            Debug = B001 /* \M218.B001 */
            Debug = BB02 /* \M218.BB02 */
        }

        F001 = 0xEFCDAB1032547698
        Local0 = F001 /* \M218.F001 */
        If (PR00)
        {
            Debug = "======== 11:"
            Debug = Local0
            Debug = F001 /* \M218.F001 */
            Debug = B001 /* \M218.B001 */
            Debug = BB02 /* \M218.BB02 */
        }

        If ((F64 == One))
        {
            If ((Local0 != 0xEFCDAB1032547698))
            {
                ERR (Arg0, Z001, 0x04F3, Zero, Zero, Local0, 0xEFCDAB1032547698)
            }
        }
        ElseIf ((Local0 != BB02))
        {
            ERR (Arg0, Z001, 0x04F8, Zero, Zero, Zero, Zero)
        }

        B002 = B0B0 /* \B0B0 */
        If ((F64 == Zero))
        {
            F321 = Zero
        }

        If ((B001 != B002))
        {
            ERR (Arg0, Z001, 0x0505, Zero, Zero, Zero, Zero)
        }

        If (PR00)
        {
            Debug = "======== 12:"
            Debug = B001 /* \M218.B001 */
            Debug = B002 /* \M218.B002 */
        }

        Local0 = SizeOf (B001)
        If ((Local0 != Arg2))
        {
            ERR (Arg0, Z001, 0x0514, Zero, Zero, Local0, Arg2)
        }

        Local0 = SizeOf (B002)
        If ((Local0 != Arg2))
        {
            ERR (Arg0, Z001, 0x051A, Zero, Zero, Local0, Arg2)
        }

        Local0 = ObjectType (F001)
        If ((Local0 != C016))
        {
            ERR (Arg0, Z001, 0x0520, Zero, Zero, Local0, C016)
        }

        If (PR00)
        {
            Debug = "======== 13:"
            Debug = Local0
        }

        If ((ERRS != ERR0))
        {
            Local0 = One
        }
        Else
        {
            Local0 = Zero
        }

        Return (Local0)
    }

    Method (M219, 0, Serialized)
    {
        Debug = "TEST: m219, Create 64-Bit Buffer Field:"
        Name (BSZ0, Zero)
        BSZ0 = BS00 /* \BS00 */
        Name (BSZ1, Zero)
        Name (N000, Zero)
        Name (NCUR, Zero)
        N000 = (BSZ0 - 0x07)
        BSZ1 = N000 /* \M219.N000 */
        While (N000)
        {
            If ((NCUR >= BSZ1))
            {
                ERR ("M219", Z001, 0x0549, Zero, Zero, NCUR, BSZ1)
                Break
            }

            B010 = B000 /* \B000 */
            B0B0 = B000 /* \B000 */
            B101 = B0FF /* \B0FF */
            Local0 = NCUR /* \M219.NCUR */
            B010 [Local0] = 0xFF
            B0B0 [Local0] = 0x98
            B101 [Local0] = Zero
            Local0++
            B010 [Local0] = 0xFF
            B0B0 [Local0] = 0x76
            B101 [Local0] = Zero
            Local0++
            B010 [Local0] = 0xFF
            B0B0 [Local0] = 0x54
            B101 [Local0] = Zero
            Local0++
            B010 [Local0] = 0xFF
            B0B0 [Local0] = 0x32
            B101 [Local0] = Zero
            Local0++
            B010 [Local0] = 0xFF
            B0B0 [Local0] = 0x10
            B101 [Local0] = Zero
            Local0++
            B010 [Local0] = 0xFF
            B0B0 [Local0] = 0xAB
            B101 [Local0] = Zero
            Local0++
            B010 [Local0] = 0xFF
            B0B0 [Local0] = 0xCD
            B101 [Local0] = Zero
            Local0++
            B010 [Local0] = 0xFF
            B0B0 [Local0] = 0xEF
            B101 [Local0] = Zero
            If (M218 ("M219", NCUR, BSZ0))
            {
                Return (One)
            }

            N000--
            NCUR++
        }

        Return (Zero)
    }

    Method (M21A, 6, Serialized)
    {
        Name (PR00, Zero)
        If (PR00)
        {
            Debug = "========:"
            Debug = Arg2
            Debug = Arg3
        }

        Name (B001, Buffer (Arg4){})
        Name (B002, Buffer (Arg4){})
        Name (INT1, Zero)
        CreateField (B001, Arg2, Arg3, F001)
        Local0 = ObjectType (F001)
        If ((Local0 != C016))
        {
            ERR (Arg0, Z001, 0x059A, Zero, Zero, Local0, C016)
        }

        B001 = B0G0 /* \B0G0 */
        If (PR00)
        {
            Debug = B001 /* \M21A.B001 */
        }

        F001 = B0F0 /* \B0F0 */
        Local0 = F001 /* \M21A.F001 */
        If (PR00)
        {
            Debug = "============ 0:"
            Debug = B0G0 /* \B0G0 */
            Debug = B0F0 /* \B0F0 */
            Debug = B0B0 /* \B0B0 */
            Debug = Local0
            Debug = B001 /* \M21A.B001 */
            Debug = "============."
        }

        If ((Local0 != Arg5))
        {
            ERR (Arg0, Z001, 0x05BB, Zero, Zero, Zero, Zero)
        }

        B002 = B0B0 /* \B0B0 */
        If ((B001 != B002))
        {
            ERR (Arg0, Z001, 0x05C3, Zero, Zero, Zero, Zero)
            If (PR00)
            {
                Debug = "EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE:"
                Debug = B0G0 /* \B0G0 */
                Debug = B0F0 /* \B0F0 */
                Debug = B0B0 /* \B0B0 */
                Debug = B001 /* \M21A.B001 */
                Debug = "RRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR."
            }
        }

        Return (Zero)
    }

    Method (M21B, 6, Serialized)
    {
        Name (PR00, Zero)
        Name (INT2, Zero)
        Name (LPN1, Zero)
        Name (LPC1, Zero)
        Name (LPN2, Zero)
        Name (LPC2, Zero)
        Name (BSZ0, Zero)
        BSZ0 = BS00 /* \BS00 */
        Name (BSF0, Zero)
        Name (BSB0, Zero)
        Name (FB00, Zero)
        Name (LB00, Zero)
        If ((Arg3 == Zero))
        {
            ERR (Arg0, Z001, 0x05FA, Zero, Zero, Zero, Zero)
            Return (Ones)
        }

        BSB0 = MBS0 (Arg2, Arg3)
        Switch (ToInteger (Arg4))
        {
            Case (Zero)
            {
                B0G0 = B000 /* \B000 */
            }
            Case (One)
            {
                B0G0 = B0FF /* \B0FF */
            }
            Default
            {
                B0G0 = B256 /* \B256 */
            }

        }

        Switch (ToInteger (Arg5))
        {
            Case (Zero)
            {
                B0F0 = B000 /* \B000 */
            }
            Case (One)
            {
                B0F0 = B0FF /* \B0FF */
            }
            Default
            {
                B0F0 = B256 /* \B256 */
            }

        }

        Local0 = Arg3
        Local0--
        Divide (Local0, 0x08, Local7, LPN1) /* \M21B.LPN1 */
        Divide (Arg3, 0x08, Local7, Local0)
        BSF0 = LPN1 /* \M21B.LPN1 */
        BSF0++
        Local6 = Buffer (BSF0){}
        Local0 = DerefOf (B0F0 [LPN1])
        If (Local7)
        {
            Local1 = (0x08 - Local7)
            Local2 = (Local0 << Local1)
            Local3 = (Local2 & 0xFF)
            Local0 = (Local3 >> Local1)
        }

        Local6 [LPN1] = Local0
        LPC1 = Zero
        While (LPN1)
        {
            Local0 = DerefOf (B0F0 [LPC1])
            Local6 [LPC1] = Local0
            LPN1--
            LPC1++
        }

        B0B0 = B0G0 /* \B0G0 */
        Divide (Arg2, 0x08, Local1, FB00) /* \M21B.FB00 */
        Local2 = DerefOf (B0B0 [FB00])
        LB00 = BSB0 /* \M21B.BSB0 */
        LB00--
        Local3 = DerefOf (B0B0 [LB00])
        Local0 = SFT1 (Local6, Local1, Arg3, Local2, Local3)
        Local2 = FB00 /* \M21B.FB00 */
        LPN2 = SizeOf (Local0)
        LPC2 = Zero
        While (LPN2)
        {
            Local1 = DerefOf (Local0 [LPC2])
            B0B0 [Local2] = Local1
            Local2++
            LPN2--
            LPC2++
        }

        M21A (Arg0, Arg1, Arg2, Arg3, BSZ0, Local6)
        Return (Zero)
    }

    Method (M21C, 4, Serialized)
    {
        Name (LPN0, Zero)
        Name (LPC0, Zero)
        Name (LPN1, Zero)
        Name (LPC1, Zero)
        Name (LPN2, Zero)
        Name (LPC2, Zero)
        Name (IB00, Zero)
        Name (NB00, Zero)
        LPN0 = Arg1
        LPC0 = Zero
        While (LPN0)
        {
            Local6 = (LPC0 * 0x06)
            IB00 = DerefOf (Arg3 [Local6])
            Local6++
            LPN1 = DerefOf (Arg3 [Local6])
            Local6++
            Local0 = DerefOf (Arg3 [Local6])
            Local6++
            Local1 = DerefOf (Arg3 [Local6])
            Local6++
            Local2 = DerefOf (Arg3 [Local6])
            Local6++
            Local3 = DerefOf (Arg3 [Local6])
            LPC1 = Zero
            While (LPN1)
            {
                NB00 = Local0
                LPN2 = Local1
                LPC2 = Zero
                While (LPN2)
                {
                    M21B (Arg0, Arg2, IB00, NB00, Local2, Local3)
                    NB00++
                    LPN2--
                    LPC2++
                }

                IB00++
                LPN1--
                LPC1++
            }

            LPC0++
            LPN0--
        }
    }

    Method (M21D, 0, Serialized)
    {
        Debug = "TEST: m21d, Create Arbitrary Length Buffer Field:"
        Name (P000, Package (0x06)
        {
            Zero, 
            0x08, 
            One, 
            0x50, 
            One, 
            0x02
        })
        M21C ("M21D", One, "p000", P000)
    }

    Method (CBF0, 0, NotSerialized)
    {
        SRMT ("m211")
        M211 ()
        SRMT ("m213")
        M213 ()
        SRMT ("m215")
        M215 ()
        SRMT ("m217")
        M217 ()
        SRMT ("m219")
        M219 ()
        SRMT ("m21d")
        M21D ()
    }

    Method (MAIN, 0, NotSerialized)
    {
        STRT (Zero)
        If (STTT ("Buffer Fields", TCLF, One, W001))
        {
            CBF0 ()
        }

        FTTT ()
        Local7 = FNSH ()
        Return (Local7)
    }
}

