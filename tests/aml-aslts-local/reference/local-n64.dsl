/*
 * Intel ACPI Component Architecture
 * AML/ASL+ Disassembler version 20260408 (64-bit version)
 * Copyright (c) 2000 - 2026 Intel Corporation
 * 
 * Disassembling to symbolic ASL+ operators
 *
 * Disassembly of /home/doc/git/.acpi-proof-work/aslts-local-upstream-n64-source-p2o_i5b2/results/local-n64.aml
 *
 * Original Table Header:
 *     Signature        "DSDT"
 *     Length           0x000049F2 (18930)
 *     Revision         0x02
 *     Checksum         0x29
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
    Name (TRCF, 0x00)
    Name (TRCH, "ASLTS")
    Name (STST, "STST")
    Name (CTST, "CTST")
    Name (PR01, 0x01)
    Name (PR02, 0x01)
    Name (TMT0, 0x00)
    Name (MTHR, 0x00)
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
    Name (C008, 0x00)
    Name (C009, 0x01)
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
    Name (C028, 0x00)
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
    Name (C080, 0x00)
    Name (C081, 0x00)
    Name (C082, 0x00)
    Name (C083, 0x00)
    Name (C084, 0x00)
    Name (C085, 0x00)
    Name (C086, 0x00)
    Name (C087, 0x00)
    Name (C088, 0x00)
    Name (C089, 0x00)
    Name (C08A, 0x00)
    Name (C08B, 0x00)
    Name (C08C, 0x00788B60)
    Name (EXCV, 0x00)
    Name (ZFFF, 0x07FF)
    Name (F64, 0x00)
    Name (ISZ0, 0x00)
    Name (ISZC, 0x00)
    Name (ETR0, 0x04B0)
    Name (ETR1, 0x0190)
    Name (ERRP, Package (ETR0, ){})
    Name (RP0P, Package (ETR0, ){})
    Name (RMRC, 0x00)
    Name (ERRS, 0x00)
    Name (ERRB, 0x00)
    Name (ERR0, 0x00)
    Name (ERR1, 0x00)
    Name (ERR2, 0x00)
    Name (ERR3, 0x00)
    Name (ERR4, 0x00)
    Name (ERR5, 0x00)
    Name (ERR6, 0x00)
    Name (ERR7, 0x00)
    Name (FNAM, 0x00)
    Method (SET0, 3, NotSerialized)
    {
        If (ERR0)
        {
            ERR ("SET0", Z062, 0x010A, 0x00, 0x00, ERR0, 0x00)
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
        CopyObject (0x00, ERR0) /* \ERR0 */
        CopyObject (0x00, ERR1) /* \ERR1 */
        CopyObject (0x00, ERR2) /* \ERR2 */
        CopyObject (0x00, FNAM) /* \FNAM */
    }

    Method (RST2, 0, NotSerialized)
    {
        ERR3 = 0x00
    }

    Method (GET2, 0, NotSerialized)
    {
        Return (ERR3) /* \ERR3 */
    }

    Name (TCLA, 0x00)
    Name (TCLF, 0x01)
    Name (TCLC, 0x02)
    Name (TCLE, 0x03)
    Name (TCLD, 0x04)
    Name (TCLS, 0x05)
    Name (TCLM, 0x06)
    Name (TCLT, 0x07)
    Name (TCLI, 0x08)
    Name (MAXC, 0x08)
    Name (TCLL, 0x00)
    Name (TIND, 0x12345678)
    Name (TSNM, "NAME_OF_TEST")
    Name (NRMT, "")
    Name (FLG5, 0x00)
    Name (FLG6, 0x00)
    Name (ABUU, 0x00)
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

        ERR5 = 0x00
        FLG5 = 0x00
        FLG6 = 0x00
    }

    Method (SRMT, 1, NotSerialized)
    {
        RPT0 ()
        ERR5 = ERRS /* \ERRS */
        If (0x01)
        {
            Concatenate (Arg0, " test started", Debug)
        }

        CopyObject (Arg0, NRMT) /* \NRMT */
    }

    Method (SKIP, 0, NotSerialized)
    {
        FLG5 = 0x01
    }

    Method (BLCK, 0, NotSerialized)
    {
        FLG6 = 0x01
    }

    Method (BEG0, 2, NotSerialized)
    {
        SET0 (Arg0, Arg1, 0x00)
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
        FLG5 = 0x00
        FLG6 = 0x00
        ERR5 = 0x00
        ERRB = PK00 (Arg1, Arg2)
        Concatenate ("TEST (", TCN0 (TCLL), Local1)
        Concatenate (Local1, "), ", Local0)
        Concatenate (Local0, TSNM, Local1)
        If (RTPT)
        {
            Local7 = 0x00
            If ((RUN0 == 0x00))
            {
                Local7 = 0x01
            }
            ElseIf ((RUN0 == 0x01))
            {
                If (Arg3)
                {
                    Local7 = 0x01
                }
            }
            ElseIf ((RUN0 == 0x02))
            {
                If ((Arg3 == 0x00))
                {
                    Local7 = 0x01
                }
            }
            ElseIf ((RUN0 == 0x03))
            {
                If ((Arg3 == RUN1))
                {
                    Local7 = 0x01
                }
            }
            ElseIf ((RUN0 == 0x04))
            {
                If ((Arg1 == RUN2))
                {
                    If ((Arg2 == RUN3))
                    {
                        Local7 = 0x01
                    }
                }
            }
        }
        Else
        {
            Local7 = 0x01
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
        CH03 ("FTTT", 0x00, 0x0249, 0x00, 0x00)
        RPT0 ()
        TSNM = "NAME_OF_TEST"
        TCLL = 0x00
        TIND = 0x12345678
        NRMT = ""
        FLG5 = 0x00
        FLG6 = 0x00
        ERR5 = 0x00
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
        Local3 = 0x00
        Local6 = 0x00
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
            Local4 = 0x00
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
        If ((ObjectType (Arg5) == 0x01))
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
        ERR3 = 0x01
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
        If ((FNAM != 0x00))
        {
            Local1 = FNAM /* \FNAM */
        }
        ElseIf ((Arg0 == ZFFF))
        {
            Local1 = SB00 (TIND, 0x00)
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
                Local1 = SB00 (TIND, 0x00)
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
        If ((Arg1 == 0x00))
        {
            ToDecimalString (Arg0, Local0)
            Concatenate ("*", Local0, Local1)
            Concatenate (Local1, ".asl", Local7)
        }
        ElseIf ((Arg1 == 0x01))
        {
            ToDecimalString (Arg0, Local0)
            Concatenate ("Demo of bug ", Local0, Local7)
        }

        Return (Local7)
    }

    Method (PRN0, 1, Serialized)
    {
        Name (LPN0, 0x00)
        Name (LPC0, 0x00)
        LPN0 = SizeOf (Arg0)
        LPC0 = 0x00
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
            ERR (Arg0, Z062, 0x03AD, 0x00, 0x00, Arg1, Arg2)
        }
    }

    Method (CH01, 4, NotSerialized)
    {
        If ((Arg3 != Ones))
        {
            ERR (Arg0, Z062, 0x03BC, 0x00, 0x00, Arg1, Arg2)
        }
    }

    Method (RNG0, 3, NotSerialized)
    {
        If ((Arg1 > Arg2))
        {
            Debug = "RNG0: RangeMin greater than RangeMax"
            Fatal (0x00, 0x00000000, 0x00)
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
            ERR ("c000 corrupted", Z062, 0x03E7, 0x00, 0x00, 0x00, 0x00)
        }

        If ((C001 != 0x05))
        {
            ERR ("c001 corrupted", Z062, 0x03EC, 0x00, 0x00, 0x00, 0x00)
        }

        If ((C002 != 0x0D))
        {
            ERR ("c002 corrupted", Z062, 0x03F1, 0x00, 0x00, 0x00, 0x00)
        }

        If ((C003 != 0x0C))
        {
            ERR ("c003 corrupted", Z062, 0x03F6, 0x00, 0x00, 0x00, 0x00)
        }

        If ((C004 != 0x06))
        {
            ERR ("c004 corrupted", Z062, 0x03FB, 0x00, 0x00, 0x00, 0x00)
        }

        If ((C005 != 0x04))
        {
            ERR ("c005 corrupted", Z062, 0x0400, 0x00, 0x00, 0x00, 0x00)
        }

        If ((C006 != 0x1F))
        {
            ERR ("c006 corrupted", Z062, 0x0405, 0x00, 0x00, 0x00, 0x00)
        }

        If ((C007 != 0x33))
        {
            ERR ("c007 corrupted", Z062, 0x040A, 0x00, 0x00, 0x00, 0x00)
        }

        If ((C008 != 0x00))
        {
            ERR ("c008 corrupted", Z062, 0x040F, 0x00, 0x00, 0x00, 0x00)
        }

        If ((C009 != 0x01))
        {
            ERR ("c009 corrupted", Z062, 0x0414, 0x00, 0x00, 0x00, 0x00)
        }

        If ((C00A != 0x02))
        {
            ERR ("c00a corrupted", Z062, 0x0419, 0x00, 0x00, 0x00, 0x00)
        }

        If ((C00B != 0x03))
        {
            ERR ("c00b corrupted", Z062, 0x041E, 0x00, 0x00, 0x00, 0x00)
        }

        If ((C00C != 0x04))
        {
            ERR ("c00c corrupted", Z062, 0x0423, 0x00, 0x00, 0x00, 0x00)
        }

        If ((C00D != 0x05))
        {
            ERR ("c00d corrupted", Z062, 0x0428, 0x00, 0x00, 0x00, 0x00)
        }

        If ((C00E != 0x06))
        {
            ERR ("c00e corrupted", Z062, 0x042D, 0x00, 0x00, 0x00, 0x00)
        }

        If ((C00F != 0x07))
        {
            ERR ("c00f corrupted", Z062, 0x0432, 0x00, 0x00, 0x00, 0x00)
        }

        If ((C010 != 0x08))
        {
            ERR ("c010 corrupted", Z062, 0x0437, 0x00, 0x00, 0x00, 0x00)
        }

        If ((C011 != 0x09))
        {
            ERR ("c011 corrupted", Z062, 0x043C, 0x00, 0x00, 0x00, 0x00)
        }

        If ((C012 != 0x0A))
        {
            ERR ("c012 corrupted", Z062, 0x0441, 0x00, 0x00, 0x00, 0x00)
        }

        If ((C013 != 0x0B))
        {
            ERR ("c013 corrupted", Z062, 0x0446, 0x00, 0x00, 0x00, 0x00)
        }

        If ((C014 != 0x0C))
        {
            ERR ("c014 corrupted", Z062, 0x044B, 0x00, 0x00, 0x00, 0x00)
        }

        If ((C015 != 0x0D))
        {
            ERR ("c015 corrupted", Z062, 0x0450, 0x00, 0x00, 0x00, 0x00)
        }

        If ((C016 != 0x0E))
        {
            ERR ("c016 corrupted", Z062, 0x0455, 0x00, 0x00, 0x00, 0x00)
        }

        If ((C017 != 0x0F))
        {
            ERR ("c017 corrupted", Z062, 0x045A, 0x00, 0x00, 0x00, 0x00)
        }

        If ((C018 != 0x10))
        {
            ERR ("c018 corrupted", Z062, 0x045F, 0x00, 0x00, 0x00, 0x00)
        }

        If ((C019 != 0x11))
        {
            ERR ("c019 corrupted", Z062, 0x0464, 0x00, 0x00, 0x00, 0x00)
        }
    }

    Method (SFT0, 4, Serialized)
    {
        Name (N000, 0x00)
        Name (NCUR, 0x00)
        N000 = Arg1
        NCUR = 0x00
        Local6 = 0x00
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
        Name (PREV, 0x00)
        Name (MS00, 0x00)
        Name (MS01, 0x00)
        Name (MS02, 0x00)
        Name (MS03, 0x00)
        Name (TAIL, 0x00)
        Name (LBT0, 0x00)
        Name (LPN0, 0x00)
        Name (LPC0, 0x00)
        Name (NB01, 0x00)
        Name (NREG, 0x00)
        Name (NB08, 0x00)
        Name (NB09, 0x00)
        Name (REST, 0x00)
        If ((Arg2 < 0x01))
        {
            ERR ("sft", Z062, 0x04CD, 0x00, 0x00, Arg2, 0x01)
            Return (Ones)
        }

        If ((Arg1 > 0x07))
        {
            ERR ("sft", Z062, 0x04D3, 0x00, 0x00, Arg1, 0x07)
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
        If ((REST == 0x00))
        {
            REST = 0x08
        }

        PREV = (Arg3 & MS00) /* \SFT1.MS00 */
        If ((Arg2 >= NB09))
        {
            NREG = 0x01
            Local7 = (Arg2 - NB09) /* \SFT1.NB09 */
            Divide (Local7, 0x08, Local1, Local0)
            NREG += Local0
        }

        LPN0 = NREG /* \SFT1.NREG */
        LPC0 = 0x00
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
            TAIL = 0x00
        }
        ElseIf ((REST <= NB08))
        {
            TAIL = 0x01
        }
        Else
        {
            TAIL = 0x02
            LBT0 = DerefOf (Arg0 [LPC0])
        }

        If ((TAIL == 0x01))
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
        Name (TMP0, 0x00)
        Name (TMP1, 0x00)
        Local7 = 0x00
        TMP0 = ObjectType (Arg1)
        If (F64)
        {
            TMP1 = ObjectType (Arg2)
            If ((TMP0 != TMP1))
            {
                ERR (Arg0, Z062, 0x055B, 0x00, 0x00, TMP0, TMP1)
                Local7 = 0x01
            }
            ElseIf ((Arg1 != Arg2))
            {
                ERR (Arg0, Z062, 0x0560, 0x00, 0x00, Arg1, Arg2)
                Local7 = 0x01
            }
        }
        Else
        {
            TMP1 = ObjectType (Arg3)
            If ((TMP0 != TMP1))
            {
                ERR (Arg0, Z062, 0x0569, 0x00, 0x00, TMP0, TMP1)
                Local7 = 0x01
            }
            ElseIf ((Arg1 != Arg3))
            {
                ERR (Arg0, Z062, 0x056E, 0x00, 0x00, Arg1, Arg3)
                Local7 = 0x01
            }
        }

        Return (Local7)
    }

    Method (M4A1, 2, Serialized)
    {
        Name (S000, " ")
        Local0 = DerefOf (Arg0 [Arg1])
        S000 [0x00] = Local0
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
            F64 = 0x00
            ISZ0 = 0x04
            ISZC = 0x08
            Debug = "32-bit mode"
        }
        Else
        {
            F64 = 0x01
            ISZ0 = 0x08
            ISZC = 0x10
            Debug = "64-bit mode"
        }

        If (CH02 ())
        {
            ERR7++
            CH03 ("STRT", 0x00, 0x05BD, 0x00, 0x00)
            EXC0 = 0x00
            EXC1 = 0x00
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
            Case (0x01)
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
                Local7 = SB00 (Arg1, 0x01)
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
            Case (0x01)
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
                Local6 = SB00 (Local1, 0x01)
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
            Local6 = SB00 (Local2, 0x00)
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
                Local6 = SB00 (Local2, 0x00)
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
        Name (LPN0, 0x00)
        Name (LPC0, 0x00)
        LPN0 = ETR1 /* \ETR1 */
        If ((ERRS < LPN0))
        {
            LPN0 = ERRS /* \ERRS */
        }

        Local0 = 0x00
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
        Name (LPN0, 0x00)
        Name (LPC0, 0x00)
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
        CH03 ("FNSH", 0x00, 0x0834, 0x00, 0x00)
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
            Concatenate ("TEST ACPICA: ", "64-bit :", Local0)
        }
        Else
        {
            Concatenate ("TEST ACPICA: ", "32-bit :", Local0)
        }

        If (ERR7)
        {
            Concatenate ("!!!! ERRORS were detected during the loading stage, # 0x", ERR7, Debug)
        }

        EXC1 = 0x00
        If ((ERRS || ERR7))
        {
            Concatenate (Local0, " FAIL : Errors # 0x", Local1)
            Concatenate (Local1, ERRS, Local2)
            Concatenate (Local2, ", Failed tests # 0x", Local1)
            Debug = Concatenate (Local1, ERR6)
            Return (0x01)
        }

        Debug = Concatenate (Local0, " PASS")
        Return (0x00)
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
        TRCF = 0x01
    }

    Method (TRC9, 0, NotSerialized)
    {
        TRCF = 0x00
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

    Name (SLCK, 0x00)
    Name (MLVL, 0x00)
    Method (MN00, 0, NotSerialized)
    {
        SLCK = 0x00
        MLVL = 0x01
        Local7 = MAIN ()
        Return (Local7)
    }

    Method (MN01, 0, NotSerialized)
    {
        SLCK = 0x01
        MLVL = 0x01
        Local7 = MAIN ()
        Return (Local7)
    }

    Name (RUN0, 0x00)
    Name (RUN1, 0x00)
    Name (RUN2, 0x00)
    Name (RUN3, 0x00)
    Name (RTPT, 0x00)
    Name (W000, 0x00)
    Name (W001, 0x00)
    Name (W002, 0x00)
    Name (W003, 0x00)
    Name (W004, 0x00)
    Name (W005, 0x00)
    Name (W006, 0x00)
    Name (W007, 0x00)
    Name (W008, 0x00)
    Name (W009, 0x00)
    Name (W00A, 0x00)
    Name (W00B, 0x00)
    Name (W00C, 0x00)
    Name (W00D, 0x00)
    Name (W01A, 0x00)
    Name (W00E, 0x00)
    Name (W00F, 0x00)
    Name (W010, 0x00)
    Name (W011, 0x00)
    Name (W012, 0x00)
    Name (W022, 0x00)
    Name (W013, 0x00)
    Name (W014, 0x00)
    Name (W015, 0x00)
    Name (W016, 0x00)
    Name (W017, 0x00)
    Name (W021, 0x00)
    Name (W018, 0x00)
    Name (W019, 0x00)
    Name (W020, 0x00)
    Method (SRTP, 1, NotSerialized)
    {
        RTPT = Arg0
    }

    Method (RTPI, 0, NotSerialized)
    {
        RUN0 = 0x00
        RUN1 = 0x00
        RUN2 = 0x01
        RUN3 = 0x03
        W000 = 0x01
        W001 = 0x01
        W002 = 0x01
        W003 = 0x01
        W004 = 0x01
        W005 = 0x01
        W006 = 0x01
        W007 = 0x01
        W008 = 0x01
        W009 = 0x01
        W00A = 0x01
        W00B = 0x01
        W00C = 0x01
        W00D = 0x01
        W00E = 0x01
        W00F = 0x01
        W010 = 0x01
        W011 = 0x01
        W021 = 0x01
        W012 = 0x01
        W013 = 0x01
        W014 = 0x01
        W015 = 0x01
        W016 = 0x01
        W019 = 0x01
        W017 = 0x01
        W018 = 0x01
    }

    Name (Y078, 0x00)
    Name (Y083, 0x00)
    Name (Y084, 0x01)
    Name (Y098, 0x01)
    Name (Y100, 0x00)
    Name (Y103, 0x01)
    Name (Y104, 0x01)
    Name (Y105, 0x01)
    Name (Y106, 0x00)
    Name (Y111, 0x01)
    Name (Y113, 0x00)
    Name (Y114, 0x00)
    Name (Y118, 0x00)
    Name (Y119, 0x00)
    Name (Y120, 0x00)
    Name (Y121, 0x00)
    Name (Y126, 0x00)
    Name (Y127, 0x00)
    Name (Y128, 0x01)
    Name (Y132, 0x00)
    Name (Y133, 0x00)
    Name (Y134, 0x00)
    Name (Y135, 0x00)
    Name (Y136, 0x01)
    Name (Y157, 0x01)
    Name (Y164, 0x01)
    Name (Y176, 0x00)
    Name (Y178, 0x01)
    Name (Y182, 0x01)
    Name (Y192, 0x01)
    Name (Y200, 0x00)
    Name (Y203, 0x00)
    Name (Y204, 0x00)
    Name (Y205, 0x00)
    Name (Y206, 0x00)
    Name (Y207, 0x00)
    Name (Y208, 0x00)
    Name (Y213, 0x00)
    Name (Y214, 0x00)
    Name (Y215, 0x00)
    Name (Y216, 0x00)
    Name (Y217, 0x00)
    Name (Y220, 0x00)
    Name (Y221, 0x01)
    Name (Y222, 0x00)
    Name (Y223, 0x01)
    Name (Y224, 0x00)
    Name (Y238, 0x00)
    Name (Y242, 0x00)
    Name (Y243, 0x00)
    Name (Y248, 0x00)
    Name (Y251, 0x00)
    Name (Y260, 0x00)
    Name (Y261, 0x00)
    Name (Y262, 0x00)
    Name (Y263, 0x00)
    Name (Y264, 0x00)
    Name (Y275, 0x00)
    Name (Y276, 0x00)
    Name (Y281, 0x00)
    Name (Y282, 0x00)
    Name (Y283, 0x01)
    Name (Y284, 0x01)
    Name (Y286, 0x01)
    Name (Y287, 0x00)
    Name (Y288, 0x00)
    Name (Y289, 0x00)
    Name (Y290, 0x00)
    Name (Y292, 0x00)
    Name (Y293, 0x00)
    Name (Y294, 0x00)
    Name (Y296, 0x00)
    Name (Y297, 0x00)
    Name (Y300, 0x00)
    Name (Y301, 0x00)
    Name (Y302, 0x00)
    Name (Y349, 0x00)
    Name (Y350, 0x00)
    Name (Y361, 0x00)
    Name (Y362, 0x00)
    Name (Y364, 0x00)
    Name (Y365, 0x00)
    Name (Y366, 0x00)
    Name (Y367, 0x00)
    Name (Y500, 0x00)
    Name (Y501, 0x00)
    Name (Y502, 0x00)
    Name (Y503, 0x00)
    Name (Y504, 0x00)
    Name (Y505, 0x00)
    Name (Y506, 0x00)
    Name (Y507, 0x00)
    Name (Y508, 0x00)
    Name (Y509, 0x00)
    Name (Y510, 0x00)
    Name (Y511, 0x00)
    Name (Y512, 0x00)
    Name (Y513, 0x00)
    Name (Y514, 0x00)
    Name (Y516, 0x00)
    Name (Y517, 0x00)
    Name (Y518, 0x00)
    Name (Y519, 0x00)
    Name (Y520, 0x00)
    Name (Y521, 0x00)
    Name (Y522, 0x01)
    Name (Y523, 0x00)
    Name (Y524, 0x00)
    Name (Y525, 0x00)
    Name (Y526, 0x00)
    Name (Y527, 0x00)
    Name (Y600, 0x00)
    Name (Y601, 0x00)
    Name (Y602, 0x01)
    Name (Y603, 0x00)
    Name (Y900, 0x00)
    Name (Y901, 0x01)
    Name (Y902, 0x01)
    Name (Q001, 0x01)
    Name (Q002, 0x00)
    Name (Q003, 0x00)
    Name (Q004, 0x00)
    Name (Q005, 0x00)
    Name (Q006, 0x00)
    Name (Q007, 0x00)
    Name (Q008, 0x00)
    Name (Q009, 0x00)
    Name (Q00A, 0x00)
    Name (Q00B, 0x00)
    Name (RN00, 0x01)
    Name (RN01, 0x00)
    Name (RN02, 0x00)
    Name (RN03, 0x00)
    Name (RN04, 0x00)
    Name (RN05, 0x00)
    Name (RN06, 0x00)
    Name (X104, 0x01)
    Name (X114, 0x01)
    Name (X127, 0x01)
    Name (X128, 0x01)
    Name (X131, 0x01)
    Name (X132, 0x01)
    Name (X133, 0x01)
    Name (X153, 0x01)
    Name (X170, 0x01)
    Name (X191, 0x01)
    Name (X192, 0x01)
    Name (X193, 0x01)
    Name (X194, 0x01)
    Name (X195, 0x00)
    Name (FLG9, 0x00)
    Name (RUN4, 0x00)
    Name (REL0, 0x16)
    Name (SETN, 0x05)
    Method (SET2, 1, Serialized)
    {
        Local0 = Arg0
        Switch (ToInteger (Local0))
        {
            Case (0x00)
            {
                Y135 = 0x00
                Y900 = 0x01
                Y901 = 0x00
                FLG9 = 0x01
                Y263 = 0x00
                Y275 = 0x00
                Y276 = 0x00
            }
            Case (0x01)
            {
                Y135 = 0x01
                Y900 = 0x00
                Y901 = 0x00
                FLG9 = 0x01
                Y263 = 0x00
                Y275 = 0x00
                Y276 = 0x00
            }
            Case (0x02)
            {
                Y135 = 0x00
                Y900 = 0x00
                Y901 = 0x01
                FLG9 = 0x00
                Y263 = 0x00
                Y275 = 0x00
                Y276 = 0x00
            }
            Case (0x03)
            {
                Y135 = 0x00
                Y900 = 0x01
                Y901 = 0x00
                FLG9 = 0x01
                Y263 = 0x01
                Y275 = 0x00
                Y276 = 0x00
                Y262 = 0x00
            }
            Case (0x04)
            {
                Y135 = 0x00
                Y900 = 0x01
                Y901 = 0x00
                FLG9 = 0x01
                Y263 = 0x01
                Y275 = 0x01
                Y276 = 0x01
                Y262 = 0x00
                Y251 = 0x00
                Y300 = 0x00
            }
            Case (0x05)
            {
                Y135 = 0x00
                Y900 = 0x01
                Y901 = 0x01
                FLG9 = 0x01
                Y263 = 0x01
                Y275 = 0x01
                Y276 = 0x01
                Y262 = 0x01
                Y251 = 0x00
                Y300 = 0x00
            }
            Case (0x06)
            {
                Y135 = 0x00
                Y900 = 0x01
                Y901 = 0x00
                FLG9 = 0x01
                Y263 = 0x01
                Y275 = 0x01
                Y276 = 0x01
                Y262 = 0x01
                Y251 = 0x01
                Y300 = 0x01
                Y902 = 0x00
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
    Name (EXC0, 0x00)
    Name (EXC1, 0x00)
    Name (EX00, 0x00)
    Name (EX01, "")
    Name (EX04, 0x00)
    Name (EX05, "")
    Name (EX0D, 0xFD)
    Name (EX0E, 0xFE)
    Name (EX0F, 0xFF)
    Name (PF00, Package (0x57)
    {
        Package (0x03)
        {
            0x00, 
            0x00, 
            "AE_OK"
        }, 

        Package (0x03)
        {
            0x01, 
            0x01, 
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
        If ((EX04 == 0x00))
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
                ERR ("_ERR", Z063, 0x02DF, 0x00, 0x00, EXC0, EXC2)
            }
        }

        EXC0++
        EXC1++
        Return (0x00)
    }

    Method (CH02, 0, NotSerialized)
    {
        If (EXC1)
        {
            Concatenate ("Some unexpected exceptions were handled, 0x", EXC1, Local0)
            ERR ("CH02", Z063, 0x02F3, 0x00, 0x00, Local0, 0x00)
        }

        Return (EXC1) /* \EXC1 */
    }

    Method (CH03, 5, NotSerialized)
    {
        Local7 = 0x00
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

        EXC0 = 0x00
        EX04 = 0x00
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
        Local5 = 0x00
        If ((Arg2 == 0xFF))
        {
            If ((EXC0 == 0x00))
            {
                Local5 = 0x01
                Debug = "ERROR: No ANY exception has arisen."
            }
        }
        Else
        {
            Local2 = DerefOf (PF00 [Arg2])
            Local3 = DerefOf (Local2 [0x01])
            Local4 = DerefOf (Local2 [0x02])
            If ((EXC0 == 0x00))
            {
                Local5 = 0x01
                Concatenate ("No exception - expected: ", Local4, Local0)
                Concatenate (Local0, "-", Local0)
                Concatenate (Local0, ST16 (Local3), Local0)
                Debug = Local0
            }
            ElseIf ((!Arg1 && (EXC0 > 0x01)))
            {
                Local5 = 0x01
                Concatenate ("More than one exception: 0x", EXC0, Local0)
                Debug = Local0
            }
            Else
            {
                If ((Arg1 == 0x01))
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
                    Local5 = 0x01
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
                    Local5 = 0x01
                    Debug = "Unexpected exception:"
                    Debug = Concatenate ("Expected: ", Local4)
                    Debug = Concatenate ("Received: ", Local7)
                }
            }
        }

        EXC0 = 0x00
        EX04 = 0x00
        If (Local5)
        {
            ERR (Arg0, Z063, 0x038A, Arg3, Arg4, Arg5, Arg6)
        }

        Return (Local5)
    }

    Method (CH05, 0, NotSerialized)
    {
        Return (CH03 ("CH05", 0x00, 0x0392, 0x00, 0x00))
    }

    Method (CH06, 3, NotSerialized)
    {
        If (EXCV)
        {
            Return (CH04 (Arg0, 0x00, Arg2, 0x00, 0x0399, 0x00, 0x00))
        }
        Else
        {
            Return (CH04 (Arg0, 0x00, 0xFF, 0x00, 0x039F, 0x00, 0x00))
        }
    }

    Method (CH07, 7, NotSerialized)
    {
        If (SLCK)
        {
            CH03 (Arg0, Arg3, 0x03AA, 0x00, Arg6)
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
                0x00, 
                0x00
            }
        Local3 = 0x00
        Local4 = EXC0 /* \EXC0 */
        Local5 = 0x00
        While (Local4)
        {
            Local0 = DerefOf (EX02 [Local5])
            If ((Local0 == Arg0))
            {
                Local1 = DerefOf (EX03 [Local5])
                If ((Local3 == 0x00))
                {
                    Local2 [0x00] = Local1
                }

                Local3++
                EX02 [Local5] = 0x00
            }

            Local4--
            Local5++
        }

        Local2 [0x01] = Local3
        Return (Local2)
    }

    Method (CH08, 6, NotSerialized)
    {
        Local2 = MTEX (Arg1)
        Local3 = DerefOf (Local2 [0x00])
        Local4 = DerefOf (Local2 [0x01])
        Local7 = 0x00
        If (Local4)
        {
            Concatenate ("Unexpected exception 0x", Local3, Local0)
            Concatenate (Local0, ", number of exceptions 0x", Local1)
            Concatenate (Local1, Local4, Local0)
            MSG0 (Arg1, Local0)
            ERR (Arg0, Z063, 0x040D, Arg2, Arg3, Arg4, Arg5)
            Local7 = 0x01
        }

        Return (Local4)
    }

    Method (CH09, 6, NotSerialized)
    {
        Local7 = MTEX (Arg1)
        Local6 = DerefOf (Local7 [0x00])
        Local7 = DerefOf (Local7 [0x01])
        Local5 = 0x00
        If ((Arg2 == 0xFF))
        {
            If ((Local7 == 0x00))
            {
                Local5 = 0x01
                MSG0 (Arg1, "ERROR: No ANY exception has arisen.")
            }
        }
        Else
        {
            Local2 = DerefOf (PF00 [Arg2])
            Local3 = DerefOf (Local2 [0x01])
            Local4 = DerefOf (Local2 [0x02])
            If ((Local7 == 0x00))
            {
                Local5 = 0x01
                Concatenate ("No exception has arisen, expected: ", Local4, Local0)
                Concatenate (", opcode 0x", Local3, Local1)
                Concatenate (Local0, Local1, Local0)
                MSG0 (Arg1, Local0)
            }
            ElseIf ((Arg0 && (Local7 > 0x01)))
            {
                Local5 = 0x01
                Concatenate ("More than one exception has arisen: 0x", Local7, Local0)
                MSG0 (Arg1, Local0)
            }
            ElseIf ((Local3 != Local6))
            {
                Local5 = 0x01
                Concatenate ("The exception 0x", Local6, Local0)
                Concatenate (Local0, " differs from expected ", Local1)
                Concatenate (Local1, ST16 (Local3), Local0)
                MSG0 (Arg1, Local0)
            }
        }

        If (Local5)
        {
            ERR ("CH09", Z063, 0x045E, Arg3, Arg4, 0x00, 0x00)
        }

        Arg5 = Local7
        Return (Local5)
    }

    Method (CH0A, 0, NotSerialized)
    {
        EXC0 = 0x00
    }

    Name (Z999, 0x01)
    Method (MA00, 7, NotSerialized)
    {
        Method (M000, 6, NotSerialized)
        {
            Method (M000, 5, NotSerialized)
            {
                Method (M000, 4, NotSerialized)
                {
                    Method (M000, 3, NotSerialized)
                    {
                        Method (M000, 2, NotSerialized)
                        {
                            Method (M000, 1, NotSerialized)
                            {
                                Method (M000, 0, NotSerialized)
                                {
                                    Method (M000, 0, NotSerialized)
                                    {
                                    }

                                    M000 ()
                                }

                                M000 ()
                            }

                            M000 (0x1C)
                        }

                        M000 (0x1A, 0x1B)
                    }

                    M000 (0x17, 0x18, 0x19)
                }

                M000 (0x13, 0x14, 0x15, 0x16)
            }

            If ((Arg0 != 0x08))
            {
                ERR (Arg0, Z999, 0x49, 0x00, 0x00, Arg0, 0x08)
            }

            If ((Arg1 != 0x09))
            {
                ERR (Arg0, Z999, 0x4C, 0x00, 0x00, Arg0, 0x09)
            }

            If ((Arg2 != 0x0A))
            {
                ERR (Arg0, Z999, 0x4F, 0x00, 0x00, Arg0, 0x0A)
            }

            If ((Arg3 != 0x0B))
            {
                ERR (Arg0, Z999, 0x52, 0x00, 0x00, Arg0, 0x0B)
            }

            If ((Arg4 != 0x0C))
            {
                ERR (Arg0, Z999, 0x55, 0x00, 0x00, Arg0, 0x0C)
            }

            If ((Arg5 != 0x0D))
            {
                ERR (Arg0, Z999, 0x58, 0x00, 0x00, Arg0, 0x0D)
            }

            M000 (0x0E, 0x0F, 0x10, 0x11, 0x12)
        }

        If ((Arg0 != 0x01))
        {
            ERR (Arg0, Z999, 0x63, 0x00, 0x00, Arg0, 0x01)
        }

        If ((Arg1 != 0x02))
        {
            ERR (Arg0, Z999, 0x66, 0x00, 0x00, Arg0, 0x02)
        }

        If ((Arg2 != 0x03))
        {
            ERR (Arg0, Z999, 0x69, 0x00, 0x00, Arg0, 0x03)
        }

        If ((Arg3 != 0x04))
        {
            ERR (Arg0, Z999, 0x6C, 0x00, 0x00, Arg0, 0x04)
        }

        If ((Arg4 != 0x05))
        {
            ERR (Arg0, Z999, 0x6F, 0x00, 0x00, Arg0, 0x05)
        }

        If ((Arg5 != 0x06))
        {
            ERR (Arg0, Z999, 0x72, 0x00, 0x00, Arg0, 0x06)
        }

        If ((Arg6 != 0x07))
        {
            ERR (Arg0, Z999, 0x75, 0x00, 0x00, Arg0, 0x07)
        }

        M000 (0x08, 0x09, 0x0A, 0x0B, 0x0C, 0x0D)
        If ((Arg0 != 0x01))
        {
            ERR (Arg0, Z999, 0x7B, 0x00, 0x00, Arg0, 0x01)
        }

        If ((Arg1 != 0x02))
        {
            ERR (Arg0, Z999, 0x7E, 0x00, 0x00, Arg0, 0x02)
        }

        If ((Arg2 != 0x03))
        {
            ERR (Arg0, Z999, 0x81, 0x00, 0x00, Arg0, 0x03)
        }

        If ((Arg3 != 0x04))
        {
            ERR (Arg0, Z999, 0x84, 0x00, 0x00, Arg0, 0x04)
        }

        If ((Arg4 != 0x05))
        {
            ERR (Arg0, Z999, 0x87, 0x00, 0x00, Arg0, 0x05)
        }

        If ((Arg5 != 0x06))
        {
            ERR (Arg0, Z999, 0x8A, 0x00, 0x00, Arg0, 0x06)
        }

        If ((Arg6 != 0x07))
        {
            ERR (Arg0, Z999, 0x8D, 0x00, 0x00, Arg0, 0x07)
        }
    }

    Method (CMO0, 0, NotSerialized)
    {
        MA00 (0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07)
    }

    Method (MAIN, 0, NotSerialized)
    {
        STRT (0x00)
        If (STTT ("Control method objects", TCLF, 0x06, W006))
        {
            SRMT ("CMO0")
            CMO0 ()
        }

        FTTT ()
        Local7 = FNSH ()
        Return (Local7)
    }
}

