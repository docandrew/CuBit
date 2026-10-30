/*
 * Intel ACPI Component Architecture
 * AML/ASL+ Disassembler version 20260408 (64-bit version)
 * Copyright (c) 2000 - 2026 Intel Corporation
 * 
 * Disassembling to symbolic ASL+ operators
 *
 * Disassembly of /home/doc/git/.acpi-proof-work/runtime-package-root-oracle-x4j04dl3/results/roots-r2.aml
 *
 * Original Table Header:
 *     Signature        "SSDT"
 *     Length           0x0000014C (332)
 *     Revision         0x02
 *     Checksum         0xE7
 *     OEM ID           "CUBIT"
 *     OEM Table ID     "PKGROOTS"
 *     OEM Revision     0x00000001 (1)
 *     Compiler ID      "INTL"
 *     Compiler Version 0x20260408 (539362312)
 */
DefinitionBlock ("", "SSDT", 2, "CUBIT", "PKGROOTS", 0x00000001)
{
    Name (MARK, Zero)
    Method (RSET, 0, NotSerialized)
    {
        MARK = Zero
    }

    Method (CNUM, 1, NotSerialized)
    {
        Local0 = Buffer (0x40)
            {
                 0xA5                                             // .
            }
        MARK++
        Return (Arg0)
    }

    Method (PRES, 0, NotSerialized)
    {
        Local0 = Buffer (0x80)
            {
                 0x5A                                             // Z
            }
        Return (Zero)
    }

    Method (PASS, 2, NotSerialized)
    {
        Return (Arg0)
    }

    Method (NAM0, 0, NotSerialized)
    {
        Name (TEMP, Package (0x03)
        {
            Buffer (CNUM (0x04))
            {
                 0x55, 0xAA, 0x00, 0xFF                           // U...
            }, 

            Package (0x02)
            {
                Buffer (CNUM (0x03))
                {
                     0x11, 0x22, 0x33                                 // ."3
                }, 

                Package (0x01)
                {
                    Buffer (CNUM (0x05))
                    {
                         0x77, 0x88, 0x99, 0xAA, 0xBB                     // w....
                    }
                }
            }, 

            Buffer (CNUM (0x02))
            {
                 0xCC, 0xDD                                       // ..
            }
        })
        Return (TEMP) /* \NAM0.TEMP */
    }

    Method (RET0, 0, NotSerialized)
    {
        Return (Package (0x03)
        {
            Buffer (CNUM (0x04))
            {
                 0x55, 0xAA, 0x00, 0xFF                           // U...
            }, 

            Package (0x02)
            {
                Buffer (CNUM (0x03))
                {
                     0x11, 0x22, 0x33                                 // ."3
                }, 

                Package (0x01)
                {
                    Buffer (CNUM (0x05))
                    {
                         0x77, 0x88, 0x99, 0xAA, 0xBB                     // w....
                    }
                }
            }, 

            Buffer (CNUM (0x02))
            {
                 0xCC, 0xDD                                       // ..
            }
        })
    }

    Method (CAL0, 0, NotSerialized)
    {
        Return (PASS (Package (0x03)
            {
                Buffer (CNUM (0x04))
                {
                     0x55, 0xAA, 0x00, 0xFF                           // U...
                }, 

                Package (0x02)
                {
                    Buffer (CNUM (0x03))
                    {
                         0x11, 0x22, 0x33                                 // ."3
                    }, 

                    Package (0x01)
                    {
                        Buffer (CNUM (0x05))
                        {
                             0x77, 0x88, 0x99, 0xAA, 0xBB                     // w....
                        }
                    }
                }, 

                Buffer (CNUM (0x02))
                {
                     0xCC, 0xDD                                       // ..
                }
            }, PRES ()))
    }

    Method (CEL0, 0, NotSerialized)
    {
        Local0 = RET0 ()
        Return (PASS (Local0, PRES ()))
    }
}

