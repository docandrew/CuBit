DefinitionBlock ("", "DSDT", 2, "CUBIT", "LENTPD00", 1) {
 Name (OSYS, 0x07E0) Name (TPTY, 2) Name (TPDM, 0) Name (IM01, Zero) Name (SDS1, One)
 Name (GMCT, Zero) Name (IMCT, Zero) Name (SXCT, Zero)
 Method (GNUM, 1) { Increment (GMCT) Return (0xA0) }
 Method (INUM, 1) { Increment (IMCT) Return (0x71) }
 Method (SRXO, 2) { Increment (SXCT) Return (Zero) }
 Scope (_SB) { Device (GPI0) { Name (_HID, "INTC1057") }
 Device (PC00) { Name (_HID, EisaId ("PNP0A08") /* PCI Express Bus */)  // _HID: Hardware ID
            Name (_CID, EisaId ("PNP0A03") /* PCI Bus */)  // _CID: Compatible ID
            Name (_SEG, Zero)  // _SEG: PCI Segment
            Method (^BN00, 0, NotSerialized)
            {
                Return (Zero)
            }

            Method (_BBN, 0, NotSerialized)  // _BBN: BIOS Bus Number
            {
                Return (BN00 ())
            } Name (HIDG, ToUUID ("3cdff6f7-4267-4555-ad05-b30a3d8938de") /* HID I2C Device */)
        Name (TP7G, ToUUID ("ef87eb82-f951-46da-84ec-14871ac6f84b") /* Unknown UUID */)
         Method (HIDD, 5, Serialized)
        {
            If ((Arg0 == HIDG))
            {
                If ((Arg2 == Zero))
                {
                    If ((Arg1 == One))
                    {
                        Return (Buffer (One)
                        {
                             0x03                                             // .
                        })
                    }
                }

                If ((Arg2 == One))
                {
                    Return (Arg4)
                }
            }

            Return (Buffer (One)
            {
                 0x00                                             // .
            })
        } Method (TP7D, 6, Serialized)
        {
            If ((Arg0 == TP7G))
            {
                If ((Arg2 == Zero))
                {
                    If ((Arg1 == One))
                    {
                        Return (Buffer (One)
                        {
                             0x03                                             // .
                        })
                    }
                }

                If ((Arg2 == One))
                {
                    Return (ConcatenateResTemplate (Arg4, Arg5))
                }
            }

            Return (Buffer (One)
            {
                 0x00                                             // .
            })
        } Method (I2CM, 3, Serialized)
        {
            Switch (ToInteger (Arg0))
            {
                Case (Zero)
                {
                    Name (IIC0, Buffer (0x23)
                    {
                        /* 0000 */  0x8E, 0x1E, 0x00, 0x01, 0x00, 0x01, 0x02, 0x00,  // ........
                        /* 0008 */  0x00, 0x01, 0x06, 0x00, 0x80, 0x1A, 0x06, 0x00,  // ........
                        /* 0010 */  0x00, 0x00, 0x5C, 0x5F, 0x53, 0x42, 0x2E, 0x50,  // ..\_SB.P
                        /* 0018 */  0x43, 0x30, 0x30, 0x2E, 0x49, 0x32, 0x43, 0x30,  // C00.I2C0
                        /* 0020 */  0x00, 0x79, 0x00                                 // .y.
                    })
                    CreateWordField (IIC0, 0x10, DAD0)
                    CreateDWordField (IIC0, 0x0C, DSP0)
                    DAD0 = Arg1
                    DSP0 = Arg2
                    Return (IIC0) /* \_SB_.PC00.I2CM.IIC0 */
                }
                Case (One)
                {
                    Name (IIC1, Buffer (0x23)
                    {
                        /* 0000 */  0x8E, 0x1E, 0x00, 0x01, 0x00, 0x01, 0x02, 0x00,  // ........
                        /* 0008 */  0x00, 0x01, 0x06, 0x00, 0x80, 0x1A, 0x06, 0x00,  // ........
                        /* 0010 */  0x00, 0x00, 0x5C, 0x5F, 0x53, 0x42, 0x2E, 0x50,  // ..\_SB.P
                        /* 0018 */  0x43, 0x30, 0x30, 0x2E, 0x49, 0x32, 0x43, 0x31,  // C00.I2C1
                        /* 0020 */  0x00, 0x79, 0x00                                 // .y.
                    })
                    CreateWordField (IIC1, 0x10, DAD1)
                    CreateDWordField (IIC1, 0x0C, DSP1)
                    DAD1 = Arg1
                    DSP1 = Arg2
                    Return (IIC1) /* \_SB_.PC00.I2CM.IIC1 */
                }
                Case (0x02)
                {
                    Name (IIC2, Buffer (0x23)
                    {
                        /* 0000 */  0x8E, 0x1E, 0x00, 0x01, 0x00, 0x01, 0x02, 0x00,  // ........
                        /* 0008 */  0x00, 0x01, 0x06, 0x00, 0x80, 0x1A, 0x06, 0x00,  // ........
                        /* 0010 */  0x00, 0x00, 0x5C, 0x5F, 0x53, 0x42, 0x2E, 0x50,  // ..\_SB.P
                        /* 0018 */  0x43, 0x30, 0x30, 0x2E, 0x49, 0x32, 0x43, 0x32,  // C00.I2C2
                        /* 0020 */  0x00, 0x79, 0x00                                 // .y.
                    })
                    CreateWordField (IIC2, 0x10, DAD2)
                    CreateDWordField (IIC2, 0x0C, DSP2)
                    DAD2 = Arg1
                    DSP2 = Arg2
                    Return (IIC2) /* \_SB_.PC00.I2CM.IIC2 */
                }
                Case (0x03)
                {
                    Name (IIC3, Buffer (0x23)
                    {
                        /* 0000 */  0x8E, 0x1E, 0x00, 0x01, 0x00, 0x01, 0x02, 0x00,  // ........
                        /* 0008 */  0x00, 0x01, 0x06, 0x00, 0x80, 0x1A, 0x06, 0x00,  // ........
                        /* 0010 */  0x00, 0x00, 0x5C, 0x5F, 0x53, 0x42, 0x2E, 0x50,  // ..\_SB.P
                        /* 0018 */  0x43, 0x30, 0x30, 0x2E, 0x49, 0x32, 0x43, 0x33,  // C00.I2C3
                        /* 0020 */  0x00, 0x79, 0x00                                 // .y.
                    })
                    CreateWordField (IIC3, 0x10, DAD3)
                    CreateDWordField (IIC3, 0x0C, DSP3)
                    DAD3 = Arg1
                    DSP3 = Arg2
                    Return (IIC3) /* \_SB_.PC00.I2CM.IIC3 */
                }
                Case (0x04)
                {
                    Name (IIC4, Buffer (0x23)
                    {
                        /* 0000 */  0x8E, 0x1E, 0x00, 0x01, 0x00, 0x01, 0x02, 0x00,  // ........
                        /* 0008 */  0x00, 0x01, 0x06, 0x00, 0x80, 0x1A, 0x06, 0x00,  // ........
                        /* 0010 */  0x00, 0x00, 0x5C, 0x5F, 0x53, 0x42, 0x2E, 0x50,  // ..\_SB.P
                        /* 0018 */  0x43, 0x30, 0x30, 0x2E, 0x49, 0x32, 0x43, 0x34,  // C00.I2C4
                        /* 0020 */  0x00, 0x79, 0x00                                 // .y.
                    })
                    CreateWordField (IIC4, 0x10, DAD4)
                    CreateDWordField (IIC4, 0x0C, DSP4)
                    DAD4 = Arg1
                    DSP4 = Arg2
                    Return (IIC4) /* \_SB_.PC00.I2CM.IIC4 */
                }
                Case (0x05)
                {
                    Name (IIC5, Buffer (0x23)
                    {
                        /* 0000 */  0x8E, 0x1E, 0x00, 0x01, 0x00, 0x01, 0x02, 0x00,  // ........
                        /* 0008 */  0x00, 0x01, 0x06, 0x00, 0x80, 0x1A, 0x06, 0x00,  // ........
                        /* 0010 */  0x00, 0x00, 0x5C, 0x5F, 0x53, 0x42, 0x2E, 0x50,  // ..\_SB.P
                        /* 0018 */  0x43, 0x30, 0x30, 0x2E, 0x49, 0x32, 0x43, 0x35,  // C00.I2C5
                        /* 0020 */  0x00, 0x79, 0x00                                 // .y.
                    })
                    CreateWordField (IIC5, 0x10, DAD5)
                    CreateDWordField (IIC5, 0x0C, DSP5)
                    DAD5 = Arg1
                    DSP5 = Arg2
                    Return (IIC5) /* \_SB_.PC00.I2CM.IIC5 */
                }
                Case (0x0C)
                {
                    Name (IIC6, Buffer (0x23)
                    {
                        /* 0000 */  0x8E, 0x1E, 0x00, 0x01, 0x00, 0x01, 0x02, 0x00,  // ........
                        /* 0008 */  0x00, 0x01, 0x06, 0x00, 0x80, 0x1A, 0x06, 0x00,  // ........
                        /* 0010 */  0x00, 0x00, 0x5C, 0x5F, 0x53, 0x42, 0x2E, 0x50,  // ..\_SB.P
                        /* 0018 */  0x43, 0x30, 0x30, 0x2E, 0x49, 0x32, 0x43, 0x36,  // C00.I2C6
                        /* 0020 */  0x00, 0x79, 0x00                                 // .y.
                    })
                    CreateWordField (IIC6, 0x10, DAD6)
                    CreateDWordField (IIC6, 0x0C, DSP6)
                    DAD6 = Arg1
                    DSP6 = Arg2
                    Return (IIC6) /* \_SB_.PC00.I2CM.IIC6 */
                }
                Case (0x0D)
                {
                    Name (IIC7, Buffer (0x23)
                    {
                        /* 0000 */  0x8E, 0x1E, 0x00, 0x01, 0x00, 0x01, 0x02, 0x00,  // ........
                        /* 0008 */  0x00, 0x01, 0x06, 0x00, 0x80, 0x1A, 0x06, 0x00,  // ........
                        /* 0010 */  0x00, 0x00, 0x5C, 0x5F, 0x53, 0x42, 0x2E, 0x50,  // ..\_SB.P
                        /* 0018 */  0x43, 0x30, 0x30, 0x2E, 0x49, 0x32, 0x43, 0x37,  // C00.I2C7
                        /* 0020 */  0x00, 0x79, 0x00                                 // .y.
                    })
                    CreateWordField (IIC7, 0x10, DAD7)
                    CreateDWordField (IIC7, 0x0C, DSP7)
                    DAD7 = Arg1
                    DSP7 = Arg2
                    Return (IIC7) /* \_SB_.PC00.I2CM.IIC7 */
                }
                Default
                {
                    Return (Zero)
                }

            }
        }
 Device (I2C1) { If (((IM01 == One) || (IM01 == Zero)))
            {
                Method (_ADR, 0, NotSerialized)  // _ADR: Address
                {
                    Return (0x00150001)
                }
            } Name (I2CN, Zero)
        Name (I2CX, Zero)
        Name (I2CI, One)
        Method (_INI, 0, NotSerialized)  // _INI: Initialize
        {
            I2CN = SDS1 /* \SDS1 */
            I2CX = One
        }

         Device (TPD0)
        {
            Name (HID2, Zero)
            Name (SBFB, Buffer (0x23)
            {
                /* 0000 */  0x8E, 0x1E, 0x00, 0x01, 0x00, 0x01, 0x02, 0x00,  // ........
                /* 0008 */  0x00, 0x01, 0x06, 0x00, 0x80, 0x1A, 0x06, 0x00,  // ........
                /* 0010 */  0x2C, 0x00, 0x5C, 0x5F, 0x53, 0x42, 0x2E, 0x50,  // ,.\_SB.P
                /* 0018 */  0x43, 0x30, 0x30, 0x2E, 0x49, 0x32, 0x43, 0x31,  // C00.I2C1
                /* 0020 */  0x00, 0x79, 0x00                                 // .y.
            })
            Name (SBFG, Buffer (0x25)
            {
                /* 0000 */  0x8C, 0x20, 0x00, 0x01, 0x00, 0x01, 0x00, 0x12,  // . ......
                /* 0008 */  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x17, 0x00,  // ........
                /* 0010 */  0x00, 0x19, 0x00, 0x23, 0x00, 0x00, 0x00, 0x00,  // ...#....
                /* 0018 */  0x00, 0x5C, 0x5F, 0x53, 0x42, 0x2E, 0x47, 0x50,  // .\_SB.GP
                /* 0020 */  0x49, 0x30, 0x00, 0x79, 0x00                     // I0.y.
            })
            Name (SBFI, Buffer (0x0B)
            {
                /* 0000 */  0x89, 0x06, 0x00, 0x15, 0x01, 0x00, 0x00, 0x00,  // ........
                /* 0008 */  0x00, 0x79, 0x00                                 // .y.
            })
            CreateWordField (SBFB, 0x10, BADR)
            CreateDWordField (SBFB, 0x0C, SPED)
            CreateWordField (SBFG, 0x17, INT1)
            CreateDWordField (SBFI, 0x05, INT2)
            Method (_INI, 0, NotSerialized)  // _INI: Initialize
            {
                If ((OSYS < 0x07DC))
                {
                    SRXO (0x09080011, One)
                }

                INT1 = GNUM (0x09080011)
                INT2 = INUM (0x09080011)
                If ((TPTY == One))
                {
                    _HID = "ELAN06FA"
                    _SUB = "ELAN0001"
                    BADR = 0x15
                    HID2 = One
                    Return (Zero)
                }

                If ((TPTY == 0x02))
                {
                    _HID = "SYNA2BA6"
                    _SUB = "SYNA0001"
                    BADR = 0x2C
                    HID2 = 0x20
                    Return (Zero)
                }

                If ((TPTY == 0x04))
                {
                    _HID = "CRQ1080"
                    _SUB = "CRQ0001"
                    BADR = 0x2C
                    HID2 = 0x20
                    Return (Zero)
                }

                If ((TPTY == 0x05))
                {
                    _HID = "FTCS0038"
                    _SUB = "FTCS0001"
                    BADR = 0x38
                    HID2 = One
                    Return (Zero)
                }
            }

            Name (_HID, "XXXX0000")  // _HID: Hardware ID
            Name (_CID, "PNP0C50" /* HID Protocol Device (I2C bus) */)  // _CID: Compatible ID
            Name (_SUB, "XXXX0000")  // _SUB: Subsystem ID
            Name (_S0W, 0x03)  // _S0W: S0 Device Wake State
            Method (_DSM, 4, Serialized)  // _DSM: Device-Specific Method
            {
                If ((Arg0 == HIDG))
                {
                    Return (HIDD (Arg0, Arg1, Arg2, Arg3, HID2))
                }

                If ((Arg0 == TP7G))
                {
                    Return (TP7D (Arg0, Arg1, Arg2, Arg3, SBFB, SBFG))
                }

                Return (Buffer (One)
                {
                     0x00                                             // .
                })
            }

            Method (_STA, 0, NotSerialized) { Return (0x1) }

            Method (_CRS, 0, NotSerialized)  // _CRS: Current Resource Settings
            {
                If ((OSYS < 0x07DC))
                {
                    Return (SBFI) /* \_SB_.PC00.I2C1.TPD0.SBFI */
                }

                If ((TPDM == Zero))
                {
                    Return (ConcatenateResTemplate (I2CM (I2CX, BADR, SPED), SBFG))
                }

                Return (ConcatenateResTemplate (I2CM (I2CX, BADR, SPED), SBFI))
            }

            }
 }
 } }
}
