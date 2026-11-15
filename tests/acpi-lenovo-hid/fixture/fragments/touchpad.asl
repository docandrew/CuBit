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

            Method (_STA, 0, NotSerialized)  // _STA: Status
            {
                If ((TPTY == Zero))
                {
                    Return (Zero)
                }
                Else
                {
                    Return (0x0F)
                }
            }

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

