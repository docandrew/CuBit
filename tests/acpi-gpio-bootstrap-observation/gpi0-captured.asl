Device (GPI0)
        {
            Method (_HID, 0, NotSerialized)  // _HID: Hardware ID
            {
                If ((GPHD == One))
                {
                    Return ("PNP0C02")
                }

                If ((PCHS == 0x03))
                {
                    Return ("INTC1057")
                }
                Else
                {
                    Return ("INTC1055")
                }
            }

            Name (LINK, "\\_SB.GPI0")
            Method (_CRS, 0, NotSerialized)  // _CRS: Current Resource Settings
            {
                Name (RBFL, Buffer (0x3B)
                {
                    /* 0000 */  0x89, 0x06, 0x00, 0x0D, 0x01, 0x0E, 0x00, 0x00,  // ........
                    /* 0008 */  0x00, 0x86, 0x09, 0x00, 0x01, 0x00, 0x00, 0x00,  // ........
                    /* 0010 */  0x00, 0x00, 0x00, 0x01, 0x00, 0x86, 0x09, 0x00,  // ........
                    /* 0018 */  0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01,  // ........
                    /* 0020 */  0x00, 0x86, 0x09, 0x00, 0x01, 0x00, 0x00, 0x00,  // ........
                    /* 0028 */  0x00, 0x00, 0x00, 0x01, 0x00, 0x86, 0x09, 0x00,  // ........
                    /* 0030 */  0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01,  // ........
                    /* 0038 */  0x00, 0x79, 0x00                                 // .y.
                })
                CreateDWordField (RBFL, 0x05, INTL)
                INTL = SGIR /* \SGIR */
                CreateDWordField (RBFL, 0x0D, CML0)
                CML0 = (SBRG + 0x006E0000)
                CreateDWordField (RBFL, 0x19, CML1)
                CML1 = (SBRG + 0x006D0000)
                CreateDWordField (RBFL, 0x25, CML4)
                CML4 = (SBRG + 0x006A0000)
                CreateDWordField (RBFL, 0x31, CML5)
                CML5 = (SBRG + 0x00690000)
                Return (RBFL) /* \_SB_.GPI0._CRS.RBFL */
            }

            Method (_STA, 0, NotSerialized)  // _STA: Status
            {
                If ((GPHD == One))
                {
                    Return (0x08)
                }

                Return (0x0F)
            }
        }