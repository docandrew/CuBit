DefinitionBlock("", "DSDT", 2, "CUBIT", "REGPHASE", 1){
 Name(PCHS,3) Name(GPHD,Zero) Name(SBRG,0xFD000000) Name(SGIR,14) Name(GADM,Zero) Name(DINI,Zero) Method(ADMT,0){Store(One,GADM)} Name(OSYS,Zero) Name(LIDS,Zero) Name(RINI,Zero) Name(SINI,Zero) Name(PINI,Zero) Name(CINI,Zero) Name(EINI,Zero) Name(DFLT,Zero)
 OperationRegion(RMEM,SystemMemory,0x1000,4)
 Method(_REG,2){Increment(DFLT)}
 Method(_INI,0){Increment(RINI)}
 Device(EARL){Name(_HID,"TEST0010") Method(_INI,0){If(LEqual(GADM,Zero)){Fatal(1,2,3)} Increment(DINI)}}
 Scope(\_SB){ Method(_INI,0){Increment(SINI)}
 Device(BRG0){Name(_HID,"TEST0001") /*STA*/
 Method(_INI,0){Increment(PINI) /*PARENT*/}
 Device(CLNT){Name(_HID,"TEST0007") Method(_INI,0){If(LEqual(GADM,Zero)){Fatal(1,2,3)} If(LNot(LEqual(\_SB.BRG0.LPCB.EC0.ECAV,One))){Fatal(1,2,3)} Increment(CINI)}}
 Device(GFX0){Name(_HID,"TEST0002") Name(CLID,Zero)}
 Device(LPCB){Name(_HID,"TEST0003") Device(EC0){Name(_HID,"PNP0C09")
 OperationRegion(ERAM,EmbeddedControl,Zero,4)
 Name(ECAV,Zero) Name(OSTY,Zero) Name(LSTE,Zero) Mutex(LFCM,0)
 Method(_INI,0){If(LEqual(GADM,Zero)){Fatal(1,2,3)} If(LNot(LEqual(ECAV,One))){Fatal(1,2,3)} Increment(EINI)}
             Method (_REG, 2, NotSerialized)  // _REG: Region Availability
            {
                If ((Arg0 == 0x03))
                {
                    ECAV = Arg1
                }

                If (((Arg0 == 0x03) && (Arg1 == One)))
                {
                    If ((OSYS == 0x07D6))
                    {
                        Local0 = One
                    }

                    If ((OSYS == 0x03E8))
                    {
                        Local0 = 0x02
                    }

                    If ((OSYS == 0x07D9))
                    {
                        Local0 = 0x03
                    }

                    If ((OSYS == 0x07DC))
                    {
                        Local0 = 0x04
                    }

                    If ((OSYS == 0x07DD))
                    {
                        Local0 = 0x05
                    }

                    If ((OSYS == 0x07DF))
                    {
                        Local0 = 0x06
                    }

                    If ((Acquire (LFCM, 0xA000) == Zero))
                    {
                        OSTY = Local0
                        If ((LSTE == Zero))
                        {
                            ^^^GFX0.CLID = Zero
                        }

                        If ((LSTE == One))
                        {
                            ^^^GFX0.CLID = 0x03
                        }

                        LIDS = LSTE /* \_SB_.PC00.LPCB.EC0_.LSTE */
                        Release (LFCM)
                    }
                }
            }

 }
 Device(AUX0){Name(_HID,"TEST0005") Name(CNRG,Zero) Name(DNRG,Zero)
 OperationRegion(ARE0,EmbeddedControl,0x10,1) OperationRegion(ARE1,EmbeddedControl,0x11,1)
 Method(_REG,2){If(LEqual(Arg1,One)){Increment(CNRG) /*RETURN*/}Else{Increment(DNRG)}}
 }}}
 }
Scope(\_SB){Device (GPI0)
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
        }}
}
