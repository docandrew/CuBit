DefinitionBlock("", "DSDT", 2, "CUBIT", "REGPHASE", 1){
 Name(GADM,Zero) Name(DINI,Zero) Method(ADMT,0){Store(One,GADM)} Name(OSYS,Zero) Name(LIDS,Zero) Name(RINI,Zero) Name(SINI,Zero) Name(PINI,Zero) Name(CINI,Zero) Name(EINI,Zero) Name(DFLT,Zero)
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
}
