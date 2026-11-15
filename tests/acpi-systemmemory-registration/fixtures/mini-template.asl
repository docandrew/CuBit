DefinitionBlock("", "DSDT", 2, "CUBIT", "ECREGMIN", 1) {
 Name(OSYS, /*OSYS*/) Name(LIDS,Zero)
 Scope(\_SB){ Device(PC00){ Name(_HID,"TEST0001")
 Device(GFX0){Name(_HID,"TEST0002") Name(CLID,Zero)}
 Device(LPCB){Name(_HID,"TEST0003") Device(EC0){Name(_HID,"PNP0C09")
 Name(ECAV,Zero) Name(OSTY,Zero) Name(LSTE,Zero) Mutex(LFCM,0)
 Method(READ,0){Return(OSTY)} Method(AVAL,0){Return(ECAV)}
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
 }}}}
}
