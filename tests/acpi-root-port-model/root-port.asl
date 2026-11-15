DefinitionBlock("","DSDT",2,"CUBIT","RPINIMDL",1){
 External(\_SB.PC00.RP04.PINI,MethodObj)
 External(\_SB.PC00.RP07.PINI,MethodObj)
 Name(LTR4,4) Name(PML4,40) Name(PNL4,400) Name(LTR7,7) Name(PML7,70) Name(PNL7,700)
 Name(HBSL,3) Name(VMR1,0x48) Name(VMDE,One) Name(STD3,One)
 Scope(_SB){Device(PC00){Name(_HID,"PNP0A08") Device(RP04) {Name(_ADR,Zero) Name(LTEN,Zero) Name(LMSL,Zero) Name(LNSL,Zero) Name(HBCS,Zero) Name(PRMV,Zero) Name(RD3C,Zero) Name(VDID,0x80868086) Method(PRES,0){Return(One)} Device(PXSX){Name(_ADR,Zero) Method(PNVM,0){Return(One)} Method(PAHC,0){Return(Zero)}}
            Method (_INI, 0, NotSerialized)  // _INI: Initialize
            {
                LTEN = LTR4 /* \LTR4 */
                LMSL = PML4 /* \PML4 */
                LNSL = PNL4 /* \PNL4 */
                If ((HBSL & One))
                {
                    HBCS = One
                }

                If ((CondRefOf (VMR1) && CondRefOf (VMDE)))
                {
                    If (((VMDE == One) && ((VMR1 & 0x08) != Zero)))
                    {
                        PRMV = One
                    }
                }

                If (PRES ())
                {
                    If (CondRefOf (PINI))
                    {
                        PINI ()
                    }
                }

                If (CondRefOf (\_SB.PC00.RP04))
                {
                    If ((VDID != 0xFFFFFFFF))
                    {
                        If (CondRefOf (\STD3))
                        {
                            If (CondRefOf (\_SB.PC00.RP04.PRMV))
                            {
                                If ((PRMV == One))
                                {
                                    RD3C = STD3 /* \STD3 */
                                }
                            }

                            If ((^PXSX.PNVM () || ^PXSX.PAHC ()))
                            {
                                RD3C = STD3 /* \STD3 */
                            }
                        }
                    }
                }
            }
}
Device(RP07) {Name(_ADR,Zero) Name(LTEN,Zero) Name(LMSL,Zero) Name(LNSL,Zero) Name(HBCS,Zero) Name(PRMV,Zero) Name(RD3C,Zero) Name(VDID,0x80868086) Method(PRES,0){Return(One)} Device(PXSX){Name(_ADR,Zero) Method(PNVM,0){Return(One)} Method(PAHC,0){Return(Zero)}}
            Method (_INI, 0, NotSerialized)  // _INI: Initialize
            {
                LTEN = LTR7 /* \LTR7 */
                LMSL = PML7 /* \PML7 */
                LNSL = PNL7 /* \PNL7 */
                If ((HBSL & 0x02))
                {
                    HBCS = One
                }

                If ((CondRefOf (VMR1) && CondRefOf (VMDE)))
                {
                    If (((VMDE == One) && ((VMR1 & 0x40) != Zero)))
                    {
                        PRMV = One
                    }
                }

                If (PRES ())
                {
                    If (CondRefOf (PINI))
                    {
                        PINI ()
                    }
                }

                If (CondRefOf (\_SB.PC00.RP07))
                {
                    If ((VDID != 0xFFFFFFFF))
                    {
                        If (CondRefOf (\STD3))
                        {
                            If (CondRefOf (\_SB.PC00.RP07.PRMV))
                            {
                                If ((PRMV == One))
                                {
                                    RD3C = STD3 /* \STD3 */
                                }
                            }

                            If ((^PXSX.PNVM () || ^PXSX.PAHC ()))
                            {
                                RD3C = STD3 /* \STD3 */
                            }
                        }
                    }
                }
            }
}}}
 Method(B000,0){\_SB.PC00.RP04._INI() Return(\_SB.PC00.RP04.RD3C)}
 Method(B001,0){\_SB.PC00.RP07._INI() Return(\_SB.PC00.RP07.RD3C)}
 Method(B002,0){Return(CondRefOf(\_SB.PC00.RP04))}
 Method(B003,0){Return(CondRefOf(\_SB.PC00.RP04.PINI))}
 Method(B004,0){CondRefOf(\_SB.PC00.RP04,Local0) Return(ObjectType(Local0))}
 Method(B005,0){CondRefOf(\_SB.PC00.RP04._INI,Local0) Return(ObjectType(Local0))}
 Method(B006,0){Return(\_SB.PC00.RP04.PRMV)}
 Method(B007,0){Return(\_SB.PC00.RP07.LTEN)}
 Method(B008,0){Return(CondRefOf(\_SB.PC00.RP04._INI))}
 Method(B009,0){Return(CondRefOf(\_SB.PC00.RP07.MISS))}
}