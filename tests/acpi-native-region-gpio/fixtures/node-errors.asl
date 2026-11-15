DefinitionBlock("", "DSDT", 2, "CUBIT", "NODEERRS", 1) {
 Name(BEFC,Zero) Name(ECNT,Zero) Name(CCNT,Zero) Name(SCNT,Zero)
 Name(ISTA,Zero) Name(SSTA,Zero) Name(FCNT,Zero) Name(ACNT,Zero) Name(POIS,Zero)
 Scope(_SB) {
  Device(PRE0) { Name(_HID,"TEST0001") Method(_INI,0){Increment(BEFC)} }
  Device(BAD0) { Name(_HID,"TEST0002")
   Method(_INI,0){Increment(ECNT) Divide(One,Zero,Local0,Local1)}
   Device(CHD0){Name(_HID,"TEST0003") Method(_INI,0){Increment(CCNT)}}
  }
  Device(LATE){Name(_HID,"TEST0004") Method(_INI,0){Increment(SCNT)}}
  Device(STAF){Name(_HID,"TEST0005")
   Method(_STA,0){Increment(ISTA) Divide(One,Zero,Local0,Local1) Return(Zero)}
   Method(_INI,0){Increment(POIS) Fatal(1,2,3)}
   Device(KID0){Name(_HID,"TEST0006") Method(_INI,0){Increment(FCNT) Store(ISTA,SSTA)}}
  }
  Device(AFT0){Name(_HID,"TEST0007") Method(_INI,0){Increment(ACNT)}}
 }
}
