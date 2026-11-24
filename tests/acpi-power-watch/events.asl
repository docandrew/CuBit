DefinitionBlock ("", "SSDT", 2, "CUBIT", "PWRWATCH", 1) {
 Device(LID0) { Name(_HID,"PNP0C0D") Method(_LID,0){Return(One)} }
 Name(PSRV,One)
 Device(AC00) { Name(_HID,"ACPI0003") Method(_PSR,0){Return(\PSRV)} }
 Device(AC01) { Name(_HID,"ACPI0003") Method(_PSR,0){Return(Zero)} }
 Device(BAT0) { Name(_HID,"PNP0C0A") Name(_STA,0x1F)
  Method(_BIF,0) { Return(Package(13){One,42000,40000,One,11000,2000,1000,One,10,"MODEL","SERIAL","LION","OEM"}) }
  Method(_BST,0) { Return(Package(4){One,10,20000,11000}) }
 }
 Device(OTHR) { Name(_HID,"TEST0001") }
 Method(WB80,0) { Notify(BAT0,0x80) Return(One) }
 Method(WB81,0) { Notify(BAT0,0x81) Return(One) }
 Method(WA80,0) { Notify(AC00,0x80) Return(One) }
 Method(WA81,0) { Notify(AC01,0x81) Return(One) }
 Method(NU80,0) { Notify(OTHR,0x80) Return(One) }
 Method(BADV,0) { Notify(BAT0,0x79) Return(One) }
 Method(CHPS,0) { Store(Zero,\PSRV) Notify(OTHR,0x80) Return(One) }
}
