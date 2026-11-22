DefinitionBlock ("", "DSDT", 2, "CUBIT", "STARTUP", 1) {
    External (LATE, IntObj)
    Name (ORD0, Zero)
    Name (OK01, Zero)
    Name (OK02, Zero)
    Device (DST0) { Name (_HID, "CUB0001") Name (PRM0, Zero) }
    Name (BPRE, One)
    Method (GO00, 0) { Return (LoadTable ("OEM1", "", "", "\\DST0", "", Zero)) }
    Name (BAFT, 2)
    Name (PKG0, Package () { LATE })
    
}
