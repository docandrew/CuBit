DefinitionBlock ("", "DSDT", 2, "CUBIT", "ACTIVE", 1) {
    Name (ORD0, Zero)
    Name (OK02, Zero)
    Name (ENIO, Zero)
    Name (FVAL, Zero)
    OperationRegion (REG0, SystemMemory, 0x1000, One)
    Field (REG0, ByteAcc, NoLock, Preserve) { FLD0, 8 }
    Device (DST0) { Name (_HID, "CUB0001") Name (PRM0, Zero) }
    Method (GO00, 1) {
        Local0 = 11
        Local1 = LoadTable ("OEM1", "", "", "\\DST0", "PRM0", Arg0)
        If (Local1) { Return (Local0) }
        Return (Zero)
    }
    Method (EVAL, 0) { Return (GO00 (77)) }
    Method (DUP0, 0) { Return (LoadTable ("OEM1", "", "", "\\DST0", "", Zero)) }
    Method (MISS, 0) { Return (LoadTable ("OEM9", "", "", "bad..path", "", Zero)) }
}
