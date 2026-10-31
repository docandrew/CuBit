DefinitionBlock ("", "DSDT", 2, "CUBIT", "ECFIELDS", 1)
{
    Name (STUP, Zero)
    Name (RVAL, Ones)
    Method (_INI, 0) { Store (0x10, STUP) }
    Name (EVCT, Zero)
    Name (EVST, Ones)
    Device (EC00) {
        Name (_HID, "PNP0C09") Name (_UID, Zero)
        OperationRegion (ERAM, EmbeddedControl, 0, 0x100)
        Field (ERAM, ByteAcc, NoLock, Preserve) { Offset (0x10), LIDS, 1 }
        Field (ERAM, ByteAcc, Lock, Preserve) { Offset (0x10), LCKD, 1 }
        Method (_STA, 0) { Return (0x0F) }
        Method (_REG, 2) {
            If (LEqual (Arg0, 3)) {
                If (LEqual (Arg1, One)) {
                    Store (LIDS, \RVAL)
                    If (LEqual (\STUP, 0x10)) { Store (One, \STUP) }
                    Else { Store (0xEE, \STUP) }
                } Else { Store (3, \STUP) }
            }
        }
        Method (_INI, 0) {
            If (LEqual (\STUP, One)) { Store (2, \STUP) }
            Else { Store (0xEE, \STUP) }
        }
        Method (_Q01, 0) { Store (LIDS, EVST) Increment (EVCT) Notify (\LID0, 0x80) }
        Method (STOR, 1) { Store (Arg0, LIDS) Return (Zero) }
    }
    Device (EC01) { Name (_HID, "PNP0C09") Name (_UID, One) Method (BRET, 0) { Return (Buffer () { 1, 2 }) } Alias (BRET, _Q03) }
    Device (EC02) { Name (_HID, "PNP0C09") Name (_UID, 2) }
    Device (LID0) {
        Name (_HID, "PNP0C0D")
        Method (_LID, 0) { Return (\EC00.LIDS) }
    }
}
