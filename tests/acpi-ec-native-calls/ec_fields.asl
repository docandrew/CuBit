DefinitionBlock ("", "DSDT", 2, "CUBIT", "ECFIELDS", 1)
{
    Device (EC00) {
        Name (_HID, "PNP0C09")
        OperationRegion (ERAM, EmbeddedControl, 0, 0x100)
        Field (ERAM, ByteAcc, NoLock, Preserve) { Offset (0x10), LIDS, 1 }
        Field (ERAM, ByteAcc, Lock, Preserve) { Offset (0x10), LCKD, 1 }
        Method (STOR, 1) { Store (Arg0, LIDS) Return (Zero) }
    }
    Device (LID0) {
        Name (_HID, "PNP0C0D")
        Method (_LID, 0) { Return (\EC00.LIDS) }
    }
}
