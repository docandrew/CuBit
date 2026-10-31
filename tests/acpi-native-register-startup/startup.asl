DefinitionBlock ("", "DSDT", 2, "CUBIT", "VIEWCOMP", 1)
{
    OperationRegion (REG0, SystemIO, 0x0600, 0x10)
    Field (REG0, ByteAcc, NoLock, Preserve)
    {
        DAT0, 8, DAT1, 8, DAT2, 8, DAT3, 8,
        DAT4, 8, DAT5, 8, DAT6, 8, DAT7, 8,
        BSEL, 8, IDXR, 8, IDAT, 8
    }
    Field (REG0, ByteAcc, NoLock, Preserve) { WIDE, 64 }
    Field (REG0, ByteAcc, NoLock, Preserve) { LOW4, 4 }
    Field (REG0, ByteAcc, NoLock, Preserve) { Offset (12), UNGR, 8 }
    BankField (REG0, BSEL, 3, ByteAcc, NoLock, Preserve)
    {
        Offset (11), BANK, 8
    }
    IndexField (IDXR, IDAT, ByteAcc, NoLock, Preserve)
    {
        Offset (1), INDX, 8
    }
    Method (WRW0, 1) { Store (Arg0, WIDE) Return (WIDE) }
    Method (RD00, 0) { Return (DAT0) }
    Method (BRW0, 1) { Store (Arg0, BANK) Return (BANK) }
    Method (IRW0, 1) { Store (Arg0, INDX) Return (INDX) }
    Method (PART, 1) { Store (Arg0, LOW4) Return (DAT0) }
    Method (UNRD, 0) { Return (UNGR) }
    Method (_INI) { Store (0x42, DAT0) }
    Scope (\_SB) {
        Method (_INI) { Store (0x43, \DAT1) }
        Device (DEV0) {
            Name (_HID, "CUB0001")
            Method (_STA) { Return (0x0F) }
            Method (_INI) { Store (0x44, \DAT2) }
        }
    }
    Name (GUID, Buffer () { 0x10, 0x20, 0x30, 0x40 })
    Method (MTH4, 4) { Store (Arg2, DAT0) Return (Arg0) }
}
