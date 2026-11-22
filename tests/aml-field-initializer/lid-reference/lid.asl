DefinitionBlock ("", "DSDT", 2, "CUBIT", "LIDTEST", 1)
{
    OperationRegion (RGN0, SystemMemory, 0x4000, 1)
    Field (RGN0, ByteAcc, NoLock, Preserve) { LVAL, 8 }
    Device (LID0)
    {
        Name (_HID, EisaId ("PNP0C0D"))
        Method (_LID, 0) { Return (LVAL) }
    }
    Method (LCLO, 0) { Store (Zero, LVAL) Notify (LID0, 0x80) Return (Zero) }
    Method (LOPN, 0) { Store (One, LVAL) Notify (LID0, 0x80) Return (Zero) }
    Method (LHI0, 0) { Store (0x80, LVAL) Notify (LID0, 0x80) Return (Zero) }
}
