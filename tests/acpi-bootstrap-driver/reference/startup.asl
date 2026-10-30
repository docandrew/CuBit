DefinitionBlock ("", "DSDT", 2, "CUBIT", "BOOTDRV", 1)
{
    OperationRegion (REG0, SystemMemory, 0x1000, 1)
    Field (REG0, ByteAcc, NoLock, Preserve) { VAL0, 8 }
    Device (DEV0)
    {
        Name (_HID, "CBT0001")
        Method (_STA, 0) { Return (0x0F) }
        Method (_INI, 0) { Store (0x5A, VAL0) Return (Zero) }
    }
    Device (LID0)
    {
        Name (_HID, EisaId ("PNP0C0D"))
        Method (_LID, 0) { Return (VAL0) }
    }
    Method (GETV, 0) { Return (VAL0) }
}
