DefinitionBlock ("", "DSDT", 2, "CUBIT", "REGALIAS", 1)
{
    Name (CALL, Zero)
    Name (SPAC, Zero)
    Name (CODE, Zero)
    Method (RNOT, 2, NotSerialized)
    {
        Increment (CALL)
        Store (Arg0, SPAC)
        Store (Arg1, CODE)
    }
    Alias (RNOT, RALS)
    Device (DV00)
    {
        Name (_HID, "CUB0001")
        OperationRegion (REG0, EmbeddedControl, 0x10, One)
        Alias (\RNOT, _REG)
    }
    Device (DV01)
    {
        Name (_HID, "CUB0002")
        OperationRegion (REG1, EmbeddedControl, 0x11, One)
        Alias (\RALS, _REG)
        Device (CHLD)
        {
            Name (_HID, "CUB0003")
            OperationRegion (REG2, EmbeddedControl, 0x12, One)
        }
    }
    Device (DV02)
    {
        Name (_HID, "CUB0004")
        OperationRegion (REG3, SystemMemory, 0x1000, One)
        Alias (\RNOT, _REG)
    }
}
