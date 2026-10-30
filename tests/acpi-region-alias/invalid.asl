DefinitionBlock ("", "DSDT", 2, "CUBIT", "REGBAD", 1)
{
    Name (CALL, Zero)
    Name (IVAL, One)
    Device (DV00)
    {
        Name (_HID, "CUB0001")
        OperationRegion (REG0, EmbeddedControl, 0x10, One)
        Alias (\IVAL, _REG)
    }
}
