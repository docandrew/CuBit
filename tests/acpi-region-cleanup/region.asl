DefinitionBlock ("", "DSDT", 2, "CUBIT", "REGCLEAN", 1)
{
    Name (CALL, Zero)
    Method (RPKG, 2) { Increment (CALL) Return (Package () {One, 2}) }
    Device (DEV0)
    {
        Name (_HID, "CUB0001")
        OperationRegion (REG0, EmbeddedControl, 0x10, One)
        OperationRegion (REG1, EmbeddedControl, 0x11, One)
        Alias (\RPKG, _REG)
    }
}
