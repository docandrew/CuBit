DefinitionBlock ("", "DSDT", 2, "CUBIT", "SRCPATH", 1)
{
    Device (I2C0) { Name (_HID, "TEST0001") Name (MARK, One) }
    Device (SCP0)
    {
        Name (_HID, "TEST0002")
        Device (I2C0) { Name (_HID, "TEST0003") Name (MARK, 2) }
        Alias (I2C0, ALS0)
        Device (CLN0) { Name (_HID, "TEST0004") }
        Device (DEV0) { Name (_HID, "TEST0005") Name (I2C0, 3) }
    }
}
