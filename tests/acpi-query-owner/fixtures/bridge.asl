DefinitionBlock ("", "DSDT", 2, "CUBIT", "QBRIDGE", 1)
{
    Device (DEV0)
    {
        Name (_HID, "PNP0C50")
        Name (_UID, "touchpad")
        Name (_CRS, ResourceTemplate () { IRQNoFlags () {5} })
    }
    Device (BUS0)
    {
        Name (_HID, "PNP0A08")
        Device (ADR0) { Name (_ADR, 0x15) }
    }
    Device (MTH0)
    {
        Name (RUNS, Zero)
        Method (_HID, 0, NotSerialized)
        {
            Increment (RUNS)
            Return ("PNP0C50")
        }
        Name (_CID, "ACPI0003")
        Alias (RUNS, _UID)
    }
}
