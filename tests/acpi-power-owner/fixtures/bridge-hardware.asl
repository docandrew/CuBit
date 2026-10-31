DefinitionBlock ("", "DSDT", 2, "CUBIT", "PSROWNER", 1)
{
    OperationRegion (ECOR, EmbeddedControl, Zero, 0x100)
    Device (AC00)
    {
        Name (_HID, "ACPI0003")
        Name (CALL, Zero)
        Name (RAW0, "unknown")
        Method (_PSR, 0, NotSerialized)
        {
            Increment (CALL)
            If (LEqual (CALL, 3)) { Notify (AC00, 0x80) }
            If (LEqual (CALL, 5)) { Return (2) }
            If (LEqual (CALL, 6)) { Return (RAW0) }
            Return (And (Subtract (CALL, One), One))
        }
    }
}
