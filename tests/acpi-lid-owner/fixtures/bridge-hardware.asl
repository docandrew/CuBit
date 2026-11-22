DefinitionBlock ("", "DSDT", 2, "CUBIT", "LIDOWNER", 1)
{
    OperationRegion (ECOR, EmbeddedControl, Zero, 0x100)
    Device (LID0)
    {
        Name (_HID, "PNP0C0D")
        Name (CALL, Zero)
        Method (_LID, 0, NotSerialized)
        {
            Increment (CALL)
            If (LEqual (CALL, 3)) { Notify (LID0, 0x80) }
            Return (Subtract (CALL, One))
        }
    }
}
