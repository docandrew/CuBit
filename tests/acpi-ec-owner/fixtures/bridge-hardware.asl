DefinitionBlock ("", "DSDT", 2, "CUBIT", "ECOWNER", 1)
{
    OperationRegion (ECOR, EmbeddedControl, Zero, 0x100)
    Device (EC00)
    {
        Name (_HID, EisaId ("PNP0C09"))
        Method (_Q01, 0, NotSerialized)
        {
            Increment (\LID0.LVAL)
            Notify (\LID0, 0x80)
        }
        Method (_Q02, 0, NotSerialized)
        {
            Xor (\AC00.PVAL, One, \AC00.PVAL)
            Notify (\AC00, 0x80)
        }
        Method (_Q03, 0, NotSerialized)
        {
            Increment (\LID0.LVAL)
            Xor (\AC00.PVAL, One, \AC00.PVAL)
            Notify (\LID0, 0x80)
            Notify (\AC00, 0x80)
        }
    }
    Device (AC00)
    {
        Name (_HID, "ACPI0003")
        Name (PVAL, Zero)
        Method (_PSR, 0, NotSerialized) { Return (PVAL) }
    }
    Device (LID0)
    {
        Name (_HID, EisaId ("PNP0C0D"))
        Name (LVAL, Zero)
        Method (_LID, 0, NotSerialized) { Return (LVAL) }
    }
}
