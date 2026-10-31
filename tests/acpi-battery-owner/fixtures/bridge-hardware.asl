DefinitionBlock ("", "DSDT", 2, "CUBIT", "BATOWNER", 1)
{
    OperationRegion (ECOR, EmbeddedControl, Zero, 0x100)
    Device (BAT0)
    {
        Name (_HID, EisaId ("PNP0C0A"))
        Name (PRES, 0x1F)
        Name (REMA, 20000)
        Method (_STA, 0) { Return (PRES) }
        Method (_BIX, 0)
        {
            Return (Package (20) {
                0, 1, 42000, 40000, 1, 11000, 2000, 1000,
                12, 100000, 0xFFFFFFFF, 10, 500, 5, 1, 10,
                "CUBIT", "SER1", "LION", ""
            })
        }
        Method (_BST, 0)
        {
            Local0 = Package (4) { 1, 100, 0, 11000 }
            Local0 [2] = REMA
            Return (Local0)
        }
    }
    Device (EC00)
    {
        Name (_HID, EisaId ("PNP0C09"))
        Method (_Q01, 0)
        {
            Subtract (\BAT0.REMA, 100, \BAT0.REMA)
            Notify (\BAT0, 0x80)
        }
        Method (_Q02, 0)
        {
            Xor (\BAT0.PRES, 0x10, \BAT0.PRES)
            Notify (\BAT0, 0x81)
        }
    }
    Device (BAT1)
    {
        Method (_HID, 0) { Return ("PNP0C0A") }
        Name (_STA, 0x1F)
        Method (_BIX, 0) { Return (\BAT0._BIX ()) }
        Method (_BST, 0) { Return (Package (4) { 1, 100, 5000, 11000 }) }
    }
}
