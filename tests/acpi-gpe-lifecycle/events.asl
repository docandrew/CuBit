DefinitionBlock ("", "DSDT", 2, "CUBIT", "GPELOOP", 1)
{
    Name (HITS, Zero)
    Name (LIDS, Zero)
    Name (ZVAL, Zero)
    Device (LID0) {
        Name (_HID, "PNP0C0D")
        Method (_LID, 0) { Return (LIDS) }
    }
    Scope (\_GPE) {
        Method (_E01, 0) { Increment (HITS) Store (Zero, LIDS) Notify (LID0, 0x80) }
        Method (_L02, 0) { Increment (HITS) Store (One, LIDS) Notify (LID0, 0x80) }
        Method (_E03, 0) { Divide (One, ZVAL, Local0, Local1) }
    }
}
