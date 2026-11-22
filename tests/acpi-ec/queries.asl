DefinitionBlock ("", "DSDT", 2, "CUBIT", "ECQUERY", 1)
{
    Name (HITS, Zero)
    Name (ZVAL, Zero)
    Name (LIDS, Zero)
    Device (LID0) { Name (_HID, "PNP0C0D") Method (_LID, 0) { Return (LIDS) } }
    Method (ALIA, 0) { Increment (HITS) }
    Method (PKGM, 0) { Increment (HITS) Return (Package () { One }) }
    Method (ARGM, 1) { Return (Arg0) }
    Method (_Q02, 0) { Add (HITS, 100, HITS) }
    Device (EC00) {
        Name (_HID, "PNP0C09")
        Method (_Q00, 0) { Add (HITS, 1000, HITS) }
        Method (_Q01, 0) { Increment (HITS) Store (One, LIDS) Notify (LID0, 0x80) }
        Name (BAD3, One) Alias (BAD3, _Q03)
        Alias (\ALIA, _Q04)
        Method (_Q05, 0) { Fatal (1, 0x12345678, 0x89) }
        Alias (\PKGM, _Q06)
        Alias (\ARGM, _Q07)
        Method (_Q08, 0) { Divide (One, ZVAL, Local0, Local1) }
        Method (_QFF, 0) { Increment (HITS) }
    }
}
