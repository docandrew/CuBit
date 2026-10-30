DefinitionBlock ("", "DSDT", 2, "CUBIT", "GPEBIND", 1)
{
    Name (HITS, Zero)
    Name (LIDS, Zero)
    Name (ZVAL, Zero)
    Device (LID0) { Name (_HID, "PNP0C0D") Method (_LID, 0) { Return (LIDS) } }
    Method (ALIA, 0) { Increment (HITS) }
    Method (PKGM, 0) { Increment (HITS) Return (Package () { One }) }
    Method (ARGM, 1) { Return (Arg0) }
    Method (LVM6, 0) { Increment (HITS) }
    Scope (\_GPE) {
        Method (_E01, 0) { Increment (HITS) Store (Zero, LIDS) Notify (LID0, 0x80) }
        Method (_L02, 0) { Increment (HITS) Store (One, LIDS) Notify (LID0, 0x80) }
        Method (_E03, 0) { Divide (One, ZVAL, Local0, Local1) }
        Alias (\ALIA, _E04)
        Name (BAD5, One) Alias (BAD5, _E05)
        Method (_E06, 0) { Increment (HITS) }
        Alias (\LVM6, _L06)
        Alias (\PKGM, _E07)
        Alias (\ARGM, _E08)
        Method (_E09, 0) { Fatal (1, 0x12345678, 0x89) }
        Method (_L00, 0) { Increment (HITS) }
        Method (_EFF, 0) { Increment (HITS) }
    }
    Device (GPEB) {
        Name (_HID, "ACPI0006")
        Method (_L0A, 0) { Increment (HITS) }
        Device (CHLD) { Name (_HID, "CBT0001") }
    }
    Scope (\_SI) { Method (_E01, 0) { Increment (HITS) } }
}
