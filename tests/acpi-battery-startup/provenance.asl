DefinitionBlock ("", "DSDT", 2, "CUBIT", "BATLEG", 1)
{
    Name (MODE, Zero)
    Name (STAF, 0x1F)
    Method (SELM, 1) { Store (Arg0, MODE) Store (0x1F, STAF) }
    Method (BPKG, 0)
    {
        If (LEqual (MODE, 2)) { Return (Package (1) { Zero }) }
        If (LEqual (MODE, 3)) { Return (7) }
        Local0 = Package (13) {
            1, 42000, 40000, 1, 11000, 2000, 1000, 1, 10,
            "CUBIT", "SER1", "LION", ""
        }
        If (LEqual (MODE, 1)) { Local0 [0] = 2 }
        If (LEqual (MODE, 4)) { Local0 [1] = "BAD" }
        If (LEqual (MODE, 5)) { Local0 [9] = "TOO-LONG!" }
        If (LEqual (MODE, 6)) {
            Local0 [1] = 0xFFFFFFFF
            Local0 [2] = 0xFFFFFFFF
            Local0 [4] = 0xFFFFFFFF
        }
        If (LEqual (MODE, 7)) { Local0 [1] = Zero }
        If (LEqual (MODE, 8)) { Store (0x0F, STAF) }
        Return (Local0)
    }
    Device (BAT0) { Name (_HID, "PNP0C0A") Method (_STA, 0) { Return (STAF) } Method (_BST, 0) { Return (Package (4) { 2, 10, 20000, 11000 }) } Method (_BIF, 0) { Return (BPKG ()) } }
    Device (ALS0) { Name (_HID, "PNP0C0A") Alias (BPKG, _BIF) }
    Device (NON0) { Name (_HID, "PNP0C0A") }
    Alias (BPKG, _BIF)

    Method (XPKG, 0) {
        If (LEqual (MODE, 1)) { Return (Package (1) { Zero }) }
        If (LEqual (MODE, 2)) { Return (7) }
        Return (Package (20) {
            0, 1, 42000, 40000, 1, 11000, 2000, 1000,
            0xFFFFFFFF, 100000, 0xFFFFFFFF, 10, 500, 5, 1, 10,
            "CUBIT", "SER1", "LION", ""
        })
    }
    Device (EXT0) {
        Method (_STA, 0) { Return (STAF) }
        Method (_BST, 0) { Return (Package (4) { 2, 10, 20000, 11000 }) }
        Name (_HID, "PNP0C0A")
        Method (_BIX, 0) { Return (XPKG ()) }
        Method (_BIF, 0) { Return (BPKG ()) }
    }
}
