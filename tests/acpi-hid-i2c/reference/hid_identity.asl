DefinitionBlock ("", "DSDT", 2, "CUBIT", "HIDIDENT", 1)
{
    Name (MODE, Zero)
    Name (CALL, Zero)
    Name (RCNT, Zero)
    Name (NSTR, "PNP0C50")
    Name (NINT, EisaId ("PNP0C50"))
    Method (SETM, 1) { Store (Arg0, MODE) Store (Zero, CALL) Store (Zero, RCNT) }
    Method (_CID, 0) { Increment (RCNT) Return ("PNP0C50") }
    Method (CVAL, 0)
    {
        Increment (CALL)
        If (LEqual (MODE, Zero)) { Return ("PNP0C50") }
        If (LEqual (MODE, 1)) { Return (EisaId ("PNP0C50")) }
        If (LEqual (MODE, 2)) { Return (Package () {"OTHER000", EisaId ("PNP0C50"), "PNP0C51"}) }
        If (LEqual (MODE, 3)) { Return (Package () {"OTHER000", "PNP0C51"}) }
        If (LEqual (MODE, 4)) { Return (Package () {}) }
        If (LEqual (MODE, 5)) { Return (Buffer () {0x50, 0x4E, 0x50, 0x30, 0x43, 0x35, 0x30}) }
        If (LEqual (MODE, 6)) { Return (Package () {"PNP0C50", Buffer () {1}}) }
        If (LEqual (MODE, 7)) { Return (Package () {Package () {"PNP0C50"}}) }
        If (LEqual (MODE, 8)) { While (One) { Noop } }
        If (LEqual (MODE, 9)) { Return (0x10000500CD041) }
        If (LEqual (MODE, 10)) { Return ("PNP0C50EXTRA") }
        If (LEqual (MODE, 11)) { Return ("pnp0c50") }
        If (LEqual (MODE, 12)) { Return (Zero) }
        If (LEqual (MODE, 13)) { Return (Package () {"PNP0C50", "OTHER000", "PNP0C51"}) }
        If (LEqual (MODE, 14)) { Return (Package () {NSTR, NINT}) }
        Return ("OTHER000")
    }
    Device (IDC0) { Name (_HID, "TEST0001") Method (_CID, 0) { Return (CVAL ()) } }
    Device (NOC0) { Name (_HID, "TEST0002") }
    Device (NAM0) { Name (_HID, "TEST0003") Name (_CID, "PNP0C50") }
    Device (INT0) { Name (_HID, "TEST0004") Name (_CID, EisaId ("PNP0C50")) }
    Device (PKG0) { Name (_HID, "TEST0005") Name (_CID, Package () {"OTHER000", "PNP0C50"}) }
    Device (ALC0)
    {
        Name (_HID, "TEST0006")
        Name (CIDS, Package () {"OTHER000", "PNP0C50"})
        Alias (CIDS, _CID)
    }
    Method (ORCL, 0) { Return (\IDC0._CID ()) }
}
