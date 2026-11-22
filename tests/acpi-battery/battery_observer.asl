DefinitionBlock ("", "DSDT", 2, "CUBIT", "BATOBS", 1)
{
    Name (MODE, Zero)
    Name (HMOD, Zero)
    Name (PMOD, Zero)
    Name (RCNT, Zero)
    Method (SETM, 2) { Store (Arg0, MODE) Store (Arg1, HMOD) }
    Method (SETP, 1) { Store (Arg0, PMOD) }
    Method (PVAL, 0)
    {
        If (LEqual (PMOD, 1)) { Return (0x0F) }
        If (LEqual (PMOD, 2)) { Return (0x10) }
        If (LEqual (PMOD, 3)) { Return (0x08) }
        If (LEqual (PMOD, 4)) { Return (0x10000001F) }
        If (LEqual (PMOD, 5)) { Return ("31") }
        If (LEqual (PMOD, 6)) { While (One) { Noop } }
        If (LEqual (PMOD, 7)) { Return (Buffer () {0x1F}) }
        Return (0x1F)
    }
    Method (BVAL, 0)
    {
        If (LEqual (MODE, 1)) { Return (Package () {3, 10, 20, 30}) }
        If (LEqual (MODE, 2)) { Return (Package () {1, 10, 20}) }
        If (LEqual (MODE, 3)) { Return (Package () {1, 10, "20", 30}) }
        If (LEqual (MODE, 4)) { Return (Package () {1, 0x80000000, 20, 30}) }
        If (LEqual (MODE, 5)) { Return (Package () {1, 10, 20, 0x100000000}) }
        If (LEqual (MODE, 6)) { While (One) { Noop } }
        If (LEqual (MODE, 7)) { Return ("battery") }
        If (LEqual (MODE, 8)) { Return (One) }
        If (LEqual (MODE, 9)) { Return (Package () {8, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF}) }
        If (LEqual (MODE, 10)) { Return (Package () {1, 10, 20, 30, 40}) }
        Return (Package () {0x0A, 1234, 5678, 11000})
    }
    Method (HVAL, 0)
    {
        If (LEqual (HMOD, 1)) { Return ("PNP0C0A") }
        If (LEqual (HMOD, 2)) { Return ("PNP0C0A0") }
        If (LEqual (HMOD, 3)) { Return (Buffer () {0x50,0x4E,0x50,0x30,0x43,0x30,0x41}) }
        If (LEqual (HMOD, 4)) { Return (0x10A0CD041) }
        If (LEqual (HMOD, 5)) { While (One) { Noop } }
        If (LEqual (HMOD, 6)) { Return ("PNP0C0B") }
        Return (EisaId ("PNP0C0A"))
    }
    Method (_BST, 0) { Increment (RCNT) Return (Package () {0,0,0,0}) }
    Method (_HID, 0) { Increment (RCNT) Return ("PNP0C0A") }
    Method (_STA, 0) { Increment (RCNT) Return (0x1F) }
    Device (BAT0)
    {
        Method (_STA, 0) { Return (PVAL ()) }
        Method (_HID, 0) { Return (HVAL ()) }
        Method (_BST, 0) { Return (BVAL ()) }
    }
    Device (NAM0)
    {
        Name (_HID, "PNP0C0A")
        Name (_BST, Package () {1, 100, 200, 300})
    }
    Device (ALS0)
    {
        Name (_HID, EisaId ("PNP0C0A"))
        Method (BSTV, 0) { Return (Package () {1, 100, 200, 300}) }
        Alias (BSTV, _BST)
    }
    Device (ALH0)
    {
        Name (_ADR, Zero)
        Name (HWID, "PNP0C0A") Alias (HWID, _HID)
        Name (_BST, Package () {1, 100, 200, 300})
    }
    Device (MIS0) { Name (_HID, "PNP0C0A") }
    Device (MIH0) { Name (_ADR, Zero) }
    Method (ORCL, 0) { Return (\ALS0._BST ()) }
}
