DefinitionBlock ("", "DSDT", 2, "CUBIT", "SLEEPO", 1)
{
    Name (MODE, Zero)
    Name (CALL, Zero)
    Method (SETM, 1) { Store (Arg0, MODE) Store (Zero, CALL) }
    Name (_S0, Package () {Zero, Zero})
    Name (SPKG, Package () {3, 4})
    Alias (SPKG, _S2)
    Method (SVAL, 0)
    {
        Increment (CALL)
        If (LEqual (MODE, 1)) { Return (Package () {7, Zero}) }
        If (LEqual (MODE, 2)) { Return (Package () {One}) }
        If (LEqual (MODE, 3)) { Return (Package () {}) }
        If (LEqual (MODE, 4)) { Return (Package () {8, One}) }
        If (LEqual (MODE, 5)) { Return (Package () {One, 0x100000001}) }
        If (LEqual (MODE, 6)) { Return (Package () {"1", 2}) }
        If (LEqual (MODE, 7)) { Return (Package () {One, "2"}) }
        If (LEqual (MODE, 8)) { Return (Buffer () {1, 2}) }
        If (LEqual (MODE, 9)) { Return (One) }
        If (LEqual (MODE, 10)) { While (One) { Noop } }
        Return (Package () {One, 2, Zero, Zero})
    }
    Method (_S3, 0) { Return (SVAL ()) }
    Method (ORCL, 0) { Return (\_S3 ()) }
}
