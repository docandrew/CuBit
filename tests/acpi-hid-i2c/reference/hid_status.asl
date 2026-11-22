DefinitionBlock ("", "DSDT", 2, "CUBIT", "HIDSTA", 1)
{
    Name (MODE, Zero)
    Name (CALL, Zero)
    Name (RCNT, Zero)
    Method (SETM, 1) { Store (Arg0, MODE) Store (Zero, CALL) }
    Method (_STA, 0) { Increment (RCNT) Return (0x0F) }
    Device (STA0)
    {
        Name (_HID, "TEST0001")
        Method (SVAL, 0)
        {
            Increment (CALL)
            If (LEqual (MODE, One)) { Return (0x0D) }
            If (LEqual (MODE, 2)) { Return (8) }
            If (LEqual (MODE, 3)) { Return (Zero) }
            If (LEqual (MODE, 4)) { Return (Buffer () {0x0F}) }
            If (LEqual (MODE, 5)) { Return (Package () {0x0F}) }
            If (LEqual (MODE, 6)) { Return ("15") }
            If (LEqual (MODE, 7)) { While (One) { Noop } }
            If (LEqual (MODE, 8)) { Return (0x10000000F) }
            Return (0x0F)
        }
        Method (_STA, 0) { Return (SVAL ()) }
    }
    Device (MISS) { Name (_HID, "TEST0002") }
    Device (NAM0) { Name (_HID, "TEST0003") Name (_STA, 0x0D) }
    Device (ALIA)
    {
        Name (_HID, "TEST0004")
        Method (STAT, 0) { Return (0x0F) }
        Alias (STAT, _STA)
    }
    Method (ORCL, 0) { Return (\ALIA._STA ()) }
}
