DefinitionBlock ("", "DSDT", 2, "CUBIT", "HIDQUERY", 1)
{
    Name (MODE, Zero)
    Name (CNT0, Zero)
    Name (CNT1, Zero)
    Name (RCNT, Zero)
    Method (SETM, 1) { Store (Arg0, MODE) Store (Zero, CNT0) Store (Zero, CNT1) Store (Zero, RCNT) }
    Method (_DSM, 4) { Increment (RCNT) If (LEqual (Arg2, Zero)) { Return (Buffer() {3}) } Return (0xEEEE) }
    Device (TPD0)
    {
        Name (_HID, "TEST0001")
        Name (_CID, "PNP0C50")
        Method (_DSM, 4)
        {
            If (LNotEqual (Arg0, ToUUID ("3CDFF6F7-4267-4555-AD05-B30A3D8938DE"))) { Return (Zero) }
            If (LNotEqual (Arg1, One)) { Return (Zero) }
            If (LNotEqual (ObjectType (Arg3), 4)) { Return (Zero) }
            If (LNotEqual (SizeOf (Arg3), Zero)) { Return (Zero) }
            If (LEqual (Arg2, Zero))
            {
                Increment (CNT0)
                If (LEqual (MODE, One)) { Return (Buffer() {0}) }
                If (LEqual (MODE, 2)) { Return (Buffer() {1}) }
                If (LEqual (MODE, 3)) { Return (Zero) }
                If (LEqual (MODE, 4)) { Return (Buffer(Zero) {}) }
                If (LEqual (MODE, 9)) { Return (Buffer() {3, 0x80}) }
                If (LEqual (MODE, 11)) { Return (Buffer() {2}) }
                If (LEqual (MODE, 12)) { Store (Zero, Index (Arg0, Zero)) }
                Return (Buffer() {3})
            }
            If (LEqual (Arg2, One))
            {
                Increment (CNT1)
                If (LEqual (MODE, 5)) { Return (Buffer() {1}) }
                If (LEqual (MODE, 6)) { Return (0x10000) }
                If (LEqual (MODE, 7)) { Return ("wrong type") }
                If (LEqual (MODE, 8)) { While (One) { Noop } }
                If (LEqual (MODE, 9)) { Return (0xFFFF) }
                If (LEqual (MODE, 10)) { Return (Zero) }
                Return (0x1234)
            }
            Return (Zero)
        }
    }
    Device (NDSM) { Name (_HID, "TEST0002") }
    Device (BADM) { Name (_HID, "TEST0003") }
    Device (ALDM)
    {
        Name (_HID, "TEST0004")
        Name (_CID, "PNP0C50")
        Method (MTH1, 4) { Return (\TPD0._DSM (Arg0, Arg1, Arg2, Arg3)) }
        Alias (MTH1, _DSM)
    }
    Method (ORC0, 0) { Return (\TPD0._DSM (ToUUID ("3CDFF6F7-4267-4555-AD05-B30A3D8938DE"), One, Zero, Package() {})) }
    Method (ORC1, 0) { Return (\TPD0._DSM (ToUUID ("3CDFF6F7-4267-4555-AD05-B30A3D8938DE"), One, One, Package() {})) }
    Method (ARQ0, 0) { Return (\ALDM._DSM (ToUUID ("3CDFF6F7-4267-4555-AD05-B30A3D8938DE"), One, Zero, Package() {})) }
    Method (ARQ1, 0) { Return (\ALDM._DSM (ToUUID ("3CDFF6F7-4267-4555-AD05-B30A3D8938DE"), One, One, Package() {})) }
}
