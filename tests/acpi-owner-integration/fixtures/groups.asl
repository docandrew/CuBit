DefinitionBlock ("", "DSDT", 2, "CUBIT", "BATOBS", 1)
{
    Name (MODE, Zero)
    Name (SCNT, Zero)
    Method (SELM, 1) { MODE = Arg0 SCNT = Zero }
    Method (IFMT, 0)
    {
        If (LEqual (MODE, 13))
        {
            Return (Package (20) {
                0, 1, 42000, 40000, 1, 11000, 2000, 1000,
                12, 100000, 0xFFFFFFFF, 10, 500, 5, 1, 10,
                "CUBIT", "SER1", "LION", ""
            })
        }
        Local0 = Package (21) {
            1, 1, 42000, 40000, 1, 11000, 2000, 1000,
            12, 100000, 0xFFFFFFFF, 10, 500, 5, 1, 10,
            "CUBIT", "SER1", "LION", "", 0
        }
        If (LOr (LEqual (MODE, 7), LEqual (MODE, 20))) { Local0 [1] = 2 }
        If (LEqual (MODE, 9)) { Local0 [16] = "TOO-LONG!" }
        If (LEqual (MODE, 16)) { Local0 [1] = Zero }
        If (LEqual (MODE, 21))
        {
            Local0 [8] = 0xFFFFFFFF Local0 [20] = 0xFFFFFFFF
            Local0 [16] = "\x7F\x7F\x7F\x7F\x7F\x7F\x7F\x7F"
        }
        Return (Local0)
    }
    Method (SFMT, 0)
    {
        If (LEqual (MODE, 10)) { Return (One) }
        If (LEqual (MODE, 8)) { Return (Package (4) { 3, 100, 20000, 11000 }) }
        If (LEqual (MODE, 15)) { Return (Package (4) { 0, 0xFFFFFFFF, 0, 11000 }) }
        If (LEqual (MODE, 21)) { Return (Package (4) { 0xFFFFFFFD, 0xFFFFFFFF, 0, 0x7FFFFFFF }) }
        Return (Package (4) { 1, 100, 20000, 11000 })
    }
    Method (PRES, 0)
    {
        Increment (SCNT)
        If (LEqual (MODE, 2)) { Return (0x0F) }
        If (LEqual (MODE, 3)) { Return (0x15) }
        If (LEqual (MODE, 4)) { Return (0x17) }
        If (LAnd (LEqual (MODE, 5), LGreater (SCNT, One))) { Return (0x0F) }
        If (LAnd (LEqual (MODE, 6), LGreater (SCNT, One))) { Divide (One, Zero, Local0, Local1) }
        If (LEqual (MODE, 11)) { Return ("BAD") }
        If (LEqual (MODE, 26)) { Return (0x02) }
        If (LEqual (MODE, 27)) { Return (0x1D) }
        If (LAnd (LEqual (MODE, 14), LGreater (SCNT, One))) { Return (0x011F) }
        Return (0x1F)
    }
    Device (BAT0)
    {
        Method (_HID, 0) { If (LEqual (MODE, One)) { Return ("PNP0000") } Return ("PNP0C0A") }
        Method (_STA, 0) { Return (PRES ()) }
        Method (_BIX, 0)
        {
            If (LEqual (MODE, 22))
            {
                Notify (\BAT0, 0x81) Notify (\BAT0, 0x81) Notify (\BAT0, 0x81)
                Notify (\BAT0, 0x81) Notify (\BAT0, 0x81) Notify (\BAT0, 0x81)
            }
            If (LOr (LEqual (MODE, 17), LEqual (MODE, 20))) { Notify (\BAT0, 0x81) }
            If (LEqual (MODE, 18)) { Notify (\OTH0, 0x81) }
            If (LEqual (MODE, 19))
            {
                Notify (\BAT0, 0x80) Notify (\OTH0, 0x81) Notify (\BAT0, 0x81)
            }
            Return (IFMT ())
        }
        Method (_BST, 0) { Return (SFMT ()) }
    }
    Device (NOIN)
    {
        Name (_HID, "PNP0C0A") Name (_STA, 0x1F)
        Method (_BST, 0) { Return (SFMT ()) }
    }
    Device (NOST)
    {
        Name (_HID, "PNP0C0A") Name (_STA, 0x1F)
        Method (_BIX, 0) { Return (IFMT ()) }
    }
    Device (DFLT)
    {
        Name (_HID, "PNP0C0A")
        Method (_BIX, 0) { Return (IFMT ()) }
        Method (_BST, 0) { Return (SFMT ()) }
    }
    Device (OTH0) { Name (_HID, "PNP0C0A") }
    Method (SEND, 1)
    {
        If (LEqual (Arg0, Zero)) { Notify (\BAT0, 0x81) }
        Else { Notify (\OTH0, 0x80) }
    }
    Method (ORCL, 0)
    {
        Local0 = \BAT0._BIX ()
        Local1 = \BAT0._BST ()
        Local2 = Package (5) { 0, 0, 0, 0, 0 }
        Local2 [0] = DerefOf (Local0 [1])
        Local2 [1] = DerefOf (Local0 [2])
        Local2 [2] = DerefOf (Local1 [0])
        Local2 [3] = DerefOf (Local1 [1])
        Local2 [4] = DerefOf (Local1 [2])
        Return (Local2)
    }
}
