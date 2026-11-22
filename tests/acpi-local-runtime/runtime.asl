DefinitionBlock ("", "DSDT", 2, "CUBIT", "LOCRUN", 1)
{
    Name (ZRO0, Zero)
    Method (ERR0, 0) { Divide (One, ZRO0, Local0, Local1) Return (Local1) }
    Name (VAL0, 0x2A)
    Name (BUF0, Buffer () {0x11, 0x22})
    Method (ADD0, 1) { Return (Add (Arg0, One)) }
    Method (SIZ0, 1) { Return (SizeOf (Arg0)) }
}
