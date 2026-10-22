DefinitionBlock ("", "DSDT", 1, "CUBIT", "FMTTEST", 1)
{
 Name (MARK, Zero)
 Name (STR0, "ABC")
 Method (CTRL, 0) {Return (One)}
 Method (WIDT, 0) {Return (ShiftRight (Ones, 32))}
 Method (TEST, 0) {Store(0x80,Index(STR0,Zero)) Store(Zero,Index(STR0,One)) Store(0xFF,Index(STR0,2)) Return(ToBuffer(ToHexString(STR0)))}
}
