DefinitionBlock ("", "DSDT", 1, "CUBIT", "FMTTEST", 1)
{
 Name (MARK, Zero)
 Name (STR0, "ABC")
 Method (CTRL, 0) {Return (One)}
 Method (WIDT, 0) {Return (ShiftRight (Ones, 32))}
 Method (TEST, 0) {Store (0x5A, Index (ToHexString (STR0), Zero)) Return (ToBuffer (STR0))}
}
