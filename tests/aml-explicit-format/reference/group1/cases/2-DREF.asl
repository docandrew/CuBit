DefinitionBlock ("", "DSDT", 2, "CUBIT", "FMTTEST", 1)
{
 Name (MARK, Zero)
 Name (STR0, "ABC")
 Method (CTRL, 0) {Return (One)}
 Method (WIDT, 0) {Return (ShiftRight (Ones, 32))}
 Method (TEST, 0) {Store (RefOf (STR0), Local0) Return (ToBuffer (ToDecimalString (Local0)))}
}
