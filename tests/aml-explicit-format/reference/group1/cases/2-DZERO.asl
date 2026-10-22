DefinitionBlock ("", "DSDT", 2, "CUBIT", "FMTTEST", 1)
{
 Name (MARK, Zero)
 Name (IVAL, Zero)
 Method (CTRL, 0) {Return (One)}
 Method (WIDT, 0) {Return (ShiftRight (Ones, 32))}
 Method (TEST, 0) {Return (ToBuffer (ToDecimalString (IVAL)))}
}
