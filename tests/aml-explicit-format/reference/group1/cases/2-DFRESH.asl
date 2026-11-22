DefinitionBlock ("", "DSDT", 2, "CUBIT", "FMTTEST", 1)
{
 Name (MARK, Zero)
 Name (BUFF, Buffer () {65,66})
 Method (CTRL, 0) {Return (One)}
 Method (WIDT, 0) {Return (ShiftRight (Ones, 32))}
 Method (TEST, 0) {Store (0x5A, Index (ToDecimalString (BUFF), Zero)) Return (BUFF)}
}
