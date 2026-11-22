DefinitionBlock ("", "DSDT", 1, "CUBIT", "FMTTEST", 1)
{
 Name (MARK, Zero)
 Name (IVAL, 0xFEDCBA9876543210)
 Method (CTRL, 0) {Return (One)}
 Method (WIDT, 0) {Return (ShiftRight (Ones, 32))}
 Method (TEST, 0) {Return (ToBuffer (ToDecimalString (IVAL)))}
}
