DefinitionBlock ("", "DSDT", 2, "CUBIT", "FMTTEST", 1)
{
 Name (MARK, Zero)
 Name (BUFF, Buffer () {0,9,10,15,16,99,100,255})
 Method (CTRL, 0) {Return (One)}
 Method (WIDT, 0) {Return (ShiftRight (Ones, 32))}
 Method (TEST, 0) {Return (ToBuffer (ToDecimalString (BUFF)))}
}
