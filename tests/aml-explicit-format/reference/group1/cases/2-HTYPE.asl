DefinitionBlock ("", "DSDT", 2, "CUBIT", "FMTTEST", 1)
{
 Name (MARK, Zero)
 Name (IVAL, 65)
 Method (CTRL, 0) {Return (One)}
 Method (WIDT, 0) {Return (ShiftRight (Ones, 32))}
 Method (TEST, 0) {Store (ToHexString(IVAL),Local0) Return (Add(Multiply(ObjectType(Local0),100),SizeOf(Local0)))}
}
