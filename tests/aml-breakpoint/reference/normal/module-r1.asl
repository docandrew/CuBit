DefinitionBlock ("", "DSDT", 1, "CUBIT", "BRKPOINT", 1)
{
 Name (MARK, Zero)
 If (One) { BreakPoint Store (0x77, MARK) } Method (TEST, 0) { Return (MARK) }
 Method (READ, 0) { Return (MARK) }
}
