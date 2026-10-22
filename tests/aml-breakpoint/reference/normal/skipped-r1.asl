DefinitionBlock ("", "DSDT", 1, "CUBIT", "BRKPOINT", 1)
{
 Name (MARK, Zero)
 Method (TEST, 0) { If (Zero) { BreakPoint Store (0x55, MARK) } Return (0x66) }
 Method (READ, 0) { Return (MARK) }
}
