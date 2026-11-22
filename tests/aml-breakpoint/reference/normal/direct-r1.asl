DefinitionBlock ("", "DSDT", 1, "CUBIT", "BRKPOINT", 1)
{
 Name (MARK, Zero)
 Method (TEST, 0) { BreakPoint Store (0x11, MARK) Return (0x33) }
 Method (READ, 0) { Return (MARK) }
}
