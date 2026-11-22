DefinitionBlock ("", "DSDT", 2, "CUBIT", "BRKPOINT", 1)
{
 Name (MARK, Zero)
 Method (CHLD, 0) { BreakPoint Store (0x22, MARK) Return (0x44) } Method (TEST, 0) { BreakPoint Return (CHLD ()) }
 Method (READ, 0) { Return (MARK) }
}
