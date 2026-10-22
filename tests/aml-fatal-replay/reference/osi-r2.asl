DefinitionBlock ("", "DSDT", 2, "CUBIT", "FATALREF", 1)
{
 Name (MARK, Zero)
 Name (CNT0, Zero)
 Method (RAWV, 0) { Return (_OSI ("Windows 2001")) }
 Method (TEST, 0) { Fatal (0xA5, 0x12345678, RAWV ()) Store (0x77, MARK) Return (0x88) }
 Method (READ, 0) { Return (MARK) }
 Method (CNTR, 0) { Return (CNT0) }
}
