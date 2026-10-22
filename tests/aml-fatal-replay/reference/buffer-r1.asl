DefinitionBlock ("", "DSDT", 1, "CUBIT", "FATALREF", 1)
{
 Name (MARK, Zero)
 Name (CNT0, Zero)
 
 Method (TEST, 0) { Fatal (0xA5, 0x12345678, Buffer (8) { 0x10, 0x32, 0x54, 0x76, 0x98, 0xBA, 0xDC, 0xFE }) Store (0x77, MARK) Return (0x88) }
 Method (READ, 0) { Return (MARK) }
 Method (CNTR, 0) { Return (CNT0) }
}
