DefinitionBlock ("", "DSDT", 2, "CUBIT", "FATALREF", 1)
{
 Name (MARK, Zero)
 Name (CNT0, Zero)
 
 Method (TEST, 0) { Fatal (0xA5, 0x12345678, 0xFEDCBA9876543210) Store (0x77, MARK) Return (0x88) }
 Method (READ, 0) { Return (MARK) }
 Method (CNTR, 0) { Return (CNT0) }
}
