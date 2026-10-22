DefinitionBlock ("", "DSDT", 1, "CUBIT", "FATALREF", 1)
{
 Name (MARK, Zero)
 Name (CNT0, Zero)
 Method (MARG, 0) { Increment (CNT0) Fatal (0xB6, 0x11223344, 0x44) Return (0x55) }
 Method (TEST, 0) { Fatal (0xA5, 0x12345678, MARG ()) Store (0x77, MARK) Return (0x88) }
 Method (READ, 0) { Return (MARK) }
 Method (CNTR, 0) { Return (CNT0) }
}
