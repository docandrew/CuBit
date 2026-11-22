DefinitionBlock ("", "SSDT", 1, "CUBIT", "STGFIELD", 1)
{
 Name (IDX0, Zero)
 Name (FLG0, Zero)
 Name (BUF0, Buffer (3) { 0x11, 0x22, 0x33 })
 Method (CNT0, 0) { Store (One, FLG0) Store (0xA1, Debug)  CopyObject (8, IDX0) Return (8) }
 Method (PRE0, 0) { Return (IDX0) }
 Method (TEST, 0) {  CreateField (BUF0, IDX0, CNT0 (), FLD0) Return (ToInteger (FLD0)) }
 Method (POST, 0) { Return (IDX0) }
 Method (FLAG, 0) { Return (FLG0) }
}
