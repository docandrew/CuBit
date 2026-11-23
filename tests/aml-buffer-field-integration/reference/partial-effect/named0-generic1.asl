DefinitionBlock ("", "SSDT", 2, "CUBIT ", "FLDPART ", 1)
{
 External (MISS, UnknownObj)
 Name (BUFF, Buffer (2) {0x41,0x42})
 Method (TEST, 0) { CreateField (BUFF, Zero, 8, FLD0) Store (0x42, FLD0) Store (Timer (), Local7) Store (Package (2) { FLD0, MISS }, Local0) Return (Zero) }
 Method (READ, 0) { Return (BUFF) }
}
