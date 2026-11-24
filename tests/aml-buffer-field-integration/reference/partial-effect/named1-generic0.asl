DefinitionBlock ("", "SSDT", 2, "CUBIT ", "FLDPART ", 1)
{
 External (MISS, UnknownObj)
 Name (BUFF, Buffer (2) {0x41,0x42})
 Method (TEST, 0) { CreateByteField (BUFF, Zero, FLD0) Store (0x42, FLD0) Store (Timer (), Local7) Name (PKG0, Package (2) { FLD0, MISS }) Return (Zero) }
 Method (READ, 0) { Return (BUFF) }
}
