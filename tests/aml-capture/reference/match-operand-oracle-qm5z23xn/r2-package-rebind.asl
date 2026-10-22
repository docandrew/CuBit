DefinitionBlock ("", "SSDT", 2, "CUBIT", "MATCHOP", 1)
{
 Name (SRC0, 0x11)
 Name (FLG0, Zero)
 Name (PKG0, Package (2) { 0x11, 0x22 })
 Method (MUT0, 0) { Store (One, FLG0) Store (0xA1, Debug) CopyObject (Package (1) { 0x22 }, PKG0) Return (Zero) }
 Method (PRE0, 0) { Return (SizeOf (PKG0)) }
 Method (TEST, 0) {  Return (Match (PKG0, MEQ, 0x11, MTR, Zero, MUT0 ())) }
 Method (POST, 0) { Return (SizeOf (PKG0)) }
 Method (FLAG, 0) { Return (FLG0) }
}
