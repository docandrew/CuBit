DefinitionBlock ("", "SSDT", 2, "CUBIT", "MATCHOP", 1)
{
 Name (SRC0, 0x11)
 Name (FLG0, Zero)
 Name (PKG0, Package (2) { 0x11, 0x22 })
 Method (MUT0, 1) { Store (One, FLG0) Store (0xA1, Debug) Store (0x22, Arg0) Return (Zero) }
 Method (PRE0, 0) { Return (SRC0) }
 Method (TEST, 0) { Store (0x11, Local0) Return (Match (PKG0, MEQ, Local0, MTR, Zero, MUT0 (RefOf (Local0)))) }
 Method (POST, 0) { Return (SRC0) }
 Method (FLAG, 0) { Return (FLG0) }
}
