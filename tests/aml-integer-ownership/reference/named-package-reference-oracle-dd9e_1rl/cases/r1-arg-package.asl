DefinitionBlock ("", "SSDT", 1, "CUBIT", "PKGREF", 1) {
Name (SRC0, Package () { 0x11, Package () { 0x22 } })
Name (DST0, Package () { 0x33 })
Method (PASS, 1) { Store (SRC0, Arg0) Return (Zero) }
Method (TEST, 0) { PASS (RefOf (DST0)) Return (SizeOf (DST0)) }
Method (TYPE, 0) { Return (ObjectType (DST0)) }
Method (SIZE, 0) { Return (SizeOf (DST0)) }
}
