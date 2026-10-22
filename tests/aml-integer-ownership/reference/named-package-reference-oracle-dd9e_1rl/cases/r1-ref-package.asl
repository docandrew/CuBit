DefinitionBlock ("", "SSDT", 1, "CUBIT", "PKGREF", 1) {
Name (SRC0, Package () { 0x11, Package () { 0x22 } })
Name (DST0, Package () { 0x33 })

Method (TEST, 0) { Store (SRC0, RefOf (DST0)) Return (SizeOf (DST0)) }
Method (TYPE, 0) { Return (ObjectType (DST0)) }
Method (SIZE, 0) { Return (SizeOf (DST0)) }
}
