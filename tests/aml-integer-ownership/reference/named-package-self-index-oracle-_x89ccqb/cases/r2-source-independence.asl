DefinitionBlock ("", "SSDT", 2, "CUBIT", "PKGSTORE", 1) {
Name (SRC0, Package () { 0x11, Package () { 0x22 } })
Name (DST0, Package () { 0x33 })
Alias (DST0, ALIA)
Method (TEST, 0) { Store (SRC0, DST0) Store (0x44, Index (SRC0, Zero)) Return (DerefOf (Index (DST0, Zero))) }
Method (GETD, 0) { Return (SizeOf (DST0)) }
Method (GETS, 0) { Return (SizeOf (SRC0)) }
Method (GETA, 0) { Return (SizeOf (ALIA)) }
Method (LEAF, 0) { Return (DerefOf (Index (SRC0, Zero))) }
Method (NEST, 0) { Return (DerefOf (Index (DerefOf (Index (SRC0, One)), Zero))) }
}
