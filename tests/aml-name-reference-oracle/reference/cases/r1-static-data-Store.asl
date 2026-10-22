DefinitionBlock ("", "SSDT", 1, "CUBIT", "NAMEOWN", 1) {
Name (SRC0, 0x11)
Name (PKG0, Package () { SRC0 })
Method (INIT, 0) { CopyObject (0x11, SRC0) Return (Zero) }
Method (TEST, 0) { Store (0x22, SRC0)
Return (DerefOf (Index (PKG0, Zero))) }
}
