DefinitionBlock ("", "SSDT", 1, "CUBIT", "NAMEOWN", 1) {
Name (CYCL, Package () { CYCL, 0x11 })
Method (INIT, 0) { Return (Zero) }
Method (TEST, 0) { Store (DerefOf (Index (CYCL, One)), Local0)
CopyObject (Zero, CYCL)
Return (Local0) }
}
