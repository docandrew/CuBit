DefinitionBlock ("", "SSDT", 2, "CUBIT", "NAMEOWN", 1) {

Method (INIT, 0) { Return (Zero) }
Method (TEST, 0) { Name (TEMP, Package () { TEMP, 0x11 })
Store (DerefOf (Index (TEMP, One)), Local0)
CopyObject (Zero, TEMP)
Return (Local0) }
}
