DefinitionBlock ("", "SSDT", 2, "CUBIT", "NAMEOWN", 1) {

Method (INIT, 0) { Return (Zero) }
Method (TEST, 0) { Name (TEMP, Package () { TEMP })
Store (TEMP, Local0)
CopyObject (Package () { 0x22, 0x33 }, TEMP)
Return (SizeOf (DerefOf (Index (Local0, Zero)))) }
}
