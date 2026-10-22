DefinitionBlock ("", "SSDT", 2, "CUBIT", "NAMEOWN", 1) {
Name (SAVE, Package () {})
Method (MAKE, 0) { Name (TEMP, Package () { TEMP }) Store (TEMP, SAVE) Return (Zero) }
Method (INIT, 0) { Return (Zero) }
Method (TEST, 0) { MAKE ()
Return (SizeOf (DerefOf (Index (SAVE, Zero)))) }
}
