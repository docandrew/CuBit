DefinitionBlock ("", "SSDT", 2, "CUBIT", "DEREFWID", 1) {
External (_OSI, MethodObj)
Method (HELP, 1) { DerefOf (RefOf (Arg0)) Return (Arg0) }
Method (TEST, 0) { Return (HELP (_OSI ("CuBitProbeSupported"))) }
}
