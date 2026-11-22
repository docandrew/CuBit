DefinitionBlock ("", "SSDT", 1, "CUBIT", "DEREFWID", 1) {
External (_OSI, MethodObj)
Method (HELP, 1) { Store (RefOf (Arg0), Local1) DerefOf (Local1) Return (Arg0) }
Method (TEST, 0) { Return (HELP (_OSI ("CuBitProbeSupported"))) }
}
