DefinitionBlock ("", "SSDT", 2, "CUBIT", "DEREFWID", 1) {
External (_OSI, MethodObj)
Method (HELP, 1) { Store (Arg0, Local0) Store (RefOf (Local0), Local1) DerefOf (Local1) Return (Local0) }
Method (TEST, 0) { Return (HELP (_OSI ("CuBitProbeSupported"))) }
}
