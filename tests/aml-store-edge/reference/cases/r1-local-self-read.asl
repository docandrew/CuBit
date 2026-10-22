DefinitionBlock ("", "SSDT", 1, "CUBIT", "STOREEDG", 1) {
External (_OSI, MethodObj)
Method (HELP, 1) { Store (Arg0, Local0) Store (Local0, Local0) Return (Local0) }
Method (TEST, 0) { Return (HELP (_OSI ("CuBitProbeSupported"))) }
}
