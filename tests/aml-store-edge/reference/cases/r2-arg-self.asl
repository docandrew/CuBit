DefinitionBlock ("", "SSDT", 2, "CUBIT", "STOREEDG", 1) {
External (_OSI, MethodObj)
Method (HELP, 1) { Store (Arg0, Arg0) Return (Arg0) }
Method (TEST, 0) { Return (HELP (_OSI ("CuBitProbeSupported"))) }
}
