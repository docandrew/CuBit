DefinitionBlock ("", "SSDT", 2, "CUBIT", "STOREEDG", 1) {
External (_OSI, MethodObj)
Method (HELP, 1) { Store (Arg0, Local0) Return (Store (Local0, Local0)) }
Method (TEST, 0) { Return (HELP (_OSI ("CuBitProbeSupported"))) }
}
