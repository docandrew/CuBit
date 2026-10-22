DefinitionBlock ("", "SSDT", 1, "CUBIT", "TOINTWID", 1) {
External (_OSI, MethodObj)
Method (HELP, 1) { ToInteger (Arg0, Local1) Return (Arg0) }
Method (TEST, 0) { Return (HELP (_OSI ("CuBitProbeSupported"))) }
}
