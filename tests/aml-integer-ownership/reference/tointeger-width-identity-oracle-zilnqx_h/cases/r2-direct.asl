DefinitionBlock ("", "SSDT", 2, "CUBIT", "TOINTWID", 1) {
External (_OSI, MethodObj)
Method (TEST, 0) { Return (ToInteger (_OSI ("CuBitProbeSupported"))) }
}
