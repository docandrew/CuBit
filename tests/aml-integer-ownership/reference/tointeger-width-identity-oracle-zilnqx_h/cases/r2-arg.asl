DefinitionBlock ("", "SSDT", 2, "CUBIT", "TOINTWID", 1) {
External (_OSI, MethodObj)
Method (HELP, 1) { If (ToInteger (Arg0)) { Noop } Return (Arg0) }
Method (TEST, 0) { Return (HELP (_OSI ("CuBitProbeSupported"))) }
}
