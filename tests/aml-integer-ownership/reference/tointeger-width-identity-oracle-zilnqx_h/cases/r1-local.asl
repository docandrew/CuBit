DefinitionBlock ("", "SSDT", 1, "CUBIT", "TOINTWID", 1) {
External (_OSI, MethodObj)
Method (HELP, 1) { Store (Arg0, Local0) If (ToInteger (Local0)) { Noop } Return (Local0) }
Method (TEST, 0) { Return (HELP (_OSI ("CuBitProbeSupported"))) }
}
