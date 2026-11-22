DefinitionBlock ("", "SSDT", 2, "CUBIT", "DEREFWID", 1) {
External (_OSI, MethodObj)
Method (HELP, 1) { Store (RefOf (Arg0), Local1) If (DerefOf (Local1)) { Noop } Return (Arg0) }
Method (TEST, 0) { Return (HELP (_OSI ("CuBitProbeSupported"))) }
}
