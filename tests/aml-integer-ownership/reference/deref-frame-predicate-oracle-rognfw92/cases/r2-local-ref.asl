DefinitionBlock ("", "SSDT", 2, "CUBIT", "DEREFWID", 1) {
External (_OSI, MethodObj)
Method (HELP, 1) { Store (Arg0, Local0) If (DerefOf (RefOf (Local0))) { Noop } Return (Local0) }
Method (TEST, 0) { Return (HELP (_OSI ("CuBitProbeSupported"))) }
}
