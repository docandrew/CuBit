DefinitionBlock ("", "SSDT", 2, "CUBIT", "DEREFWID", 1) {
External (_OSI, MethodObj)
Method (HELP, 1) { If (DerefOf (RefOf (Arg0))) { Noop } Return (Arg0) }
Method (TEST, 0) { Return (HELP (_OSI ("CuBitProbeSupported"))) }
}
