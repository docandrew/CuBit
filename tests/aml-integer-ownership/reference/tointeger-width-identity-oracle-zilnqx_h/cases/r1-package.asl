DefinitionBlock ("", "SSDT", 1, "CUBIT", "TOINTWID", 1) {
External (_OSI, MethodObj)
Name (PKG0, Package () {Zero})
Method (PRAW, 0) { Store (_OSI ("CuBitProbeSupported"), Index (PKG0, Zero)) Return (PKG0) }
Method (TEST, 0) { Return (ToInteger (Index (PKG0, Zero))) }
Method (PAFT, 0) { Return (PKG0) }
}
