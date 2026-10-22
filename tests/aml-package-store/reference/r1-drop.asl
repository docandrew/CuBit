DefinitionBlock ("", "SSDT", 1, "CUBIT", "OWNTRANS", 1) {
External (_OSI, MethodObj)
Name (PKG0, Package () {Zero}) Name (FLAG, Zero)
Method (INIT, 0) { CopyObject (Package () {Zero}, PKG0) Store (Zero, FLAG) Store (_OSI ("CuBitProbeSupported"), Index (PKG0, Zero)) Return (Zero) }
Method (PASS, 1) { Increment (FLAG) CopyObject (Package () {Zero}, PKG0) Return (Arg0) }
Method (RAW0, 0) { Return (PASS (Index (PKG0, Zero))) }
Method (TEST, 0) { Store (PASS (Index (PKG0, Zero)), Local0) Return (Local0) }
Method (EXPR, 0) { Return (Store (PASS (Index (PKG0, Zero)), Local0)) }
Method (POST, 0) { Return (PKG0) } Method (FLG0, 0) { Return (FLAG) }
}
