DefinitionBlock ("", "SSDT", 1, "CUBIT", "OSICOPY", 1) {
External (_OSI, MethodObj)
Name (PKG0, Package () {Zero})
Method (PRAW, 0) { Store (_OSI ("CuBitProbeSupported"), Index (PKG0, Zero)) Return (PKG0) }
Method (PDRE, 0) { Return (DerefOf (Index (PKG0, Zero))) }
Method (PAFT, 0) { Return (PKG0) }
Method (PLOC, 0) { Store (Package () {Zero}, Local0) Store (_OSI ("CuBitProbeSupported"), Index (Local0, Zero)) Return (Local0) }
Method (CLOC, 0) { CopyObject (_OSI ("CuBitProbeSupported"), Local0) Return (Local0) }
Method (CEXP, 0) { Return (CopyObject (_OSI ("CuBitProbeSupported"), Local0)) }
Method (CPSR, 1) { CopyObject (Arg0, Local0) Return (Arg0) }
Method (CPDS, 1) { CopyObject (Arg0, Local0) Return (Local0) }
Method (CSRC, 0) { Return (CPSR (_OSI ("CuBitProbeSupported"))) }
Method (CDST, 0) { Return (CPDS (_OSI ("CuBitProbeSupported"))) }
Method (PAS0, 1) { Return (Arg0) }
Method (PASS, 0) { Return (PAS0 (_OSI ("CuBitProbeSupported"))) }
Method (DRET, 0) { Return (_OSI ("CuBitProbeSupported")) }
}
