DefinitionBlock ("", "SSDT", 1, "CUBIT", "OSISTORE", 1) {
External (_OSI, MethodObj)
Name (NVAL, Zero)
Name (PKG0, Package () {Zero})
Method (DRET, 0) { Return (_OSI ("CuBitProbeSupported")) }
Method (LOCL, 0) { Store (_OSI ("CuBitProbeSupported"), Local0) Return (Local0) }
Method (LEXP, 0) { Return (Store (_OSI ("CuBitProbeSupported"), Local0)) }
Method (ASUB, 1) { Store (_OSI ("CuBitProbeSupported"), Arg0) Return (Arg0) }
Method (ARGT, 0) { Return (ASUB (Zero)) }
Method (AXPR, 1) { Return (Store (_OSI ("CuBitProbeSupported"), Arg0)) }
Method (AEXP, 0) { Return (AXPR (Zero)) }
Method (RSLT, 0) { Store (Zero, Local0) Store (RefOf (Local0), Local1) Store (_OSI ("CuBitProbeSupported"), DerefOf (Local1)) Return (Local0) }
Method (RARG, 0) { Store (Zero, Local0) ASUB (RefOf (Local0)) Return (Local0) }
Method (NMST, 0) { Store (_OSI ("CuBitProbeSupported"), NVAL) Return (NVAL) }
Method (NEXP, 0) { Return (Store (_OSI ("CuBitProbeSupported"), NVAL)) }
Method (PIND, 0) { Store (_OSI ("CuBitProbeSupported"), Index (PKG0, Zero)) Return (DerefOf (Index (PKG0, Zero))) }
Method (SHRS, 1) { Store (Arg0, Local0) Return (Arg0) }
Method (SHRD, 1) { Store (Arg0, Local0) Return (Local0) }
Method (SRC0, 0) { Return (SHRS (_OSI ("CuBitProbeSupported"))) }
Method (DST0, 0) { Return (SHRD (_OSI ("CuBitProbeSupported"))) }
Method (PAS0, 1) { Return (Arg0) }
Method (PASS, 0) { Return (PAS0 (_OSI ("CuBitProbeSupported"))) }
}
