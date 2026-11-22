DefinitionBlock ("", "SSDT", 2, "CUBIT", "STOREEDG", 1) {
External (_OSI, MethodObj)
Name (BEFR, Zero) Name (FLAG, Zero) Name (AFTR, Zero)
Name (BUF0, Buffer (One) {Zero})
Method (INIT, 0) { Store (Zero, BEFR) Store (Zero, FLAG) Store (Zero, AFTR) Store (Zero, Index (BUF0, Zero)) Return (Zero) }
Method (SRC0, 0) { Increment (BEFR) Return (_OSI ("CuBitProbeSupported")) }
Method (TARG, 0) { Increment (FLAG) Return (0x02) }
Method (TEST, 0) { Store (SRC0 (), Index (BUF0, TARG ())) Store (One, AFTR) Return (Zero) }
Method (PRE0, 0) { Return (BEFR) } Method (POST, 0) { Return (FLAG) }
Method (AFT0, 0) { Return (AFTR) } Method (BVAL, 0) { Return (BUF0) }
}
