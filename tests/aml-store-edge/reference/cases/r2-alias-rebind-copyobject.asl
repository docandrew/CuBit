DefinitionBlock ("", "SSDT", 2, "CUBIT", "STOREEDG", 1) {
External (_OSI, MethodObj)
Name (SRC0, 0x11) Alias (SRC0, ALS0)
Name (FLAG, Zero) Name (BUF0, Buffer (0x02) {Zero, Zero})
Method (INIT, 0) { CopyObject (0x11, SRC0) Store (Zero, FLAG) Store (Zero, Index (BUF0, One)) Return (Zero) }
Method (MUT0, 0) { Increment (FLAG) CopyObject (0x22, SRC0) Return (One) }
Method (TEST, 0) { Store (DerefOf (RefOf (ALS0)), Index (BUF0, MUT0 ())) Return (DerefOf (Index (BUF0, One))) }
Method (POST, 0) { Return (SRC0) } Method (FLG0, 0) { Return (FLAG) } Method (BVAL, 0) { Return (BUF0) }
}
