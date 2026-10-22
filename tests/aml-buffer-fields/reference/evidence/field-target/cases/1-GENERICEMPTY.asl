DefinitionBlock ("", "DSDT", 1, "CUBIT", "CPYFIELD", 1) {
Name (SRC0, Package (1) {})
Name (TBUF, Buffer (2) {0xAA, 0xBB})
Name (MARK, Zero)
Method (CTRL, 0) { Return (One) }
Method (WIDT, 0) { Return (ShiftRight (Ones, 32)) }
Method (TEST, 0) {
 CreateField (TBUF, Zero, 8, FLD0)
 Store (One, MARK)
 CopyObject (Index (SRC0, Zero), FLD0)
 Store (0x02, MARK)
 Store (ToInteger (FLD0), Local0)
 Store (0x42, Index (SRC0, Zero))
 Store (0x03, MARK)
 Store (ToInteger (FLD0), Local1)
 Store (DerefOf (Index (TBUF, Zero)), Local2)
 Store (0x04, MARK)
 Return (Add (Multiply (ObjectType (FLD0), 0x1000000), Add (Multiply (Local0, 0x10000), Add (Multiply (Local1, 0x100), Local2))))
}
}
