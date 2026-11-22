DefinitionBlock ("", "DSDT", 2, "CUBIT", "CPYINDEX", 1) {
Name (BUF0, Package (1) {0x41})
Name (DST0, Zero)
Name (MARK, Zero)
Method (CTRL, 0) { Return (One) }
Method (WIDT, 0) { Return (ShiftRight (Ones, 32)) }
Method (TEST, 0) {
 CopyObject (Index (BUF0, Zero), DST0)
 Store (One, MARK)
 Store (0x42, Index (BUF0, Zero))
 Store (0x02, MARK)
 Store (DerefOf (DST0), Local1)
 Store (0x03, MARK)
 Return (Local1)
}
}
