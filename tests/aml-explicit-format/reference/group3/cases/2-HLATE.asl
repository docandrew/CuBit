DefinitionBlock ("", "DSDT", 2, "CUBIT", "FMTLATE", 1)
{
 Name (MARK, Zero)
 Name (PKG0, Package () {Zero})
 Method (CTRL, 0) {Return (One)}
 Method (WIDT, 0) {Return (ShiftRight (Ones, 32))}
 Method (TGT0, 1) {Store (0x22, Arg0) Store (0x22, MARK) Return (Zero)}
 Method (TEST, 0)
 {
   Store (0x11, Local0)
   Return (ToBuffer (ToHexString (Local0, Index (PKG0, TGT0 (RefOf (Local0))))))
 }
}
