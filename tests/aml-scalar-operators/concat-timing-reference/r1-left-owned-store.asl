DefinitionBlock ("", "DSDT", 1, "CUBIT", "CATTIME ", 1)
{
 Name (SRC0, 0x11)
 Name (PKG0, Package (1) {Zero})
 Method (CTRL, 0) { Return (0xA5) }
 Method (WIDT, 0) { Return (Ones) }
 Method (SRCV, 0) { Return (SRC0) }
 Method (TVAL, 0) { Return (DerefOf (Index (PKG0, Zero))) }
 Method (MUTR, 0) { Store (0x22, SRC0) Return (0x33) }
 Method (MUTT, 0) { Store (0x22, SRC0) Return (Zero) }
 Method (TEST, 0) {
  CopyObject (0x11, SRC0)
  Return (Concatenate (DerefOf (RefOf (SRC0)), MUTR (), Index (PKG0, Zero)))
 }
}
