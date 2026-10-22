DefinitionBlock ("", "DSDT", 2, "CUBIT", "NAMETIME", 1)
{
 Name (SRC0, 0x11)
 Name (BUF0, Buffer (2) {0,0})
 Method (MUT0, 0) { CopyObject (0x22, SRC0) Return (One) }
 Method (TEST, 0) {
  CopyObject (0x11, SRC0)
  Store (Zero, Index (BUF0, One))
  Store (SRC0, Index (BUF0, MUT0 ()))
  Return (Or (ShiftLeft (SRC0, 8), DerefOf (Index (BUF0, One))))
 }
}
