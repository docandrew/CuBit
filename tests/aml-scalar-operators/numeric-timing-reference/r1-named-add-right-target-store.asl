DefinitionBlock ("", "DSDT", 1, "CUBIT", "NUMTIME ", 1)
{
 Name (SRC0, 0x11)
 Name (BUF0, Buffer (2) {0,0})
 Method (CTRL, 0) { Return (0xA5) }
 Method (WIDT, 0) { Return (Ones) }
 Method (SRCV, 0) { Return (SRC0) }
 Method (BUFV, 0) { Return (DerefOf (Index (BUF0, One))) }
 Method (MUT0, 0) { Store (0x22, SRC0) Return (One) }
 Method (TEST, 0) {
  CopyObject (0x11, SRC0)
  Store (Zero, Index (BUF0, One))
  Return (Add (One, SRC0, Index (BUF0, MUT0 ())))
 }
}
