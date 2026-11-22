DefinitionBlock ("", "DSDT", 1, "CUBIT", "RESTIME ", 1)
{
 Name (SRC0, Buffer (6) {0x84,1,0,0x11,0x79,0})
 Name (PKG0, Package (1) {Zero})
 Name (SEQN, Zero)
 Method (CTRL, 0) { Return (0xA5) }
 Method (WIDT, 0) { Return (Ones) }
 Method (SRCV, 0) { Return (SRC0) }
 Method (TVAL, 0) { Return (DerefOf (Index (PKG0, Zero))) }
 Method (MRKV, 0) { Return (SEQN) }
 Method (MUTR, 0) {
  Store (0xA1, Debug)
  Store (Add (Multiply (SEQN, 10), One), SEQN)
  CopyObject (Buffer (6) {0x84,1,0,0x22,0x79,0}, SRC0)
  Return (Buffer (6) {0x84,1,0,0x33,0x79,0})
 }
 Method (MUTT, 0) {
  Store (0xA2, Debug)
  Store (Add (Multiply (SEQN, 10), 2), SEQN)
  CopyObject (Buffer (6) {0x84,1,0,0x22,0x79,0}, SRC0)
  Return (Zero)
 }
 Method (TEST, 0) {
  CopyObject (Buffer (6) {0x84,1,0,0x11,0x79,0}, SRC0)
  Store (Zero, SEQN)
  Return (ConcatenateResTemplate (DerefOf (RefOf (SRC0)), MUTR (), Index (PKG0, MUTT ())))
 }
}
