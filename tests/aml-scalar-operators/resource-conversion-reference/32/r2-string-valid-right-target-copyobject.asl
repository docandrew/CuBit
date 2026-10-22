DefinitionBlock ("", "DSDT", 2, "CUBIT", "RESTIME ", 1)
{
 Name (SRC0, "qAy")
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
  CopyObject ("qBy", SRC0)
  Return (Buffer (6) {0x84,1,0,0x33,0x79,0})
 }
 Method (MUTT, 0) {
  Store (0xA2, Debug)
  Store (Add (Multiply (SEQN, 10), 2), SEQN)
  CopyObject ("qBy", SRC0)
  Return (Zero)
 }
 Method (TEST, 0) {
  CopyObject ("qAy", SRC0)
  Store (Zero, SEQN)
  Return (ConcatenateResTemplate (Buffer (6) {0x84,1,0,0x33,0x79,0}, DerefOf (RefOf (SRC0)), Index (PKG0, MUTT ())))
 }
}
