DefinitionBlock ("", "DSDT", 2, "CUBIT", "DIVTIME ", 1)
{
 Name (SRC0, 3)
 Name (BUF0, Buffer (2) {0,0})
 Name (SEQN, Zero)
 Method (CTRL, 0) { Return (0xA5) }
 Method (WIDT, 0) { Return (Ones) }
 Method (SRCV, 0) { Return (SRC0) }
 Method (RVAL, 0) { Return (DerefOf (Index (BUF0, Zero))) }
 Method (QVAL, 0) { Return (DerefOf (Index (BUF0, One))) }
 Method (MRKV, 0) { Return (SEQN) }
 Method (RMUT, 0) {
  Store (0xA1, Debug)
  Store (Add (Multiply (SEQN, 10), One), SEQN)
  CopyObject (4, SRC0)
  Return (Zero)
 }
 Method (QMUT, 0) {
  Store (0xA2, Debug)
  Store (Add (Multiply (SEQN, 10), 2), SEQN)
  CopyObject (5, SRC0)
  Return (One)
 }
 Method (TEST, 0) {
  CopyObject (3, SRC0)
  Store (Zero, SEQN)
  Store (Zero, Index (BUF0, Zero))
  Store (Zero, Index (BUF0, One))
  Return (Divide (0x39, DerefOf (RefOf (SRC0)), Index (BUF0, RMUT ()), Index (BUF0, QMUT ())))
 }
}
