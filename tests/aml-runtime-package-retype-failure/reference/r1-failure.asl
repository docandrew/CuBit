DefinitionBlock ("", "DSDT", 1, "CUBIT", "RTFAIL", 1)
{
 Name (MARK, Zero) Name (SEEN, Zero) Name (HOLD, Zero)
 Name (DIV0, Zero) Name (DONE, Zero)
 Method (CNT0, 1) { Increment (MARK) Store (0x55, Arg0)
   Store (DerefOf (Arg0), SEEN) CopyObject (Arg0, HOLD) Return (2) }
 Method (LATE, 0) { Increment (MARK) Return (Divide (One, DIV0)) }
 Method (TEST, 0) { Store (Zero, MARK) Store (Zero, SEEN) Store (Zero, DONE)
   Name (P000, Package (2) {Buffer (CNT0 (RefOf (P000))) {}, Buffer (LATE ()) {}})
   Store (One, DONE) Return (P000) }
 Method (OBS0, 0) { Return (MARK) }
 Method (OBS1, 0) { Return (SEEN) }
 Method (OBS2, 0) { Return (DONE) }
 Method (READ, 0) { Return (DerefOf (HOLD)) }
}
