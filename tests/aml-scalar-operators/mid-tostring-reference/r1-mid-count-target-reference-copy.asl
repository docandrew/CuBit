DefinitionBlock ("", "DSDT", 1, "CUBIT", "MIDTIME", 1) {
 Name (SRC0, One) Name (FLG0, Zero) Name (DST0, Package (1) {Zero})
 Method (TARG, 0) { Increment (FLG0) Store (0xA1, Debug) CopyObject (3, SRC0) Return (Zero) }
 Method (LATE, 0) { Increment (FLG0) Store (0xA2, Debug) CopyObject (3, SRC0) Return (3) }
 Method (TEST, 0) { Store (One, SRC0) Store (Zero, FLG0) Return (Mid ("ABCDEFGH", One, DerefOf (RefOf (SRC0)), Index (DST0, TARG ()))) }
 Method (POST, 0) { Return (SRC0) } Method (FLAG, 0) { Return (FLG0) }
 Method (DEST, 0) { Return (DerefOf (Index (DST0, Zero))) }
}
