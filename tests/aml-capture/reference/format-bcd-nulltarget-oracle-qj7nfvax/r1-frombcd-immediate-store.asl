DefinitionBlock ("", "DSDT", 1, "CUBIT", "FMTBCD", 1) {
 Name (SRC0, 0x12) Name (FLG0, Zero) Name (DST0, Package (1) {Zero})
 Method (TARG, 0) { Store (One, FLG0) Store (0xA1, Debug) Store (0x34, SRC0) Return (Zero) }
 Method (PRE0, 0) { Return (SRC0) }
 Method (TEST, 0) {  Return (FromBCD (DerefOf (RefOf (SRC0)))) }
 Method (POST, 0) { Return (SRC0) }
 Method (FLAG, 0) { Return (FLG0) }
 Method (DEST, 0) { Return (DerefOf (Index (DST0, Zero))) }
}
