DefinitionBlock ("", "DSDT", 1, "CUBIT", "FMTBCD", 1) {
 Name (SRC0, 0x11) Name (FLG0, Zero) Name (DST0, Package (1) {Zero})
 Method (TARG, 0) { Store (One, FLG0) Store (0xA1, Debug) CopyObject (0x22, SRC0) Return (Zero) }
 Method (PRE0, 0) { Return (SRC0) }
 Method (TEST, 0) {  Return (ToHexString (DerefOf (RefOf (SRC0)), Index (DST0, TARG ()))) }
 Method (POST, 0) { Return (SRC0) }
 Method (FLAG, 0) { Return (FLG0) }
 Method (DEST, 0) { Return (DerefOf (Index (DST0, Zero))) }
}
