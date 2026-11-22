DefinitionBlock ("", "DSDT", 1, "CUBIT", "RETYPE", 1)
{
 Name (MARK, Zero)
 Name (TYPE, Zero)
 Method (CNT0, 1) { Increment (MARK) Store (Buffer (3) {0xA1,0xB2,0xC3}, Arg0) Return (2) }
 Method (OBS0, 0) { Return (MARK) }
 Method (OBS1, 0) { Return (TYPE) }
 Method (TEST, 0) { Store (Zero, MARK) Store (Zero, TYPE)
 Name (P000, Package (2) {Buffer (CNT0 (RefOf (P000))) {}, One})
 Store (ObjectType (P000), TYPE)
 Return (P000) }
}
