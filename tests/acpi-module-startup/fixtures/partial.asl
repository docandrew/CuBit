DefinitionBlock ("", "DSDT", 2, "CUBIT", "MODDATUM", 1)
{
 Name (OUT0, Zero)
 OperationRegion (REG0, EmbeddedControl, Zero, 2)
 Field (REG0, ByteAcc, NoLock, Preserve) { FLD1, 8, FLD2, 8 }
 Field (REG0, ByteAcc, NoLock, Preserve) { , 4, PAIR, 8 }
 Method (_INI, 0) {
   Store (5, FLD1) Store (0xE0, FLD2)
   Store (0xAB, PAIR) Store (PAIR, OUT0)
 }
}
