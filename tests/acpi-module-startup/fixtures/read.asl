DefinitionBlock ("", "DSDT", 2, "CUBIT", "MODSTART", 1)
{
 Name (MARK, Zero)
 OperationRegion (REG0, EmbeddedControl, Zero, 2)
 Field (REG0, ByteAcc, NoLock, Preserve) { FLD1, 8, FLD2, 8 }
 Store (0x33, FLD1)
 Store (FLD1, FLD2)
 Store (0x44, FLD1)
 Method (_INI, 0) { Store (0x5A, MARK) }
}
