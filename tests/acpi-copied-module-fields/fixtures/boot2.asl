DefinitionBlock ("", "DSDT", 2, "CUBIT", "MDBOOT", 1)
{
    OperationRegion (REG0, EmbeddedControl, 0, 2)
    Field (REG0, ByteAcc, NoLock, Preserve) { FLD1, 8, FLD2, 8 }
    Method (DUPL, 0) { Return (LoadTable ("SSDT", "CUBIT", "MDCOPY", "", "", Zero)) }
    Store (0x33, FLD1)
    Name (LRES, Zero)
    Store (LoadTable ("SSDT", "CUBIT", "MDCOPY", "", "", Zero), LRES)
}
