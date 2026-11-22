DefinitionBlock ("", "DSDT", 2, "CUBIT", "FLDDEREF", 1)
{
    DataTableRegion (REG0, "DSDT", "", "")
    Field (REG0, ByteAcc, NoLock, Preserve) { FLD0, 8 }
    Method (DIR0, 0) { Return (FLD0) }
    Method (IND0, 0) { CondRefOf (FLD0, Local0) Return (DerefOf (Local0)) }
    Method (AREF, 1) { CondRefOf (FLD0, Arg0) Return (DerefOf (Arg0)) }
    Method (REF0, 0) { Return (DerefOf (RefOf (FLD0))) }
}
