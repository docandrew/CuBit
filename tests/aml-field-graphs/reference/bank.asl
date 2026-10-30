DefinitionBlock ("", "DSDT", 2, "CUBIT", "FLDGRAPH", 1)
{
    OperationRegion (REG0, SystemMemory, 0x1000, 4)
    Field (REG0, ByteAcc, NoLock, Preserve)
    { SEL0, 8, IDX0, 8, DAT0, 8, MEM0, 8 }
    BankField (REG0, SEL0, 3, ByteAcc, NoLock, Preserve)
    { Offset (1), BIDX, 8, BDAT, 8 }
    IndexField (BIDX, BDAT, ByteAcc, NoLock, Preserve)
    { Offset (4), IVAL, 8 }
    BankField (REG0, BIDX, 5, ByteAcc, NoLock, Preserve)
    { Offset (3), BVAL, 8 }
    BankField (REG0, IVAL, 7, ByteAcc, NoLock, Preserve)
    { Offset (3), MVAL, 8 }
    Method (INIT, 0)
    { Store (0xA5, DAT0) Store (0x5A, MEM0) Return (Zero) }
    Method (RIND, 0) { Return (IVAL) }
    Method (RBAN, 0) { Return (BVAL) }
    Method (RMIX, 0) { Return (MVAL) }
}
