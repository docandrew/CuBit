DefinitionBlock ("", "DSDT", 2, "CUBIT", "BNKINDEX", 1)
{
    OperationRegion (REG0, SystemMemory, 0x1000, 4)
    Field (REG0, ByteAcc, NoLock, Preserve) { IDX0, 8, DAT0, 8, MDAT, 8 }
    IndexField (IDX0, DAT0, ByteAcc, NoLock, Preserve) { Offset (4), BNK0, 8 }
    BankField (REG0, BNK0, 3, ByteAcc, NoLock, Preserve) { Offset (2), VAL0, 8, }
    BankField (REG0, BNK0, 3, ByteAcc, NoLock, Preserve) { Offset (2), PART, 4, }
    Method (IN00, 0) { Store (0xA5, MDAT) Return (Zero) }
    Method (READ, 0) { Return (VAL0) }
    Method (WRIT, 0) { Store (6, PART) Return (Zero) }
    Method (FULL, 0) { Store (0x5A, VAL0) Return (Zero) }
}
