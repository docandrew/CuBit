DefinitionBlock ("", "DSDT", 2, "CUBIT", "BANKTEST", 1)
{
    Name (CNT0, Zero)
    Name (SEL0, 3)
    Name (SEEN, Zero)
    Method (BVAL, 0)
    {
        Increment (CNT0)
        Return (SEL0)
    }
    OperationRegion (REG0, SystemMemory, 0x1000, 4)
    Field (REG0, ByteAcc, NoLock, Preserve) { BNK0, 8, DAT0, 8, DAT1, 8 }
    BankField (REG0, BNK0, BVAL (), ByteAcc, NoLock, Preserve)
    {
        Offset (1), VAL0, 8,
        VAL1, 8
    }
    Store (CNT0, SEEN)
    Store (7, SEL0)
    Method (INIT, 0) { Store (0xA5, DAT0) Store (0x5A, DAT1) Return (Zero) }
    Method (READ, 0) { Return (VAL0) }
    Method (RDN1, 0) { Return (VAL1) }
    Method (MOVE, 0) { Store (9, SEL0) Return (CNT0) }
    Method (WRIT, 0) { Store (0xC3, VAL0) Return (DAT0) }
}
