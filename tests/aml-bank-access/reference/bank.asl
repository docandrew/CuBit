DefinitionBlock ("", "DSDT", 2, "CUBIT", "BANKRUN", 1)
{
    Name (CNT0, Zero)
    Name (SEL0, 3)
    Method (BVAL, 0) { Increment (CNT0) Return (SEL0) }
    OperationRegion (REG0, SystemMemory, 0x1000, 4)
    Field (REG0, ByteAcc, NoLock, Preserve) { BNK0, 8, DAT0, 8, DAT1, 8 }
    Method (INIT, 0) { Store (0xA5, DAT0) Store (0x5A, DAT1) Return (Zero) }
    Method (DYNB, 0, Serialized)
    {
        BankField (REG0, BNK0, BVAL (), ByteAcc, NoLock, Preserve)
        { Offset (1), TMP0, 8, TMP1, 8 }
        Return (TMP0)
    }
    Method (MOVE, 0) { Store (7, SEL0) Return (CNT0) }
    Method (DYNW, 0, Serialized)
    {
        BankField (REG0, BNK0, BVAL (), ByteAcc, NoLock, Preserve)
        { Offset (1), TMP0, 8 }
        Store (0xC3, TMP0)
        Return (Zero)
    }
    Method (DYNP, 0, Serialized)
    {
        BankField (REG0, BNK0, BVAL (), ByteAcc, NoLock, Preserve)
        { Offset (1), TMP0, 4 }
        Store (6, TMP0)
        Return (Zero)
    }
    Method (DYNO, 0, Serialized)
    {
        BankField (REG0, BNK0, BVAL (), ByteAcc, NoLock, WriteAsOnes)
        { Offset (1), TMP0, 4 }
        Store (2, TMP0)
        Return (Zero)
    }
    Method (DYNZ, 0, Serialized)
    {
        BankField (REG0, BNK0, BVAL (), ByteAcc, NoLock, WriteAsZeros)
        { Offset (1), TMP0, 4 }
        Store (4, TMP0)
        Return (Zero)
    }
    Method (DYNM, 0, Serialized)
    {
        BankField (REG0, BNK0, BVAL (), ByteAcc, NoLock, Preserve)
        { Offset (1), , 4, TMP0, 12 }
        Store (0xABC, TMP0)
        Return (Zero)
    }
    Method (LOOP, 0, Serialized)
    {
        Local0 = Zero
        While (Local0 < 2)
        {
            BankField (REG0, BNK0, Local0, ByteAcc, NoLock, Preserve)
            { Offset (1), TMP0, 8, TMP1, 8 }
            Increment (Local0)
        }
        Return (TMP1)
    }
}
