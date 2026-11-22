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
        Return (ObjectType (TMP0))
    }
    Method (MOVE, 0) { Store (7, SEL0) Return (CNT0) }
    Name (FLAG, Zero)
    If (FLAG)
    {
        OperationRegion (REGX, SystemMemory, 0x2000, 4)
        Field (REG0, ByteAcc, NoLock, Preserve) { Offset (3), BNKX, 8 }
    }
    Method (RERR, 0, Serialized)
    {
        BankField (REGX, BNK0, BVAL (), ByteAcc, NoLock, Preserve)
        { TMP0, 8 }
        Return (ObjectType (TMP0))
    }
    Method (BERR, 0, Serialized)
    {
        BankField (REG0, BNKX, BVAL (), ByteAcc, NoLock, Preserve)
        { TMP0, 8 }
        Return (ObjectType (TMP0))
    }
    Method (DUPL, 0, Serialized)
    {
        Local0 = Zero
        While (Local0 < 2)
        {
            BankField (REG0, BNK0, BVAL (), ByteAcc, NoLock, Preserve)
            { TMP0, 8 }
            Increment (Local0)
        }
        Return (Zero)
    }
    Name (DIV0, Zero)
    Method (FVAL, 0)
    {
        Increment (CNT0)
        Return (Divide (One, DIV0))
    }
    Method (FAIL, 0, Serialized)
    {
        BankField (REG0, BNK0, FVAL (), ByteAcc, NoLock, Preserve)
        { TMP0, 8 }
        Return (ObjectType (TMP0))
    }
}
