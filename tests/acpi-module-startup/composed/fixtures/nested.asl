DefinitionBlock ("", "DSDT", 2, "CUBIT", "MODNEST", 1)
{
    Name (OUT0, Zero)
    OperationRegion (REG0, SystemMemory, 0x1000, 4)
    Field (REG0, ByteAcc, NoLock, Preserve) { IDX0, 8, DAT0, 8, MDAT, 8 }
    IndexField (IDX0, DAT0, ByteAcc, NoLock, Preserve) { Offset (4), BNK0, 8 }
    BankField (REG0, BNK0, 3, ByteAcc, NoLock, Preserve) { Offset (2), FVAL, 8 }
    BankField (REG0, BNK0, 3, ByteAcc, NoLock, Preserve) { Offset (2), PART, 4 }
#ifdef ORACLE
    Method (TEST, 0) {
        Store (0xA5, MDAT)
        Store (6, PART)
        Store (FVAL, OUT0)
        Return (OUT0)
    }
#else
    Store (6, PART)
    Store (FVAL, OUT0)
#endif
}
