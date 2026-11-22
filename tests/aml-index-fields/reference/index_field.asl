DefinitionBlock ("", "DSDT", 2, "CUBIT", "IDXFIELD", 1)
{
    OperationRegion (REG0, SystemIO, 0x1230, 2)
    Field (REG0, ByteAcc, NoLock, Preserve) { IDX0, 8, DAT0, 8 }
    IndexField (IDX0, DAT0, ByteAcc, NoLock, Preserve)
    {
        AccessAs (ByteAcc),
        F000, 8,
        F001, 8,
        Offset (3),
        F003, 8,
        F040, 4,
        F044, 4
    }
    Method (TST0, 0)
    {
        Store (0xA5, DAT0)
        Store (F003, Local0)
        Return (Local0 | (IDX0 << 8) | (DAT0 << 16))
    }
    Method (TST1, 0)
    {
        Store (0x5A, F001)
        Return (IDX0 | (DAT0 << 8))
    }
    Method (TST2, 0)
    {
        Store (0xA5, DAT0)
        Store (3, F044)
        Return (IDX0 | (DAT0 << 8))
    }
    IndexField (IDX0, DAT0, ByteAcc, NoLock, Preserve) { Offset (256), BIG0, 8 }
    IndexField (IDX0, DAT0, ByteAcc, Lock, Preserve) { LCK0, 8 }
    Method (RDM0, 0) { Return (F003) }
    Method (WRM0, 0) { Store (3, F044) Return (Zero) }
    Method (WFM0, 0) { Store (0x5A, F001) Return (Zero) }
    Method (RDB0, 0) { Return (BIG0) }
    Method (RDL0, 0) { Return (LCK0) }
    Method (DYN0, 0, Serialized)
    {
        IndexField (IDX0, DAT0, ByteAcc, NoLock, Preserve) { Offset (2), TMP0, 8 }
        Return (TMP0)
    }
}
