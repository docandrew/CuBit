DefinitionBlock ("", "DSDT", 2, "CUBIT", "IDXBULK", 1)
{
    OperationRegion (REG0, SystemMemory, 0x1000, 16)
    Field (REG0, QWordAcc, NoLock, Preserve) { IDX0, 64 }
    Field (REG0, ByteAcc, NoLock, Preserve) { Offset (8), DAT0, 8 }
    IndexField (IDX0, DAT0, ByteAcc, NoLock, Preserve) { F512, 4096 }
    IndexField (IDX0, DAT0, ByteAcc, NoLock, Preserve) { F4K0, 32768 }
    IndexField (IDX0, DAT0, ByteAcc, NoLock, Preserve) { F16K, 131072 }
    Method (INIT, 0) { Store (0xEF, DAT0) Return (Zero) }
    Method (R512, 0) { Return (F512) }
    Method (R4K0, 0) { Return (F4K0) }
    Method (R16K, 0) { Return (F16K) }
}
