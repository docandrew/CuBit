DefinitionBlock ("", "DSDT", 2, "CUBIT", "IDXDATA", 1)
{
    OperationRegion (REG0, SystemMemory, 0x1000, 16)
    Field (REG0, ByteAcc, NoLock, Preserve) { IDX0, 8 }
    Field (REG0, DWordAcc, NoLock, Preserve) { Offset (4), DAT0, 32 }
    IndexField (IDX0, DAT0, ByteAcc, NoLock, Preserve) { Offset (4), IBYT, 8 }
    IndexField (IDX0, DAT0, ByteAcc, NoLock, Preserve) { Offset (4), INIB, 4 }
    IndexField (IDX0, DAT0, WordAcc, NoLock, Preserve) { Offset (8), IWRD, 16 }
    IndexField (IDX0, DAT0, AnyAcc, NoLock, Preserve) { Offset (4), IANY, 8 }
    Method (INIT, 0) { Store (0xABCD1234, DAT0) Return (Zero) }
    Method (RB08, 0) { Return (IBYT) }
    Method (WB08, 0) { Store (0xC3, IBYT) Return (Zero) }
    Method (PB04, 0) { Store (6, INIB) Return (Zero) }
    Method (RB16, 0) { Return (IWRD) }
    Method (WB16, 0) { Store (0x5678, IWRD) Return (Zero) }
    Method (RANY, 0) { Return (IANY) }
    IndexField (IDX0, DAT0, ByteAcc, NoLock, Preserve)
    { Offset (4), , 4, U008, 8, U012, 12, U016, 16, U064, 64 }
    IndexField (IDX0, DAT0, ByteAcc, NoLock, Preserve)
    { Offset (4), A016, 16, A064, 64 }
    IndexField (IDX0, DAT0, QWordAcc, NoLock, Preserve)
    { Offset (8), Q064, 64 }
    IndexField (IDX0, DAT0, ByteAcc, NoLock, WriteAsOnes)
    { Offset (4), ION0, 4 }
    IndexField (IDX0, DAT0, ByteAcc, NoLock, WriteAsZeros)
    { Offset (4), IZR0, 4 }
    Method (RU08, 0) { Return (U008) }
    Method (RU12, 0) { Return (U012) }
    Method (RU16, 0) { Return (U016) }
    Method (RU64, 0) { Return (U064) }
    Method (RA16, 0) { Return (A016) }
    Method (RA64, 0) { Return (A064) }
    Method (RQ64, 0) { Return (Q064) }
    Method (WQ64, 0) { Store (0x1122334455667788, Q064) Return (Zero) }
    Method (WONE, 0) { Store (6, ION0) Return (Zero) }
    Method (WZER, 0) { Store (6, IZR0) Return (Zero) }
    IndexField (IDX0, DAT0, ByteAcc, NoLock, WriteAsOnes)
    { Offset (4), ION8, 8 }
    Method (WON8, 0) { Store (0xA5, ION8) Return (Zero) }
}
