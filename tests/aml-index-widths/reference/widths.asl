DefinitionBlock ("", "DSDT", 2, "CUBIT", "IDXWIDTH", 1)
{
    OperationRegion (REG0, SystemMemory, 0x1000, 32)
    Field (REG0, ByteAcc, NoLock, Preserve)
    {
        IDX0, 8,
        Offset (8), DATB, 8,
        AccessAs (WordAcc), Offset (10), DATW, 16,
        AccessAs (DWordAcc), Offset (12), DATD, 32,
        AccessAs (QWordAcc), Offset (16), DATQ, 64
    }
    IndexField (IDX0, DATB, AnyAcc, NoLock, Preserve) { Offset (3), ANY0, 8 }
    IndexField (IDX0, DATW, WordAcc, NoLock, Preserve) { Offset (3), WRD0, 16 }
    IndexField (IDX0, DATD, DWordAcc, NoLock, Preserve) { Offset (3), DWR0, 32 }
    IndexField (IDX0, DATQ, QWordAcc, NoLock, Preserve) { Offset (7), QWR0, 64 }
    IndexField (IDX0, DATW, WordAcc, NoLock, WriteAsOnes) { Offset (5), ONE0, 4 }
    IndexField (IDX0, DATW, WordAcc, NoLock, WriteAsZeros) { Offset (5), ZER0, 4 }
    Method (IN00, 0)
    {
        Store (Zero, IDX0)
        Store (0xA5, DATB)
        Store (0xA5C3, DATW)
        Store (0xA5C3E187, DATD)
        Store (Buffer () { 0x18, 0x37, 0x56, 0x75, 0x94, 0xB3, 0xD2, 0xF1 }, DATQ)
        Return (Zero)
    }
    Method (RAB0, 0) { Return (ANY0) }
    Method (RAW0, 0) { Return (WRD0) }
    Method (RAD0, 0) { Return (DWR0) }
    Method (RAQ0, 0) { Return (QWR0) }
    Method (WWR0, 0) { Store (0x1234, WRD0) Return (DATW) }
    Method (WDR0, 0) { Store (0x12345678, DWR0) Return (DATD) }
    Method (WQR0, 0)
    {
        Store (Buffer () { 0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88 }, QWR0)
        Return (DATQ)
    }
    Method (WON0, 0) { Store (3, ONE0) Return (Zero) }
    Method (WZE0, 0) { Store (3, ZER0) Return (Zero) }
}
