DefinitionBlock ("", "DSDT", 2, "CUBIT", "BNKWIDTH", 1)
{
    OperationRegion (REG0, SystemMemory, 0x1000, 64)
    Field (REG0, ByteAcc, NoLock, Preserve)
    {
        BNK0, 8,
        Offset (8), DATB, 8,
        AccessAs (WordAcc), Offset (16), DAW0, 16, DAW1, 16,
        AccessAs (DWordAcc), Offset (24), DAD0, 32, DAD1, 32,
        AccessAs (QWordAcc), Offset (40), DAQ0, 64, DAQ1, 64
    }
    BankField (REG0, BNK0, 3, AnyAcc, NoLock, Preserve) { Offset (8), ANY0, 8 }
    BankField (REG0, BNK0, 3, WordAcc, NoLock, Preserve) { Offset (17), WRD0, 16 }
    BankField (REG0, BNK0, 3, DWordAcc, NoLock, Preserve) { Offset (25), DWR0, 32 }
    BankField (REG0, BNK0, 3, QWordAcc, NoLock, Preserve) { Offset (41), QWR0, 64 }
    BankField (REG0, BNK0, 3, WordAcc, NoLock, WriteAsOnes) { Offset (17), ONE0, 4 }
    BankField (REG0, BNK0, 3, WordAcc, NoLock, WriteAsZeros) { Offset (17), ZER0, 4 }
    Method (IN00, 0)
    {
        Store (Zero, BNK0)
        Store (0xA5, DATB)
        Store (0xA5C3, DAW0) Store (0x6B4D, DAW1)
        Store (0xA5C3E187, DAD0) Store (0x6B4D2F09, DAD1)
        Store (Buffer () { 0x18, 0x37, 0x56, 0x75, 0x94, 0xB3, 0xD2, 0xF1 }, DAQ0)
        Store (Buffer () { 0x09, 0x28, 0x47, 0x66, 0x85, 0xA4, 0xC3, 0xE2 }, DAQ1)
        Return (Zero)
    }
    Method (RAB0, 0) { Return (ANY0) }
    Method (RAW0, 0) { Return (WRD0) }
    Method (RAD0, 0) { Return (DWR0) }
    Method (RAQ0, 0) { Return (QWR0) }
    Method (WWR0, 0) { Store (0x1234, WRD0) Return (Zero) }
    Method (WDR0, 0) { Store (0x12345678, DWR0) Return (Zero) }
    Method (WQR0, 0)
    {
        Store (Buffer () { 0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88 }, QWR0)
        Return (Zero)
    }
    Method (WON0, 0) { Store (3, ONE0) Return (Zero) }
    Method (WZE0, 0) { Store (3, ZER0) Return (Zero) }
}
