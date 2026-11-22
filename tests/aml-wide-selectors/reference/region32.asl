DefinitionBlock ("", "DSDT", 1, "CUBIT", "WIDESEL", 1)
{
    OperationRegion (REG0, SystemMemory, 0x1000, 32)
    Field (REG0, QWordAcc, NoLock, Preserve)
    { Q000, 64, Q001, 64, Q002, 64, Q003, 64 }
    Field (REG0, QWordAcc, NoLock, Preserve) { , 3, DSEL, 73 }
    Field (REG0, ByteAcc, NoLock, Preserve) { Offset (16), DAT0, 8, Offset (20), BDAT, 8 }
    IndexField (DSEL, DAT0, ByteAcc, NoLock, Preserve) { Offset (4), ISEL, 80 }
    BankField (REG0, DSEL, 0x12345678, QWordAcc, NoLock, Preserve) { Offset (16), VAL0, 8 }
    BankField (REG0, ISEL, 3, ByteAcc, NoLock, Preserve) { Offset (20), NEST, 8 }
    Field (REG0, QWordAcc, NoLock, Preserve) { Offset (16), WDAT, 80 }
    IndexField (DSEL, WDAT, ByteAcc, NoLock, Preserve) { Offset (4), IVAL, 8 }
    IndexField (DSEL, WDAT, ByteAcc, NoLock, Preserve) { Offset (4), , 3, IPRE, 3 }
    IndexField (DSEL, WDAT, ByteAcc, NoLock, WriteAsOnes) { Offset (4), , 3, IONE, 3 }
    IndexField (DSEL, WDAT, ByteAcc, NoLock, WriteAsZeros) { Offset (4), , 3, IZER, 3 }
    Method (INIT, 0)
    {
        Store (0xA3A2A1A0, Q000)
        Store (0xABAAA9A8, Q001)
        Store (0xB3B2B1B0, Q002)
        Store (0xBBBAB9B8, Q003)
        Return (Zero)
    }
    Method (BRED, 0) { Return (VAL0) }
    Method (BWRT, 0) { Store (Buffer () {0x5A}, VAL0) Return (Zero) }
    Method (NRED, 0) { Return (NEST) }
    Method (NWRT, 0) { Store (Buffer () {0x5A}, NEST) Return (Zero) }
    Method (WFUL, 0) { Store (Buffer () {0x5A}, IVAL) Return (WDAT) }
    Method (WONE, 0) { Store (Buffer () {0x5A}, IONE) Return (WDAT) }
    Method (WZER, 0) { Store (Buffer () {0x5A}, IZER) Return (WDAT) }
    Method (WPRV, 0) { Store (3, IPRE) Return (Zero) }
    Method (WRED, 0) { Return (IVAL) }
}
