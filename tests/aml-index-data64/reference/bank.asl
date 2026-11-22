DefinitionBlock ("", "DSDT", 2, "CUBIT", "IDX64DAT", 1)
{
    OperationRegion (REG0, SystemMemory, 0x1000, 16)
    Field (REG0, ByteAcc, NoLock, Preserve) { IDX0, 8 }
    Field (REG0, QWordAcc, NoLock, Preserve) { Offset (8), DAT0, 64 }
    IndexField (IDX0, DAT0, ByteAcc, NoLock, Preserve) { Offset (4), IB08, 8 }
    IndexField (IDX0, DAT0, WordAcc, NoLock, Preserve) { Offset (8), IW16, 16 }
    IndexField (IDX0, DAT0, DWordAcc, NoLock, Preserve) { Offset (4), ID32, 32 }
    IndexField (IDX0, DAT0, QWordAcc, NoLock, Preserve) { Offset (8), IQ64, 64 }
    IndexField (IDX0, DAT0, QWordAcc, NoLock, Preserve) { , 4, IU64, 64 }
    IndexField (IDX0, DAT0, ByteAcc, NoLock, Preserve) { Offset (4), IP04, 4 }
    IndexField (IDX0, DAT0, ByteAcc, NoLock, WriteAsOnes) { Offset (4), IO04, 4 }
    IndexField (IDX0, DAT0, ByteAcc, NoLock, WriteAsZeros) { Offset (4), IZ04, 4 }
    Method (IN00, 0) { Store (Buffer () {0xEF,0xCD,0xAB,0x89,0x67,0x45,0x23,0x01}, DAT0) Return (Zero) }
    Method (RBYT, 0) { Return (IB08) }
    Method (RWRD, 0) { Return (IW16) }
    Method (RDWD, 0) { Return (ID32) }
    Method (RQWD, 0) { Return (IQ64) }
    Method (RUQW, 0) { Return (IU64) }
    Method (PBYT, 0) { Store (6, IP04) Return (Zero) }
    Method (WONE, 0) { Store (6, IO04) Return (Zero) }
    Method (WZER, 0) { Store (6, IZ04) Return (Zero) }
    Method (WBYT, 0) { Store (Buffer () {0xA5}, IB08) Return (Zero) }
    Method (WQWD, 0) { Store (Buffer () {0x88,0x77,0x66,0x55,0x44,0x33,0x22,0x11}, IQ64) Return (Zero) }
    Method (WUQW, 0) { Store (Buffer () {0x88,0x77,0x66,0x55,0x44,0x33,0x22,0x11}, IU64) Return (Zero) }
}
