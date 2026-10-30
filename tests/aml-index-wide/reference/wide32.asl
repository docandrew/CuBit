DefinitionBlock ("", "DSDT", 1, "CUBIT", "IDXWIDE", 1)
{
    OperationRegion (REG0, SystemMemory, 0x1000, 16)
    Field (REG0, ByteAcc, NoLock, Preserve) { IDX0, 8, DAT0, 8 }
    IndexField (IDX0, DAT0, ByteAcc, NoLock, Preserve) { Offset (4), F080, 80 }
    IndexField (IDX0, DAT0, ByteAcc, NoLock, Preserve) { Offset (4), , 4, U080, 80 }
    IndexField (IDX0, DAT0, ByteAcc, NoLock, Preserve) { Offset (4), , 4, P073, 73 }
    IndexField (IDX0, DAT0, WordAcc, NoLock, Preserve) { Offset (4), W080, 80 }
    IndexField (IDX0, DAT0, QWordAcc, NoLock, Preserve) { , 4, Q080, 80 }
    IndexField (IDX0, DAT0, ByteAcc, NoLock, WriteAsOnes) { Offset (4), O080, 80 }
    IndexField (IDX0, DAT0, ByteAcc, NoLock, WriteAsZeros) { Offset (4), Z080, 80 }
    IndexField (IDX0, DAT0, ByteAcc, NoLock, Preserve) { HUGE, 524289 }
    Method (INIT, 0) { Store (0xEF, DAT0) Return (Zero) }
    Method (R080, 0) { Return (F080) }
    Method (RU80, 0) { Return (U080) }
    Method (RP73, 0) { Return (P073) }
    Method (RW80, 0) { Return (W080) }
    Method (RQ80, 0) { Return (Q080) }
    Method (WF80, 0) { Store (Buffer () {0,1,2,3,4,5,6,7,8,9}, F080) Return (DAT0) }
    Method (WP80, 0) { Store (Buffer () {0x10,0x21,0x32,0x43,0x54,0x65,0x76,0x87,0x98,0xA9}, U080) Return (DAT0) }
    Method (WO80, 0) { Store (Buffer () {0,1,2,3,4,5,6,7,8,9}, O080) Return (DAT0) }
    Method (WZ80, 0) { Store (Buffer () {0,1,2,3,4,5,6,7,8,9}, Z080) Return (DAT0) }
    Method (WS80, 0) { Store (0x12345678, F080) Return (DAT0) }
    Method (WB80, 0) { Store (Buffer () {0x12,0x34,0x56}, F080) Return (DAT0) }
    Method (RMAX, 0) { Return (HUGE) }
    Method (ALOC, 0, Serialized) { Name (LBUF, Buffer (65536) {}) If (SizeOf (LBUF) != 65536) { Return (Zero) } Store (0x77, IDX0) Return (F080) }
}
