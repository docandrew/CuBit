DefinitionBlock ("", "DSDT", 1, "CUBIT", "REGWIDE", 1)
{
    OperationRegion (REG0, SystemMemory, 0x1000, 32)
    Field (REG0, ByteAcc, NoLock, Preserve)
    { SEL0, 8, Offset (4), D000, 8, D001, 8, D002, 8, D003, 8, D004, 8,
      D005, 8, D006, 8, D007, 8, D008, 8, D009, 8, D010, 8 }
    Field (REG0, ByteAcc, NoLock, Preserve) { Offset (4), D080, 80 }
    Field (REG0, ByteAcc, NoLock, Preserve) { Offset (4), , 4, U080, 80 }
    Field (REG0, ByteAcc, NoLock, Preserve) { Offset (4), P073, 73 }
    Field (REG0, WordAcc, NoLock, Preserve) { Offset (4), W080, 80 }
    Field (REG0, QWordAcc, NoLock, Preserve) { Offset (4), Q080, 80 }
    Field (REG0, ByteAcc, NoLock, WriteAsOnes) { Offset (4), , 4, O080, 80 }
    Field (REG0, ByteAcc, NoLock, WriteAsZeros) { Offset (4), , 4, Z080, 80 }
    BankField (REG0, SEL0, 3, ByteAcc, NoLock, Preserve) { Offset (4), B080, 80 }
    BankField (REG0, SEL0, 3, ByteAcc, NoLock, Preserve) { Offset (4), , 4, BU80, 80 }
    BankField (REG0, SEL0, 3, ByteAcc, NoLock, Preserve) { Offset (4), BP73, 73 }
    BankField (REG0, SEL0, 3, WordAcc, NoLock, Preserve) { Offset (4), BW80, 80 }
    BankField (REG0, SEL0, 3, QWordAcc, NoLock, Preserve) { Offset (4), BQ80, 80 }
    BankField (REG0, SEL0, 3, ByteAcc, NoLock, WriteAsOnes) { Offset (4), , 4, BO80, 80 }
    BankField (REG0, SEL0, 3, ByteAcc, NoLock, WriteAsZeros) { Offset (4), , 4, BZ80, 80 }
    Method (INIT, 0)
    { Store (Zero, SEL0) Store (0x10, D000) Store (0x21, D001) Store (0x32, D002)
      Store (0x43, D003) Store (0x54, D004) Store (0x65, D005) Store (0x76, D006)
      Store (0x87, D007) Store (0x98, D008) Store (0xA9, D009) Store (0xBA, D010) Return (Zero) }
    Method (RD80, 0) { Return (D080) }
    Method (RU80, 0) { Return (U080) }
    Method (RP73, 0) { Return (P073) }
    Method (RW80, 0) { Return (W080) }
    Method (RQ80, 0) { Return (Q080) }
    Method (RB80, 0) { Return (B080) }
    Method (RBU0, 0) { Return (BU80) }
    Method (RBP3, 0) { Return (BP73) }
    Method (RBW0, 0) { Return (BW80) }
    Method (RBQ0, 0) { Return (BQ80) }
    Method (SD80, 0) { Store (Buffer () {0,1,2,3,4,5,6,7,8,9}, D080) Return (D080) }
    Method (SB80, 0) { Store (Buffer () {0,1,2,3,4,5,6,7,8,9}, B080) Return (D080) }
    Method (SDU0, 0) { Store (Buffer () {0,1,2,3,4,5,6,7,8,9}, U080) Return (D080) }
    Method (SBU0, 0) { Store (Buffer () {0,1,2,3,4,5,6,7,8,9}, BU80) Return (D080) }
    Method (SDO0, 0) { Store (Buffer () {0,1,2,3,4,5,6,7,8,9}, O080) Return (D080) }
    Method (SBO0, 0) { Store (Buffer () {0,1,2,3,4,5,6,7,8,9}, BO80) Return (D080) }
    Method (SDZ0, 0) { Store (Buffer () {0,1,2,3,4,5,6,7,8,9}, Z080) Return (D080) }
    Method (SBZ0, 0) { Store (Buffer () {0,1,2,3,4,5,6,7,8,9}, BZ80) Return (D080) }
    Method (SDIN, 0) { Store (0x12345678, D080) Return (D080) }
    Method (SBIN, 0) { Store (0x12345678, B080) Return (D080) }
    Method (SDSH, 0) { Store (Buffer () {0x12,0x34,0x56}, D080) Return (D080) }
    Method (SBSH, 0) { Store (Buffer () {0x12,0x34,0x56}, B080) Return (D080) }
    Method (SW80, 0) { Store (Buffer () {0,1,2,3,4,5,6,7,8,9}, W080) Return (D080) }
    Method (SQ80, 0) { Store (Buffer () {0,1,2,3,4,5,6,7,8,9}, Q080) Return (D080) }
    Method (SBW0, 0) { Store (Buffer () {0,1,2,3,4,5,6,7,8,9}, BW80) Return (D080) }
    Method (SBQ0, 0) { Store (Buffer () {0,1,2,3,4,5,6,7,8,9}, BQ80) Return (D080) }
}
