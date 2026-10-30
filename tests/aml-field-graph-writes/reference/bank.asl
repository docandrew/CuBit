DefinitionBlock ("", "DSDT", 2, "CUBIT", "FLDWRITE", 1)
{
    OperationRegion (REG0, SystemMemory, 0x1000, 4)
    Field (REG0, ByteAcc, NoLock, Preserve)
    { SEL0, 8, IDX0, 8, DAT0, 8, MEM0, 8 }
    BankField (REG0, SEL0, 3, ByteAcc, NoLock, Preserve)
    { Offset (1), BIDX, 8, BDAT, 8 }
    IndexField (BIDX, BDAT, ByteAcc, NoLock, Preserve)
    { Offset (4), IVAL, 8 }
    IndexField (BIDX, BDAT, ByteAcc, NoLock, Preserve)
    { Offset (4), IPAR, 4 }
    BankField (REG0, BIDX, 5, ByteAcc, NoLock, Preserve)
    { Offset (3), BVAL, 8 }
    BankField (REG0, BIDX, 5, ByteAcc, NoLock, Preserve)
    { Offset (3), BPAR, 4 }
    BankField (REG0, IVAL, 7, ByteAcc, NoLock, Preserve)
    { Offset (3), MVAL, 8 }
    BankField (REG0, IVAL, 7, ByteAcc, NoLock, Preserve)
    { Offset (3), MPAR, 4 }
    IndexField (BIDX, BDAT, ByteAcc, NoLock, WriteAsOnes) { Offset (4), ION0, 4 }
    IndexField (BIDX, BDAT, ByteAcc, NoLock, WriteAsZeros) { Offset (4), IZR0, 4 }
    BankField (REG0, BIDX, 5, ByteAcc, NoLock, WriteAsOnes) { Offset (3), BON0, 4 }
    BankField (REG0, BIDX, 5, ByteAcc, NoLock, WriteAsZeros) { Offset (3), BZR0, 4 }
    BankField (REG0, IVAL, 7, ByteAcc, NoLock, WriteAsOnes) { Offset (3), MON0, 4 }
    BankField (REG0, IVAL, 7, ByteAcc, NoLock, WriteAsZeros) { Offset (3), MZR0, 4 }
    Method (INIT, 0) { Store (0xA5, DAT0) Store (0x5A, MEM0) Return (Zero) }
    Method (WIND, 0) { Store (0xC3, IVAL) Return (Zero) }
    Method (WBAN, 0) { Store (0xA6, BVAL) Return (Zero) }
    Method (WMIX, 0) { Store (0xC3, MVAL) Return (Zero) }
    Method (WPIN, 0) { Store (6, IPAR) Return (Zero) }
    Method (WPBA, 0) { Store (6, BPAR) Return (Zero) }
    Method (WPMI, 0) { Store (6, MPAR) Return (Zero) }    Method (WION, 0) { Store (2, ION0) Return (Zero) }
    Method (WBON, 0) { Store (2, BON0) Return (Zero) }
    Method (WMON, 0) { Store (2, MON0) Return (Zero) }
    Method (WIZE, 0) { Store (2, IZR0) Return (Zero) }
    Method (WBZE, 0) { Store (2, BZR0) Return (Zero) }
    Method (WMZE, 0) { Store (2, MZR0) Return (Zero) }

}
