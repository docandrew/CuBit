DefinitionBlock ("", "DSDT", 2, "CUBIT", "MODWIDE", 1)
{
    Name (OUT0, Buffer (9) {})
    OperationRegion (REG0, SystemMemory, 0x1000, 11)
    Field (REG0, ByteAcc, NoLock, Preserve) { SEL0, 8, DAT0, 8 }
#ifdef INDEX
    IndexField (SEL0, DAT0, ByteAcc, NoLock, Preserve) { , 4, FVAL, 72 }
#else
#ifdef BANK
    BankField (REG0, SEL0, 3, ByteAcc, NoLock, Preserve) { Offset (1), , 4, FVAL, 72 }
#else
    Field (REG0, ByteAcc, NoLock, Preserve) { Offset (1), , 4, FVAL, 72 }
#endif
#endif
#ifdef ORACLE
    Field (REG0, ByteAcc, NoLock, Preserve) { Offset (1), RAWB, 80 }
    Method (TEST, 0) {
        Store (Buffer (10) {0xA5,0xA5,0xA5,0xA5,0xA5,0xA5,0xA5,0xA5,0xA5,0xA5}, RAWB)
#endif
#ifdef SCALAR
        Store (0xFEDCBA9876543210, FVAL)
#else
        Store (Buffer (9) {0x88,0x77,0x66,0x55,0x44,0x33,0x22,0x11,0xFE}, FVAL)
#endif
        Store (FVAL, OUT0)
#ifdef ORACLE
        Return (OUT0)
    }
#endif
}
