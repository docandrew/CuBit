DefinitionBlock ("", "DSDT", 2, "CUBIT", "MODDEPS", 1)
{
    Name (SEEN, Zero)
    OperationRegion (REG0, SystemMemory, 0x1000, 11)
#ifdef DATA_FIELD
    Field (REG0, ByteAcc, NoLock, Preserve) { SEL0, 8, WDAT, 80 }
    IndexField (SEL0, WDAT, ByteAcc, NoLock, Preserve) { Offset (2), FVAL, 8 }
#else
    Field (REG0, ByteAcc, NoLock, Preserve) { WSEL, 80, MDAT, 8 }
    BankField (REG0, WSEL, 0xFEDCBA9876543210, ByteAcc, NoLock, Preserve)
        { Offset (10), FVAL, 8 }
    BankField (REG0, WSEL, 0xFEDCBA9876543210, ByteAcc, NoLock, Preserve)
        { Offset (10), PART, 4 }
#endif
#ifdef ORACLE
    Field (REG0, ByteAcc, NoLock, Preserve) { RAW0, 88 }
    Method (TEST, 0) {
        Store (Buffer (11) {0xA5,0xA5,0xA5,0xA5,0xA5,0xA5,0xA5,0xA5,0xA5,0xA5,0xA5}, RAW0)
#endif
#ifdef DATA_FIELD
#ifdef DATA_READ
        Store (FVAL, SEEN)
#else
        Store (0xC3, FVAL)
#endif
#else
        Store (6, PART)
        Store (FVAL, SEEN)
#endif
#ifdef ORACLE
        Return (RAW0)
    }
#endif
}
