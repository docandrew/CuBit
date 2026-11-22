DefinitionBlock ("", "DSDT", 2, "CUBIT", "MODRECIP", 1)
{
    Name (OUT0, Zero)
    OperationRegion (REG0, SystemMemory, 0x1000, 4)
    Field (REG0, ByteAcc, NoLock, Preserve) { BSEL, 8, IDAT, 8, MDAT, 8 }
    BankField (REG0, BSEL, 3, ByteAcc, NoLock, Preserve) { Offset (1), BNKX, 8 }
    IndexField (BNKX, MDAT, ByteAcc, NoLock, Preserve) { Offset (4), FVAL, 8 }
    IndexField (BNKX, MDAT, ByteAcc, NoLock, Preserve) { Offset (4), PART, 4 }
#ifdef ORACLE
    Method (TEST, 0) {
        Store (0xA5, MDAT)
        Store (6, PART)
        Store (FVAL, OUT0)
        Return (OUT0)
    }
#else
    Store (6, PART)
    Store (FVAL, OUT0)
#endif
}
