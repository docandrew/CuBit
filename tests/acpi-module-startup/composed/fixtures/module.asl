DefinitionBlock ("", "DSDT", 2, "CUBIT", "MODCOMPO", 1)
{
    Name (OUT0, Zero)
    OperationRegion (REG0, EmbeddedControl, Zero, 2)
    Field (REG0, ByteAcc, NoLock, Preserve) { SEL0, 8, DAT0, 8 }
#ifdef BANK
    BankField (REG0, SEL0, 3, ByteAcc, NoLock, Preserve) { Offset (1), FVAL, 8 }
#else
    IndexField (SEL0, DAT0, ByteAcc, NoLock, Preserve) { Offset (2), FVAL, 8 }
#endif
#ifdef ORACLE
    Method (TEST, 0) {
        Store (0xA5, FVAL)
        Store (FVAL, OUT0)
        Store (0xC3, FVAL)
        Return (OUT0)
    }
#else
    Store (FVAL, OUT0)
    Store (0xC3, FVAL)
#endif
}
