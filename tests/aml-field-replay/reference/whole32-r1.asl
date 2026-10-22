DefinitionBlock ("", "DSDT", 1, "CUBIT", "FLDCORE", 1)
{
    Name (MARK, Zero)
    OperationRegion (RGN0, SystemMemory, 0x4000, 4)
    Field (RGN0, DWordAcc, NoLock, Preserve) { FLD0, 32 }
    Method (SRC0, 0) {
        Multiply (MARK, 10, MARK)
        Add (MARK, One, MARK)
        Return (0x123456789ABCDEF0)
    }
    Method (BAD0, 0) {
        Multiply (MARK, 10, MARK)
        Add (MARK, 3, MARK)
        Store (Zero, Local0)
        Divide (One, Local0, Local1, Local2)
        Return (0x55)
    }
    Method (TEST, 0) {
        Store (Zero, MARK)
        Store (SRC0 (), FLD0)
        Store (FLD0, Local0)
        Multiply (MARK, 10, MARK)
        Add (MARK, 2, MARK)
        Return (Local0)
    }
    Method (FAIL, 0) { Store (BAD0 (), FLD0) Return (0x66) }
    Method (OBS0, 0) { Return (MARK) }
    Method (PROB, 0) { Return (FLD0) }
}
