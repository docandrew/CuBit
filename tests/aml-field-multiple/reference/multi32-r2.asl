DefinitionBlock ("", "DSDT", 2, "CUBIT", "MULTIFLD", 1)
{
    Name (MARK, Zero)
    OperationRegion (RGN0, SystemMemory, 0x4000, 0x10)
    Field (RGN0, DWordAcc, NoLock, Preserve) { F001, 32, F002, 32, F003, 32 }
    Method (SRC0, 0) { Multiply (MARK, 10, MARK) Add (MARK, One, MARK) Return (0x12345678) }
    Method (SRC1, 0) { Multiply (MARK, 10, MARK) Add (MARK, 2, MARK) Return (Add (F001, One)) }
    Method (BAD0, 0) {
        Multiply (MARK, 10, MARK) Add (MARK, 3, MARK)
        Store (Zero, Local0) Divide (One, Local0, Local1, Local2) Return (One)
    }
    Method (TEST, 0) {
        Store (Zero, MARK)
        Store (SRC0 (), F001) Store (SRC1 (), F002) Store (0x77, F003)
        Return (Add (F001, F002))
    }
    Method (FAIL, 0) {
        Store (One, MARK) Store (0x87654321, F001)
        Store (BAD0 (), F002) Store (0x55, F003) Return (0x99)
    }
    Method (OBS0, 0) { Return (MARK) }
    Method (PR01, 0) { Return (F001) }
    Method (PR02, 0) { Return (F002) }
    Method (PR03, 0) { Return (F003) }
}
