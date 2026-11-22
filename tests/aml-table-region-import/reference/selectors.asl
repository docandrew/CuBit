DefinitionBlock ("", "DSDT", 2, "CUBIT", "TREGMOD", 1)
{
    Name (SIG0, "DSDT")
    Name (OEM0, "CUBIT")
    Name (TID0, "TREGMOD")
    Name (CNT0, Zero)
    Method (SGN0, 0) { Increment (CNT0) Return (SIG0) }
    Method (OEM1, 0) { Increment (CNT0) Return (OEM0) }
    Method (TID1, 0) { Increment (CNT0) Return (TID0) }
    DataTableRegion (REG0, SIG0, OEM0, TID0)
    Field (REG0, ByteAcc, NoLock, Preserve) { Offset (8), REV0, 8 }
    DataTableRegion (REG1, SGN0 (), OEM1 (), TID1 ())
    Field (REG1, ByteAcc, NoLock, Preserve) { Offset (8), REV1, 8 }
    Method (READ, 0) { Return (Package () { REV0, REV1 }) }
    Method (RDN0, 0) { Return (REV0) }
    Method (RDN1, 0) { Return (REV1) }
}
