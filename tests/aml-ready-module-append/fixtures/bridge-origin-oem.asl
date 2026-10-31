DefinitionBlock ("", "OEM1", 2, "CUBIT", "ORIGIN", 1)
{
    Name (NEWV, 0x2A)
    Method (NVAL, 0) { Return (NEWV) }
    OperationRegion (RGN1, SystemMemory, 0x2000, 4)
    Field (RGN1, ByteAcc, NoLock, Preserve) { FLD1, 8 }
    DataTableRegion (TRG1, "DSDT", "", "")
    Field (TRG1, ByteAcc, NoLock, Preserve) { HDR1, 8 }
    Name (BUF1, Buffer () { 1, 2, 3 })
    CreateByteField (BUF1, Zero, BFL1)
    Event (EVT1)
    Mutex (MTX1, 0)
    Device (DVC1) { Name (_HID, "CUB0003") }
    External (\DEV0, DeviceObj)
    Alias (\DEV0, LNK1)
}
