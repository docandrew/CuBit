DefinitionBlock ("", "DSDT", 2, "CUBIT", "ORPHREG", 1)
{
    Name (CALL, Zero)
    Name (SPAC, Zero)
    Name (CODE, Zero)
    Name (IVAL, One)
    Method (RNOT, 2) { Increment (CALL) Store (Arg0, SPAC) Store (Arg1, CODE) }
    Alias (RNOT, RALS)
    Method (RPKG, 2) { Increment (CALL) Return (Package () {One, 2}) }
    Method (RARG, 1) { Increment (CALL) }
    Method (RFAT, 2) { Fatal (Zero, 0x1234, 0x5678) }
    Device (EC00) { Name (_HID, "CUB0001") Alias (\RNOT, _REG) }
    Device (GP00) { Name (_HID, "CUB0002") Alias (\RALS, _REG) }
    Device (MEM0) {
        Name (_HID, "CUB0003") Alias (\RNOT, _REG)
        OperationRegion (MEMR, SystemMemory, 0x1000, One)
    }
    Device (RGEC) {
        Name (_HID, "CUB0004") Alias (\RNOT, _REG)
        OperationRegion (ECXR, EmbeddedControl, 0x10, One)
    }
    Device (RGGP) {
        Name (_HID, "CUB0005") Alias (\RNOT, _REG)
        OperationRegion (GPXR, GeneralPurposeIO, Zero, One)
    }
    Device (DFRD) {
        Name (_HID, "CUB0006") Alias (\RNOT, _REG)
#ifdef DEFERRED_CASE
        DataTableRegion (UNKN, "ZZZZ", "", "")
#endif
    }
    Device (PARN) {
        Name (_HID, "CUB0007") Alias (\RNOT, _REG)
        Device (CHLD) { Name (_HID, "CUB0008") }
    }
    Device (BAD0) { Name (_HID, "CUB0009") Alias (\IVAL, _REG) }
    Device (CP00) { Name (_HID, "CUB0010") Alias (\RPKG, _REG) }
    Device (AR00) { Name (_HID, "CUB0011") Alias (\RARG, _REG) }
    Device (FT00) { Name (_HID, "CUB0012") Alias (\RFAT, _REG) }
}
