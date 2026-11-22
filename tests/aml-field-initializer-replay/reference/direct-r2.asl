DefinitionBlock ("", "DSDT", 2, "CUBIT", "INITIO", 1)
{
    Name (MARK, Zero)
    OperationRegion (RGN0, SystemMemory, 0x4000, 0x10)
    Field (RGN0, ByteAcc, NoLock, Preserve) { F001, 8, F002, 8, F003, 8 }
    Method (RSET, 0, NotSerialized) {
        Store (0x0F, F001) Store (Zero, F002) Store (Zero, F003)
        Store (Zero, MARK) Return (Zero)
    }
    Method (OBS0, 0, NotSerialized) { Return (MARK) }
    Method (PR01, 0, NotSerialized) { Return (F001) }
    Method (PR02, 0, NotSerialized) { Return (F002) }
    Method (PR03, 0, NotSerialized) { Return (F003) }
    Scope (\_SB) {
        Device (DVA0) {
            Name (_HID, "CUB0001")
            Field (\RGN0, ByteAcc, NoLock, Preserve) { _STA, 8 }
            Method (_INI, 0, NotSerialized) {
                Store (One, \MARK)
                Store (0x22, \F002)
                
                Store (0x12, \MARK)
                Store (0x33, \F003)
                Store (0x123, \MARK)
                
            }
        }
    }
}
