DefinitionBlock ("", "DSDT", 2, "CUBIT", "PARTIO", 1)
{
    OperationRegion (RGN0, SystemMemory, 0x4000, 0x20)
    Field (RGN0, ByteAcc, NoLock, Preserve) { B000, 8, B001, 8, B002, 8, B003, 8, B004, 8, B005, 8, B006, 8, B007, 8, B008, 8, B009, 8, B010, 8, B011, 8 }
    Field (RGN0, ByteAcc, NoLock, Preserve) { , 7, VAL0, 12 }
    Method (RSET, 0, NotSerialized) {
        Store (0xA5, B000)
        Store (0x5A, B001)
        Store (0x3C, B002)
        Store (0xC3, B003)
        Store (0x96, B004)
        Store (0x69, B005)
        Store (0xF0, B006)
        Store (0x0F, B007)
        Store (0x33, B008)
        Store (0xCC, B009)
        Store (0x87, B010)
        Store (0x78, B011)
        Return (Zero)
    }
    Method (BUFR, 0, NotSerialized) { RSET () Store (Buffer (8) {0xC7, 0x34, 0x12, 0x5A, 0x96, 0x69, 0xA5, 0x3C}, VAL0) Return (VAL0) }
    Method (STRG, 0, NotSerialized) { RSET () Store ("Az2!", VAL0) Return (VAL0) }
    Method (P000, 0, NotSerialized) { Return (B000) }
    Method (P001, 0, NotSerialized) { Return (B001) }
    Method (P002, 0, NotSerialized) { Return (B002) }
    Method (P003, 0, NotSerialized) { Return (B003) }
    Method (P004, 0, NotSerialized) { Return (B004) }
    Method (P005, 0, NotSerialized) { Return (B005) }
    Method (P006, 0, NotSerialized) { Return (B006) }
    Method (P007, 0, NotSerialized) { Return (B007) }
    Method (P008, 0, NotSerialized) { Return (B008) }
    Method (P009, 0, NotSerialized) { Return (B009) }
    Method (P010, 0, NotSerialized) { Return (B010) }
    Method (P011, 0, NotSerialized) { Return (B011) }
}
