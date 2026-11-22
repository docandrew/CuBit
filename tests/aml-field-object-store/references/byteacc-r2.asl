DefinitionBlock ("", "DSDT", 2, "CUBIT", "WHOLEOBJ", 1)
{
    OperationRegion (RGN0, SystemMemory, 0x4000, 0x10)
    Field (RGN0, ByteAcc, NoLock, Preserve) { F001, 8, F002, 8, F003, 8 }
    Method (RSET, 0, NotSerialized) { Store (0xA5, F001) Store (0x5A, F002) Store (0x3C, F003) Return (Zero) }
    Method (BUFR, 0, NotSerialized) { RSET () Store (Buffer (8) {0xC7, 0x34, 0x12, 0x5A, 0x96, 0x69, 0xA5, 0x3C}, F001) Return (F001) }
    Method (STRG, 0, NotSerialized) { RSET () Store ("Az2!", F001) Return (F001) }
    Method (SDEC, 0, NotSerialized) { RSET () Store ("1234", F001) Return (F001) }
    Method (SMP0, 0, NotSerialized) { RSET () Store ("", F001) Return (F001) }
    Method (BMP0, 0, NotSerialized) { RSET () Store (Buffer (Zero) {}, F001) Return (F001) }
    Method (PR01, 0, NotSerialized) { Return (F001) }
    Method (PR02, 0, NotSerialized) { Return (F002) }
    Method (PR03, 0, NotSerialized) { Return (F003) }
}
