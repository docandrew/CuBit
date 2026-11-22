DefinitionBlock ("", "DSDT", 2, "CUBIT", "PASSIVE", 1)
{
    Name (HITS, Zero)
    OperationRegion (REG0, SystemMemory, 0x1000, One)
    Field (REG0, ByteAcc, NoLock, Preserve) { VAL0, 8 }
    Device (DEV0)
    {
        Name (_HID, "INTC1057")
        Name (_CID, Package () { EisaId ("PNP0C50"), "ELAN06FA", "CBT0001", "PCI\\VEN_1234&DEV_ABCD" })
        Name (_UID, "unit-a")
        Name (_ADR, 0x00010000)
        Name (_STA, 0x0F)
        Name (_CRS, ResourceTemplate () {
            Memory32Fixed (ReadWrite, 0xFD000000, 0x1000)
        })
    }
    Device (INT0)
    {
        Name (_HID, EisaId ("PNP0C50"))
        Name (_CID, "PNP0C50")
        Name (_UID, 7)
        Name (_CRS, Buffer () {0x79, 0})
    }
    Device (METH)
    {
        Method (_HID, 0) { Increment (HITS) Return ("INTC1057") }
        Method (_CID, 0) { Increment (HITS) Return ("PNP0C50") }
        Method (_UID, 0) { Increment (HITS) Return (7) }
        Method (_ADR, 0) { Increment (HITS) Return (0x00010000) }
        Method (_CRS, 0) { Increment (HITS) Return (Buffer () {0x79, 0}) }
        Method (_STA, 0) { Increment (HITS) Return (0x0F) }
    }
    Device (FILD) { Name (_ADR, Zero) Alias (\VAL0, _HID) Alias (\VAL0, _STA) }
    Device (NONE) { Name (_ADR, Zero) }
    Device (UNSP)
    {
        Name (_ADR, Zero)
        Name (BUF0, Buffer () {1})
        Alias (BUF0, _UID)
    }
    Device (ZSTA) { Name (_HID, "CBT0002") Name (_STA, Zero) }
    Device (ST32) { Name (_HID, "CBT0003") Name (_STA, 0x80000001) }
    Device (ALIA) { Name (_HID, "CBT0004") Name (FACT, 0x09) Alias (FACT, _STA) }
    Device (STTX) { Name (_HID, "CBT0005") Name (FACT, "bad-status") Alias (FACT, _STA) }
    Device (STBF) { Name (_HID, "CBT0006") Name (FACT, Buffer () {0x0F}) Alias (FACT, _STA) }
    Device (STPK) { Name (_HID, "CBT0007") Name (FACT, Package () {0x0F}) Alias (FACT, _STA) }
    Device (PARN)
    {
        Name (_HID, "CBT0008")
        Name (_STA, Zero)
        Device (CHLD) { Name (_HID, "CBT0009") Name (_ADR, 0x00010000) }
    }
    Device (DUPL)
    {
        Name (_HID, "PNP0C50")
        Name (_CID, Package () { "PNP0C50", EisaId ("PNP0C50") })
    }
    Device (MCID)
    {
        Method (_HID, 0) { Increment (HITS) Return ("PNP0C50") }
        Name (_CID, "PNP0C50")
    }
    Device (LCID)
    {
        Name (_HID, "CBT0010")
        Name (_CID, "VENDOR\\BUS_CHANNEL_0123456789_ABCDEFGHIJKLMNOPQRSTUVWXYZ_COMPLETE_IDENTIFIER_FINAL_Z")
        Name (_CRS, Buffer () {
            0x86,9,0,1,0,0,0,0xFD,0,0x10,0,0,
            0x89,6,0,1,0,0,0,0,0,0x79,0
        })
    }
    Device (ECID) {
        Name (_HID, "CBT0011") Name (_CID, Package () {})
        Name (_CRS, Buffer () {
            0x23,0x28,0,0x31,
            0x8C,32,0,1,1,1,0,9,0,0,0,0,0,0,23,0,0,29,0,35,0,0,0,
            7,0,7,0,0xFF,0xFF,0x5C,0x47,0x50,0x49,0x4F,0,
            0x8E,21,0,2,0,1,6,0,0,1,6,0,0xA0,0x86,1,0,0x2C,0,
            0x5C,0x49,0x32,0x43,0x30,0,
            0x71,0xAB,0x79,0
        })
    }
    Device (ACID)
    {
        Name (_HID, "CBT0012")
        Name (CIDS, Package () {"CBT0099", "PNP0C50"})
        Alias (CIDS, _CID)
        Name (_CRS, Buffer () {0x79,1})
    }
    Method (SETM, 1)
    {
        If (LEqual (Arg0, One)) { Store (Package () { Buffer () {1} }, \DEV0._CID) }
        If (LEqual (Arg0, 2)) { Store (Buffer () {1}, \DEV0._UID) }
        If (LEqual (Arg0, 3)) { Store ("bad-address", \DEV0._ADR) }
    }
}
