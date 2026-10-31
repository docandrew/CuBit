DefinitionBlock ("", "DSDT", 2, "CUBIT", "ECUID", 1)
{
    Name (_UID, Zero)
    Name (CNT0, Zero)
    Name (RCNT, Zero)
    Device (EC00) {
        Name (_HID, "PNP0C09") Name (_UID, Zero) Name (_GPE, 9)
        Method (_CRS, 0) {
            Increment (RCNT)
            Return (ResourceTemplate () {
                IO (Decode16, 0x62, 0x62, 0, 1)
                IO (Decode16, 0x66, 0x66, 0, 1)
            })
        }
    }
    Alias (EC00, ECOA)
    Device (EC01) { Name (_ADR, Zero) Name (UVAL, One) Alias (UVAL, _UID) }
    Device (EC02) { Name (_ADR, Zero) Method (_UID, 0) { Increment (CNT0) Return (2) } }
    Device (EC03) { Name (_ADR, Zero) Name (_UID, "2") }
    Device (EC04) { Name (_ADR, Zero) Method (BRET, 0) { Increment (CNT0) Return (Buffer () { 1, 2 }) } Alias (BRET, _UID) }
    Device (EC05) { Name (_ADR, Zero) Name (VALU, Zero) }
    Device (EC06) { Name (_ADR, Zero) Name (_UID, 0xFFFFFFFF) }
    Device (EC07) { Name (_ADR, Zero) Name (_UID, 0x100000000) }
    Device (BUS0) { Name (_ADR, Zero) Device (EC00) { Name (_ADR, Zero) Name (_UID, 7) } }
}
