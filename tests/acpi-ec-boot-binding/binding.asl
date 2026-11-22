DefinitionBlock ("", "DSDT", 2, "CUBIT", "ECUID", 1)
{
    Name (_UID, Zero)
    Name (CNT0, Zero)
    Device (EC00) { Name (_HID, "PNP0C09") Name (_UID, Zero) }
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
