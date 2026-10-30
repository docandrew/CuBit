DefinitionBlock ("", "DSDT", 2, "CUBIT", "POWSRC", 1)
{
    Name (MODE, Zero)
    Name (HMOD, Zero)
    Name (CALL, Zero)
    Name (HCNT, Zero)
    Name (RCNT, Zero)
    Method (SETM, 2)
    {
        Store (Arg0, MODE) Store (Arg1, HMOD)
        Store (Zero, CALL) Store (Zero, HCNT)
    }
    Method (PVAL, 0)
    {
        Increment (CALL)
        If (LEqual (MODE, Zero)) { Return (Zero) }
        If (LEqual (MODE, One)) { Return (One) }
        If (LEqual (MODE, 2)) { Return (2) }
        If (LEqual (MODE, 3)) { Return (0x100000001) }
        If (LEqual (MODE, 4)) { Return (Buffer () {1}) }
        If (LEqual (MODE, 5)) { Return ("1") }
        If (LEqual (MODE, 6)) { Return (Package () {1}) }
        If (LEqual (MODE, 7)) { While (One) { Noop } }
        Return (One)
    }
    Method (HVAL, 0)
    {
        Increment (HCNT)
        If (LEqual (HMOD, 1)) { Return ("ACPI0004") }
        If (LEqual (HMOD, 2)) { Return ("ACPI00030") }
        If (LEqual (HMOD, 3)) { Return (Buffer () {0x41,0x43,0x50,0x49,0x30,0x30,0x30,0x33}) }
        If (LEqual (HMOD, 4)) { Return (Package () {"ACPI0003"}) }
        If (LEqual (HMOD, 5)) { Return (EisaId ("PNP0C0A")) }
        If (LEqual (HMOD, 6)) { While (One) { Noop } }
        If (LEqual (HMOD, 7)) { Return ("ACPI000") }
        Return ("ACPI0003")
    }
    Method (_PSR, 0) { Increment (RCNT) Return (One) }
    Method (_HID, 0) { Increment (RCNT) Return ("ACPI0003") }
    Device (AC00)
    {
        Name (_STA, 0x0F)
        Method (_HID, 0) { Return (HVAL ()) }
        Method (_PSR, 0) { Return (PVAL ()) }
    }
    Device (NAM0) { Name (_HID, "ACPI0003") Name (_PSR, One) }
    Device (ALS0)
    {
        Name (_HID, "ACPI0003")
        Method (PSRC, 0) { Return (One) }
        Alias (PSRC, _PSR)
    }
    Device (ALH0)
    {
        Name (_ADR, Zero)
        Name (HWID, "ACPI0003") Alias (HWID, _HID)
        Name (_PSR, Zero)
    }
    Device (MIS0) { Name (_HID, "ACPI0003") }
    Device (MIH0) { Name (_ADR, Zero) }
    Method (ORCL, 0) { Return (\ALS0._PSR ()) }
}
