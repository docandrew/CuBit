DefinitionBlock ("", "DSDT", 2, "CUBIT", "DEPS", 1)
{
    Name (HITS, Zero)
    Device (GPO0) { Name (_HID, "INTC1057") }
    Device (I2C0) { Name (_HID, "CBT0001") }
    Alias (GPO0, GPAL)
    Name (DLST, Package () { GPO0 })
    Device (TPD0) {
        Name (_HID, "ELAN06FA")
        Name (_CID, "PNP0C50")
        Name (_DEP, Package () { GPO0, I2C0, GPO0 })
    }
    Device (ALS0) { Name (_HID, "CBT0002") Name (_DEP, Package () { GPAL }) }
    Device (ALDP) { Name (_HID, "CBT0003") Alias (\DLST, _DEP) }
    Device (EMP0) { Name (_HID, "CBT0004") Name (_DEP, Package () {}) }
    Device (NONE) { Name (_HID, "CBT0005") }
    Device (BAD0) { Name (_HID, "CBT0006") Name (BADP, Package () { GPO0, 7 }) Alias (BADP, _DEP) }
    Device (WRNG) { Name (_HID, "CBT0007") Name (VAL0, Zero) Alias (VAL0, _DEP) }
    Device (METH) {
        Name (_HID, "CBT0008")
        Method (_DEP, 0) { Increment (HITS) Return (Package () { GPO0 }) }
    }
    Device (PARN) {
        Name (_HID, "CBT0009")
        Name (_DEP, Package () { GPO0 })
        Device (CHLD) { Name (_HID, "CBT0010") }
    }
    Device (SELF) { Name (_HID, "CBT0011") Name (_DEP, Package () { SELF }) }
    Device (CYCA) { Name (_HID, "CBT0012") Name (_DEP, Package () { CYCB }) }
    Device (CYCB) { Name (_HID, "CBT0013") Name (_DEP, Package () { CYCA }) }
    Device (DYN0) {
        Name (_HID, "CBT0014")
        Method (_DEP, 0) { Increment (HITS) Return (Package () { GPO0, I2C0, GPO0 }) }
    }
    Method (NPKG, 0) { Increment (HITS) Return (One) }
    Device (WTYP) { Name (_HID, "CBT0015") Alias (\NPKG, _DEP) }
    Method (NSTR, 0) { Increment (HITS) Return ("not-a-device-list") }
    Device (STR0) { Name (_HID, "CBT0018") Alias (\NSTR, _DEP) }
    Name (ZVAL, Zero)
    Device (ERR0) {
        Name (_HID, "CBT0019")
        Method (_DEP, 0) {
            Divide (One, ZVAL, Local0, Local1)
            Return (Package () { GPO0 })
        }
    }
    Device (BMOD) {
        Name (_HID, "CBT0016")
        Name (BVAL, Package () { GPO0, 7 })
        Method (_DEP, 0) { Increment (HITS) Return (BVAL) }
    }
    Device (BUD0) {
        Name (_HID, "CBT0017")
        Method (_DEP, 0) {
            Increment (HITS) Increment (HITS) Increment (HITS) Increment (HITS)
            Increment (HITS) Increment (HITS) Increment (HITS) Increment (HITS)
            Return (Package () { GPO0 })
        }
    }
}
