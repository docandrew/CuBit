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
}
