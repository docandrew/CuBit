DefinitionBlock ("", "DSDT", 2, "CUBIT", "EVNTSM", 1)
{
    Name (LVAL, One)
    Name (PVAL, One)
    Name (CAPA, 3000)
    Device (LID0) {
        Name (_HID, "PNP0C0D")
        Method (_LID, 0) { Return (LVAL) }
    }
    Device (AC00) {
        Name (_HID, "ACPI0003")
        Method (_PSR, 0) { Return (PVAL) }
    }
    Device (BAT0) {
        Name (_HID, "PNP0C0A")
        Name (_STA, 0x1F)
        Method (_BST, 0) { Return (Package () {One, 100, CAPA, 11000}) }
    }
    Device (TPD0) {
        Name (_HID, "TEST0001")
        Name (_CID, "PNP0C50")
        Name (_STA, 0x0F)
        Name (_CRS, ResourceTemplate () {
            I2cSerialBusV2 (0x15, ControllerInitiated, 400000, AddressingMode7Bit,
                "\\I2C0", 0, ResourceConsumer, , Exclusive)
            GpioInt (Level, ActiveLow, Exclusive, PullUp, 0, "\\GPIO", 0, ResourceConsumer) {0x2A}
        })
        Method (_DSM, 4) {
            If (LNotEqual (Arg0, ToUUID ("3CDFF6F7-4267-4555-AD05-B30A3D8938DE"))) { Return (Zero) }
            If (LNotEqual (Arg1, One)) { Return (Zero) }
            If (LEqual (Arg2, Zero)) { Return (Buffer () {3}) }
            If (LEqual (Arg2, One)) { Return (0x20) }
            Return (Zero)
        }
    }
    Method (EVT0, 0) {
        Store (Zero, LVAL)
        Store (Zero, PVAL)
        Store (2000, CAPA)
        Notify (LID0, 0x80)
        Notify (AC00, 0x80)
        Notify (BAT0, 0x80)
    }
}
