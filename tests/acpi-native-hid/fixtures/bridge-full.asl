DefinitionBlock ("", "DSDT", 2, "CUBIT", "HIDOWNER", 1)
{
    Device (TPD0)
    {
        Name (_HID, "TEST0001")
        Name (_CID, "PNP0C50")
        Name (_STA, 0x0F)
        Method (_CRS, 0)
        {
            Return (ResourceTemplate () {
                I2cSerialBusV2 (0x15, ControllerInitiated, 400000, AddressingMode7Bit,
                    "\\I2C0", 0, ResourceConsumer, , Exclusive)
                GpioInt (Level, ActiveLow, Exclusive, PullUp, 0,
                    "\\GPIO", 0, ResourceConsumer) {0x2A}
            })
        }
        Method (_DSM, 4)
        {
            If (LNotEqual (Arg0, ToUUID ("3CDFF6F7-4267-4555-AD05-B30A3D8938DE"))) { Return (Zero) }
            If (LNotEqual (Arg1, One)) { Return (Zero) }
            If (LEqual (Arg2, Zero)) { Return (Buffer () {3}) }
            If (LEqual (Arg2, One)) { Return (0x20) }
            Return (Zero)
        }
    }
    Device (I2C0) { Name (_HID, "TEST0002") }
    Device (GPIO) { Name (_HID, "INTC1057") }
    Device (TPD1)
    {
        Name (_HID, "TEST0003")
        Name (_CID, Package () {"OTHER000", "PNP0C50"})
        Name (_STA, 0x0F)
        Method (_CRS, 0) { Return (\TPD0._CRS ()) }
        Method (_DSM, 4) { Return (\TPD0._DSM (Arg0, Arg1, Arg2, Arg3)) }
    }
    Device (TP02) { Name (_HID, "TEST0004") Name (_CID, "PNP0C50") Name (_STA, 0x0F)
        Method (_CRS, 0) { Return (\TPD0._CRS ()) }
        Method (_DSM, 4) { Return (\TPD0._DSM (Arg0, Arg1, Arg2, Arg3)) } }
    Device (TP03) { Name (_HID, "TEST0004") Name (_CID, "PNP0C50") Name (_STA, 0x0F)
        Method (_CRS, 0) { Return (\TPD0._CRS ()) }
        Method (_DSM, 4) { Return (\TPD0._DSM (Arg0, Arg1, Arg2, Arg3)) } }
    Device (TP04) { Name (_HID, "TEST0004") Name (_CID, "PNP0C50") Name (_STA, 0x0F)
        Method (_CRS, 0) { Return (\TPD0._CRS ()) }
        Method (_DSM, 4) { Return (\TPD0._DSM (Arg0, Arg1, Arg2, Arg3)) } }
    Device (TP05) { Name (_HID, "TEST0004") Name (_CID, "PNP0C50") Name (_STA, 0x0F)
        Method (_CRS, 0) { Return (\TPD0._CRS ()) }
        Method (_DSM, 4) { Return (\TPD0._DSM (Arg0, Arg1, Arg2, Arg3)) } }
    Device (TP06) { Name (_HID, "TEST0004") Name (_CID, "PNP0C50") Name (_STA, 0x0F)
        Method (_CRS, 0) { Return (\TPD0._CRS ()) }
        Method (_DSM, 4) { Return (\TPD0._DSM (Arg0, Arg1, Arg2, Arg3)) } }
    Device (TP07) { Name (_HID, "TEST0004") Name (_CID, "PNP0C50") Name (_STA, 0x0F)
        Method (_CRS, 0) { Return (\TPD0._CRS ()) }
        Method (_DSM, 4) { Return (\TPD0._DSM (Arg0, Arg1, Arg2, Arg3)) } }
    Device (TP08) { Name (_HID, "TEST0004") Name (_CID, "PNP0C50") Name (_STA, 0x0F)
        Method (_CRS, 0) { Return (\TPD0._CRS ()) }
        Method (_DSM, 4) { Return (\TPD0._DSM (Arg0, Arg1, Arg2, Arg3)) } }
    Device (TP09) { Name (_HID, "TEST0004") Name (_CID, "PNP0C50") Name (_STA, 0x0F)
        Method (_CRS, 0) { Return (\TPD0._CRS ()) }
        Method (_DSM, 4) { Return (\TPD0._DSM (Arg0, Arg1, Arg2, Arg3)) } }
    Device (TP0A) { Name (_HID, "TEST0004") Name (_CID, "PNP0C50") Name (_STA, 0x0F)
        Method (_CRS, 0) { Return (\TPD0._CRS ()) }
        Method (_DSM, 4) { Return (\TPD0._DSM (Arg0, Arg1, Arg2, Arg3)) } }
    Device (TP0B) { Name (_HID, "TEST0004") Name (_CID, "PNP0C50") Name (_STA, 0x0F)
        Method (_CRS, 0) { Return (\TPD0._CRS ()) }
        Method (_DSM, 4) { Return (\TPD0._DSM (Arg0, Arg1, Arg2, Arg3)) } }
    Device (TP0C) { Name (_HID, "TEST0004") Name (_CID, "PNP0C50") Name (_STA, 0x0F)
        Method (_CRS, 0) { Return (\TPD0._CRS ()) }
        Method (_DSM, 4) { Return (\TPD0._DSM (Arg0, Arg1, Arg2, Arg3)) } }
    Device (TP0D) { Name (_HID, "TEST0004") Name (_CID, "PNP0C50") Name (_STA, 0x0F)
        Method (_CRS, 0) { Return (\TPD0._CRS ()) }
        Method (_DSM, 4) { Return (\TPD0._DSM (Arg0, Arg1, Arg2, Arg3)) } }
    Device (TP0E) { Name (_HID, "TEST0004") Name (_CID, "PNP0C50") Name (_STA, 0x0F)
        Method (_CRS, 0) { Return (\TPD0._CRS ()) }
        Method (_DSM, 4) { Return (\TPD0._DSM (Arg0, Arg1, Arg2, Arg3)) } }
    Device (TP0F) { Name (_HID, "TEST0004") Name (_CID, "PNP0C50") Name (_STA, 0x0F)
        Method (_CRS, 0) { Return (\TPD0._CRS ()) }
        Method (_DSM, 4) { Return (\TPD0._DSM (Arg0, Arg1, Arg2, Arg3)) } }
    Device (TP10) { Name (_HID, "TEST0004") Name (_CID, "PNP0C50") Name (_STA, 0x0F)
        Method (_CRS, 0) { Return (\TPD0._CRS ()) }
        Method (_DSM, 4) { Return (\TPD0._DSM (Arg0, Arg1, Arg2, Arg3)) } }
}
