DefinitionBlock ("", "DSDT", 2, "CUBIT", "HIDDSC", 1)
{
    Name (MODE, Zero)
    Name (CALL, Zero)
    Method (SETM, 1) { Store (Arg0, MODE) Store (Zero, CALL) }
    Name (PAIR, ResourceTemplate ()
    {
        I2cSerialBusV2 (0x15, ControllerInitiated, 400000, AddressingMode7Bit,
            "\\I2C0", 0, ResourceConsumer, , Exclusive, RawDataBuffer () {0xAA})
        GpioInt (Level, ActiveLow, Exclusive, PullUp, 0, "\\GPIO", 0, ResourceConsumer) {0x2A}
    })
    Name (I2ON, ResourceTemplate ()
    {
        I2cSerialBus (0x2FF, ControllerInitiated, 1000000, AddressingMode10Bit,
            "^I2C1", 2, ResourceConsumer)
    })
    Name (GPON, ResourceTemplate ()
    {
        GpioInt (Edge, ActiveHigh, SharedAndWake, PullNone, 123, "\\GPIO", 0, ResourceConsumer) {0x1234}
    })
    Name (I2DU, ResourceTemplate ()
    {
        I2cSerialBus (0x15, ControllerInitiated, 400000, AddressingMode7Bit, "\\I2C0", 0, ResourceConsumer)
        I2cSerialBus (0x16, ControllerInitiated, 400000, AddressingMode7Bit, "\\I2C1", 0, ResourceConsumer)
        GpioInt (Level, ActiveLow, Exclusive, PullUp, 0, "\\GPIO", 0, ResourceConsumer) {0x2A}
    })
    Name (GPDU, ResourceTemplate ()
    {
        I2cSerialBus (0x15, ControllerInitiated, 400000, AddressingMode7Bit, "\\I2C0", 0, ResourceConsumer)
        GpioInt (Level, ActiveLow, Exclusive, PullUp, 0, "\\GPIO", 0, ResourceConsumer) {0x2A}
        GpioInt (Level, ActiveHigh, Exclusive, PullDown, 0, "\\GPIO", 0, ResourceConsumer) {0x2B}
    })
    Name (OTHR, ResourceTemplate ()
    {
        IO (Decode16, 0x100, 0x100, 1, 8)
        GpioIo (Exclusive, PullNone, 0, 0, IoRestrictionInputOnly, "\\GPIO", 0, ResourceConsumer) {0x30}
        SpiSerialBus (1, PolarityLow, FourWireMode, 8, ControllerInitiated, 1000000,
            ClockPolarityLow, ClockPhaseFirst, "\\SPI0", 0, ResourceConsumer)
        I2cSerialBus (0x15, ControllerInitiated, 400000, AddressingMode7Bit, "\\I2C0", 0, ResourceConsumer)
        GpioInt (Level, ActiveLow, Exclusive, PullUp, 0, "\\GPIO", 0, ResourceConsumer) {0x2A}
    })
    Name (DEPS, ResourceTemplate ()
    {
        StartDependentFn (0, 0) { IO (Decode16, 0x100, 0x100, 1, 8) }
        EndDependentFn ()
    })
    Name (EMPT, ResourceTemplate () {})
    Method (CVAL, 0)
    {
        Increment (CALL)
        If (LEqual (MODE, 1)) { Return (I2ON) }
        If (LEqual (MODE, 2)) { Return (GPON) }
        If (LEqual (MODE, 3)) { Return (I2DU) }
        If (LEqual (MODE, 4)) { Return (GPDU) }
        If (LEqual (MODE, 5)) { Return (OTHR) }
        If (LEqual (MODE, 6)) { Return (DEPS) }
        If (LEqual (MODE, 7)) { Return (EMPT) }
        Return (PAIR)
    }
    Device (TPD0)
    {
        Name (_HID, "TEST0001")
        Name (_CID, Package () {"OTHER000", "PNP0C50"})
        Method (_STA, 0)
        {
            If (LEqual (MODE, 8)) { Return (0x0D) }
            If (LEqual (MODE, 11)) { Return (Zero) }
            If (LEqual (MODE, 12)) { Return (3) }
            Return (0x0F)
        }
        Method (_CRS, 0) { Return (CVAL ()) }
        Method (_DSM, 4)
        {
            If (LNotEqual (Arg0, ToUUID ("3CDFF6F7-4267-4555-AD05-B30A3D8938DE"))) { Return (Zero) }
            If (LNotEqual (Arg1, One)) { Return (Zero) }
            If (LEqual (Arg2, Zero)) { Return (Buffer () {3}) }
            If (LEqual (Arg2, One))
            {
                If (LEqual (MODE, 9)) { Return (Zero) }
                If (LEqual (MODE, 10)) { Return (0xFFFF) }
                Return (0x20)
            }
            Return (Zero)
        }
    }
    Name (RCNT, Zero)
    Method (_CRS, 0) { Increment (RCNT) Return (PAIR) }
    Device (MRES) { Name (_HID, "TEST0002") Name (_CID, "PNP0C50") }
    Device (ALRS)
    {
        Name (_HID, "TEST0003")
        Name (_CID, "PNP0C50")
        Alias (\PAIR, _CRS)
        Method (_DSM, 4) { Return (\TPD0._DSM (Arg0, Arg1, Arg2, Arg3)) }
    }
    Method (ORCL, 0) { Return (\TPD0._CRS ()) }
    // No-argument oracle wrapper supplies all four _DSM arguments, including
    // a real empty package rather than debugger command-line shorthand.
    Method (OREG, 0)
    {
        Return (\TPD0._DSM
            (ToUUID ("3CDFF6F7-4267-4555-AD05-B30A3D8938DE"),
             One, One, Package () {}))
    }
}
