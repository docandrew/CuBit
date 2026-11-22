DefinitionBlock ("", "DSDT", 2, "CUBIT", "ECCRS", 1)
{
    Name (CNT0, Zero)
    Device (GPEB) { Name (_HID, "ACPI0006") }
    Device (ECIO) {
        Name (_HID, "PNP0C09") Name (_GLK, One) Name (_GPE, 9)
        Method (_CRS, 0) { Increment (CNT0) Return (ResourceTemplate () {
            IO (Decode16, 0x62, 0x62, 0, 1)
            IO (Decode16, 0x66, 0x66, 0, 1)
        }) }
    }
    Device (ECFI) {
        Name (_HID, EisaId ("PNP0C09")) Name (_GPE, 9)
        Name (_CRS, ResourceTemplate () { FixedIO (0x62, 1) FixedIO (0x66, 1) })
    }
    Device (ECFM) {
        Name (_HID, "PNP0C09") Name (_GLK, Zero) Name (_GPE, 9)
        Name (_CRS, ResourceTemplate () {
            Memory32Fixed (ReadOnly, 0xFF000003, 1)
            Memory32Fixed (ReadWrite, 0xFF000007, 1)
        })
    }
    Device (ECMM) {
        Name (_HID, "PNP0C09") Name (_GLK, Zero)
        Name (GPES, Package () { GPEB, 0x11 }) Alias (GPES, _GPE)
        Name (_CRS, ResourceTemplate () {
            QWordMemory (ResourceConsumer, PosDecode, MinFixed, MaxFixed,
                NonCacheable, ReadWrite, 0, 0x100001003, 0x100001003, 0, 1)
            QWordMemory (ResourceConsumer, PosDecode, MinFixed, MaxFixed,
                NonCacheable, ReadWrite, 0, 0x100001007, 0x100001007, 0, 1)
        })
    }
    Device (ECGI) {
        Name (_HID, "PNP0C09")
        Name (_CRS, ResourceTemplate () {
            Memory32Fixed (ReadWrite, 0xFF000003, 1)
            Memory32Fixed (ReadWrite, 0xFF000007, 1)
            GpioInt (Level, ActiveLow, Exclusive, PullUp, 0, "\\GPO0", 0, ResourceConsumer) {42}
        })
    }
    Device (ECXX) {
        Name (_HID, "PNP0C09") Name (_GPE, 9)
        Name (_CRS, ResourceTemplate () {
            FixedIO (0x62, 1) FixedIO (0x66, 1) FixedIO (0x68, 1)
        })
    }
    Device (ECNO) { Name (_HID, "PNP0C09") Name (_GPE, 9) Name (_CRS, ResourceTemplate () {}) }
    Device (ECIR) {
        Name (_HID, "PNP0C09") Name (_GPE, 9)
        Name (_CRS, ResourceTemplate () {
            IRQNoFlags () {1} FixedIO (0x62, 1) FixedIO (0x66, 1)
        })
    }
    Device (ECLM) {
        Name (_HID, "PNP0C09") Name (_GPE, 9)
        Name (_CRS, ResourceTemplate () {
            Memory32 (ReadWrite, 0xFE000003, 0xFE000003, 1, 1)
            Memory32 (ReadWrite, 0xFE000007, 0xFE000007, 1, 1)
        })
    }
    Device (ECAI) {
        Name (_HID, "PNP0C09") Name (_GPE, 9)
        Name (_CRS, ResourceTemplate () {
            WordIO (ResourceConsumer, MinFixed, MaxFixed, PosDecode, EntireRange, 0, 0x62, 0x62, 0, 1)
            DWordIO (ResourceConsumer, MinFixed, MaxFixed, PosDecode, EntireRange, 0, 0x66, 0x66, 0, 1)
        })
    }
    Device (ECEX) {
        Name (_HID, "PNP0C09") Name (_GPE, 9)
        Name (_CRS, ResourceTemplate () {
            Interrupt (ResourceConsumer, Level, ActiveLow, Exclusive) {9}
            FixedIO (0x62, 1) FixedIO (0x66, 1)
        })
    }
    Device (ECDP) {
        Name (_HID, "PNP0C09")
        Name (_CRS, ResourceTemplate () {
            FixedIO (0x62, 1) FixedIO (0x66, 1)
            GpioInt (Level, ActiveLow, Exclusive, PullUp, 0, "\\GPO0", 0, ResourceConsumer) {42}
            GpioInt (Level, ActiveLow, Exclusive, PullUp, 0, "\\GPO0", 0, ResourceConsumer) {43}
        })
    }
}
